/*
** Copyright (C) 2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
**
** This program is free software; you can redistribute it and/or modify it
** under the terms of the GNU General Public License as published by the
** Free Software Foundation; either version 3, or (at your option) any
** later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software Foundation,
** Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
**
*/

#include "mu-indexer.hh"

#include <config.h>

#include <atomic>
#include <mutex>
#include <vector>
#include <thread>
#include <condition_variable>
#include <iostream>
#include <atomic>
#include <chrono>
using namespace std::chrono_literals;

#include <xapian.h>

#include "mu-scanner.hh"
#include "utils/mu-async-queue.hh"
#include "utils/mu-error.hh"
#include "../mu-store.hh"

using namespace Mu;

struct IndexState {
        enum State { Idle, Scanning, Cleaning };
        static const char* name(State s) {
                switch(s) {
                case Idle:     return "idle";
                case Scanning: return "scanning";
                case Cleaning: return "cleaning";
                default:       return "<error>";
                }
        }

        bool operator==(State rhs) const { return state_ == rhs; }

        void change_to(State new_state) {
                g_debug ("changing indexer state %s->%s",
                         name((State)state_), name((State)new_state));
                state_ = new_state;
        }

private:
        State state_{Idle};
};

struct Indexer::Private {
        Private (Mu::Store& store):
                store_{store},
                scanner_{store_.metadata().root_maildir,
                [this](auto&& path, auto&& statbuf, auto&& info){
                        return handler(path, statbuf, info);
                }},
                max_message_size_{store_.metadata().max_message_size} {

                g_message ("created indexer for %s -> %s",
                           store.metadata().root_maildir.c_str(),
                           store.metadata().database_path.c_str());
        }

        ~Private() { stop(); }

        bool dir_predicate (const std::string& path, const struct dirent* dirent) const;
        bool handler (const std::string& fullpath, struct stat *statbuf,
                           Scanner::HandleType htype);

        void maybe_start_worker();
        void worker();

        bool cleanup();

        bool start(const Indexer::Config& conf);
        bool stop();

        Indexer::Config conf_;
        Store&          store_;
        Scanner         scanner_;
        const size_t    max_message_size_;

        time_t                   dirstamp_{};
        std::size_t              max_workers_;
        std::vector<std::thread> workers_;
        std::thread              scanner_worker_;

        AsyncQueue<std::string> fq_;

        Progress   progress_;
        IndexState state_;

        std::mutex lock_, wlock_;
};


bool
Indexer::Private::handler (const std::string& fullpath, struct stat *statbuf,
                           Scanner::HandleType htype)
{
        switch (htype) {
        case Scanner::HandleType::EnterDir:
        case Scanner::HandleType::EnterNewCur: {
                // in lazy-mode, we ignore this dir if its dirstamp suggest it
                // is up-to-date (this is _not_ always true; hence we call it
                // lazy-mode); only for actual message dirs, since the dir
                // tstamps may not bubble up.
                dirstamp_ = store_.dirstamp(fullpath);
                if (conf_.lazy_check &&
                    dirstamp_ == statbuf->st_mtime &&
                    htype == Scanner::HandleType::EnterNewCur) {
                        g_debug("skip %s (seems up-to-date)", fullpath.c_str());
                        return false;
                }

                // don't index dirs with '.noindex'
                auto noindex = ::access((fullpath + "/.noindex").c_str(), F_OK) == 0;
                if (noindex) {
                        g_debug ("skip %s (has .noindex)", fullpath.c_str());
                        return false; // don't descend into this dir.
                }

                // don't index dirs with '.noupdate', unless we do a full
                // (re)index.
                if (!conf_.ignore_noupdate) {
                        auto noupdate = ::access((fullpath + "/.noupdate").c_str(), F_OK) == 0;
                        if (noupdate) {
                                g_debug ("skip %s (has .noupdate)", fullpath.c_str());
                                return false;
                        }
                }

                g_debug ("process %s", fullpath.c_str());
                return true;

        }
        case Scanner::HandleType::LeaveDir: {
                store_.set_dirstamp(fullpath, statbuf->st_mtime);
                return true;
        }

        case Scanner::HandleType::File: {

                if ((size_t)statbuf->st_size > max_message_size_) {
                        g_debug ("skip %s (too big: %" G_GINT64_FORMAT " bytes)",
                                 fullpath.c_str(), (gint64)statbuf->st_size);
                        return false;
                }

                // if the message is not in the db yet, or not up-to-date, queue
                // it for updating/inserting.
                if (statbuf->st_mtime <= dirstamp_ &&
                    store_.contains_message (fullpath))  {
                        //g_debug ("skip %s: already up-to-date");
                        return false;
                }

                fq_.push(std::string{fullpath});
                return true;
        }
        default:
                g_return_val_if_reached (false);
                return false;
        }
}

void
Indexer::Private::maybe_start_worker()
{
        std::lock_guard<std::mutex> wlock{wlock_};

        if (fq_.size() > workers_.size() && workers_.size() < max_workers_)
                workers_.emplace_back(std::thread([this]{worker();}));
}

void
Indexer::Private::worker()
{
        std::string item;

        g_debug ("started worker");

        while (state_ == IndexState::Scanning || !fq_.empty()) {

                if (!fq_.pop (item, 250ms))
                        continue;

                //g_debug ("popped (n=%zu) path %s", fq_.size(), item.c_str());
                ++progress_.processed;

                try {
                        store_.add_message(item);
                        ++progress_.updated;

                } catch (const Mu::Error& er) {
                        g_warning ("error adding message @ %s: %s",
                                   item.c_str(), er.what());
                }

                maybe_start_worker();
        }
}

bool
Indexer::Private::cleanup()
{
        g_debug ("starting cleanup");

        size_t n{};
        std::vector<Store::Id> orphans; // store messages without files.
        store_.for_each_message_path([&](Store::Id id, const std::string &path) {

                ++n;
                if (::access(path.c_str(), R_OK) != 0) {
                        g_debug ("cannot read %s (id=%u); queueing for removal from store",
                                 path.c_str(), id);
                        orphans.emplace_back(id);
                }

                return state_ == IndexState::Cleaning;
        });

        g_debug("remove %zu message(s) from store", orphans.size());
        store_.remove_messages (orphans);
        progress_.removed += orphans.size();

        return true;
}


bool
Indexer::Private::start(const Indexer::Config& conf)
{
        stop();

        conf_ = conf;
        if (conf_.max_threads == 0)
                max_workers_ = std::thread::hardware_concurrency();
        else
                max_workers_ = conf.max_threads;

        g_debug ("starting indexer with <= %zu worker thread(s)", max_workers_);
        g_debug ("indexing: %s; clean-up: %s",
                 conf_.scan ? "yes" : "no",
                 conf_.cleanup ? "yes" : "no");

        workers_.emplace_back(std::thread([this]{worker();}));

        state_.change_to(IndexState::Scanning);
        scanner_worker_ = std::thread([this]{
                progress_ = {};

                if (conf_.scan) {
                        g_debug("starting scanner");
                        if (!scanner_.start()) { // blocks.
                                g_warning ("failed to start scanner");
                                goto leave;
                        }
                        g_debug ("scanner finished with %zu file(s) in queue",
                                 fq_.size());
                }

                // now there may still be messages in the work queue...
                // finish those; it's possible that the thread has already
                // stopped, so stop when there's no progress. A bit ugly, fixed
                // in >= 1.7
                {
                        while (!fq_.empty()) {
                                const auto n = fq_.size();
                                std::this_thread::sleep_for(250ms);
                                if (fq_.size() >= n)
                                        break; // no progress.
                        }
                }

                if (conf_.cleanup) {
                        g_debug ("starting cleanup");
                        state_.change_to(IndexState::Cleaning);
                        cleanup();
                        g_debug ("cleanup finished");
                }

                store_.commit();
                leave:
                state_.change_to(IndexState::Idle);
        });

        g_debug ("started indexer");

        return true;
}

bool
Indexer::Private::stop()
{
        scanner_.stop();
        state_.change_to(IndexState::Idle);

        const auto w_n = workers_.size();

        fq_.clear();
        if (scanner_worker_.joinable())
                scanner_worker_.join();

        for (auto&& w: workers_)
                if (w.joinable())
                        w.join();
        workers_.clear();

        if (w_n > 0)
                g_debug ("stopped indexer (joined %zu worker(s))", w_n);

        return true;
}

Indexer::Indexer (Store& store):
        priv_{std::make_unique<Private>(store)}
{}

Indexer::~Indexer() = default;

bool
Indexer::start(const Indexer::Config& conf)
{
        const auto mdir{priv_->store_.metadata().root_maildir};
        if (G_UNLIKELY(access (mdir.c_str(), R_OK) != 0)) {
                g_critical("'%s' is not readable: %s", mdir.c_str(), g_strerror (errno));
                return false;
        }

        std::lock_guard<std::mutex> l(priv_->lock_);
        if (is_running())
                return true;

        return priv_->start(conf);
}

bool
Indexer::stop()
{
        std::lock_guard<std::mutex> l(priv_->lock_);

        if (!is_running())
                return true;

        g_debug ("stopping indexer");
        return priv_->stop();
}

bool
Indexer::is_running() const
{
        return !(priv_->state_ == IndexState::Idle) || !priv_->fq_.empty();
}

Indexer::Progress
Indexer::progress() const
{
        priv_->progress_.running =
                priv_->state_ == IndexState::Idle ? false : true;

        return priv_->progress_;
}
