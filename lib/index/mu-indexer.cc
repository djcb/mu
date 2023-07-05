/*
** Copyright (C) 2020-2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <algorithm>
#include <mutex>
#include <vector>
#include <thread>
#include <condition_variable>
#include <iostream>
#include <atomic>
#include <chrono>
using namespace std::chrono_literals;

#include "mu-scanner.hh"
#include "utils/mu-async-queue.hh"
#include "utils/mu-error.hh"
#include "../mu-store.hh"

using namespace Mu;

struct IndexState {
	enum State { Idle,
		     Scanning,
		     Finishing,
		     Cleaning
	};
	static const char* name(State s) {
		switch (s) {
		case Idle:
			return "idle";
		case Scanning:
			return "scanning";
		case Finishing:
			return "finishing";
		case Cleaning:
			return "cleaning";
		default:
			return "<error>";
		}
	}

	bool operator==(State rhs) const {
		return state_.load() == rhs;
	}
	bool operator!=(State rhs) const {
		return state_.load() != rhs;
	}
	void change_to(State new_state) {
		mu_debug("changing indexer state {}->{}", name((State)state_),
			name((State)new_state));
		state_.store(new_state);
	}

private:
	std::atomic<State> state_{Idle};
};

struct Indexer::Private {
	Private(Mu::Store& store)
	    : store_{store}, scanner_{store_.root_maildir(),
				      [this](auto&& path,
					     auto&& statbuf, auto&& info) {
					      return handler(path, statbuf, info);
				      }},
	      max_message_size_{store_.config().get<Mu::Config::Id::MaxMessageSize>()} {
		mu_message("created indexer for {} -> {} (batch-size: {})",
			  store.root_maildir(), store.path(),
			   store.config().get<Mu::Config::Id::BatchSize>());
	}

	~Private() {
		stop();
	}

	bool dir_predicate(const std::string& path, const struct dirent* dirent) const;
	bool handler(const std::string& fullpath, struct stat* statbuf, Scanner::HandleType htype);

	void maybe_start_worker();
	void item_worker();
	void scan_worker();

	bool add_message(const std::string& path);

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

	struct WorkItem {
		std::string full_path;
		enum Type {
			Dir,
			File
		};
		Type type;
	};

	AsyncQueue<WorkItem> todos_;

	Progress   progress_;
	IndexState state_;
	std::mutex lock_, w_lock_;

	std::atomic<time_t> completed_;
};

bool
Indexer::Private::handler(const std::string& fullpath, struct stat* statbuf,
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
		if (conf_.lazy_check && dirstamp_ >= statbuf->st_ctime &&
		    htype == Scanner::HandleType::EnterNewCur) {
			mu_debug("skip {} (seems up-to-date: {} >= {})", fullpath,
				 time_to_string("%FT%T", dirstamp_),
				 time_to_string("%FT%T", statbuf->st_ctime));
			return false;
		}

		// don't index dirs with '.noindex'
		auto noindex = ::access((fullpath + "/.noindex").c_str(), F_OK) == 0;
		if (noindex) {
			mu_debug("skip {} (has .noindex)", fullpath);
			return false; // don't descend into this dir.
		}

		// don't index dirs with '.noupdate', unless we do a full
		// (re)index.
		if (!conf_.ignore_noupdate) {
			auto noupdate = ::access((fullpath + "/.noupdate").c_str(), F_OK) == 0;
			if (noupdate) {
				mu_debug("skip {} (has .noupdate)", fullpath);
				return false;
			}
		}

		mu_debug("checked {}", fullpath);
		return true;
	}
	case Scanner::HandleType::LeaveDir: {
		todos_.push({fullpath, WorkItem::Type::Dir});
		return true;
	}

	case Scanner::HandleType::File: {
		++progress_.checked;

		if ((size_t)statbuf->st_size > max_message_size_) {
			mu_debug("skip {} (too big: {} bytes)", fullpath, statbuf->st_size);
			return false;
		}

		// if the message is not in the db yet, or not up-to-date, queue
		// it for updating/inserting.
		if (statbuf->st_ctime <= dirstamp_ && store_.contains_message(fullpath))
			return false;

		// push the remaining messages to our "todo" queue for
		// (re)parsing and adding/updating to the database.
		todos_.push({fullpath, WorkItem::Type::File});
		return true;
	}
	default:
		g_return_val_if_reached(false);
		return false;
	}
}

void
Indexer::Private::maybe_start_worker()
{
	std::lock_guard lock{w_lock_};

	if (todos_.size() > workers_.size() && workers_.size() < max_workers_) {
		workers_.emplace_back(std::thread([this] { item_worker(); }));
		mu_debug("added worker {}", workers_.size());
	}
}

bool
Indexer::Private::add_message(const std::string& path)
{
	/*
	 * Having the lock here makes things a _lot_ slower.
	 *
	 * The reason for having the lock is some helgrind warnings;
	 * but it believed those are _false alarms_
	 * https://gitlab.gnome.org/GNOME/glib/-/issues/2662
	 *
	 *	std::unique_lock lock{w_lock_};
	 */
	auto msg{Message::make_from_path(path)};
	if (!msg) {
		mu_warning("failed to create message from {}: {}", path, msg.error().what());
		return false;
	}
	auto res = store_.add_message(msg.value(), true /*use-transaction*/);
	if (!res) {
		mu_warning("failed to add message @ {}: {}", path, res.error().what());
		return false;
	}

	return true;
}

void
Indexer::Private::item_worker()
{
	WorkItem item;

	mu_debug("started worker");

	while (state_ == IndexState::Scanning) {
		if (!todos_.pop(item, 250ms))
			continue;
		try {
			switch (item.type) {
			case WorkItem::Type::File: {
				if (G_LIKELY(add_message(item.full_path)))
					++progress_.updated;
			} break;
			case WorkItem::Type::Dir:
				store_.set_dirstamp(item.full_path, ::time(NULL));
				break;
			default:
				g_warn_if_reached();
				break;
			}
		} catch (const Mu::Error& er) {
			mu_warning("error adding message @ {}: {}", item.full_path, er.what());
		}

		maybe_start_worker();
		std::this_thread::yield();
	}
}

bool
Indexer::Private::cleanup()
{
	mu_debug("starting cleanup");

	size_t                 n{};
	std::vector<Store::Id> orphans; // store messages without files.
	store_.for_each_message_path([&](Store::Id id, const std::string& path) {
		++n;
		if (::access(path.c_str(), R_OK) != 0) {
			mu_debug("cannot read {} (id={}); queuing for removal from store",
				 path, id);
			orphans.emplace_back(id);
		}

		return state_ == IndexState::Cleaning;
	});

	if (orphans.empty())
		mu_debug("nothing to clean up");
	else {
		mu_debug("removing {} stale message(s) from store", orphans.size());
		store_.remove_messages(orphans);
		progress_.removed += orphans.size();
	}

	return true;
}

void
Indexer::Private::scan_worker()
{
	progress_.reset();

	if (conf_.scan) {
		mu_debug("starting scanner");
		if (!scanner_.start()) { // blocks.
			mu_warning("failed to start scanner");
			state_.change_to(IndexState::Idle);
			return;
		}
		mu_debug("scanner finished with {} file(s) in queue", todos_.size());
	}

	// now there may still be messages in the work queue...
	// finish those; this is a bit ugly; perhaps we should
	// handle SIGTERM etc.

	if (!todos_.empty()) {
		const auto workers_size = std::invoke([this] {
			std::lock_guard lock{w_lock_};
			return workers_.size();
		});
		mu_debug("process {} remaining message(s) with {} worker(s)",
			todos_.size(), workers_size);
		while (!todos_.empty())
			std::this_thread::sleep_for(100ms);
	}
	// and let the worker finish their work.
	state_.change_to(IndexState::Finishing);
	for (auto&& w : workers_)
		if (w.joinable())
			w.join();

	if (conf_.cleanup) {
		mu_debug("starting cleanup");
		state_.change_to(IndexState::Cleaning);
		cleanup();
		mu_debug("cleanup finished");
	}

	completed_ = ::time({});
	state_.change_to(IndexState::Idle);
}

bool
Indexer::Private::start(const Indexer::Config& conf)
{
	stop();

	conf_ = conf;
	if (conf_.max_threads == 0) {
		/* benchmarking suggests that ~4 threads is the fastest (the
		 * real bottleneck is the database, so adding more threads just
		 * slows things down)
		 */
		max_workers_ = std::min(4U, std::thread::hardware_concurrency());
	} else
		max_workers_ = conf.max_threads;

	mu_debug("starting indexer with <= {} worker thread(s)", max_workers_);
	mu_debug("indexing: {}; clean-up: {}", conf_.scan ? "yes" : "no",
		 conf_.cleanup ? "yes" : "no");

	state_.change_to(IndexState::Scanning);
	/* kick off the first worker, which will spawn more if needed. */
	workers_.emplace_back(std::thread([this] { item_worker(); }));
	/* kick the disk-scanner thread */
	scanner_worker_ = std::thread([this] { scan_worker(); });

	mu_debug("started indexer");

	return true;
}

bool
Indexer::Private::stop()
{
	scanner_.stop();

	todos_.clear();
	if (scanner_worker_.joinable())
		scanner_worker_.join();

	state_.change_to(IndexState::Idle);
	for (auto&& w : workers_)
		if (w.joinable())
			w.join();
	workers_.clear();

	return true;
}

Indexer::Indexer(Store& store)
    : priv_{std::make_unique<Private>(store)}
{}

Indexer::~Indexer() = default;

bool
Indexer::start(const Indexer::Config& conf)
{
	const auto mdir{priv_->store_.root_maildir()};
	if (G_UNLIKELY(access(mdir.c_str(), R_OK) != 0)) {
		mu_critical("'{}' is not readable: {}", mdir, g_strerror(errno));
		return false;
	}

	std::lock_guard lock(priv_->lock_);
	if (is_running())
		return true;

	return priv_->start(conf);
}

bool
Indexer::stop()
{
	std::lock_guard lock{priv_->lock_};

	if (!is_running())
		return true;

	mu_debug("stopping indexer");
	return priv_->stop();
}

bool
Indexer::is_running() const
{
	return priv_->state_ != IndexState::Idle;
}

const Indexer::Progress&
Indexer::progress() const
{
	priv_->progress_.running = priv_->state_ == IndexState::Idle ? false : true;

	return priv_->progress_;
}

time_t
Indexer::completed() const
{
	return priv_->completed_;
}
