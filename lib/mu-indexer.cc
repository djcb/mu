/*
** Copyright (C) 2020-2024 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <string_view>
#include <chrono>
using namespace std::chrono_literals;

#include "mu-store.hh"

#include "mu-scanner.hh"
#include "utils/mu-async-queue.hh"
#include "utils/mu-error.hh"

#include "utils/mu-utils-file.hh"

using namespace Mu;

// states

enum struct State { Idle, Scanning, Draining };
constexpr std::string_view
format_as(State s)
{
	switch (s) {
	case State::Idle:     return "idle";
	case State::Scanning: return "scanning";
	case State::Draining: return "draining";
	default: return "<error>";
	}
}

struct Indexer::Private {
	Private(Mu::Store& store)
	    : store_{store},
	      store_worker_{store.store_worker()},
	      scanner_{store_.root_maildir(),
		[this](auto&& path,
		       auto&& statbuf, auto&& info) {
			return scan_handler(path, statbuf, info);
		}},
	      max_message_size_{store_.config().get<Mu::Config::Id::MaxMessageSize>()},
	      was_empty_{store.empty()} {

		mu_message("created indexer for {} -> "
			   "{} (batch-size: {}; was-empty: {}; ngrams: {})",
			   store.root_maildir(), store.path(),
			   store.config().get<Mu::Config::Id::BatchSize>(),
			   was_empty_,
			   store.config().get<Mu::Config::Id::SupportNgrams>());
	}

	~Private() { force_cleanup(); }

	void force_cleanup() {
		switch_state(State::Idle);
		scanner_.stop();
		if (scanner_worker_.joinable())
			scanner_worker_.join();
		msg_paths_.clear();
		for (auto&& w : workers_)
			if (w.joinable())
				w.join();
		store_worker_.clear();
	}

	bool dir_predicate(const std::string& path, const struct dirent* dirent) const;
	bool scan_handler(const std::string& fullpath, struct stat* statbuf,
			  Scanner::HandleType htype);

	void maybe_start_worker();
	void item_worker();
	void scan_worker();

	bool start(const Indexer::Config& conf, bool block);
	bool stop();

	bool is_running() const { return state_ != State::Idle; }
	void switch_state(State new_state) {
		mu_debug("changing indexer state {}->{}", state_, new_state);
		state_ = new_state;
	}

	// pace a bit so scan_items queue doesn't get too big.
	void pace_scan_worker() {
		while (msg_paths_.size() > 8 * max_workers_) {
			std::this_thread::sleep_for(25ms);
			continue;
		}
	}
	// pace a bit so store-worker queue doesn't get too big.
	void pace_store_worker() {
		while (store_worker_.size() > 8) {
			std::this_thread::sleep_for(25ms);
			continue;
		}
	}

	Indexer::Config conf_;
	const Store&    store_;
	StoreWorker&    store_worker_;
	Scanner         scanner_;
	const size_t    max_message_size_;

	::time_t                 dirstamp_{};
	std::size_t              max_workers_;
	std::vector<std::thread> workers_;
	std::thread              scanner_worker_;

	AsyncQueue<std::string> msg_paths_;

	Progress   progress_{};
	std::atomic<State> state_{State::Idle};
	std::mutex lock_, w_lock_;
	std::atomic<time_t> completed_{};
	bool was_empty_{};
};

bool
Indexer::Private::scan_handler(const std::string& fullpath, struct stat* statbuf,
			       Scanner::HandleType htype)
{
	switch (htype) {
	case Scanner::HandleType::EnterDir:
	case Scanner::HandleType::EnterNewCur: {
		if (fullpath.length() > MaxTermLength) {
			// currently the db uses the path as a key, and
			// therefore it cannot be too long. We'd get an error
			// later anyway but for now it's useful for surviving
			// circular symlinks
			mu_warning("'{}' is too long; ignore", fullpath);
			return false;
		}

		// in lazy-mode, we ignore this dir if its dirstamp suggests it
		// is up-to-date (this is _not_ always true; hence we call it
		// lazy-mode); only for actual message dirs, since the dir
		// tstamps may not bubble up.U
		dirstamp_ = store_.dirstamp(fullpath);
		if (conf_.lazy_check && dirstamp_ >= statbuf->st_ctime &&
		    htype == Scanner::HandleType::EnterNewCur) {
			mu_debug("skip {} (seems up-to-date: {:%FT%T} >= {:%FT%T})",
				 fullpath, mu_time(dirstamp_), mu_time(statbuf->st_ctime));
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
		// directly push to store worker, bypass scan-items queue
		pace_store_worker();
		store_worker_.push(StoreWorker::SetDirStamp{fullpath, ::time({})});
		return true;
	}

	case Scanner::HandleType::File: {
		++progress_.checked;

		if (static_cast<size_t>(statbuf->st_size) > max_message_size_) {
			mu_debug("skip {} (too big: {} bytes)", fullpath, statbuf->st_size);
			return false;
		}

		// if the message is not in the db yet, or not up-to-date, queue
		// it for updating/inserting.
		if (statbuf->st_ctime <= dirstamp_ && store_.contains_message(fullpath))
			return false;

		// push the remaining messages to our "scan-items" queue for
		// (re)parsing and adding/updating to the database.
		pace_scan_worker();
		msg_paths_.push(std::string{fullpath}); // move?
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

	if (msg_paths_.size() > workers_.size() && workers_.size() < max_workers_) {
		workers_.emplace_back(std::thread([this] { item_worker(); }));
		mu_debug("added worker {}", workers_.size());
	}
}

void
Indexer::Private::item_worker()
{
	mu_debug("started worker");

	while (state_ == State::Scanning ||
	       (state_ == State::Draining && !msg_paths_.empty())) {

		std::string msgpath;
		if (!msg_paths_.pop(msgpath, 250ms))
			continue;

		auto msg{Message::make_from_path(msgpath, store_.message_options())};
		if (!msg) {
			mu_warning("failed to create message from {}: {}",
				   msgpath, msg.error().what());
			continue;
		}

		pace_store_worker(); /* slow down if store-worker q gets too big */

		// if the store was empty, we know that the message is
		// completely new and can use the fast path (Xapians
		// 'add_document' rather than 'replace_document)
		if (was_empty_)
			store_worker_.push(StoreWorker::AddMessage{std::move(*msg)});
		else
			store_worker_.push(StoreWorker::UpdateMessage{std::move(*msg)});
		++progress_.updated;

		maybe_start_worker();
	}
}

void
Indexer::Private::scan_worker()
{
	if (conf_.scan) {
		mu_debug("starting scanner");
		if (!scanner_.start()) { // blocks.
			mu_warning("failed to start scanner");
			switch_state(State::Idle);
			return;
		}
		mu_debug("scanner finished with {} file(s) in queue", msg_paths_.size());
	}

	if (conf_.cleanup) {
		mu_debug("starting cleanup with work-item(s) left: {}",
			 store_worker_.size());

		std::vector<Store::Id> orphans; // store messages without files.
		store_.for_each_message_path([&](Store::Id id, const std::string& path) {
			if (::access(path.c_str(), R_OK) != 0) {
				mu_debug("orphan: cannot read {} (id={})", path, id);
				orphans.emplace_back(id);
			}
			return true;
		});
		progress_.removed = orphans.size();
		if (!orphans.empty()) {
			mu_info("removing {} orphan message(s)", orphans.size());
			store_worker_.push(StoreWorker::RemoveMessages{std::move(orphans)});
		}
	}

	completed_ = ::time({});
	store_worker_.push(StoreWorker::SetLastIndex{completed_});

	stop();
}

bool
Indexer::Private::start(const Indexer::Config& conf, bool block)
{
	force_cleanup();

	conf_ = conf;
	if (conf_.max_threads == 0) {
		/* benchmarking suggests that ~4 threads is the fastest (the
		 * real bottleneck is the database, so adding more threads just
		 * slows things down)  */
		max_workers_ = std::min(4U, std::thread::hardware_concurrency());
	} else
		max_workers_ = conf.max_threads;

	if (store_.empty() && conf_.lazy_check) {
		mu_debug("turn off lazy check since we have an empty store");
		conf_.lazy_check = false;
	}

	mu_debug("starting indexer with <= {} worker thread(s)", max_workers_);
	mu_debug("indexing: {}; clean-up: {}", conf_.scan ? "yes" : "no",
		 conf_.cleanup ? "yes" : "no");

	progress_.reset();
	switch_state(State::Scanning);
	/* kick off the first worker, which will spawn more if needed. */
	workers_.emplace_back(std::thread([this]{item_worker();}));
	/* kick the file-system-scanner thread */
	if (scanner_worker_.joinable())
		scanner_worker_.join(); // kill old one
	scanner_worker_ = std::thread([this]{scan_worker();});

	mu_debug("started indexer in {}-mode", block ? "blocking" : "non-blocking");
	if (block) {
		while(is_running()) {
			using namespace std::chrono_literals;
			std::this_thread::sleep_for(100ms);
		}
	}

	return true;
}

bool
Indexer::Private::stop()
{
	switch_state(State::Draining);

	scanner_.stop();
	// cannot join scanner_worker_ here since it may be our
	// current thread.

	// wait for completion.
	while (!msg_paths_.empty()) {
		mu_debug("scan-items left: {}", msg_paths_.size());
		std::this_thread::sleep_for(250ms);
	}

	for (auto&& w : workers_)
		if (w.joinable())
			w.join();
	workers_.clear();

	store_worker_.push(StoreWorker::EndTransaction());

	// wait for completion.
	while (!store_worker_.empty()) {
		mu_debug("work-items left: {}", store_worker_.size());
		std::this_thread::sleep_for(250ms);
	}

	switch_state(State::Idle);

	return true;
}

Indexer::Indexer(Store& store)
    : priv_{std::make_unique<Private>(store)}
{}

Indexer::~Indexer() = default;

bool
Indexer::start(const Indexer::Config& conf, bool block)
{
	const auto mdir{priv_->store_.root_maildir()};
	if (G_UNLIKELY(access(mdir.c_str(), R_OK) != 0)) {
		mu_critical("'{}' is not readable: {}", mdir, g_strerror(errno));
		return false;
	}

	std::lock_guard lock(priv_->lock_);
	if (is_running())
		return true;

	return priv_->start(conf, block);
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
	return priv_->is_running();
}

const Indexer::Progress&
Indexer::progress() const
{
	priv_->progress_.running = priv_->state_ == State::Idle ? false : true;

	return priv_->progress_;
}

::time_t
Indexer::completed() const
{
	return priv_->completed_;
}


#if BUILD_TESTS
#include "mu-test-utils.hh"

static void
test_index_basic()
{
	allow_warnings();

	TempDir tdir;
	auto store = Store::make_new(tdir.path(), MU_TESTMAILDIR2);
	assert_valid_result(store);
	g_assert_true(store->empty());

	Indexer& idx{store->indexer()};

	g_assert_false(idx.is_running());
	g_assert_true(idx.stop());
	g_assert_cmpuint(idx.completed(),==, 0);

	const auto& prog{idx.progress()};
	g_assert_false(prog.running);
	g_assert_cmpuint(prog.checked,==, 0);
	g_assert_cmpuint(prog.updated,==, 0);
	g_assert_cmpuint(prog.removed,==, 0);

	Indexer::Config conf{};
	conf.ignore_noupdate = true;

	{
		const auto start{time({})};
		g_assert_true(idx.start(conf));
		while (idx.is_running())
			g_usleep(10000);

		g_assert_false(idx.is_running());
		g_assert_true(idx.stop());

		g_assert_cmpuint(idx.completed() - start, <, 5);

		g_assert_false(prog.running);
		g_assert_cmpuint(prog.checked,==, 14);
		g_assert_cmpuint(prog.updated,==, 14);
		g_assert_cmpuint(prog.removed,==, 0);

		g_assert_cmpuint(store->size(),==,14);
	}

	conf.lazy_check	     = true;
	conf.max_threads     = 1;
	conf.ignore_noupdate = false;

	{
		const auto start{time({})};
		g_assert_true(idx.start(conf));
		while (idx.is_running())
			g_usleep(10000);

		g_assert_false(idx.is_running());
		g_assert_true(idx.stop());

		g_assert_cmpuint(idx.completed() - start, <, 3);

		g_assert_false(prog.running);
		g_assert_cmpuint(prog.checked,==, 0);
		g_assert_cmpuint(prog.updated,==, 0);
		g_assert_cmpuint(prog.removed,==, 0);

		g_assert_cmpuint(store->size(),==, 14);
	}
}


static void
test_index_lazy()
{
	allow_warnings();

	TempDir tdir;
	auto store = Store::make_new(tdir.path(), MU_TESTMAILDIR2);
	assert_valid_result(store);
	g_assert_true(store->empty());
	Indexer& idx{store->indexer()};

	Indexer::Config conf{};
	conf.lazy_check	     = true;
	conf.ignore_noupdate = false;

	const auto start{time({})};
	g_assert_true(idx.start(conf));
	while (idx.is_running())
		g_usleep(10000);

	g_assert_false(idx.is_running());
	g_assert_true(idx.stop());

	g_assert_cmpuint(idx.completed() - start, <, 3);

	const auto& prog{idx.progress()};
	g_assert_false(prog.running);
	g_assert_cmpuint(prog.checked,==, 6);
	g_assert_cmpuint(prog.updated,==, 6);
	g_assert_cmpuint(prog.removed,==, 0);

	g_assert_cmpuint(store->size(),==, 6);
}

static void
test_index_cleanup()
{
	allow_warnings();

	TempDir tdir;
	auto mdir = join_paths(tdir.path(), "Test");
	{
		auto res = run_command({"cp", "-r", MU_TESTMAILDIR2, mdir});
		assert_valid_result(res);
		g_assert_cmpuint(res->exit_code,==, 0);
	}

	auto store = Store::make_new(tdir.path(), mdir);
	assert_valid_result(store);
	g_assert_true(store->empty());
	Indexer& idx{store->indexer()};

	Indexer::Config conf{};
	conf.ignore_noupdate = true;

	g_assert_true(idx.start(conf));
	while (idx.is_running())
		g_usleep(10000);

	g_assert_false(idx.is_running());
	g_assert_true(idx.stop());
	g_assert_cmpuint(store->size(),==, 14);

	// remove a message
	{
		auto mpath = join_paths(mdir, "bar", "cur", "mail6");
		auto res = run_command({"rm", mpath});
		assert_valid_result(res);
		g_assert_cmpuint(res->exit_code,==, 0);
	}

	// no cleanup, # stays the same
	conf.cleanup = false;
	g_assert_true(idx.start(conf));
	while (idx.is_running())
		g_usleep(10000);
	g_assert_false(idx.is_running());
	g_assert_true(idx.stop());
	g_assert_cmpuint(store->size(),==, 14);

	// cleanup, message is gone from store.
	conf.cleanup = true;
	g_assert_true(idx.start(conf));
	while (idx.is_running())
		g_usleep(10000);
	g_assert_false(idx.is_running());
	g_assert_true(idx.stop());
	g_assert_cmpuint(store->size(),==, 13);
}


int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	g_test_add_func("/index/basic", test_index_basic);
	g_test_add_func("/index/lazy", test_index_lazy);
	g_test_add_func("/index/cleanup", test_index_cleanup);

	return g_test_run();

}
#endif /*BUILD_TESTS*/
