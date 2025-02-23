/*
** Copyright (C) 2020-2025 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <atomic>
#include <unordered_map>
#include <unordered_set>
#include <chrono>
using namespace std::chrono_literals;

#include "mu-store.hh"

#include "mu-scanner.hh"
#include "utils/mu-async-queue.hh"
#include "utils/mu-error.hh"

#include "utils/mu-utils-file.hh"

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
	      max_message_size_{store_.config().get<Mu::Config::Id::MaxMessageSize>()},
	      was_empty_{store.empty()} {

		mu_message("created indexer for {} -> "
			   "{} (batch-size: {}; was-empty: {}; ngrams: {})",
			   store.root_maildir(), store.path(),
			   store.config().get<Mu::Config::Id::BatchSize>(),
			   was_empty_,
			   store.config().get<Mu::Config::Id::SupportNgrams>());
	}

	~Private() {
		stop();
	}

	bool dir_predicate(const std::string& path, const struct dirent* dirent) const;
	bool handler(const std::string& fullpath, struct stat* statbuf, Scanner::HandleType htype);

	void maybe_start_worker();

	void scan_worker();

	bool add_message(const std::string& path);

	bool cleanup();
	bool start(const Indexer::Config& conf, bool block);
	bool stop();

	bool is_running() const { return state_ != IndexState::Idle; }

	Indexer::Config conf_;
	Store&          store_;
	Scanner         scanner_;
	const size_t    max_message_size_;

	::time_t                 dirstamp_{};
	std::thread              scanner_worker_;

	struct WorkItem {
		std::string full_path;
		enum Type {
			Dir,
			File
		};
		Type type;
	};

	void handle_item(WorkItem&& item);

	Progress   progress_{};
	IndexState state_{};
	std::mutex lock_, w_lock_;
	std::atomic<time_t> completed_{};
	bool was_empty_{};

	uint64_t last_index_{};
};

bool
Indexer::Private::handler(const std::string& fullpath, struct stat* statbuf,
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

		// in lazy-mode, we ignore this dir if its dirstamp suggest it
		// is up-to-date (this is _not_ always true; hence we call it
		// lazy-mode); only for actual message dirs, since the dir
		// tstamps may not bubble up.
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
		handle_item({fullpath, WorkItem::Type::Dir});
		return true;
	}

	case Scanner::HandleType::File: {
		++progress_.checked;
		if (conf_.lazy_check && static_cast<uint64_t>(statbuf->st_ctime) < last_index_) {
			// in lazy mode, ignore the file if it has not changed
			// since the last indexing op.
			return false;
		}

		if (static_cast<size_t>(statbuf->st_size) > max_message_size_) {
			mu_debug("skip {} (too big: {} bytes)", fullpath, statbuf->st_size);
			return false;
		}
		// if the message is not in the db yet, or not up-to-date, queue
		// it for updating/inserting.
		if (statbuf->st_ctime <= dirstamp_ && store_.contains_message(fullpath))
			return false;

		handle_item({fullpath, WorkItem::Type::File});
		return true;
	}
	default:
		g_return_val_if_reached(false);
		return false;
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
	 */
	//std::unique_lock lock{w_lock_};
	auto msg{Message::make_from_path(path, store_.message_options())};
	if (!msg) {
		mu_warning("failed to create message from {}: {}", path, msg.error().what());
		return false;
	}
	// if the store was empty, we know that the message is completely new
	// and can use the fast path (Xapians 'add_document' rather than
	// 'replace_document)
	auto res = store_.consume_message(std::move(msg.value()), was_empty_);
	if (!res) {
		mu_warning("failed to add message @ {}: {}", path, res.error().what());
		return false;
	}

	return true;
}


void
Indexer::Private::handle_item(WorkItem&& item)
{
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
}

bool
Indexer::Private::cleanup()
{
	mu_debug("starting cleanup");

	size_t                 n{};
	std::vector<Store::Id> orphans; // store messages without files.

	using DirFiles = std::unordered_set<std::string>;
	std::unordered_map<std::string, DirFiles> dir_cache;

	// get a set of file names in this directory.
	const auto get_dir_files = [](const std::string& path) -> DirFiles {
		DirFiles ret;
		if (auto dir{::opendir(path.c_str())}; dir) {
			dirent* dentry{};
			while ((dentry = ::readdir(dir))) {
				ret.emplace(dentry->d_name);
			}
			::closedir(dir);
		}

		return ret;
	};

	// is the file present in our set?
	const auto is_file_present = [&](const std::string& path) -> bool {
		std::string dir = dirname(path);
		auto [it, inserted] = dir_cache.try_emplace(dir);
		DirFiles& dir_files = it->second;
		if (inserted) {
			dir_files = get_dir_files(dir);
		}
		return dir_files.find(basename(path)) != dir_files.end();
	};

	store_.for_each_message_path([&](Store::Id id, const std::string& path) {
		++n;
		if (!is_file_present(path)) {
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
		mu_debug("scanner finished");
	}

	// and let the worker finish their work.
	state_.change_to(IndexState::Finishing);

	if (conf_.cleanup) {
		mu_debug("starting cleanup");
		state_.change_to(IndexState::Cleaning);
		cleanup();
		mu_debug("cleanup finished");
	}

	completed_ = ::time({});
	// attempt to commit to disk.
	store_.xapian_db().request_commit(true);
	store_.config().set<Mu::Config::Id::LastIndex>(completed_);
	state_.change_to(IndexState::Idle);
}

bool
Indexer::Private::start(const Indexer::Config& conf, bool block)
{
	stop();

	conf_ = conf;

	if (store_.empty() && conf_.lazy_check) {
		mu_debug("turn off lazy check since we have an empty store");
		conf_.lazy_check = false;
	}

	mu_debug("starting indexer");
	mu_debug("indexing: {}; clean-up: {}", conf_.scan ? "yes" : "no",
		 conf_.cleanup ? "yes" : "no");

	// remember the _previous_ indexing, so in lazy mode we can skip
	// those files.
	last_index_ = store_.config().get<Mu::Config::Id::LastIndex>();

	state_.change_to(IndexState::Scanning);
	/* kick the disk-scanner thread */
	scanner_worker_ = std::thread([this] { scan_worker(); });

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
	scanner_.stop();

	if (scanner_worker_.joinable())
		scanner_worker_.join();

	state_.change_to(IndexState::Idle);

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
	priv_->progress_.running = priv_->state_ == IndexState::Idle ? false : true;

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
