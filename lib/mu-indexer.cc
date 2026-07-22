/*
** Copyright (C) 2020-2026 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <chrono>
#include <string_view>
#include <ranges>
using namespace std::chrono_literals;

#include "mu-store.hh"

#include "mu-scanner.hh"
#include "utils/mu-error.hh"

#include "utils/mu-utils-file.hh"

using namespace Mu;

struct IndexState {
	enum State { Idle,
		     Scanning,
		     Finishing,
		     Cleaning,
		     Aborting,
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
		case Aborting:
			return "aborting";
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
			   store.config().get<Mu::Config::Id::NgramsEnabled>());
	}

	~Private() {
		stop();
	}

	bool dir_predicate(const std::string& path, const struct dirent* dirent) const;
	bool handler(const std::string& fullpath, struct stat* statbuf, Scanner::HandleType htype);

	void maybe_start_worker();

	void prefetch_path_terms(bool for_cleanup);
	bool mark_seen(const std::string& path);
	void mark_seen_dir(const std::string& dir_path);

	void scan_worker();

	bool add_message(const std::string& path);

	void cleanup();
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
	Option<time_t> started_;
	std::atomic<time_t> completed_{};
	bool was_empty_{};

	uint64_t last_index_{};

	using PathTerm = std::pair<std::string, bool>;
        std::vector<PathTerm> db_path_terms_;
        /**< all path-terms in the store (at scan-start),
	in Xapian's (ascending) and a seen marker */
	bool			use_db_path_terms_{};
};

void
Indexer::Private::prefetch_path_terms(bool for_cleanup)
{
	use_db_path_terms_ = false;
	db_path_terms_.clear();

	// The in-memory path-terms serve two purposes:
	//
	// i. checking whether some message is already in the store
	//    without db access
	// ii. unless we need it for cleanup: whatever
	//    is left unmarked after scanning is an orphan.
	if (!for_cleanup && conf_.lazy_check)
		return;

	db_path_terms_.reserve(store_.size());
	store_.for_each_term(Field::Id::Path, [&](const std::string& term) {
		db_path_terms_.emplace_back(term, false/*!seen*/);
		return true;
	});
	use_db_path_terms_ = true;

	mu_debug("prefetched {} path-term(s)", db_path_terms_.size());
}

bool
Indexer::Private::mark_seen(const std::string& path)
{
	if (!use_db_path_terms_)
		return false;

	// N.B. Xapian yields terms in ascending byte-lexicographic order, so
	// db_path_terms_ is sorted and we can binary-search.
	const auto term{field_from_id(Field::Id::Path).xapian_term(path)};
	const auto it{std::ranges::lower_bound(db_path_terms_, term, {},
					       &PathTerm::first)};
	if (it == db_path_terms_.end() || it->first != term)
		return false; // not in the store.

	it->second = true;
	return true;
}

void
Indexer::Private::mark_seen_dir(const std::string& dir_path)
{
	if (!use_db_path_terms_)
		return;

	// mark all store messages under dir_path as seen
	const auto prefix{field_from_id(Field::Id::Path)
			.xapian_term(dir_path + "/")};
	for (auto it = std::ranges::lower_bound(db_path_terms_, prefix, {},
						&PathTerm::first);
	     it != db_path_terms_.end() && it->first.starts_with(prefix); ++it)
		it->second = true;
}

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
			mark_seen_dir(fullpath);
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
				mark_seen_dir(fullpath);
				return false;
			}
		}

		mu_debug("checked {}", fullpath);
		return true;
	}
	case Scanner::HandleType::LeaveDir: {
		// don't touch dirstamps in a cleanup-only run: nothing was
		// indexed, so the dir is not to be considered up-to-date.
		if (conf_.scan)
			handle_item({fullpath, WorkItem::Type::Dir});
		return true;
	}

	case Scanner::HandleType::File: {
		++progress_.checked;
		// this file is present in the file-system; also remember
		// whether it is in the store already.
		const auto in_store{mark_seen(fullpath)};

		if (!conf_.scan)
			return false; // cleanup-only run; only mark.

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
		if (statbuf->st_ctime <= dirstamp_ &&
		    (use_db_path_terms_ ? in_store : store_.contains_message(fullpath)))
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
			store_.set_dirstamp(item.full_path, started_.value());
			break;
		default:
			g_warn_if_reached();
			break;
		}
	} catch (const Mu::Error& er) {
		mu_warning("error adding message @ {}: {}", item.full_path, er.what());
	}
}

void
Indexer::Private::cleanup()
{
	mu_debug("starting cleanup after scan");

	// during the scan, each file (and each wholesale-skipped
	// directory) marked its path-term; whatever is left unmarked is
	// an orphan: a message in the store without a file in the maildir.
	//
	// N.B. this snapshot was taken at scan-start, so messages that
	// appeared in the store _during_ the scan are not in it, and thus
	// cannot be orphaned by mistake.
	std::vector<std::string> orphan_terms;
	for (auto&& [term, seen] : db_path_terms_)
		if (!seen)
			orphan_terms.emplace_back(std::move(term));

	if (orphan_terms.empty())
		mu_debug("nothing to clean up");
	else {
		size_t nr_removed = store_.remove_messages_by_term(
			orphan_terms, [&](size_t nr_cleaned_up_so_far) {
				progress_.removed = nr_cleaned_up_so_far;
			});
		mu_debug("removing {} stale message(s) from store", nr_removed);
		progress_.removed = nr_removed;
		if (nr_removed != orphan_terms.size())
			mu_warning("should have removed {} messages but actually removed {}",
				   orphan_terms.size(), nr_removed);
	}
}

void
Indexer::Private::scan_worker()
{
	progress_.reset();
	started_ = time(NULL);

	// cleanup gets its orphans by marking the in-memory path-terms
	// during the file-system scan, so we need the scanner even for a
	// cleanup-only run.
	if (conf_.scan || conf_.cleanup) {
		prefetch_path_terms(conf_.cleanup);
		mu_debug("starting scanner");
		if (!scanner_.start()) { // blocks.
			mu_warning("failed to start scanner");
			state_.change_to(IndexState::Idle);
			return;
		}
		mu_debug("scanner finished");
	}

	bool aborted = state_ == IndexState::Aborting;

	state_.change_to(IndexState::Cleaning);

	if (!conf_.cleanup)
		mu_debug("cleanup: not running as requested");
	else if (aborted)
		mu_debug("cleanup: disabling because indexer aborted");
	else
		cleanup();

	// release the in-memory path-terms.
	use_db_path_terms_ = false;
	db_path_terms_	   = {};

	aborted = state_ == IndexState::Aborting;
	if (!aborted && conf_.scan) {
		// Store started time, not ending time, so that next time we run we know to scan
		// anything that appeared during our scan.
		store_.config().set<Mu::Config::Id::LastIndex>(started_.value());
	}

	completed_ = ::time({});
	// attempt to commit to disk.
	store_.xapian_db().request_commit(true);
	state_.change_to(IndexState::Idle);
	started_ = Nothing;
}

bool
Indexer::Private::start(const Indexer::Config& conf, bool block)
{
	stop();

	conf_ = conf;

	// refresh: the indexer may be re-used for multiple runs, and the
	// add_document fast-path is only valid while the store is empty.
	was_empty_ = store_.empty();

	if (was_empty_ && conf_.lazy_check) {
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
	if (state_ != IndexState::Idle)
		state_.change_to(IndexState::Aborting);
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

	mu_test_skip_valgrind_return();

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

	mu_test_skip_valgrind_return();

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

	// remove another message
	{
		auto mpath = join_paths(mdir, "bar", "cur", "mail5");
		auto res = run_command({"rm", mpath});
		assert_valid_result(res);
		g_assert_cmpuint(res->exit_code,==, 0);
	}

	// cleanup-only run (no scan); message is gone from store.
	conf.scan = false;
	g_assert_true(idx.start(conf));
	while (idx.is_running())
		g_usleep(10000);
	g_assert_false(idx.is_running());
	g_assert_true(idx.stop());
	g_assert_cmpuint(store->size(),==, 12);
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
