/*
** Copyright (C) 2008-2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "config.h"
#include "mu-cmd.hh"

#include <chrono>
#include <thread>
#include <atomic>

#include <errno.h>
#include <string.h>
#include <cstdio>
#include <signal.h>
#include <unistd.h>

#include "mu-store.hh"

using namespace Mu;

static std::atomic<bool> caught_signal;

static void
sig_handler(int _sig)
{
	caught_signal = true;
}

static void
install_sig_handler(void)
{
	struct sigaction action;
	int              i, sigs[] = {SIGINT, SIGHUP, SIGTERM};

	sigemptyset(&action.sa_mask);
	action.sa_flags   = SA_RESETHAND;
	action.sa_handler = sig_handler;

	for (i = 0; i != G_N_ELEMENTS(sigs); ++i)
		if (sigaction(sigs[i], &action, NULL) != 0)
			mu_critical("set sigaction for {} failed: {}",
				   sigs[i], g_strerror(errno));
}

static void
print_stats(const Indexer::Progress& stats, bool color)
{
	const char* kars = "-\\|/";
	static auto i    = 0U;

	MaybeAnsi col{color};
	using Color = MaybeAnsi::Color;

	mu_print("{}{}{} indexing messages; "
		 "checked: {}{}{}; "
		 "updated/new: {}{}{}; "
		 "cleaned-up: {}{}{}",
		 col.fg(Color::Yellow), kars[++i % 4], col.reset(),
		 col.fg(Color::Green), static_cast<size_t>(stats.checked), col.reset(),
		 col.fg(Color::Green), static_cast<size_t>(stats.updated), col.reset(),
		 col.fg(Color::Green), static_cast<size_t>(stats.removed), col.reset());
}

Result<void>
Mu::mu_cmd_index(const Options& opts)
{
	auto store = std::invoke([&]{
		if (opts.index.reindex)
			return Store::make(opts.runtime_path(RuntimePath::XapianDb),
					   Store::Options::ReInit|Store::Options::Writable);
		else
			return Store::make(opts.runtime_path(RuntimePath::XapianDb),
			       Store::Options::Writable);
	});

	if (!store)
		return Err(store.error());

	const auto mdir{store->root_maildir()};
	if (G_UNLIKELY(::access(mdir.c_str(), R_OK) != 0))
		return Err(Error::Code::File, "'{}' is not readable: {}",
			   mdir, g_strerror(errno));

	MaybeAnsi col{!opts.nocolor};
	using Color = MaybeAnsi::Color;
	if (!opts.quiet) {
		if (opts.index.lazycheck)
			mu_print("lazily ");

		mu_println("indexing maildir {}{}{} -> "
			   "store {}{}{}",
			   col.fg(Color::Green), store->root_maildir(), col.reset(),
			   col.fg(Color::Blue), store->path(), col.reset());
	}

	Mu::Indexer::Config conf{};
	conf.cleanup    = !opts.index.nocleanup;
	conf.lazy_check = opts.index.lazycheck;
	// ignore .noupdate with an empty store.
	conf.ignore_noupdate = store->empty();

	install_sig_handler();

	auto& indexer{store->indexer()};
	indexer.start(conf);
	while (!caught_signal && indexer.is_running()) {
		if (!opts.quiet)
			print_stats(indexer.progress(), !opts.nocolor);

		std::this_thread::sleep_for(std::chrono::milliseconds(100));

		if (!opts.quiet) {
			mu_print("\r");
			::fflush({});
		}
	}

	indexer.stop();

	if (!opts.quiet) {
		print_stats(indexer.progress(), !opts.nocolor);
		mu_print("\n");
		::fflush({});
	}

	return Ok();
}


#ifdef BUILD_TESTS

/*
 * Tests.
 *
 */
#include <config.h>
#include <mu-store.hh>
#include "utils/mu-test-utils.hh"


static void
test_mu_index(size_t batch_size=0)
{
	TempDir temp_dir{};

	const auto mu_home{temp_dir.path()};

	auto res1 = run_command({MU_PROGRAM, "--quiet", "init", "--batch-size",
			mu_format("{}", batch_size == 0 ? 10000 : batch_size),
			"--muhome", mu_home, "--maildir" , MU_TESTMAILDIR2});
	assert_valid_command(res1);

	auto res2 = run_command({MU_PROGRAM, "--quiet", "index",
			"--muhome", mu_home});
	assert_valid_command(res2);

	auto&& store = unwrap(Store::make(join_paths(temp_dir.path(), "xapian")));
	g_assert_cmpuint(store.size(),==,14);
}


static void
test_mu_index_basic()
{
	test_mu_index();
}

static void
test_mu_index_batch()
{
	test_mu_index(2);
}

int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	g_test_add_func("/cmd/index/basic", test_mu_index_basic);
	g_test_add_func("/cmd/index/batch", test_mu_index_batch);

	return g_test_run();
}

#endif /*BUILD_TESTS*/
