/*
** Copyright (C) 2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

using namespace Mu;

#ifndef BUILD_TESTS

Result<void>
Mu::mu_cmd_init(const Options& opts)
{
	auto store = std::invoke([&]()->Result<Store> {

		/*
		 * reinit
		 */
		if (opts.init.reinit)
			return Store::make(opts.runtime_path(RuntimePath::XapianDb),
					   Store::Options::ReInit|Store::Options::Writable);
		/*
		 * full init
		 */

		/* not provided, nor could we find a good default */
		if (opts.init.maildir.empty())
			return Err(Error::Code::InvalidArgument,
				   "missing --maildir parameter and could "
				   "not determine default");
		else if (!g_path_is_absolute(opts.init.maildir.c_str()))
			return Err(Error{Error::Code::File,
					"--maildir is not absolute"});

		MemDb mdb;
		Config conf{mdb};

		if (opts.init.max_msg_size)
			conf.set<Config::Id::MaxMessageSize>(*opts.init.max_msg_size);
		if (opts.init.batch_size && *opts.init.batch_size != 0)
			conf.set<Config::Id::BatchSize>(*opts.init.batch_size);
		if (!opts.init.my_addresses.empty())
			conf.set<Config::Id::PersonalAddresses>(opts.init.my_addresses);
		if (!opts.init.ignored_addresses.empty())
			conf.set<Config::Id::IgnoredAddresses>(opts.init.ignored_addresses);
		if (opts.init.support_ngrams)
			conf.set<Config::Id::SupportNgrams>(true);

		return Store::make_new(opts.runtime_path(RuntimePath::XapianDb),
				       opts.init.maildir, conf);
	});

	if (!store)
		return Err(store.error());

	if (!opts.quiet) {

		mu_println("mu has been {} with the following properties:",
			   opts.init.reinit ? "reinitialized" : "created");
		// mildly hacky
		Options opts_copy{opts};
		opts_copy.info.topic = "store";
		mu_cmd_info(*store, opts_copy);

		mu_println("Database is empty. You can use 'mu index' to fill it.");
	}

	return Ok();
}



#else /* BUILD_TESTS */

/*
 * Tests.
 *
 */
#include <config.h>
#include <mu-store.hh>
#include "utils/mu-test-utils.hh"


static void
test_mu_init_basic()
{
	TempDir temp_dir{};

	const auto mu_home{temp_dir.path()};

	auto res1 = run_command({MU_PROGRAM, "--quiet", "init",
			"--muhome", mu_home, "--maildir" , MU_TESTMAILDIR2});
	assert_valid_command(res1);

	auto&& store = unwrap(Store::make(join_paths(temp_dir.path(), "xapian")));
	g_assert_true(store.empty());
}

static void
test_mu_init_maildir()
{
	TempDir temp_dir{};

	const auto mu_home{temp_dir.path()};

	g_setenv("MAILDIR", MU_TESTMAILDIR2, 1);
	auto res1 = run_command({MU_PROGRAM, "--quiet", "init",
			"--muhome", mu_home});
	assert_valid_command(res1);

	auto&& store = unwrap(Store::make(join_paths(temp_dir.path(), "xapian")));
	g_assert_true(store.empty());
	assert_equal(store.root_maildir(), MU_TESTMAILDIR2);
}

int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	g_test_add_func("/cmd/init/basic", test_mu_init_basic);
	g_test_add_func("/cmd/init/maildir", test_mu_init_maildir);

	return g_test_run();
}

#endif /*BUILD_TESTS*/
