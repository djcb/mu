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

#include "mu-maildir.hh"

using namespace Mu;

Mu::Result<void>
Mu::mu_cmd_mkdir(const Options& opts)
{
	for (auto&& dir: opts.mkdir.dirs) {
		if (auto&& res =
		    maildir_mkdir(dir, opts.mkdir.mode); !res)
			return res;
	}

	return Ok();
}



#ifdef BUILD_TESTS
/*
 * Tests.
 *
 */

#include "utils/mu-test-utils.hh"

static void
test_mkdir_single()
{
	auto testroot{unwrap(make_temp_dir())};
	auto testdir1{join_paths(testroot, "testdir1")};

	auto res = run_command({MU_PROGRAM, "mkdir", testdir1});
	assert_valid_command(res);

	g_assert_true(check_dir(join_paths(testdir1, "cur"), true, true));
	g_assert_true(check_dir(join_paths(testdir1, "new"), true, true));
	g_assert_true(check_dir(join_paths(testdir1, "tmp"), true, true));
}

static void
test_mkdir_multi()
{
	auto testroot{unwrap(make_temp_dir())};
	auto testdir2{join_paths(testroot, "testdir2")};
	auto testdir3{join_paths(testroot, "testdir3")};

	auto res = run_command({MU_PROGRAM, "mkdir", testdir2, testdir3});
	assert_valid_command(res);

	g_assert_true(check_dir(join_paths(testdir2, "cur"), true, true));
	g_assert_true(check_dir(join_paths(testdir2, "new"), true, true));
	g_assert_true(check_dir(join_paths(testdir3, "tmp"), true, true));

	g_assert_true(check_dir(join_paths(testdir3, "cur"), true, true));
	g_assert_true(check_dir(join_paths(testdir3, "new"), true, true));
	g_assert_true(check_dir(join_paths(testdir3, "tmp"), true, true));
}

int
main(int argc, char* argv[]) try {

	mu_test_init(&argc, &argv);

	g_test_add_func("/cmd/mkdir/single", test_mkdir_single);
	g_test_add_func("/cmd/mkdir/multi", test_mkdir_multi);

	return g_test_run();

} catch (const Error& e) {
	mu_printerrln("{}", e.what());
	return 1;
} catch (...) {
	mu_printerrln("caught exception");
	return 1;
}
#endif /*BUILD_TESTS*/
