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

Result<void>
Mu::mu_cmd_remove(Mu::Store& store, const Options& opts)
{
	for (auto&& file: opts.remove.files) {
		const auto res = store.remove_message(file);
		if (!res)
			return Err(Error::Code::File, "failed to remove {}", file.c_str());
		else
			mu_debug("removed message @ {}", file);
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
test_remove_ok()
{
	auto testhome{unwrap(make_temp_dir())};
	auto dbpath{runtime_path(RuntimePath::XapianDb, testhome)};

	/* create a writable copy */
	const auto testmdir = join_paths(testhome, "test-maildir");
	const auto testmsg  = join_paths(testmdir, "/cur/1220863042.12663_1.mindcrime!2,S");
	auto cres = run_command({CP_PROGRAM, "-r", MU_TESTMAILDIR, testmdir});
	assert_valid_command(cres);

	{
		auto&& store = unwrap(Store::make_new(dbpath, testmdir));
		auto res = store.add_message(testmsg);
		assert_valid_result(res);
		g_assert_true(store.contains_message(testmsg));
	}

	{ // remove the same
		auto res = run_command({MU_PROGRAM, "remove",
				mu_format("--muhome={}", testhome),
				testmsg});
		assert_valid_command(res);
	}

	{
		auto&& store = unwrap(Store::make(dbpath));
		g_assert_false(!!store.contains_message(testmsg));
		g_assert_cmpuint(::access(testmsg.c_str(), F_OK), ==, 0);
	}

	remove_directory(testhome);
}


int
main(int argc, char* argv[]) try {

	mu_test_init(&argc, &argv);

	g_test_add_func("/cmd/remove/ok", test_remove_ok);

	return g_test_run();

} catch (const Error& e) {
	mu_printerrln("{}", e.what());
	return 1;
} catch (...) {
	mu_printerrln("caught exception");
	return 1;
}
#endif /*BUILD_TESTS*/
