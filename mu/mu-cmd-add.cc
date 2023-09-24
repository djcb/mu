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
Mu::mu_cmd_add(Mu::Store& store, const Options& opts)
{
	for (auto&& file: opts.add.files) {
		const auto docid{store.add_message(file)};
		if (!docid)
			return Err(docid.error());
		else
			mu_debug("added message @ {}, docid={}", file, *docid);
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
test_add_ok()
{
	auto testhome{unwrap(make_temp_dir())};
	auto dbpath{runtime_path(RuntimePath::XapianDb, testhome)};

	{
		unwrap(Store::make_new(dbpath, MU_TESTMAILDIR));
	}

	{
		auto res = run_command({MU_PROGRAM, "add", mu_format("--muhome={}", testhome),
				MU_TESTMAILDIR "/cur/1220863042.12663_1.mindcrime!2,S"});
		assert_valid_command(res);
	}

	{
		auto&& store = Store::make(dbpath);
		assert_valid_result(store);
		g_assert_cmpuint(store->size(),==,1);
	}

	{ // re-add the same
		auto res = run_command({MU_PROGRAM, "add", mu_format("--muhome={}",testhome),
				MU_TESTMAILDIR "/cur/1220863042.12663_1.mindcrime!2,S"});
		assert_valid_command(res);
	}

	{
		auto&& store = Store::make(dbpath);
		assert_valid_result(store);
		g_assert_cmpuint(store->size(),==,1);
	}


	remove_directory(testhome);
}

static void
test_add_fail()
{
	auto testhome{unwrap(make_temp_dir())};
	auto dbpath{runtime_path(RuntimePath::XapianDb, testhome)};

	{
		unwrap(Store::make_new(dbpath, MU_TESTMAILDIR2));
	}

	{ // wrong maildir
		auto res = run_command({MU_PROGRAM, "add", mu_format("--muhome={}", testhome),
				       MU_TESTMAILDIR "/cur/1220863042.12663_1.mindcrime!2,S"});
		assert_valid_result(res);
		g_assert_cmpuint(res->exit_code,!=,0);
	}


	{ // non-existent
		auto res = run_command({MU_PROGRAM, "add", mu_format("--muhome={}", testhome),
				"/foo/bar/non-existent"});
		assert_valid_result(res);
		g_assert_cmpuint(res->exit_code,!=,0);
	}

	remove_directory(testhome);
}


int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	g_test_add_func("/cmd/add/ok", test_add_ok);
	g_test_add_func("/cmd/add/fail", test_add_fail);

	return g_test_run();
}
#endif /*BUILD_TESTS*/
