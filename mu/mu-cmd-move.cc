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

#include "mu-store.hh"
#include "mu-maildir.hh"
#include "message/mu-message-file.hh"

 #include <unistd.h>

using namespace Mu;


Result<void>
Mu::mu_cmd_move(Mu::Store& store, const Options& opts)
{
	const auto& src{opts.move.src};
	if (::access(src.c_str(), R_OK) != 0 || determine_dtype(src) != DT_REG)
		return Err(Error::Code::InvalidArgument,
			   "Source is not a readable file");

	auto id{store.find_message_id(src)};
	if (!id)
		return Err(Error{Error::Code::InvalidArgument,
				"Source file is not present in database"}
			.add_hint("Perhaps run mu index?"));

	std::string dest{opts.move.dest};
	Option<const std::string&> dest_path;
	if (dest.empty() && opts.move.flags.empty())
		return Err(Error::Code::InvalidArgument,
			   "Must have at least one of destination and flags");
	else if (!dest.empty()) {
		const auto mdirs{store.maildirs()};

		if (!seq_some(mdirs, [&](auto &&d){ return d == dest;}))
			return Err(Error{Error::Code::InvalidArgument,
					"No maildir '{}' in store", dest}
				.add_hint("Try 'mu mkdir'"));
		else
			dest_path = dest;
	}

	auto old_flags{flags_from_path(src)};
	if (!old_flags)
		return Err(Error::Code::InvalidArgument, "failed to determine old flags");

	Flags new_flags{};
	if (!opts.move.flags.empty()) {
		if (auto&& nflags{flags_from_expr(to_string_view(opts.move.flags),
						  *old_flags)}; !nflags)
			return Err(Error::Code::InvalidArgument, "Invalid flags");
		else
			new_flags = flags_maildir_file(*nflags);

		if (any_of(new_flags & Flags::New) && new_flags != Flags::New)
			return Err(Error{Error::Code::File,
					"the New flag cannot be combined with others"}
				.add_hint("See the mu-move manpage"));
	}

	Store::MoveOptions move_opts{};
	if (opts.move.change_name)
		move_opts |= Store::MoveOptions::ChangeName;
	if (opts.move.update_dups)
		move_opts |= Store::MoveOptions::DupFlags;
	if (opts.move.dry_run)
		move_opts |= Store::MoveOptions::DryRun;

	auto id_paths = store.move_message(*id, dest_path, new_flags, move_opts);
	if (!id_paths)
		return Err(std::move(id_paths.error()));

	for (const auto&[_id, path]: *id_paths)
		mu_println("{}", path);

	return Ok();
}



#ifdef BUILD_TESTS
/*
 * Tests.
 *
 */

#include "utils/mu-test-utils.hh"

static void
test_move_dry_run()
{
	allow_warnings();

	TempDir tdir;
	const auto dbpath{runtime_path(RuntimePath::XapianDb, tdir.path())};

	auto res = run_command0({CP_PROGRAM, "-r", MU_TESTMAILDIR, tdir.path()});
	assert_valid_command(res);

	const auto testpath{join_paths(tdir.path(), "testdir")};
	const auto src{join_paths(testpath, "cur", "1220863042.12663_1.mindcrime!2,S")};
	{
		auto store = Store::make_new(dbpath, testpath, {});
		assert_valid_result(store);
		g_assert_true(store->indexer().start({}, true/*block*/));
	}

	// make a message 'New'
	{
		auto res = run_command0({MU_PROGRAM, "move", "--muhome", tdir.path(), src,
				"--flags", "N", "--dry-run"});
		assert_valid_command(res);

		auto dst{join_paths(testpath, "new", "1220863042.12663_1.mindcrime")};
		assert_equal(res->standard_out, dst + '\n');

		g_assert_true(::access(dst.c_str(), F_OK) != 0);
		g_assert_true(::access(src.c_str(), F_OK) == 0);
	}

	// change some flags
	{
		auto res = run_command0({MU_PROGRAM, "move", "--muhome", tdir.path(), src,
				"--flags", "FP", "--dry-run"});
		assert_valid_command(res);

		auto dst{join_paths(testpath, "cur", "1220863042.12663_1.mindcrime!2,FP")};
		assert_equal(res->standard_out, dst + '\n');
	}

	// change some relative flag
	{
		auto res = run_command0({MU_PROGRAM, "move", "--muhome", tdir.path(), src,
				"--flags", "+F", "--dry-run"});
		assert_valid_command(res);

		auto dst{join_paths(testpath, "cur", "1220863042.12663_1.mindcrime!2,FS")};
		assert_equal(res->standard_out, dst + '\n');
	}

	{
		auto res = run_command0({MU_PROGRAM, "move", "--muhome", tdir.path(), src,
				"--flags", "-S+P+T", "--dry-run"});
		assert_valid_command(res);

		auto dst{join_paths(testpath, "cur", "1220863042.12663_1.mindcrime!2,PT")};
		assert_equal(res->standard_out, dst + '\n');
	}

	// change maildir
	for (auto& o : {"o1", "o2"})
		assert_valid_result(maildir_mkdir(join_paths(tdir.path(), "testdir", o)));

	{
		auto res = run_command0({MU_PROGRAM, "move", "--muhome", tdir.path(), src,
				"/o1", "--flags", "-S+F", "--dry-run"});
		assert_valid_command(res);
		assert_equal(res->standard_out,
			     join_paths(testpath,
					"o1/cur", "1220863042.12663_1.mindcrime!2,F") + "\n");
	}

	// change-dups; first create some dups and index them.
	assert_valid_result(run_command0({CP_PROGRAM, src, join_paths(testpath, "o1/cur")}));
	assert_valid_result(run_command0({CP_PROGRAM, src, join_paths(testpath, "o2/cur")}));
	{
		auto store = Store::make(dbpath, Store::Options::Writable);
		assert_valid_result(store);
		g_assert_true(store->indexer().start({}, true/*block*/));
	}

	// change some flags + update dups
	{
		auto res = run_command0({MU_PROGRAM, "move", "--muhome", tdir.path(), src,
				"--flags", "-S+S+T+R", "--update-dups", "--dry-run"});
		assert_valid_command(res);

		auto p{join_paths(testpath, "cur", "1220863042.12663_1.mindcrime!2,RST")};
		auto p1{join_paths(testpath, "o1", "cur", "1220863042.12663_1.mindcrime!2,RS")};
		auto p2{join_paths(testpath, "o2", "cur", "1220863042.12663_1.mindcrime!2,RS")};

		assert_equal(res->standard_out, mu_format("{}\n{}\n{}\n", p, p1, p2));
	}
}


static void
test_move_real()
{
	allow_warnings();

	TempDir tdir;
	const auto dbpath{runtime_path(RuntimePath::XapianDb, tdir.path())};

	auto res = run_command0({CP_PROGRAM, "-r", MU_TESTMAILDIR, tdir.path()});
	assert_valid_command(res);

	const auto testpath{join_paths(tdir.path(), "testdir")};
	const auto src{join_paths(testpath, "cur", "1220863042.12663_1.mindcrime!2,S")};
	{
		auto store = Store::make_new(dbpath, testpath, {});
		assert_valid_result(res);
		g_assert_true(store->indexer().start({}, true/*block*/));
	}

	{
		auto res = run_command0({MU_PROGRAM, "move", "--muhome", tdir.path(), src,
				"--flags", "N"});
		assert_valid_command(res);
		auto dst{join_paths(testpath, "new", "1220863042.12663_1.mindcrime")};
		g_assert_true(::access(dst.c_str(), F_OK) == 0);
		g_assert_true(::access(src.c_str(), F_OK) != 0);
	}

	// change flags, maildir, update-dups
	// change-dups; first create some dups and index them.
	const auto src2{join_paths(testpath, "cur", "1305664394.2171_402.cthulhu!2,")};
	for (auto& o : {"o1", "o2", "o3"})
		assert_valid_result(maildir_mkdir(join_paths(tdir.path(), "testdir", o)));
	assert_valid_result(run_command0({CP_PROGRAM, src2, join_paths(testpath, "o1/cur")}));
	assert_valid_result(run_command0({CP_PROGRAM, src2, join_paths(testpath, "o2/new")}));
	{
		auto store = Store::make(dbpath, Store::Options::Writable);
		assert_valid_result(store);
		g_assert_true(store->indexer().start({}, true/*block*/));
	}

	auto res2 = run_command0({MU_PROGRAM, "move", "--muhome", tdir.path(), src2, "/o3",
			"--flags", "-S+S+T+R", "--update-dups", "--change-name"});
	assert_valid_command(res2);

	auto store = Store::make(dbpath, Store::Options::Writable);
	assert_valid_result(store);
	g_assert_true(store->indexer().start({}, true/*block*/));

	for (auto&& f: split(res2->standard_out, "\n")) {
		//mu_println(">> {}", f);
		if (f.length() > 2)
			g_assert_true(::access(f.c_str(), F_OK) == 0);
	}
}

int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	g_test_add_func("/cmd/move/dry-run", test_move_dry_run);
	g_test_add_func("/cmd/move/real", test_move_real);

	return g_test_run();

}
#endif /*BUILD_TESTS*/
