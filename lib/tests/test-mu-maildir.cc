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

#include <glib.h>
#include <glib/gstdio.h>

#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <vector>
#include <fstream>

#include "utils/mu-test-utils.hh"
#include "mu-maildir.hh"
#include "utils/mu-utils.hh"
#include "utils/mu-utils-file.hh"
#include "utils/mu-result.hh"

using namespace Mu;

static void
test_maildir_mkdir_01()
{
	TempDir temp_dir;
	auto mdir = join_paths(temp_dir.path(), "cuux");
	auto res{maildir_mkdir(mdir, 0755, false/*!noindex*/)};
	assert_valid_result(res);

	for (auto sub : {"tmp", "cur", "new"}) {
		auto subpath = join_paths(mdir, sub);
		g_assert_cmpuint(g_access(subpath.c_str(), R_OK), ==, 0);
		g_assert_cmpuint(g_access(subpath.c_str(), W_OK), ==, 0);
	}

	auto noindex = join_paths(mdir, ".noindex");
	g_assert_cmpuint(g_access(noindex.c_str(), F_OK), !=, 0);
}

static void
test_maildir_mkdir_02()
{
	TempDir temp_dir;
	auto mdir = join_paths(temp_dir.path(), "cuux");
	auto res{maildir_mkdir(mdir, 0755, true/*noindex*/)};
	assert_valid_result(res);

	for (auto sub : {"tmp", "cur", "new"}) {
		auto subpath = join_paths(mdir, sub);
		g_assert_cmpuint(g_access(subpath.c_str(), R_OK), ==, 0);
		g_assert_cmpuint(g_access(subpath.c_str(), W_OK), ==, 0);
	}

	auto noindex = join_paths(mdir, ".noindex");
	g_assert_cmpuint(g_access(noindex.c_str(), F_OK), ==, 0);
}

static void
test_maildir_mkdir_03()
{
	TempDir temp_dir;
	auto mdir = join_paths(temp_dir.path(), "cuux");

	// create part already
	auto curdir = join_paths(mdir, "cur");
	g_assert_cmpuint(g_mkdir_with_parents(curdir.c_str(), 0755), ==, 0);

	auto res{maildir_mkdir(mdir, 0755, false/*!noindex*/)};
	assert_valid_result(res);

	// should still work.
	for (auto sub : {"tmp", "cur", "new"}) {
		auto subpath = join_paths(mdir, sub);
		g_assert_cmpuint(g_access(subpath.c_str(), R_OK), ==, 0);
		g_assert_cmpuint(g_access(subpath.c_str(), W_OK), ==, 0);
	}

	auto noindex = join_paths(mdir, ".noindex");
	g_assert_cmpuint(g_access(noindex.c_str(), F_OK), !=, 0);
}


static void
test_maildir_mkdir_04()
{
	allow_warnings();

	if (geteuid() == 0) {
		g_test_skip("not useful when run as root");
		return;
	}

	TempDir temp_dir;
	auto mdir = join_paths(temp_dir.path(), "cuux");
	g_assert_cmpuint(g_mkdir_with_parents(mdir.c_str(), 0755), ==, 0);

	auto curdir = join_paths(mdir, "cur");
	g_assert_cmpuint(g_mkdir_with_parents(curdir.c_str(), 0000), ==, 0);

	/* this should fail now, because cur is not read/writable  */
	auto res = maildir_mkdir(mdir, 0755, false);
	g_assert_false(!!res);
}

static gboolean
ignore_error(const char* log_domain, GLogLevelFlags log_level, const gchar* msg, gpointer user_data)
{
	return FALSE; /* don't abort */
}

static void
test_maildir_mkdir_05(void)
{
	/* this must fail */
	g_test_log_set_fatal_handler((GTestLogFatalFunc)ignore_error, NULL);

	g_assert_false(!!maildir_mkdir({}, 0755, true));
}

[[maybe_unused]] static void
assert_matches_regexp(const char* str, const char* rx)
{
	if (!g_regex_match_simple(rx, str, (GRegexCompileFlags)0, (GRegexMatchFlags)0)) {
		if (g_test_verbose())
			g_print("%s does not match %s", str, rx);
		g_assert(0);
	}
}


static void
test_determine_target_ok(void)
{
	struct TestCase {
		std::string old_path;
		std::string root_maildir;
		std::string target_maildir;
		Flags new_flags;
		bool new_name;
		std::string expected;
	};
	const std::vector<TestCase> testcases = {
		 TestCase{ /* change some flags */
			"/home/foo/Maildir/test/cur/123456:2,FR",
			"/home/foo/Maildir",
			{},
			Flags::Seen | Flags::Passed,
			false,
			"/home/foo/Maildir/test/cur/123456:2,PS"
		},

		 TestCase{ /* from cur -> new */
			"/home/foo/Maildir/test/cur/123456:2,FR",
			"/home/foo/Maildir",
			{},
			Flags::New,
			false,
			"/home/foo/Maildir/test/new/123456"
		},

		 TestCase{ /* from new->cur */
			"/home/foo/Maildir/test/cur/123456",
			"/home/foo/Maildir",
			{},
			Flags::Seen | Flags::Flagged,
			false,
			"/home/foo/Maildir/test/cur/123456:2,FS"
		 },

		 TestCase{ /* change maildir */
			"/home/foo/Maildir/test/cur/123456:2,FR",
			"/home/foo/Maildir",
			"/test2",
			Flags::Flagged | Flags::Replied,
			false,
			"/home/foo/Maildir/test2/cur/123456:2,FR"
		},
		 TestCase{ /* remove all flags */
			"/home/foo/Maildir/test/new/123456",
			"/home/foo/Maildir",
			{},
			Flags::None,
			false,
			"/home/foo/Maildir/test/cur/123456:2,"
		},
	};

	for (auto&& testcase: testcases) {
		const auto res = maildir_determine_target(
			testcase.old_path,
			testcase.root_maildir,
			testcase.target_maildir,
			testcase.new_flags,
			testcase.new_name);
		g_assert_true(!!res);
		g_assert_cmpstr(testcase.expected.c_str(), ==,
				res.value().c_str());
	}
}



static void
test_determine_target_fail(void)
{
	struct TestCase {
		std::string old_path;
		std::string root_maildir;
		std::string target_maildir;
		Flags new_flags;
		bool new_name;
		std::string expected;
	};
	const std::vector<TestCase> testcases = {
		 TestCase{ /* fail: no absolute path */
			"../foo/Maildir/test/cur/123456:2,FR-not-absolute",
			"/home/foo/Maildir",
			{},
			Flags::Seen | Flags::Passed,
			false,
			"/home/foo/Maildir/test/cur/123456:2,PS"
		},

		 TestCase{ /* fail: no absolute root */
			"/home/foo/Maildir/test/cur/123456:2,FR",
			"../foo/Maildir-not-absolute",
			{},
			Flags::New,
			false,
			"/home/foo/Maildir/test/new/123456"
		},

		 TestCase{ /* fail: maildir must start with '/' */
			"/home/foo/Maildir/test/cur/123456",
			"/home/foo/Maildir",
			"mymaildirwithoutslash",
			Flags::Seen | Flags::Flagged,
			false,
			"/home/foo/Maildir/test/cur/123456:2,FS"
		 },

		 TestCase{ /* fail: path must be below maildir */
			"/home/foo/Maildir/test/cur/123456:2,FR",
			"/home/bar/Maildir",
			"/test2",
			Flags::Flagged | Flags::Replied,
			false,
			"/home/foo/Maildir/test2/cur/123456:2,FR"
		},
		 TestCase{ /* fail: New cannot be combined */
			"/home/foo/Maildir/test/new/123456",
			"/home/foo/Maildir",
			{},
			Flags::New | Flags::Replied,
			false,
			"/home/foo/Maildir/test/cur/123456:2,"
		},
	};

	for (auto&& testcase: testcases) {
		const auto res = maildir_determine_target(
			testcase.old_path,
			testcase.root_maildir,
			testcase.target_maildir,
			testcase.new_flags,
			testcase.new_name);
		g_assert_false(!!res);
	}
}



static void
test_maildir_get_new_path_01(void)
{
	struct {
		std::string	oldpath;
		Flags		flags;
		std::string	newpath;
	} paths[] = {{"/home/foo/Maildir/test/cur/123456:2,FR",
		      Flags::Replied,
		      "/home/foo/Maildir/test/cur/123456:2,R"},
		     {"/home/foo/Maildir/test/cur/123456:2,FR",
		      Flags::New,
		      "/home/foo/Maildir/test/new/123456"},
		     {"/home/foo/Maildir/test/new/123456:2,FR",
		      (Flags::Seen | Flags::Replied),
		      "/home/foo/Maildir/test/cur/123456:2,RS"},
		     {"/home/foo/Maildir/test/new/1313038887_0.697",
			(Flags::Seen | Flags::Flagged | Flags::Passed),
			"/home/foo/Maildir/test/cur/1313038887_0.697:2,FPS"},
		     {"/home/foo/Maildir/test/new/1313038887_0.697:2,",
			(Flags::Seen | Flags::Flagged | Flags::Passed),
			"/home/foo/Maildir/test/cur/1313038887_0.697:2,FPS"},
		     /* note the ':2,' suffix on the new message is
		      * removed */

		     {"/home/foo/Maildir/trash/new/1312920597.2206_16.cthulhu",
		      Flags::Seen,
		      "/home/foo/Maildir/trash/cur/1312920597.2206_16.cthulhu:2,S"}};

	for (int i = 0; i != G_N_ELEMENTS(paths); ++i) {
		const auto newpath{maildir_determine_target(paths[i].oldpath,
							    "/home/foo/Maildir",
							    {}, paths[i].flags, false)};
		assert_valid_result(newpath);
		assert_equal(*newpath, paths[i].newpath);
	}
}

static void
test_maildir_get_new_path_02(void)
{
	struct {
		std::string	oldpath;
		Flags		flags;
		std::string	targetdir;
		std::string	newpath;
		std::string     root_maildir;
	} paths[] = {{"/home/foo/Maildir/test/cur/123456:2,FR",
		      Flags::Replied,
		      "/blabla",
		      "/home/foo/Maildir/blabla/cur/123456:2,R",
		      "/home/foo/Maildir"},
		     {"/home/bar/Maildir/test/cur/123456:2,FR",
		      Flags::New,
		      "/coffee",
		      "/home/bar/Maildir/coffee/new/123456",
		      "/home/bar/Maildir"
		     },
		     {"/home/cuux/Maildir/test/new/123456",
		      (Flags::Seen | Flags::Replied),
		      "/tea",
		      "/home/cuux/Maildir/tea/cur/123456:2,RS",
		      "/home/cuux/Maildir"},
		     {"/home/boy/Maildir/test/new/1313038887_0.697:2,",
		      (Flags::Seen | Flags::Flagged | Flags::Passed),
		      "/stuff",
		      "/home/boy/Maildir/stuff/cur/1313038887_0.697:2,FPS",
		      "/home/boy/Maildir"}};

	for (int i = 0; i != G_N_ELEMENTS(paths); ++i) {
		auto newpath{maildir_determine_target(paths[i].oldpath,
						      paths[i].root_maildir,
						      paths[i].targetdir,
						      paths[i].flags,
						      false)};
		assert_valid_result(newpath);
		assert_equal(*newpath, paths[i].newpath);
	}
}


static void
test_maildir_get_new_path_custom_real(bool change_name)
{
	struct {
		std::string	oldpath;
		Flags		flags;
		std::string	targetdir;
		std::string	newpath;
		std::string     root_maildir;
	} paths[] = {{"/home/foo/Maildir/test/cur/123456:2,FR",
		      Flags::Replied,
		      "/blabla",
		      "/home/foo/Maildir/blabla/cur/123456:2,R",
		      "/home/foo/Maildir"},
		     {"/home/foo/Maildir/test/cur/123456:2,hFeRllo123",
		      Flags::Flagged,
		      "/blabla",
		      "/home/foo/Maildir/blabla/cur/123456:2,F",
		      "/home/foo/Maildir"},
		     {"/home/foo/Maildir/test/cur/123456:2,abc",
		      Flags::Passed,
		      "/blabla",
		      "/home/foo/Maildir/blabla/cur/123456:2,P",
		      "/home/foo/Maildir"}};

	for (int i = 0; i != G_N_ELEMENTS(paths); ++i) {
		auto newpath{maildir_determine_target(paths[i].oldpath,
						      paths[1].root_maildir,
						      paths[i].targetdir,
						      paths[i].flags,
						      change_name)};
		assert_valid_result(newpath);
		if (change_name)
			g_assert_true(*newpath != paths[i].newpath); // weak test
		else
			assert_equal(*newpath, paths[i].newpath);
	}
}


static void
test_maildir_get_new_path_custom(void)
{
	return test_maildir_get_new_path_custom_real(false);
}


static void
test_maildir_get_new_path_custom_change_name(void)
{
	return test_maildir_get_new_path_custom_real(true);
}


static void
test_maildir_from_path(void)
{
	unsigned u;

	struct {
		std::string path, exp;
	} cases[] = {{"/home/foo/Maildir/test/cur/123456:2,FR", "/test"},
		     {"/home/foo/Maildir/lala/new/1313038887_0.697:2,", "/lala"}};

	for (u = 0; u != G_N_ELEMENTS(cases); ++u) {
		auto mdir{maildir_from_path(cases[u].path, "/home/foo/Maildir")};
		assert_valid_result(mdir);
		assert_equal(*mdir, cases[u].exp);
	}
}

static void
test_maildir_link()
{
	TempDir tmpdir;

	assert_valid_result(maildir_mkdir(tmpdir.path() + "/foo"));
	assert_valid_result(maildir_mkdir(tmpdir.path() + "/bar"));

	const auto srcpath1 = tmpdir.path() + "/foo/cur/msg1";
	const auto srcpath2 = tmpdir.path() + "/foo/new/msg2";

	{
		std::ofstream stream(srcpath1);
		stream.write("cur", 3);
		g_assert_true(stream.good());
		stream.close();
	}

	{
		std::ofstream stream(srcpath2);
		stream.write("new", 3);
		g_assert_true(stream.good());
		stream.close();
	}

	assert_valid_result(maildir_link(srcpath1, tmpdir.path() + "/bar", false));
	assert_valid_result(maildir_link(srcpath2, tmpdir.path() + "/bar", false));

	const auto dstpath1 = tmpdir.path() + "/bar/cur/msg1";
	const auto dstpath2 = tmpdir.path() + "/bar/new/msg2";

	g_assert_true(g_access(dstpath1.c_str(), F_OK) == 0);
	g_assert_true(g_access(dstpath2.c_str(), F_OK) == 0);

	g_assert_false(!!maildir_clear_links("/nonexistent/bla/foo/xuux"));

	assert_valid_result(maildir_clear_links(tmpdir.path() + "/bar"));
	g_assert_false(g_access(dstpath1.c_str(), F_OK) == 0);
	g_assert_false(g_access(dstpath2.c_str(), F_OK) == 0);
}


static void
test_maildir_move(bool assume_remote)
{
	TempDir tmpdir;

	assert_valid_result(maildir_mkdir(tmpdir.path() + "/foo"));
	assert_valid_result(maildir_mkdir(tmpdir.path() + "/bar"));

	const auto srcpath1{join_paths(tmpdir.path(), "/foo/cur/msg1")};
	const auto srcpath2{join_paths(tmpdir.path(), "/foo/new/msg2")};

	{
		std::ofstream stream(srcpath1);
		stream.write("cur", 3);
		g_assert_true(stream.good());
		stream.close();
	}

	{
		std::ofstream stream(srcpath2);
		stream.write("new", 3);
		g_assert_true(stream.good());
		stream.close();
	}

	const auto dstpath = tmpdir.path() + "/test1";

	assert_valid_result(maildir_move_message(srcpath1, dstpath, assume_remote));
	assert_valid_result(maildir_move_message(srcpath2, dstpath, assume_remote));

	assert_valid_result(maildir_move_message(dstpath, dstpath)); // self-move is okay.
}

static void
test_maildir_move_vanilla()
{
	test_maildir_move(false/*!assume_remote*/);
}

static void
test_maildir_move_remote()
{
	test_maildir_move(true/*assume_remote*/);
}


int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	/* mu_util_maildir_mkmdir */
	g_test_add_func("/maildir/mkdir-01", test_maildir_mkdir_01);
	g_test_add_func("/maildir/mkdir-02", test_maildir_mkdir_02);
	g_test_add_func("/maildir/mkdir-03", test_maildir_mkdir_03);
	g_test_add_func("/maildir/mkdir-04", test_maildir_mkdir_04);
	g_test_add_func("/maildir/mkdir-05", test_maildir_mkdir_05);

	g_test_add_func("/maildir/determine-target-ok", test_determine_target_ok);
	g_test_add_func("/maildir/determine-target-fail", test_determine_target_fail);

	// /* get/set flags */
	g_test_add_func("/maildir/get-new-path-01", test_maildir_get_new_path_01);
	g_test_add_func("/maildir/get-new-path-02", test_maildir_get_new_path_02);
	g_test_add_func("/maildir/get-new-path-custom", test_maildir_get_new_path_custom);
	g_test_add_func("/maildir/get-new-path-custom-change-name",
			test_maildir_get_new_path_custom_change_name);

	g_test_add_func("/maildir/from-path", test_maildir_from_path);

	g_test_add_func("/maildir/link", test_maildir_link);
	g_test_add_func("/maildir/move-vanilla", test_maildir_move_vanilla);
	g_test_add_func("/maildir/move-remote", test_maildir_move_remote);

	return g_test_run();
}
