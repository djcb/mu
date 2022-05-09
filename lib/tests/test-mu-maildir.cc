/*
** Copyright (C) 2008-2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "test-mu-common.hh"
#include "mu-maildir.hh"
#include "utils/mu-util.h"

using namespace Mu;

static void
test_maildir_mkdir_01(void)
{
	int          i;
	gchar *      tmpdir, *mdir, *tmp;
	const gchar* subs[] = {"tmp", "cur", "new"};

	tmpdir = test_mu_common_get_random_tmpdir();
	mdir   = g_strdup_printf("%s%c%s", tmpdir, G_DIR_SEPARATOR, "cuux");

	g_assert_true(!!maildir_mkdir(mdir, 0755, FALSE));

	for (i = 0; i != G_N_ELEMENTS(subs); ++i) {
		gchar* dir;

		dir = g_strdup_printf("%s%c%s", mdir, G_DIR_SEPARATOR, subs[i]);
		g_assert_cmpuint(g_access(dir, R_OK), ==, 0);
		g_assert_cmpuint(g_access(dir, W_OK), ==, 0);
		g_free(dir);
	}

	tmp = g_strdup_printf("%s%c%s", mdir, G_DIR_SEPARATOR, ".noindex");
	g_assert_cmpuint(g_access(tmp, F_OK), !=, 0);

	g_free(tmp);
	g_free(tmpdir);
	g_free(mdir);
}

static void
test_maildir_mkdir_02(void)
{
	int          i;
	gchar *      tmpdir, *mdir, *tmp;
	const gchar* subs[] = {"tmp", "cur", "new"};

	tmpdir = test_mu_common_get_random_tmpdir();
	mdir   = g_strdup_printf("%s%c%s", tmpdir, G_DIR_SEPARATOR, "cuux");

	g_assert_true(!!maildir_mkdir(mdir, 0755, TRUE));

	for (i = 0; i != G_N_ELEMENTS(subs); ++i) {
		gchar* dir;

		dir = g_strdup_printf("%s%c%s", mdir, G_DIR_SEPARATOR, subs[i]);
		g_assert_cmpuint(g_access(dir, R_OK), ==, 0);

		g_assert_cmpuint(g_access(dir, W_OK), ==, 0);
		g_free(dir);
	}

	tmp = g_strdup_printf("%s%c%s", mdir, G_DIR_SEPARATOR, ".noindex");
	g_assert_cmpuint(g_access(tmp, F_OK), ==, 0);

	g_free(tmp);
	g_free(tmpdir);
	g_free(mdir);
}

static void
test_maildir_mkdir_03(void)
{
	int          i;
	gchar *      tmpdir, *mdir, *tmp;
	const gchar* subs[] = {"tmp", "cur", "new"};

	tmpdir = test_mu_common_get_random_tmpdir();
	mdir   = g_strdup_printf("%s%c%s", tmpdir, G_DIR_SEPARATOR, "cuux");

	/* create part of the structure already... */
	{
		gchar* dir;
		dir = g_strdup_printf("%s%ccur", mdir, G_DIR_SEPARATOR);
		g_assert_cmpuint(g_mkdir_with_parents(dir, 0755), ==, 0);
		g_free(dir);
	}

	/* this should still work */
	g_assert_true(!!maildir_mkdir(mdir, 0755, FALSE));

	for (i = 0; i != G_N_ELEMENTS(subs); ++i) {
		gchar* dir;

		dir = g_strdup_printf("%s%c%s", mdir, G_DIR_SEPARATOR, subs[i]);
		g_assert_cmpuint(g_access(dir, R_OK), ==, 0);
		g_assert_cmpuint(g_access(dir, W_OK), ==, 0);
		g_free(dir);
	}

	tmp = g_strdup_printf("%s%c%s", mdir, G_DIR_SEPARATOR, ".noindex");
	g_assert_cmpuint(g_access(tmp, F_OK), !=, 0);

	g_free(tmp);
	g_free(tmpdir);
	g_free(mdir);
}

static void
test_maildir_mkdir_04(void)
{
	gchar *tmpdir, *mdir;

	tmpdir = test_mu_common_get_random_tmpdir();
	mdir   = g_strdup_printf("%s%c%s", tmpdir, G_DIR_SEPARATOR, "cuux");

	/* create part of the structure already... */
	{
		gchar* dir;
		g_assert_cmpuint(g_mkdir_with_parents(mdir, 0755), ==, 0);
		dir = g_strdup_printf("%s%ccur", mdir, G_DIR_SEPARATOR);
		g_assert_cmpuint(g_mkdir_with_parents(dir, 0000), ==, 0);
		g_free(dir);
	}

	/* this should fail now, because cur is not read/writable  */
	if (geteuid() != 0)
		g_assert_false(!!maildir_mkdir(mdir, 0755, false));

	g_free(tmpdir);
	g_free(mdir);
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

static void
test_maildir_flags_from_path(void)
{
	int i;
	struct {
		const char*  path;
		Flags flags;
	} paths[] = {
		{"/home/foo/Maildir/test/cur/123456:2,FSR",
		 (Flags::Replied | Flags::Seen | Flags::Flagged)},
		{"/home/foo/Maildir/test/new/123456", Flags::New},
		{/* NOTE: when in new/, the :2,.. stuff is ignored */
			"/home/foo/Maildir/test/new/123456:2,FR",
			Flags::New},
		{"/home/foo/Maildir/test/cur/123456:2,DTP",
		 (Flags::Draft | Flags::Trashed | Flags::Passed)},
		{"/home/foo/Maildir/test/cur/123456:2,S", Flags::Seen}};

	for (i = 0; i != G_N_ELEMENTS(paths); ++i) {
		auto res{maildir_flags_from_path(paths[i].path)};
		g_assert_true(!!res);
		if (g_test_verbose())
			g_print("%s -> <%s>\n", paths[i].path,
				to_string(res.value()).c_str());
		g_assert_true(res.value() == paths[i].flags);
	}
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











// static void
// test_maildir_determine_target(void)
// {
//	int i;

//	struct {
//		std::string	oldpath;
//		Flags    flags;
//		std::string	newpath;
//	} paths[] = {{"/home/foo/Maildir/test/cur/123456:2,FR",
//		      Flags::Replied,
//		      "/home/foo/Maildir/test/cur/123456:2,R"},
//		     {"/home/foo/Maildir/test/cur/123456:2,FR",
//		      Flags::New,
//		      "/home/foo/Maildir/test/new/123456"},
//		     {"/home/foo/Maildir/test/new/123456:2,FR",
//		      (Flags::Seen | Flags::Replied),
//		      "/home/foo/Maildir/test/cur/123456:2,RS"},
//		     {"/home/foo/Maildir/test/new/1313038887_0.697:2,",
//		      (Flags::Seen | Flags::Flagged | Flags::Passed),
//		      "/home/foo/Maildir/test/cur/1313038887_0.697:2,FPS"},
//		     {"/home/djcb/Maildir/trash/new/1312920597.2206_16.cthulhu",
//		      Flags::Seen,
//		      "/home/djcb/Maildir/trash/cur/1312920597.2206_16.cthulhu:2,S"}};

//	for (i = 0; i != G_N_ELEMENTS(paths); ++i) {
//		const auto res{maildir_determine_target(paths[i].oldpath,
//							   "/home/foo/Maildir",
//							   {},
//							   paths[i].flags, false)};
//		g_assert_true(res && res.value() == paths[i].newpath);
//		char *newbase = g_path_get_basename(newpath->c_str());
//		assert_matches_regexp(newbase,
//				      "\\d+\\."
//				      "[[:xdigit:]]{16}\\."
//				      "[[:alnum:]][[:alnum:]-]+(:2,.*)?");
//		g_free(newbase);
//	}
// }

// static void
// test_maildir_get_new_path_01(void)
// {
//	struct {
//		std::string	oldpath;
//		Flags    flags;
//		std::string	newpath;
//	} paths[] = {{"/home/foo/Maildir/test/cur/123456:2,FR",
//		      Flags::Replied,
//		      "/home/foo/Maildir/test/cur/123456:2,R"},
//		     {"/home/foo/Maildir/test/cur/123456:2,FR",
//		      Flags::New,
//		      "/home/foo/Maildir/test/new/123456"},
//		     {"/home/foo/Maildir/test/new/123456:2,FR",
//		      (Flags::Seen | Flags::Replied),
//		      "/home/foo/Maildir/test/cur/123456:2,RS"},
//		     {"/home/foo/Maildir/test/new/1313038887_0.697:2,",
//		      (Flags::Seen | Flags::Flagged | Flags::Passed),
//		      "/home/foo/Maildir/test/cur/1313038887_0.697:2,FPS"},
//		     {"/home/djcb/Maildir/trash/new/1312920597.2206_16.cthulhu",
//		      Flags::Seen,
//		      "/home/djcb/Maildir/trash/cur/1312920597.2206_16.cthulhu:2,S"}};

//	for (int i = 0; i != G_N_ELEMENTS(paths); ++i) {
//		const auto newpath{maildir_determine_target(
//				paths[i].oldpath,
//				"/home/foo/maildir",
//				{}, paths[i].flags, false)};
//		g_assert_true(newpath.has_value());
//		g_assert_true(*newpath == paths[i].newpath);
//	}
// }

// static void
// test_maildir_get_new_path_02(void)
// {
//	struct {
//		std::string	oldpath;
//		Flags    flags;
//		std::string	targetdir;
//		std::string	newpath;
//	} paths[] = {{"/home/foo/Maildir/test/cur/123456:2,FR",
//		      Flags::Replied,
//		      "/home/foo/Maildir/blabla",
//		      "/home/foo/Maildir/blabla/cur/123456:2,R"},
//		     {"/home/foo/Maildir/test/cur/123456:2,FR",
//		      Flags::New,
//		      "/home/bar/Maildir/coffee",
//		      "/home/bar/Maildir/coffee/new/123456"},
//		     {"/home/foo/Maildir/test/new/123456",
//		      (Flags::Seen | Flags::Replied),
//		      "/home/cuux/Maildir/tea",
//		      "/home/cuux/Maildir/tea/cur/123456:2,RS"},
//		     {"/home/foo/Maildir/test/new/1313038887_0.697:2,",
//		      (Flags::Seen | Flags::Flagged | Flags::Passed),
//		      "/home/boy/Maildir/stuff",
//		      "/home/boy/Maildir/stuff/cur/1313038887_0.697:2,FPS"}};

//	for (int i = 0; i != G_N_ELEMENTS(paths); ++i) {
//		auto newpath{maildir_determine_target(paths[i].oldpath,
//							 paths[i].targetdir,
//							 paths[i].flags,
//							 false)};
//		g_assert_true(newpath.has_value());
//		g_assert_true(*newpath == paths[i].newpath);
//	}
// }

// static void
// test_maildir_get_new_path_custom(void)
// {
//	struct {
//		std::string	oldpath;
//		Flags    flags;
//		std::string	targetdir;
//		std::string	newpath;
//	} paths[] = {{"/home/foo/Maildir/test/cur/123456:2,FR",
//		      Flags::Replied,
//		      "/home/foo/Maildir/blabla",
//		      "/home/foo/Maildir/blabla/cur/123456:2,R"},
//		     {"/home/foo/Maildir/test/cur/123456:2,hFeRllo123",
//		      Flags::Flagged,
//		      "/home/foo/Maildir/blabla",
//		      "/home/foo/Maildir/blabla/cur/123456:2,Fhello123"},
//		     {"/home/foo/Maildir/test/cur/123456:2,abc",
//		      Flags::Passed,
//		      "/home/foo/Maildir/blabla",
//		      "/home/foo/Maildir/blabla/cur/123456:2,Pabc"}};

//	for (int i = 0; i != G_N_ELEMENTS(paths); ++i) {
//		auto newpath{maildir_get_new_path(paths[i].oldpath,
//					      paths[i].targetdir,
//					      paths[i].flags,
//					      FALSE)};
//		g_assert_true(newpath);
//		g_assert_true(*newpath == paths[i].newpath);
//	}
// }

// static void
// test_maildir_from_path(void)
// {
//	unsigned u;

//	struct {
//		std::string path, exp;
//	} cases[] = {{"/home/foo/Maildir/test/cur/123456:2,FR", "/home/foo/Maildir/test"},
//		     {"/home/foo/Maildir/lala/new/1313038887_0.697:2,", "/home/foo/Maildir/lala"}};

//	for (u = 0; u != G_N_ELEMENTS(cases); ++u) {
//		auto mdir{maildir_from_path(cases[u].path)};
//		g_assert_true(mdir.has_value());
//		g_assert_true(*mdir == cases[u].exp);
//	}
// }

int
main(int argc, char* argv[])
{
	g_test_init(&argc, &argv, NULL);

	/* mu_util_maildir_mkmdir */
	g_test_add_func("/mu-maildir/mu-maildir-mkdir-01", test_maildir_mkdir_01);
	g_test_add_func("/mu-maildir/mu-maildir-mkdir-02", test_maildir_mkdir_02);
	g_test_add_func("/mu-maildir/mu-maildir-mkdir-03", test_maildir_mkdir_03);
	g_test_add_func("/mu-maildir/mu-maildir-mkdir-04", test_maildir_mkdir_04);
	g_test_add_func("/mu-maildir/mu-maildir-mkdir-05", test_maildir_mkdir_05);

	g_test_add_func("/mu-maildir/mu-maildir-flags-from-path",
			test_maildir_flags_from_path);


	g_test_add_func("/mu-maildir/mu-maildir-determine-target-ok",
			test_determine_target_ok);


	// /* get/set flags */
	// g_test_add_func("/mu-maildir/mu-maildir-get-new-path-new",
	//                 test_maildir_get_new_path_new);

	// g_test_add_func("/mu-maildir/mu-maildir-get-new-path-01", test_maildir_get_new_path_01);
	// g_test_add_func("/mu-maildir/mu-maildir-get-new-path-02", test_maildir_get_new_path_02);
	// g_test_add_func("/mu-maildir/mu-maildir-get-new-path-custom",
	//                 test_maildir_get_new_path_custom);
	// g_test_add_func("/mu-maildir/mu-maildir-get-flags-from-path",
	//                 test_maildir_get_flags_from_path);

	// g_test_add_func("/mu-maildir/mu-maildir-from-path",
	//                 test_maildirx_from_path);

	g_log_set_handler(
	    NULL,
	    (GLogLevelFlags)(G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL | G_LOG_FLAG_RECURSION),
	    (GLogFunc)black_hole,
	    NULL);

	return g_test_run();
}
