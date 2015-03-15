/*
** Copyright (C) 2008-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#if HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include <glib.h>
#include <glib/gstdio.h>

#include <stdlib.h>
#include <unistd.h>
#include <limits.h>

#include "test-mu-common.h"
#include "lib/mu-util.h"

static void
test_mu_util_dir_expand_00 (void)
{
#ifdef HAVE_WORDEXP_H
	gchar *got, *expected;

	got = mu_util_dir_expand ("~/IProbablyDoNotExist");
	expected = g_strdup_printf ("%s%cIProbablyDoNotExist",
				    getenv("HOME"), G_DIR_SEPARATOR);

	g_assert_cmpstr (got,==,expected);

	g_free (got);
	g_free (expected);
#endif /*HAVE_WORDEXP_H*/
}

static void
test_mu_util_dir_expand_01 (void)
{
	/* XXXX: the testcase does not work when using some dir
	 * setups; (see issue #585), although the code should still
	 * work. Turn of the test for now */
	return;

	
#ifdef HAVE_WORDEXP_H
	{
	gchar *got, *expected;

	got = mu_util_dir_expand ("~/Desktop");
	expected = g_strdup_printf ("%s%cDesktop",
				    getenv("HOME"), G_DIR_SEPARATOR);

	g_assert_cmpstr (got,==,expected);

	g_free (got);
	g_free (expected);
	}
#endif /*HAVE_WORDEXP_H*/
}



static void
test_mu_util_guess_maildir_01 (void)
{
	char *got;
	const char *expected;

	/* skip the test if there's no /tmp */
	if (access ("/tmp", F_OK))
		return;

	g_setenv ("MAILDIR", "/tmp", TRUE);

	got = mu_util_guess_maildir ();
	expected = "/tmp";

	g_assert_cmpstr (got,==,expected);
	g_free (got);
}


static void
test_mu_util_guess_maildir_02 (void)
{
	char *got, *mdir;

	g_unsetenv ("MAILDIR");

	mdir = g_strdup_printf ("%s%cMaildir",
				getenv("HOME"), G_DIR_SEPARATOR);
	got = mu_util_guess_maildir ();

	if (access (mdir, F_OK) == 0)
		g_assert_cmpstr (got, ==, mdir);
	else
		g_assert_cmpstr (got, == , NULL);

	g_free (got);
	g_free (mdir);
}


static void
test_mu_util_check_dir_01 (void)
{
	if (g_access ("/usr/bin", F_OK) == 0) {
		g_assert_cmpuint (
			mu_util_check_dir ("/usr/bin", TRUE, FALSE) == TRUE,
			==,
			g_access ("/usr/bin", R_OK) == 0);
	}
}


static void
test_mu_util_check_dir_02 (void)
{
	if (g_access ("/tmp", F_OK) == 0) {
		g_assert_cmpuint (
			mu_util_check_dir ("/tmp", FALSE, TRUE) == TRUE,
			==,
			g_access ("/tmp", W_OK) == 0);
	}
}


static void
test_mu_util_check_dir_03 (void)
{
	if (g_access (".", F_OK) == 0) {
		g_assert_cmpuint (
			mu_util_check_dir (".", TRUE, TRUE) == TRUE,
			==,
			g_access (".", W_OK | R_OK) == 0);
	}
}


static void
test_mu_util_check_dir_04 (void)
{
	/* not a dir, so it must be false */
	g_assert_cmpuint (
		mu_util_check_dir ("test-util.c", TRUE, TRUE),
		==,
		FALSE);
}


static void
test_mu_util_str_from_strv_01 (void)
{
	const gchar *strv[] = { "aap", "noot", "mies", NULL };
	gchar *str = mu_util_str_from_strv (strv);
	g_assert_cmpstr (str, ==, "aap noot mies");
	g_free (str);

}

static void
test_mu_util_str_from_strv_02 (void)
{
	const gchar *strv[] = { "test", NULL };
	gchar *str = mu_util_str_from_strv (strv);
	g_assert_cmpstr (str, ==, "test");
	g_free (str);

}

static void
test_mu_util_str_from_strv_03 (void)
{
	const gchar *strv[] = { NULL };
	gchar *str = mu_util_str_from_strv (strv);
	g_assert_cmpstr (str, ==, "");
	g_free (str);
}


static void
test_mu_util_get_dtype_with_lstat (void)
{
	g_assert_cmpuint (
		mu_util_get_dtype_with_lstat (MU_TESTMAILDIR), ==, DT_DIR);
	g_assert_cmpuint (
		mu_util_get_dtype_with_lstat (MU_TESTMAILDIR2), ==, DT_DIR);
	g_assert_cmpuint (
		mu_util_get_dtype_with_lstat (MU_TESTMAILDIR2 "/Foo/cur/mail5"),
		==, DT_REG);
}


static void
test_mu_util_supports (void)
{
	gboolean has_guile;
	gchar *path;

	has_guile = FALSE;
#ifdef BUILD_GUILE
	has_guile = TRUE;
#endif /*BUILD_GUILE*/

	g_assert_cmpuint (mu_util_supports (MU_FEATURE_GUILE),	== ,has_guile);
	g_assert_cmpuint (mu_util_supports (MU_FEATURE_CRYPTO),	== ,TRUE);

	path = g_find_program_in_path ("gnuplot");
	g_free (path);

	g_assert_cmpuint (mu_util_supports (MU_FEATURE_GNUPLOT),==,path ? TRUE : FALSE);

	g_assert_cmpuint (
		mu_util_supports (MU_FEATURE_GNUPLOT|MU_FEATURE_GUILE|MU_FEATURE_CRYPTO),
		==,
		has_guile && path ? TRUE : FALSE);
}


static void
test_mu_util_program_in_path (void)
{
	g_assert_cmpuint (mu_util_program_in_path("ls"),==,TRUE);
}



int
main (int argc, char *argv[])
{
	g_test_init (&argc, &argv, NULL);

	/* mu_util_dir_expand */
	g_test_add_func ("/mu-util/mu-util-dir-expand-00", test_mu_util_dir_expand_00);
	g_test_add_func ("/mu-util/mu-util-dir-expand-01", test_mu_util_dir_expand_01);

	/* mu_util_guess_maildir */
	g_test_add_func ("/mu-util/mu-util-guess-maildir-01",
			 test_mu_util_guess_maildir_01);
	g_test_add_func ("/mu-util/mu-util-guess-maildir-02",
			 test_mu_util_guess_maildir_02);

	/* mu_util_check_dir */
	g_test_add_func ("/mu-util/mu-util-check-dir-01", test_mu_util_check_dir_01);
	g_test_add_func ("/mu-util/mu-util-check-dir-02", test_mu_util_check_dir_02);
	g_test_add_func ("/mu-util/mu-util-check-dir-03", test_mu_util_check_dir_03);
	g_test_add_func ("/mu-util/mu-util-check-dir-04", test_mu_util_check_dir_04);

	/* test_mu_util_str_from_strv */
	g_test_add_func ("/mu-util/mu-util-str-from-strv-01",
			 test_mu_util_str_from_strv_01);
	g_test_add_func ("/mu-util/mu-util-str-from-strv-02",
			 test_mu_util_str_from_strv_02);
	g_test_add_func ("/mu-util/mu-util-str-from-strv-03",
			 test_mu_util_str_from_strv_03);

	g_test_add_func ("/mu-util/mu-util-get-dtype-with-lstat",
			 test_mu_util_get_dtype_with_lstat);

	g_test_add_func ("/mu-util/mu-util-supports", test_mu_util_supports);
	g_test_add_func ("/mu-util/mu-util-program-in-path", test_mu_util_program_in_path);


	g_log_set_handler (NULL,
			   G_LOG_LEVEL_DEBUG|
			   G_LOG_LEVEL_MESSAGE|
			   G_LOG_LEVEL_INFO, (GLogFunc)black_hole, NULL);

	return g_test_run ();
}
