/* 
** Copyright (C) 2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include <glib.h>
#include <stdlib.h>
#include <unistd.h>

#include "src/mu-util.h"

static void
test_mu_util_dir_expand_01 (void)
{
	gchar *got, *expected;
	
	got = mu_util_dir_expand ("~/Desktop");
	expected = g_strdup_printf ("%s%cDesktop",
				    getenv("HOME"), G_DIR_SEPARATOR);
	
	g_assert_cmpstr (got,==,expected);
	
	g_free (got);
	g_free (expected);
	
}

static void
test_mu_util_dir_expand_02 (void)
{
	gchar *got, *expected, *tmp;
	
	tmp = g_strdup_printf ("~%s/Desktop", getenv("LOGNAME"));
	got = mu_util_dir_expand (tmp);
	expected = g_strdup_printf ("%s%cDesktop",
				    getenv("HOME"), G_DIR_SEPARATOR);
	
	g_assert_cmpstr (got,==,expected);

	g_free (tmp);
	g_free (got);
	g_free (expected);
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
shutup (void) {}

int
main (int argc, char *argv[])
{
	g_test_init (&argc, &argv, NULL);

	/* mu_util_dir_expand */
	g_test_add_func ("/mu-util/mu-util-dir-expand-01",
			 test_mu_util_dir_expand_01);
	g_test_add_func ("/mu-util/mu-util-dir-expand-02",
			 test_mu_util_dir_expand_02);

	/* mu_util_guess_maildir */
	g_test_add_func ("/mu-util/mu-util-guess-maildir-01",
			 test_mu_util_guess_maildir_01);
	g_test_add_func ("/mu-util/mu-util-guess-maildir-02",
			 test_mu_util_guess_maildir_02);
	
	g_log_set_handler (NULL,
			   G_LOG_LEVEL_DEBUG|
			   G_LOG_LEVEL_MESSAGE|
			   G_LOG_LEVEL_INFO, (GLogFunc)shutup, NULL);

	return g_test_run ();
}
