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

#include "src/mu-maildir.h"

static char*
random_tmpdir (void)
{
        return g_strdup_printf ("%s%cmu-test-%x", g_get_tmp_dir(),
                                G_DIR_SEPARATOR,
                                (int)random()*getpid()*(int)time(NULL));
}


static void
test_mu_maildir_mkmdir_01 (void)
{
	int i;
	gchar *tmpdir, *mdir, *tmp;
	const gchar *subs[] = {"tmp", "cur", "new"};
	
	tmpdir = random_tmpdir ();
	mdir   = g_strdup_printf ("%s%c%s", tmpdir, G_DIR_SEPARATOR,
				  "cuux");
	
	g_assert_cmpuint (mu_maildir_mkmdir (mdir, 0755, FALSE),
			  ==, TRUE);

	for (i = 0; i != G_N_ELEMENTS(subs); ++i) {
		gchar* dir;
		
		dir = g_strdup_printf ("%s%c%s", mdir, G_DIR_SEPARATOR,
				       subs[i]);
		g_assert_cmpuint (g_access (dir, R_OK), ==, 0);
		g_assert_cmpuint (g_access (dir, W_OK), ==, 0);
		g_free (dir);
	}

	tmp = g_strdup_printf ("%s%c%s", mdir, G_DIR_SEPARATOR, ".noindex");
	g_assert_cmpuint (g_access (tmp, F_OK), !=, 0);
	
	g_free (tmp);
	g_free (tmpdir);
	g_free (mdir);
	
}


static void
test_mu_maildir_mkmdir_02 (void)
{
	int i;
	gchar *tmpdir, *mdir, *tmp;
	const gchar *subs[] = {"tmp", "cur", "new"};
	
	tmpdir = random_tmpdir ();
	mdir   = g_strdup_printf ("%s%c%s", tmpdir, G_DIR_SEPARATOR,
				  "cuux");
	
	g_assert_cmpuint (mu_maildir_mkmdir (mdir, 0755, TRUE),
			  ==, TRUE);

	for (i = 0; i != G_N_ELEMENTS(subs); ++i) {
		gchar* dir;
		
		dir = g_strdup_printf ("%s%c%s", mdir, G_DIR_SEPARATOR,
				       subs[i]);
		g_assert_cmpuint (g_access (dir, R_OK), ==, 0);

		g_assert_cmpuint (g_access (dir, W_OK), ==, 0);
		g_free (dir);
	}
	
	tmp = g_strdup_printf ("%s%c%s", mdir, G_DIR_SEPARATOR, ".noindex");
	g_assert_cmpuint (g_access (tmp, F_OK), ==, 0);
	
	g_free (tmp);
	g_free (tmpdir);
	g_free (mdir);	
}



static gboolean
ignore_error (const char* log_domain, GLogLevelFlags log_level, const gchar* msg,
	      gpointer user_data)
{
	return FALSE; /* don't abort */
}

static void
test_mu_maildir_mkmdir_03 (void)
{	
	/* this must fail */
	g_test_log_set_fatal_handler ((GTestLogFatalFunc)ignore_error, NULL);

	g_assert_cmpuint (mu_maildir_mkmdir (NULL, 0755, TRUE),
					    ==, FALSE);
}
		

static void
shutup (void) {}



int
main (int argc, char *argv[])
{
	g_test_init (&argc, &argv, NULL);

	/* mu_util_dir_expand */
 	g_test_add_func ("/mu-maildir/mu-maildir-mkmdir-01",
			 test_mu_maildir_mkmdir_01);
	g_test_add_func ("/mu-maildir/mu-maildir-mkmdir-02",
			 test_mu_maildir_mkmdir_02);
	g_test_add_func ("/mu-maildir/mu-maildir-mkmdir-03",
			 test_mu_maildir_mkmdir_03);

	g_log_set_handler (NULL,
			   G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL| G_LOG_FLAG_RECURSION,
			   (GLogFunc)shutup, NULL);
	
	return g_test_run ();
}
