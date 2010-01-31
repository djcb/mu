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
#include <glib/gstdio.h>

#include <stdlib.h>
#include <unistd.h>
#include <string.h>


#include "src/mu-query-xapian.h"

static char*
random_tmpdir (void)
{
        return g_strdup_printf ("%s%cmu-test-%x", g_get_tmp_dir(),
                                G_DIR_SEPARATOR,
                                (int)random()*getpid()*(int)time(NULL));
}



/* static gboolean */
/* ignore_error (const char* log_domain, GLogLevelFlags log_level, const gchar* msg, */
/* 	      gpointer user_data) */
/* { */
/* 	return FALSE; /\* don't abort *\/ */
/* } */

static void shutup (void) {}


static gchar*
fill_database (void)
{
	gchar *cmdline, *tmpdir, *xpath;
	
	tmpdir = random_tmpdir();
	cmdline = g_strdup_printf ("%s index --muhome=%s --maildir=%s%ctestdir"
				   " --quiet",
				   MU_PROGRAM, tmpdir, ABS_SRCDIR, G_DIR_SEPARATOR);
	g_assert (g_spawn_command_line_sync (cmdline, NULL, NULL, NULL, NULL));
	g_free (cmdline);

	xpath= g_strdup_printf ("%s%c%s", tmpdir, G_DIR_SEPARATOR, "xapian");
	g_free (tmpdir);
	
	return xpath;
}

typedef struct  {
	const char *query;
	size_t count; /* expected number of matches */
} QResults;

static void
test_mu_query_01 (void)
{
	MuQueryXapian *query;
	gchar *xpath;
	int i;
	
	QResults queries[] = {
		{ "basic",              3 },
		{ "question",           5 },
		{ "thanks",             2 },
		{ "subject:elisp",      1 },
		{ "html",               4 },
		{ "html AND contains",  1 },
		{ "from:pepernoot",     0 },		
	};

	
	xpath = fill_database ();
	g_assert (xpath != NULL);
	
	query = mu_query_xapian_new (xpath);

	for (i = 0; i != G_N_ELEMENTS(queries); ++i) {
		
		int count;
		MuMsgIterXapian *iter;
		
		iter = mu_query_xapian_run (query, queries[i].query, NULL, FALSE, 0);

		count = 0;
		while (!mu_msg_iter_xapian_is_done (iter)) {
			mu_msg_iter_xapian_next (iter);
			++count;
		}
		
		g_assert_cmpuint (queries[i].count, ==, count);
		mu_msg_iter_xapian_destroy (iter);
	}

	mu_query_xapian_destroy (query);
	g_free (xpath);
}

int
main (int argc, char *argv[])
{
	g_test_init (&argc, &argv, NULL);

	/* mu_util_maildir_mkmdir */
 	g_test_add_func ("/mu-query/test-mu-query-01",
			 test_mu_query_01);
	
	g_log_set_handler (NULL,
			   G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL| G_LOG_FLAG_RECURSION,
			   (GLogFunc)shutup, NULL);
	
	return g_test_run ();
}
