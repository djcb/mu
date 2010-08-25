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


#include "test-mu-common.h"
#include "src/mu-query-xapian.h"


static void shutup (void) {}


static gchar*
fill_database (void)
{
	gchar *cmdline, *tmpdir, *xpath;
	
	tmpdir = test_mu_common_get_random_tmpdir();
	cmdline = g_strdup_printf ("%s index --muhome=%s --maildir=%s"
				   " --quiet",
				   MU_PROGRAM, tmpdir, MU_TESTMAILDIR);
	g_assert (g_spawn_command_line_sync (cmdline, NULL, NULL, NULL, NULL));
	g_free (cmdline);

	xpath= g_strdup_printf ("%s%c%s", tmpdir, G_DIR_SEPARATOR, "xapian");
	g_free (tmpdir);
	
	return xpath;
}

static guint
count_matches (	MuMsgIter *iter)
{
	guint count;

	if (mu_msg_iter_is_null(iter))
		return 0;

	count = 1;
	while (mu_msg_iter_next (iter))
		++count;

	return count;
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
		{ "from:pepernoot",     0 }
	};
	xpath = fill_database ();
	g_assert (xpath != NULL);
	
	query = mu_query_xapian_new (xpath);

	for (i = 0; i != G_N_ELEMENTS(queries); ++i) {
		int count = 0;
		MuMsgIter *iter =
			mu_query_xapian_run (query, queries[i].query, NULL,
					     FALSE, 1);

		g_assert_cmpuint (queries[i].count, ==, count_matches(iter));
		mu_msg_iter_destroy (iter);
	}

	mu_query_xapian_destroy (query);
	g_free (xpath);
}


static void
test_mu_query_02 (void)
{
	MuMsgIter *iter;
	MuQueryXapian *query;
	const char* q;
	gchar *xpath;
	int i;
	
	xpath = fill_database ();
	g_assert (xpath != NULL);
	
	query = mu_query_xapian_new (xpath);
	g_assert (query);

	q = "i:f7ccd24b0808061357t453f5962w8b61f9a453b684d0@mail.gmail.com";
	iter = mu_query_xapian_run (query, q, NULL, FALSE, 0);
	
	g_assert (iter);
	g_assert_cmpuint (count_matches(iter), ==, 1);

	mu_query_xapian_destroy (query);
	g_free (xpath);
}


static void
test_mu_query_03 (void)
{
	MuQueryXapian *query;
	gchar *xpath;
	int i;
	
	QResults queries[] = {
		{ "t:help-gnu-emacs@gnu.org",1 },
	};
	xpath = fill_database ();
	g_assert (xpath != NULL);
	
	query = mu_query_xapian_new (xpath);

	for (i = 0; i != G_N_ELEMENTS(queries); ++i) {
		int count = 0;
		MuMsgIter *iter =
			mu_query_xapian_run (query, queries[i].query, NULL,
					     FALSE, 1);

		g_assert_cmpuint (queries[i].count, ==, count_matches(iter));
		mu_msg_iter_destroy (iter);
	}

	mu_query_xapian_destroy (query);
	g_free (xpath);
}


int
main (int argc, char *argv[])
{
	g_test_init (&argc, &argv, NULL);

	g_test_add_func ("/mu-query/test-mu-query-01", test_mu_query_01);
 	g_test_add_func ("/mu-query/test-mu-query-02", test_mu_query_02);
	/* g_test_add_func ("/mu-query/test-mu-query-03", test_mu_query_03); */
	
	g_log_set_handler (NULL,
			   G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL| G_LOG_FLAG_RECURSION,
			   (GLogFunc)shutup, NULL);
	
	return g_test_run ();
}
