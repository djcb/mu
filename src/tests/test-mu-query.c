/* 
** Copyright (C) 2008-2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include "src/mu-query.h"

static gchar*
fill_database (void)
{
	gchar *cmdline, *tmpdir, *xpath;
	
	tmpdir = test_mu_common_get_random_tmpdir();
	cmdline = g_strdup_printf ("%s index --muhome=%s --maildir=%s"
				   " --quiet",
				   MU_PROGRAM, tmpdir, MU_TESTMAILDIR);

	/* g_printerr ("\n%s\n", cmdline); */
	
	g_assert (g_spawn_command_line_sync (cmdline, NULL, NULL,
					     NULL, NULL));
	g_free (cmdline);

	xpath= g_strdup_printf ("%s%c%s", tmpdir, G_DIR_SEPARATOR, "xapian");
	g_free (tmpdir);
	
	return xpath;
}


/* note: this also *moves the iter* */
static guint
run_and_count_matches (const char *xpath, const char *query)
{
	MuQuery  *mquery;
	MuMsgIter *iter;
	guint count;
	
	mquery = mu_query_new (xpath, NULL);
	g_assert (query);

	/* g_printerr ("\n=>'%s'\n", query); */
	
	iter = mu_query_run (mquery, query, MU_MSG_FIELD_ID_NONE,
			     FALSE, 1, NULL);
	mu_query_destroy (mquery);
	g_assert (iter);

	for (count = 0; !mu_msg_iter_is_done(iter);
	     mu_msg_iter_next(iter), ++count);
	
	mu_msg_iter_destroy (iter);
	
	return count;
}


typedef struct  {
	const char *query;
	size_t count; /* expected number of matches */
} QResults;

static void
test_mu_query_01 (void)
{
	gchar *xpath;
	int i;
	
	QResults queries[] = {
		{ "basic",              3 },
		{ "question",           5 },
		{ "thanks",             2 },
		{ "html",               4 },
		{ "subject:elisp",      1 },
		{ "html AND contains",  1 },
		{ "html and contains",  1 },
		{ "from:pepernoot",     0 },
		{ "foo:pepernoot",      0 },
		{ "funky",              1 },
		{ "fünkÿ",              1 },
	};

	
	xpath = fill_database ();
	g_assert (xpath != NULL);
	
 	for (i = 0; i != G_N_ELEMENTS(queries); ++i) 
		g_assert_cmpuint (run_and_count_matches (xpath, queries[i].query),
				  ==, queries[i].count);

	g_free (xpath);
}

static void
test_mu_query_02 (void)
{
	const char* q;
	gchar *xpath;
	
	xpath = fill_database ();
	g_assert (xpath);
	
	q = "i:f7ccd24b0808061357t453f5962w8b61f9a453b684d0@mail.gmail.com";

	g_assert_cmpuint (run_and_count_matches(xpath, q), ==, 1);
	g_free (xpath);
}


static void
test_mu_query_03 (void)
{
	gchar *xpath;
	int i;
	
	QResults queries[] = {
		{ "ploughed", 1},
		{ "i:3BE9E6535E3029448670913581E7A1A20D852173@"
		  "emss35m06.us.lmco.com", 1},

		/* subsets of the words in the subject should match */
		{ "s:gcc include search order" , 1},
		{ "s:gcc include search" , 1},
		{ "s:search order" , 1},
		{ "s:include" , 1},
		
		{ "s:lisp", 1},
		{ "s:LISP", 1},
		
		{ "s:Learning LISP; Scheme vs elisp.", 1},
		{ "subject:Re Learning LISP; Scheme vs elisp.", 1},
		{ "to:help-gnu-emacs@gnu.org", 4},
		{ "t:help-gnu-emacs", 0},
	};
	
	xpath = fill_database ();
	g_assert (xpath != NULL);
	
 	for (i = 0; i != G_N_ELEMENTS(queries); ++i)
		g_assert_cmpuint (run_and_count_matches (xpath, queries[i].query),
				  ==, queries[i].count);
	g_free (xpath);

}


static void
test_mu_query_04 (void)
{
	gchar *xpath;
	int i;
	
	QResults queries[] = {
		{ "frodo@example.com", 1}, /* does not match: see mu-find (1) */
		{ "f:frodo@example.com", 1},
		{ "f:Frodo Baggins", 1},
		{ "bilbo@anotherexample.com", 1}, /* same things */
		{ "t:bilbo@anotherexample.com", 1},
		{ "t:bilbo", 1},
		{ "f:bilbo", 0},
 		{ "baggins", 1},
		{ "prio:high", 1},
		{ "prio:normal", 3},
		{ "prio:h", 1},
		{ "prio:l", 7},
		{ "not prio:l", 4}
	};
	
	xpath = fill_database ();
	g_assert (xpath != NULL);
	
 	for (i = 0; i != G_N_ELEMENTS(queries); ++i) 
		g_assert_cmpuint (run_and_count_matches (xpath, queries[i].query),
				  ==, queries[i].count);
	g_free (xpath);

}


static void
test_mu_query_05 (void)
{
	MuQuery *query;
	MuMsgIter *iter;
	MuMsg *msg;
	gchar *xpath;
	GError *err;
	
	xpath = fill_database ();
	g_assert (xpath != NULL);

	query = mu_query_new (xpath, NULL);
	iter = mu_query_run (query, "fünkÿ", MU_MSG_FIELD_ID_NONE,
			     FALSE, 1, NULL);
	err = NULL;
	msg = mu_msg_iter_get_msg (iter, &err);
	if (!msg) {
		g_warning ("error getting message: %s", err->message);
		g_error_free (err);
		g_assert_not_reached ();
	}
	
	g_assert_cmpstr (mu_msg_get_subject(msg),==, 
			 "Greetings from Lothlórien");
	g_assert_cmpstr (mu_msg_get_summary(msg,5),==,
			 "Let's write some fünkÿ text using umlauts. Foo.");
	
	mu_msg_unref (msg);
	mu_msg_iter_destroy (iter);
	mu_query_destroy (query);
	g_free (xpath);
}


int
main (int argc, char *argv[])
{
	int rv;
	
	g_test_init (&argc, &argv, NULL);	
	g_test_add_func ("/mu-query/test-mu-query-01", test_mu_query_01);
	g_test_add_func ("/mu-query/test-mu-query-02", test_mu_query_02); 
	g_test_add_func ("/mu-query/test-mu-query-03", test_mu_query_03);
	g_test_add_func ("/mu-query/test-mu-query-04", test_mu_query_04);
	g_test_add_func ("/mu-query/test-mu-query-05", test_mu_query_05);
	
	g_log_set_handler (NULL,
			   G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL| G_LOG_FLAG_RECURSION,
			   (GLogFunc)black_hole, NULL);

	mu_msg_gmime_init ();
	rv = g_test_run ();
	mu_msg_gmime_uninit ();
	
	return rv;
}

