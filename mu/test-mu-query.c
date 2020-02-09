/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

/*
** Copyright (C) 2008-2017 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <locale.h>

#include "test-mu-common.h"
#include "mu-query.h"
#include "utils/mu-str.h"
#include "mu-store.hh"

static char* DB_PATH1 = NULL;
static char* DB_PATH2 = NULL;

static gchar*
fill_database (const char *testdir)
{
	gchar *cmdline, *tmpdir, *xpath;

	tmpdir = test_mu_common_get_random_tmpdir();
	cmdline = g_strdup_printf (
		"/bin/sh -c '"
		"%s init  --muhome=%s --maildir=%s --quiet ; "
		"%s index --muhome=%s  --quiet'",
		MU_PROGRAM,  tmpdir, testdir,
		MU_PROGRAM,  tmpdir);


	if (g_test_verbose())
		g_printerr ("\n%s\n", cmdline);

	g_assert (g_spawn_command_line_sync (cmdline, NULL, NULL,
					     NULL, NULL));
	g_free (cmdline);

	xpath= g_strdup_printf ("%s%c%s", tmpdir,
				G_DIR_SEPARATOR, "xapian");
	g_free (tmpdir);

	return xpath;
}



static void
assert_no_dups (MuMsgIter *iter)
{
	GHashTable *hash;

	hash = g_hash_table_new_full (g_str_hash, g_str_equal,
				      (GDestroyNotify)g_free, NULL);

	mu_msg_iter_reset (iter);
	while (!mu_msg_iter_is_done(iter)) {
		MuMsg *msg;
		msg = mu_msg_iter_get_msg_floating (iter);
		/* make sure there are no duplicates */
		g_assert (!g_hash_table_lookup (hash, mu_msg_get_path (msg)));
		g_hash_table_insert (hash, g_strdup (mu_msg_get_path(msg)),
				     GUINT_TO_POINTER(TRUE));
		mu_msg_iter_next (iter);
	}
	mu_msg_iter_reset (iter);
	g_hash_table_destroy (hash);
}


/* note: this also *moves the iter* */
static guint
run_and_count_matches_with_query_flags (const char *xpath, const char *query,
					MuQueryFlags flags)
{
	MuQuery  *mquery;
	MuMsgIter *iter;
	MuStore *store;
	guint count1, count2;
	GError *err;

	err = NULL;
	store = mu_store_new_readable (xpath, &err);
	if (err) {
		g_printerr ("error: %s\n", err->message);
		g_clear_error (&err);
		err = NULL;
	}
	g_assert (store);

	mquery = mu_query_new (store, &err);
	if (err) {
		g_printerr ("error: %s\n", err->message);
		g_clear_error (&err);
		err = NULL;
	}

	g_assert (mquery);

	mu_store_unref (store);

	if (g_test_verbose()) {
		char *x;
		g_print ("\n==> query: %s\n", query);
		x = mu_query_internal (mquery, query, FALSE, NULL);
		g_print ("==> mquery: '%s'\n", x);
		g_free (x);
		x = mu_query_internal_xapian (mquery, query, NULL);
		g_print ("==> xquery: '%s'\n", x);
		g_free (x);
	}

	iter = mu_query_run (mquery, query, MU_MSG_FIELD_ID_NONE, -1,
			     flags, NULL);
	mu_query_destroy (mquery);
	g_assert (iter);

	assert_no_dups (iter);

	/* run query twice, to test mu_msg_iter_reset */
	for (count1 = 0; !mu_msg_iter_is_done(iter);
	     mu_msg_iter_next(iter), ++count1);

	mu_msg_iter_reset (iter);

	assert_no_dups (iter);

	for (count2 = 0; !mu_msg_iter_is_done(iter);
	     mu_msg_iter_next(iter), ++count2);

	mu_msg_iter_destroy (iter);

	g_assert_cmpuint (count1, ==, count2);

	return count1;
}

static guint
run_and_count_matches (const char *xpath, const char *query)
{
	return run_and_count_matches_with_query_flags (
		xpath, query, MU_QUERY_FLAG_NONE);
}

typedef struct  {
	const char *query;
	size_t count; /* expected number of matches */
} QResults;

static void
test_mu_query_01 (void)
{
	int i;
	QResults queries[] = {
		{ "basic",              3 },
		{ "question",           5 },
		{ "thanks",             2 },
		{ "html",               4 },
		{ "subject:exception",  1 },
		{ "exception",          1 },
		{ "subject:A&B",        1 },
		{ "A&B",                1 },
		{ "subject:elisp",      1 },
		{ "html AND contains",  1 },
		{ "html and contains",  1 },
		{ "from:pepernoot",     0 },
		{ "foo:pepernoot",      0 },
		{ "funky",              1 },
		{ "fünkÿ",              1 },
		//	{ "",                   18 },
		{ "msgid:abcd$efgh@example.com", 1},
		{ "i:abcd$efgh@example.com", 1},
	};

	for (i = 0; i != G_N_ELEMENTS(queries); ++i)
		g_assert_cmpuint (run_and_count_matches (DB_PATH1,
							 queries[i].query),
				  ==, queries[i].count);
}

static void
test_mu_query_02 (void)
{
	const char* q;
	q = "i:f7ccd24b0808061357t453f5962w8b61f9a453b684d0@mail.gmail.com";
	g_assert_cmpuint (run_and_count_matches (DB_PATH1, q), ==, 1);
}


static void
test_mu_query_03 (void)
{
	int i;
	QResults queries[] = {
		{ "ploughed", 1},
		{ "i:3BE9E6535E3029448670913581E7A1A20D852173@"
		  "emss35m06.us.lmco.com", 1},
		{ "i:!&!AAAAAAAAAYAAAAAAAAAOH1+8mkk+lLn7Gg5fke7"
		  "FbCgAAAEAAAAJ7eBDgcactKhXL6r8cEnJ8BAAAAAA==@"
		  "example.com", 1},

		/* subsets of the words in the subject should match */
		{ "s:gcc include search order" , 1},
		{ "s:gcc include search" , 1},
		{ "s:search order" , 1},
		{ "s:include" , 1},

		{ "s:lisp", 1},
		{ "s:LISP", 1},

		/* { "s:\"Re: Learning LISP; Scheme vs elisp.\"", 1}, */
		/* { "subject:Re: Learning LISP; Scheme vs elisp.", 1}, */
		/* { "subject:\"Re: Learning LISP; Scheme vs elisp.\"", 1}, */
		{ "to:help-gnu-emacs@gnu.org", 4},
		{ "t:help-gnu-emacs", 4},
		{ "flag:flagged", 1}
	};

	for (i = 0; i != G_N_ELEMENTS(queries); ++i)
		g_assert_cmpuint (run_and_count_matches (DB_PATH1,
							 queries[i].query),
				  ==, queries[i].count);
}


static void
test_mu_query_04 (void)
{
	int i;

	QResults queries[] = {
		{ "frodo@example.com", 1},
		{ "f:frodo@example.com", 1},
		{ "f:Frodo Baggins", 1},
		{ "bilbo@anotherexample.com", 1},
		{ "t:bilbo@anotherexample.com", 1},
		{ "t:bilbo", 1},
		{ "f:bilbo", 0},
		{ "baggins", 1},
		{ "prio:h", 1},
		{ "prio:high", 1},
		{ "prio:normal", 11},
		{ "prio:l", 7},
		{ "not prio:l", 12},
	};

	for (i = 0; i != G_N_ELEMENTS(queries); ++i)
		g_assert_cmpuint (run_and_count_matches (DB_PATH1,
							 queries[i].query),
				  ==, queries[i].count);
}


static void
test_mu_query_logic (void)
{
	int i;
	QResults queries[] = {
		{ "subject:gcc" , 1},
		{ "subject:lisp" , 1},
		{ "subject:gcc OR subject:lisp" , 2},
		{ "subject:gcc or subject:lisp" , 2},
		{ "subject:gcc AND subject:lisp" , 0},

		{ "subject:gcc OR (subject:scheme AND subject:elisp)" , 2},
		{ "(subject:gcc OR subject:scheme) AND subject:elisp" , 1}
	};

	for (i = 0; i != G_N_ELEMENTS(queries); ++i)
		g_assert_cmpuint (run_and_count_matches (DB_PATH1,
							 queries[i].query),
				  ==, queries[i].count);
}




static void
test_mu_query_accented_chars_01 (void)
{
	MuQuery *query;
	MuMsgIter *iter;
	MuMsg *msg;
	MuStore *store;
	GError *err;
	gchar *summ;

	store = mu_store_new_readable (DB_PATH1, NULL);
	g_assert (store);

	query = mu_query_new (store, NULL);
	mu_store_unref (store);

	iter = mu_query_run (query, "fünkÿ", MU_MSG_FIELD_ID_NONE,
			     -1, MU_QUERY_FLAG_NONE, NULL);
	err = NULL;
	msg = mu_msg_iter_get_msg_floating (iter); /* don't unref */
	if (!msg) {
		g_warning ("error getting message: %s", err->message);
		g_error_free (err);
		g_assert_not_reached ();
	}

	g_assert_cmpstr (mu_msg_get_subject(msg),==,
			 "Greetings from Lothlórien");
	/* TODO: fix this again */

	summ = mu_str_summarize (mu_msg_get_body_text
				 (msg, MU_MSG_OPTION_NONE), 5);
	g_assert_cmpstr (summ,==,
			 "Let's write some fünkÿ text using umlauts. Foo.");
	g_free (summ);

	mu_msg_iter_destroy (iter);
	mu_query_destroy (query);
}

static void
test_mu_query_accented_chars_02 (void)
{
	int i;

	QResults queries[] = {
		{ "f:mü", 1},
		{ "s:motörhead", 1},
		{ "t:Helmut", 1},
		{ "t:Kröger", 1},
		{ "s:MotorHeäD", 1},
		{ "queensryche", 1},
		{ "Queensrÿche", 1}
	};

	for (i = 0; i != G_N_ELEMENTS(queries); ++i)
		g_assert_cmpuint (run_and_count_matches (DB_PATH1,
							 queries[i].query),
				  ==, queries[i].count);
}


static void
test_mu_query_accented_chars_fraiche (void)
{
	int i;

	QResults queries[] = {
		{ "crème fraîche", 1},
		{ "creme fraiche", 1},
		{ "fraîche crème", 1},
		{ "будланула", 1},
		{ "БУДЛАНУЛА", 1},
		{ "CRÈME FRAÎCHE", 1},
		{ "CREME FRAICHE", 1}
	};

	for (i = 0; i != G_N_ELEMENTS(queries); ++i) {

		if (g_test_verbose ())
			g_print ("'%s'\n", queries[i].query);

		g_assert_cmpuint (run_and_count_matches (DB_PATH2,
							 queries[i].query),
				  ==, queries[i].count);
	}
}

static void
test_mu_query_wildcards (void)
{
	int i;

	QResults queries[] = {
		{ "f:mü", 1},
		{ "s:mo*", 1},
		{ "t:Helm*", 1},
		{ "queensryche", 1},
		{ "Queen*", 1},
	};

	for (i = 0; i != G_N_ELEMENTS(queries); ++i)
		g_assert_cmpuint (run_and_count_matches (DB_PATH1,
							 queries[i].query),
				  ==, queries[i].count);
}


static void
test_mu_query_dates_helsinki (void)
{
	gchar *xpath;
	int i;
	const char *old_tz;

	QResults queries[] = {
		{ "date:20080731..20080804", 5},
		{ "date:20080731..20080804 s:gcc", 1},
		{ "date:200808110803..now", 7},
		{ "date:200808110803..today",7},
		{ "date:200808110801..now", 7}
	};

	old_tz = set_tz ("Europe/Helsinki");

	xpath = fill_database (MU_TESTMAILDIR);
	g_assert (xpath != NULL);

	for (i = 0; i != G_N_ELEMENTS(queries); ++i)
		g_assert_cmpuint (run_and_count_matches
				  (xpath, queries[i].query),
				  ==, queries[i].count);

	g_free (xpath);
	set_tz (old_tz);

}

static void
test_mu_query_dates_sydney (void)
{
	gchar *xpath;
	int i;
	const char *old_tz;

	QResults queries[] = {
		{ "date:20080731..20080804", 5},
		{ "date:20080731..20080804 s:gcc", 1},
		{ "date:200808110803..now", 7},
		{ "date:200808110803..today", 7},
		{ "date:200808110801..now", 7}
	};

	old_tz = set_tz ("Australia/Sydney");

	xpath = fill_database (MU_TESTMAILDIR);
	g_assert (xpath != NULL);

	for (i = 0; i != G_N_ELEMENTS(queries); ++i)
		g_assert_cmpuint (run_and_count_matches
				  (xpath, queries[i].query),
				  ==, queries[i].count);

	g_free (xpath);
	set_tz (old_tz);

}

static void
test_mu_query_dates_la (void)
{
	gchar *xpath;
	int i;
	const char *old_tz;

	QResults queries[] = {
		{ "date:20080731..20080804", 5},
		{ "date:2008-07-31..2008-08-04", 5},
		{ "date:20080804..20080731", 5},
		{ "date:20080731..20080804 s:gcc", 1},
		{ "date:200808110803..now", 6},
		{ "date:200808110803..today", 6},
		{ "date:200808110801..now", 6}
	};

	old_tz = set_tz ("America/Los_Angeles");

	xpath = fill_database (MU_TESTMAILDIR);
	g_assert (xpath != NULL);

	for (i = 0; i != G_N_ELEMENTS(queries); ++i) {
		/* g_print ("%s\n", queries[i].query); */
		g_assert_cmpuint (run_and_count_matches
				  (xpath, queries[i].query),
				  ==, queries[i].count);
	}

	g_free (xpath);
	set_tz (old_tz);
}




static void
test_mu_query_sizes (void)
{
	int i;
	QResults queries[] = {
		{ "size:0b..2m", 19},
		{ "size:3b..2m", 19},
		{ "size:2k..4k", 4},

		{ "size:0b..2m", 19},
		{ "size:2m..0b", 19},
	};

	for (i = 0; i != G_N_ELEMENTS(queries); ++i)
		g_assert_cmpuint (run_and_count_matches (DB_PATH1,
							 queries[i].query),
				  ==, queries[i].count);

}


static void
test_mu_query_attach (void)
{
	int i;
	QResults queries[] = {
		{ "j:sittingbull.jpg", 1},
		{ "file:custer", 0},
		{ "file:custer.jpg", 1}
	};

	for (i = 0; i != G_N_ELEMENTS(queries); ++i) {
		if (g_test_verbose())
			g_print ("query: %s\n", queries[i].query);
		g_assert_cmpuint (run_and_count_matches (DB_PATH2,
							 queries[i].query),
				  ==, queries[i].count);
	}
}





static void
test_mu_query_msgid (void)
{
	int i;
	QResults queries[] = {
		{ "i:CAHSaMxZ9rk5ASjqsbXizjTQuSk583=M6TORHz"
		  "=bfogtmbGGs5A@mail.gmail.com", 1},
		{ "msgid:CAHSaMxZ9rk5ASjqsbXizjTQuSk583=M6TORHz="
		  "bfogtmbGGs5A@mail.gmail.com", 1},

	};

	for (i = 0; i != G_N_ELEMENTS(queries); ++i) {
		if (g_test_verbose())
			g_print ("query: %s\n", queries[i].query);
		g_assert_cmpuint (run_and_count_matches (DB_PATH2,
							 queries[i].query),
				  ==, queries[i].count);
	}
}


static void
test_mu_query_tags (void)
{
	int i;
	QResults queries[] = {
		{ "x:paradise", 1},
		{ "tag:lost", 1},
		{ "tag:lost tag:paradise", 1},
		{ "tag:lost tag:horizon", 0},
		{ "tag:lost OR tag:horizon", 1},
		{ "x:paradise,lost", 0},
		{ "x:paradise AND x:lost", 1},
		{ "x:\\\\backslash", 1},
	};

	for (i = 0; i != G_N_ELEMENTS(queries); ++i)
		g_assert_cmpuint (run_and_count_matches (DB_PATH2,
							 queries[i].query),
				  ==, queries[i].count);
}




static void
test_mu_query_wom_bat (void)
{
	int i;
	QResults queries[] = {
		{ "maildir:/wom_bat", 3},
		//{ "\"maildir:/wom bat\"", 3},
		// as expected, no longer works with new parser
	};

	for (i = 0; i != G_N_ELEMENTS(queries); ++i)
		g_assert_cmpuint (run_and_count_matches (DB_PATH2,
							 queries[i].query),
				  ==, queries[i].count);
}



static void
test_mu_query_signed_encrypted (void)
{
	int i;
	QResults queries[] = {
		{ "flag:encrypted", 2},
		{ "flag:signed", 2},
	};

	for (i = 0; i != G_N_ELEMENTS(queries); ++i)
		g_assert_cmpuint (run_and_count_matches (DB_PATH1,
							 queries[i].query),
				  ==, queries[i].count);
}


static void
test_mu_query_multi_to_cc (void)
{
	int i;
	QResults queries[] = {
		{ "to:a@example.com", 1},
		{ "cc:d@example.com", 1},
		{ "to:b@example.com", 1},
		{ "cc:e@example.com", 1},
		{ "cc:e@example.com AND cc:d@example.com", 1},
	};

	for (i = 0; i != G_N_ELEMENTS(queries); ++i)
		g_assert_cmpuint (run_and_count_matches (DB_PATH1,
							 queries[i].query),
				  ==, queries[i].count);
}

static void
test_mu_query_tags_02 (void)
{
	int i;
	QResults queries[] = {
		{ "x:paradise", 1},
		{ "tag:@NextActions", 1},
		{ "x:queensrÿche", 1},
		{ "tag:lost OR tag:operation*", 2},
	};

	for (i = 0; i != G_N_ELEMENTS(queries); ++i) {
		g_assert_cmpuint (run_and_count_matches (DB_PATH2,
							 queries[i].query),
				  ==, queries[i].count);
	}
}

/* Tests for https://github.com/djcb/mu/issues/380

   On certain platforms, something goes wrong during compilation and
   the --related option doesn't work.
*/
static void
test_mu_query_threads_compilation_error (void)
{
	gchar *xpath;

	xpath = fill_database (MU_TESTMAILDIR);
	g_assert (xpath != NULL);

	g_assert_cmpuint (run_and_count_matches_with_query_flags
			  (xpath, "msgid:uwsireh25.fsf@one.dot.net",
			  MU_QUERY_FLAG_NONE),
			  ==, 1);

	g_assert_cmpuint (run_and_count_matches_with_query_flags
			  (xpath, "msgid:uwsireh25.fsf@one.dot.net",
			   MU_QUERY_FLAG_INCLUDE_RELATED),
			  ==, 3);

	g_free (xpath);
}



int
main (int argc, char *argv[])
{
	int rv;

	setlocale (LC_ALL, "");

	g_test_init (&argc, &argv, NULL);

	DB_PATH1 = fill_database (MU_TESTMAILDIR);
	g_assert (DB_PATH1);

	DB_PATH2 = fill_database (MU_TESTMAILDIR2);
	g_assert (DB_PATH2);

	g_test_add_func ("/mu-query/test-mu-query-01", test_mu_query_01);
	g_test_add_func ("/mu-query/test-mu-query-02", test_mu_query_02);
	g_test_add_func ("/mu-query/test-mu-query-03", test_mu_query_03);
	g_test_add_func ("/mu-query/test-mu-query-04", test_mu_query_04);

	g_test_add_func ("/mu-query/test-mu-query-signed-encrypted",
			 test_mu_query_signed_encrypted);
	g_test_add_func ("/mu-query/test-mu-query-multi-to-cc",
			 test_mu_query_multi_to_cc);
	g_test_add_func ("/mu-query/test-mu-query-logic", test_mu_query_logic);

	g_test_add_func ("/mu-query/test-mu-query-accented-chars-1",
			 test_mu_query_accented_chars_01);
	g_test_add_func ("/mu-query/test-mu-query-accented-chars-2",
			 test_mu_query_accented_chars_02);
	g_test_add_func ("/mu-query/test-mu-query-accented-chars-fraiche",
			 test_mu_query_accented_chars_fraiche);

	g_test_add_func ("/mu-query/test-mu-query-msgid",
			 test_mu_query_msgid);

	g_test_add_func ("/mu-query/test-mu-query-wom-bat",
			 test_mu_query_wom_bat);

	g_test_add_func ("/mu-query/test-mu-query-wildcards",
			 test_mu_query_wildcards);
	g_test_add_func ("/mu-query/test-mu-query-sizes",
			 test_mu_query_sizes);

	g_test_add_func ("/mu-query/test-mu-query-dates-helsinki",
			 test_mu_query_dates_helsinki);
	g_test_add_func ("/mu-query/test-mu-query-dates-sydney",
			 test_mu_query_dates_sydney);
	g_test_add_func ("/mu-query/test-mu-query-dates-la",
			 test_mu_query_dates_la);

	g_test_add_func ("/mu-query/test-mu-query-attach",
			 test_mu_query_attach);
	g_test_add_func ("/mu-query/test-mu-query-tags",
			 test_mu_query_tags);
	g_test_add_func ("/mu-query/test-mu-query-tags_02",
			 test_mu_query_tags_02);

	g_test_add_func ("/mu-query/test-mu-query-threads-compilation-error",
			 test_mu_query_threads_compilation_error);

	if (!g_test_verbose())
	    g_log_set_handler (NULL,
			       G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL|
			       G_LOG_FLAG_RECURSION,
			       (GLogFunc)black_hole, NULL);

	rv = g_test_run ();

	g_free (DB_PATH1);
	g_free (DB_PATH2);

	return rv;
}
