/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include <glib.h>
#include <glib/gstdio.h>

#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include "test-mu-common.h"
#include "mu-query.h"
#include "utils/mu-str.h"

struct _tinfo {
	const char *threadpath;
	const char *msgid;
	const char *subject;
};
typedef struct _tinfo tinfo;

static void
assert_tinfo_equal (const tinfo *expected, const tinfo *actual)
{
	g_assert_cmpstr (expected->threadpath,==,actual->threadpath);
	g_assert_cmpstr (expected->subject,==,actual->subject);
	g_assert_cmpstr (expected->msgid,==,actual->msgid);
}

static void
tinfo_init_from_iter (tinfo *item, MuMsgIter *iter)
{
	MuMsg *msg;
	const MuMsgIterThreadInfo *ti;

	msg = mu_msg_iter_get_msg_floating (iter);
	g_assert (msg);

	ti = mu_msg_iter_get_thread_info (iter);
	if (!ti)
		g_print ("%s: thread info not found\n", mu_msg_get_msgid (msg));
	g_assert (ti);

	item->threadpath = ti->threadpath;
	item->subject = mu_msg_get_subject (msg);
	item->msgid = mu_msg_get_msgid (msg);

	if (g_test_verbose())
		g_print ("%s %s %s\n",
			 item->threadpath, item->subject, item->msgid);

}

static void
foreach_assert_tinfo_equal (MuMsgIter *iter, const tinfo items[], guint n_items)
{
	guint u;

	u = 0;
	while (!mu_msg_iter_is_done (iter) && u < n_items) {
		tinfo ti;

		tinfo_init_from_iter (&ti, iter);

		g_assert (u < n_items);
		assert_tinfo_equal (&items[u], &ti);

		++u;
		mu_msg_iter_next (iter);
	}
	g_assert (u == n_items);
}

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
		g_print ("%s\n", cmdline);

	g_assert (g_spawn_command_line_sync (cmdline, NULL, NULL,
					     NULL, NULL));
	g_free (cmdline);
	xpath= g_strdup_printf ("%s%c%s", tmpdir,
				G_DIR_SEPARATOR, "xapian");
	g_free (tmpdir);

	return xpath;
}

/* note: this also *moves the iter* */
static MuMsgIter*
run_and_get_iter_full (const char *xpath, const char *query,
		       MuMsgFieldId sort_field, MuQueryFlags flags)
{
	MuQuery  *mquery;
	MuStore *store;
	MuMsgIter *iter;

	store = mu_store_new_readable (xpath, NULL);
	g_assert (store);

	mquery = mu_query_new (store, NULL);
	mu_store_unref (store);
	g_assert (query);

	flags |= MU_QUERY_FLAG_THREADS;
	iter = mu_query_run (mquery, query, sort_field, -1, flags, NULL);
	mu_query_destroy (mquery);
	g_assert (iter);

	return iter;
}

static MuMsgIter*
run_and_get_iter (const char *xpath, const char *query)
{
	return run_and_get_iter_full (xpath, query, MU_MSG_FIELD_ID_DATE,
				      MU_QUERY_FLAG_NONE);
}

static void
test_mu_threads_01 (void)
{
	gchar *xpath;
	MuMsgIter *iter;

	const tinfo items [] = {
		{"0",   "root0@msg.id",  "root0"},
		{"0:0", "child0.0@msg.id", "Re: child 0.0"},
		{"0:1",   "child0.1@msg.id", "Re: child 0.1"},
		{"0:1:0", "child0.1.0@msg.id", "Re: child 0.1.0"},
		{"1",   "root1@msg.id", "root1"},
		{"2",   "root2@msg.id", "root2"},
		/* next one's been promoted 2.0.0 => 2.0 */
		{"2:0", "child2.0.0@msg.id", "Re: child 2.0.0"},
		/* next one's been promoted 3.0.0.0.0 => 3 */
		{"3", "child3.0.0.0.0@msg.id", "Re: child 3.0.0.0"},

		/* two children of absent root 4.0 */
		{"4:0", "child4.0@msg.id", "Re: child 4.0"},
		{"4:1", "child4.1@msg.id", "Re: child 4.1"}
	};

	xpath = fill_database (MU_TESTMAILDIR3);
	g_assert (xpath != NULL);

	iter = run_and_get_iter (xpath, "abc");
	g_assert (iter);
	g_assert (!mu_msg_iter_is_done(iter));

	foreach_assert_tinfo_equal (iter, items, G_N_ELEMENTS (items));

	g_free (xpath);
	mu_msg_iter_destroy (iter);
}

static void
test_mu_threads_rogue (void)
{
	gchar *xpath;
	MuMsgIter *iter;
	tinfo *items;

	tinfo items1 [] = {
		{"0",     "cycle0@msg.id",     "cycle0"},
		{"0:0",   "cycle0.0@msg.id",   "cycle0.0"},
		{"0:0:0", "cycle0.0.0@msg.id", "cycle0.0.0"},
		{"0:1",   "rogue0@msg.id",     "rogue0"},
	};

	tinfo items2 [] = {
		{"0",     "cycle0.0@msg.id",   "cycle0.0"},
		{"0:0",   "cycle0@msg.id",     "cycle0"},
		{"0:0:0", "rogue0@msg.id",     "rogue0" },
		{"0:1",   "cycle0.0.0@msg.id", "cycle0.0.0"}
	};

	xpath = fill_database (MU_TESTMAILDIR3);
	g_assert (xpath != NULL);

	iter = run_and_get_iter (xpath, "def");
	g_assert (iter);
	g_assert (!mu_msg_iter_is_done(iter));

	/* due to the random order in files can be indexed, there are two possible ways
	 * for the threads to be built-up; both are okay */
	if (g_strcmp0 (mu_msg_get_msgid(mu_msg_iter_get_msg_floating (iter)),
		       "cycle0@msg.id") == 0)
		items = items1;
	else
		items = items2;

	foreach_assert_tinfo_equal (iter, items, G_N_ELEMENTS (items1));

	g_free (xpath);
	mu_msg_iter_destroy (iter);
}

static MuMsgIter*
query_testdir (const char *query, MuMsgFieldId sort_field,  gboolean descending)
{
	MuMsgIter *iter;
	gchar *xpath;
	MuQueryFlags flags;

	flags = MU_QUERY_FLAG_NONE;
	if (descending)
		flags |= MU_QUERY_FLAG_DESCENDING;

	xpath = fill_database (MU_TESTMAILDIR3);
	g_assert (xpath != NULL);

	iter = run_and_get_iter_full (xpath, query, sort_field, flags);
	g_assert (iter != NULL);
	g_assert (!mu_msg_iter_is_done (iter));

	g_free (xpath);
	return iter;
}

static void
check_sort_by_subject (const char *query, const tinfo expected[],
		       guint n_expected, gboolean descending)
{
	MuMsgIter *iter;

	iter = query_testdir (query, MU_MSG_FIELD_ID_SUBJECT, descending);
	foreach_assert_tinfo_equal (iter, expected, n_expected);
	mu_msg_iter_destroy (iter);
}

static void
check_sort_by_subject_asc (const char *query, const tinfo expected[],
			   guint n_expected)
{
	check_sort_by_subject (query, expected, n_expected, FALSE);
}

static void
check_sort_by_subject_desc (const char *query, const tinfo expected[],
			    guint n_expected)
{
	check_sort_by_subject (query, expected, n_expected, TRUE);
}

static void
test_mu_threads_sort_1st_child_promotes_thread (void)
{
	const char *query = "maildir:/sort/1st-child-promotes-thread";

	const tinfo expected_asc [] = {
		{ "0", "A@msg.id", "A"},
		{ "1", "C@msg.id", "C"},
		{ "2", "B@msg.id", "B"},
		{ "2:0", "D@msg.id", "D"},
	};
	const tinfo expected_desc [] = {
		{ "0", "B@msg.id", "B"},
		{ "0:0", "D@msg.id", "D"},
		{ "1", "C@msg.id", "C"},
		{ "2", "A@msg.id", "A"},
	};

	check_sort_by_subject_asc (query, expected_asc,
				   G_N_ELEMENTS (expected_asc));
	check_sort_by_subject_desc (query, expected_desc,
				    G_N_ELEMENTS (expected_desc));
}

static void
test_mu_threads_sort_2nd_child_promotes_thread (void)
{
	const char *query = "maildir:/sort/2nd-child-promotes-thread";

	const tinfo expected_asc [] = {
		{ "0", "A@msg.id", "A"},
		{ "1", "D@msg.id", "D"},
		{ "2", "B@msg.id", "B"},
		{ "2:0", "C@msg.id", "C"},
		{ "2:1", "E@msg.id", "E"},
	};
	const tinfo expected_desc [] = {
		{ "0", "B@msg.id", "B"},
		{ "0:0", "C@msg.id", "C"},
		{ "0:1", "E@msg.id", "E"},
		{ "1", "D@msg.id", "D"},
		{ "2", "A@msg.id", "A"},
	};

	check_sort_by_subject_asc (query, expected_asc,
				   G_N_ELEMENTS (expected_asc));
	check_sort_by_subject_desc (query, expected_desc,
				    G_N_ELEMENTS (expected_desc));
}

static void
test_mu_threads_sort_orphan_promotes_thread (void)
{
	const char *query = "maildir:/sort/2nd-child-promotes-thread NOT B";

	/* B lost, C & E orphaned but not promoted */
	const tinfo expected_asc [] = {
		{ "0", "A@msg.id", "A"},
		{ "1", "D@msg.id", "D"},
		{ "2:0", "C@msg.id", "C"},
		{ "2:1", "E@msg.id", "E"},
	};
	const tinfo expected_desc [] = {
		{ "0:0", "C@msg.id", "C"},
		{ "0:1", "E@msg.id", "E"},
		{ "1", "D@msg.id", "D"},
		{ "2", "A@msg.id", "A"},
	};

	check_sort_by_subject_asc (query, expected_asc,
				   G_N_ELEMENTS (expected_asc));
	check_sort_by_subject_desc (query, expected_desc,
				    G_N_ELEMENTS (expected_desc));
}

/* Won't normally happen when sorting by date. */
static void
test_mu_threads_sort_child_does_not_promote_thread (void)
{
	const char *query = "maildir:/sort/child-does-not-promote-thread";

	const tinfo expected_asc [] = {
		{ "0", "Y@msg.id", "Y"},
		{ "0:0", "A@msg.id", "A"},
		{ "1", "X@msg.id", "X"},
		{ "2", "Z@msg.id", "Z"},
	};
	const tinfo expected_desc [] = {
		{ "0", "Z@msg.id", "Z"},
		{ "1", "X@msg.id", "X"},
		{ "2", "Y@msg.id", "Y"},
		{ "2:0", "A@msg.id", "A"},
	};

	check_sort_by_subject_asc (query, expected_asc,
				   G_N_ELEMENTS (expected_asc));
	check_sort_by_subject_desc (query, expected_desc,
				    G_N_ELEMENTS (expected_desc));
}

static void
test_mu_threads_sort_grandchild_promotes_thread (void)
{
	const char *query = "maildir:/sort/grandchild-promotes-thread";

	const tinfo expected_asc [] = {
		{ "0", "A@msg.id", "A"},
		{ "1", "D@msg.id", "D"},
		{ "2", "B@msg.id", "B"},
		{ "2:0", "C@msg.id", "C"},
		{ "2:0:0", "E@msg.id", "E"},
	};
	const tinfo expected_desc [] = {
		{ "0", "B@msg.id", "B"},
		{ "0:0", "C@msg.id", "C"},
		{ "0:0:0", "E@msg.id", "E"},
		{ "1", "D@msg.id", "D"},
		{ "2", "A@msg.id", "A"},
	};

	check_sort_by_subject_asc (query, expected_asc,
				   G_N_ELEMENTS (expected_asc));
	check_sort_by_subject_desc (query, expected_desc,
				    G_N_ELEMENTS (expected_desc));
}

static void
test_mu_threads_sort_granchild_promotes_only_subthread (void)
{
	const char *query = "maildir:/sort/grandchild-promotes-only-subthread";

	const tinfo expected_asc [] = {
		{ "0", "A@msg.id", "A"},
		{ "1", "B@msg.id", "B"},
		{ "1:0", "C@msg.id", "C"},
		{ "1:1", "D@msg.id", "D"},
		{ "1:1:0", "F@msg.id", "F"},
		{ "1:2", "E@msg.id", "E"},
		{ "2", "G@msg.id", "G"},
	};
	const tinfo expected_desc [] = {
		{ "0", "G@msg.id", "G"},
		{ "1", "B@msg.id", "B"},
		{ "1:0", "C@msg.id", "C"},
		{ "1:1", "D@msg.id", "D"},
		{ "1:1:0", "F@msg.id", "F"},
		{ "1:2", "E@msg.id", "E"},
		{ "2", "A@msg.id", "A"},
	};

	check_sort_by_subject_asc (query, expected_asc,
				   G_N_ELEMENTS (expected_asc));
	check_sort_by_subject_desc (query, expected_desc,
				    G_N_ELEMENTS (expected_desc));
}
int
main (int argc, char *argv[])
{
	int rv;

	g_test_init (&argc, &argv, NULL);

	g_test_add_func ("/mu-query/test-mu-threads-01", test_mu_threads_01);
	g_test_add_func ("/mu-query/test-mu-threads-rogue", test_mu_threads_rogue);
	g_test_add_func ("/mu-query/test-mu-threads-sort-1st-child-promotes-thread",
			 test_mu_threads_sort_1st_child_promotes_thread);
	g_test_add_func ("/mu-query/test-mu-threads-sort-2nd-child-promotes-thread",
			 test_mu_threads_sort_2nd_child_promotes_thread);
	g_test_add_func ("/mu-query/test-mu-threads-orphan-promotes-thread",
			 test_mu_threads_sort_orphan_promotes_thread);
	g_test_add_func ("/mu-query/test-mu-threads-sort-child-does-not-promote-thread",
			 test_mu_threads_sort_child_does_not_promote_thread);
	g_test_add_func ("/mu-query/test-mu-threads-sort-grandchild-promotes-thread",
			 test_mu_threads_sort_grandchild_promotes_thread);
	g_test_add_func ("/mu-query/test-mu-threads-sort-grandchild-promotes-only-subthread",
			 test_mu_threads_sort_granchild_promotes_only_subthread);

	g_log_set_handler (NULL,
			   G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL| G_LOG_FLAG_RECURSION,
			   (GLogFunc)black_hole, NULL);

	rv = g_test_run ();

	return rv;
}
