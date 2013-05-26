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
#include "mu-str.h"

static gchar*
fill_database (const char *testdir)
{
	gchar *cmdline, *tmpdir, *xpath;

	tmpdir = test_mu_common_get_random_tmpdir();
	cmdline = g_strdup_printf ("%s index --muhome=%s --maildir=%s"
				   " --quiet",
				   MU_PROGRAM, tmpdir, testdir);
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
run_and_get_iter (const char *xpath, const char *query)
{
	MuQuery  *mquery;
	MuStore *store;
	MuMsgIter *iter;

	store = mu_store_new_read_only (xpath, NULL);
	g_assert (store);

	mquery = mu_query_new (store, NULL);
	mu_store_unref (store);
	g_assert (query);

	iter = mu_query_run (mquery, query, MU_MSG_FIELD_ID_DATE,
			     -1, MU_QUERY_FLAG_THREADS, NULL);
	mu_query_destroy (mquery);
	g_assert (iter);

	return iter;
}


static void
test_mu_threads_01 (void)
{
	gchar *xpath;
	MuMsgIter *iter;
	unsigned u;

	struct {
		const char* threadpath;
		const char *msgid;
		const char* subject;
	}   items [] = {
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

	u = 0;
	while (!mu_msg_iter_is_done (iter) && u < G_N_ELEMENTS(items)) {
		MuMsg *msg;
		const MuMsgIterThreadInfo *ti;

		ti = mu_msg_iter_get_thread_info (iter);
		if (!ti)
			g_print ("%s: thread info not found for %u\n",
				 __FUNCTION__, (unsigned)mu_msg_iter_get_docid(iter));
		g_assert(ti);

		msg = mu_msg_iter_get_msg_floating (iter);
		g_assert (msg);

		if (g_test_verbose())
			g_print ("%s %s %s\n", ti->threadpath,
				 mu_msg_get_msgid(msg),
				 mu_msg_get_path (msg));

		g_assert (u < G_N_ELEMENTS(items));

		g_assert_cmpstr (ti->threadpath,==,items[u].threadpath);
		g_assert_cmpstr (mu_msg_get_subject(msg),==,items[u].subject);
		g_assert_cmpstr (mu_msg_get_msgid(msg),==,items[u].msgid);

		++u;
		mu_msg_iter_next (iter);
	}
	g_assert (u == G_N_ELEMENTS(items));

	g_free (xpath);
	mu_msg_iter_destroy (iter);
}


struct _tinfo {
	const char* threadpath;
	const char *msgid;
	const char* subject;
};
typedef struct _tinfo tinfo;

static void
test_mu_threads_rogue (void)
{
	gchar *xpath;
	MuMsgIter *iter;
	unsigned u;
	tinfo *items;

	tinfo items1 [] = {
		{"0",     "cycle0@msg.id",  "cycle0"},
		{"0:0",   "cycle0.0@msg.id", "cycle0.0"},
		{"0:0:0", "cycle0.0.0@msg.id", "cycle0.0.0"},
		{"0:1",    "rogue0@msg.id", "rogue0"},
	};

	tinfo items2 [] = {
		{"0",   "cycle0.0@msg.id", "cycle0.0"},
		{"0:0",     "cycle0@msg.id",  "cycle0"},
		{"0:0:0",  "rogue0@msg.id", "rogue0" },
		{"0:1", "cycle0.0.0@msg.id", "cycle0.0.0"}
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

	u = 0;
	while (!mu_msg_iter_is_done (iter) && u < G_N_ELEMENTS(items1)) {
		MuMsg *msg;
		const MuMsgIterThreadInfo *ti;

		ti = mu_msg_iter_get_thread_info (iter);
		if (!ti)
			g_print ("%s: thread info not found\n",
				 mu_msg_get_msgid(mu_msg_iter_get_msg_floating (iter)));
		g_assert(ti);

		msg = mu_msg_iter_get_msg_floating (iter); /* don't unref */
		/* g_print ("%s %s %s\n", ti->threadpath, */
		/* 	 mu_msg_get_msgid(msg), */
		/* 	 mu_msg_get_path (msg) */
		/* 	); */

		g_assert (u < G_N_ELEMENTS(items1));

		g_assert_cmpstr (ti->threadpath,==,(items)[u].threadpath);
		g_assert_cmpstr (mu_msg_get_subject(msg),==,(items)[u].subject);
		g_assert_cmpstr (mu_msg_get_msgid(msg),==,(items)[u].msgid);

		++u;
		mu_msg_iter_next (iter);
	}
	g_assert (u == G_N_ELEMENTS(items1));

	g_free (xpath);
	mu_msg_iter_destroy (iter);
}



int
main (int argc, char *argv[])
{
	int rv;

	g_test_init (&argc, &argv, NULL);

	g_test_add_func ("/mu-query/test-mu-threads-01", test_mu_threads_01);
	g_test_add_func ("/mu-query/test-mu-threads-rogue", test_mu_threads_rogue);

	g_log_set_handler (NULL,
			   G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL| G_LOG_FLAG_RECURSION,
			   (GLogFunc)black_hole, NULL);

	rv = g_test_run ();

	return rv;
}
