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

#if HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include <glib.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#include <locale.h>

#include "test-mu-common.h"
#include "mu-store.h"

static void
test_mu_store_new_destroy (void)
{
	MuStore *store;
	gchar* tmpdir;
	GError *err;

	tmpdir = test_mu_common_get_random_tmpdir();
	g_assert (tmpdir);

	err = NULL;
	store = mu_store_new_writable (tmpdir, NULL, FALSE, &err);
	g_assert_no_error (err);
	g_assert (store);

	g_assert_cmpuint (0,==,mu_store_count (store, NULL));

	mu_store_flush (store);
	mu_store_unref (store);

	g_free (tmpdir);
}


static void
test_mu_store_version (void)
{
	MuStore *store;
	gchar* tmpdir;
	GError *err;

	tmpdir = test_mu_common_get_random_tmpdir();
	g_assert (tmpdir);

	err = NULL;
	store = mu_store_new_writable (tmpdir, NULL, FALSE, &err);
	g_assert (store);
	mu_store_unref (store);
	store = mu_store_new_read_only (tmpdir, &err);
	g_assert (store);

	g_assert (err == NULL);

	g_assert_cmpuint (0,==,mu_store_count (store, NULL));
	g_assert_cmpstr (MU_STORE_SCHEMA_VERSION,==,
			 mu_store_version(store));

	mu_store_unref (store);
	g_free (tmpdir);
}


static void
test_mu_store_store_msg_and_count (void)
{
	MuMsg *msg;
	MuStore *store;
	gchar* tmpdir;

	tmpdir = test_mu_common_get_random_tmpdir();
	g_assert (tmpdir);

	store = mu_store_new_writable (tmpdir, NULL, FALSE, NULL);
	g_assert (store);
	g_free (tmpdir);

	g_assert_cmpuint (0,==,mu_store_count (store, NULL));

	/* add one */
	msg = mu_msg_new_from_file (
		MU_TESTMAILDIR "/cur/1283599333.1840_11.cthulhu!2,",
		NULL, NULL);
	g_assert (msg);
	g_assert_cmpuint (mu_store_add_msg (store, msg, NULL),
			  !=, MU_STORE_INVALID_DOCID);
	g_assert_cmpuint (1,==,mu_store_count (store, NULL));
	g_assert_cmpuint (TRUE,==,mu_store_contains_message
			  (store,
			   MU_TESTMAILDIR "/cur/1283599333.1840_11.cthulhu!2,", NULL));
	mu_msg_unref (msg);

	/* add another one */
	msg = mu_msg_new_from_file (MU_TESTMAILDIR2
				    "/bar/cur/mail3", NULL, NULL);
	g_assert (msg);
	g_assert_cmpuint (mu_store_add_msg (store, msg, NULL),
			  !=, MU_STORE_INVALID_DOCID);
	g_assert_cmpuint (2,==,mu_store_count (store, NULL));
	g_assert_cmpuint (TRUE,==,
			  mu_store_contains_message (store, MU_TESTMAILDIR2
						     "/bar/cur/mail3", NULL));
	mu_msg_unref (msg);

	/* try to add the first one again. count should be 2 still */
	msg = mu_msg_new_from_file
		(MU_TESTMAILDIR "/cur/1283599333.1840_11.cthulhu!2,",
		 NULL, NULL);
	g_assert (msg);
	g_assert_cmpuint (mu_store_add_msg (store, msg, NULL),
			  !=, MU_STORE_INVALID_DOCID);
	g_assert_cmpuint (2,==,mu_store_count (store, NULL));

	mu_msg_unref (msg);
	mu_store_unref (store);
}


static void
test_mu_store_store_msg_remove_and_count (void)
{
	MuMsg *msg;
	MuStore *store;
	gchar* tmpdir;
	GError *err;

	tmpdir = test_mu_common_get_random_tmpdir();
	g_assert (tmpdir);

	store = mu_store_new_writable (tmpdir, NULL, FALSE, NULL);
	g_assert (store);

	g_assert_cmpuint (0,==,mu_store_count (store, NULL));

	/* add one */
	err = NULL;
	msg = mu_msg_new_from_file (
		MU_TESTMAILDIR "/cur/1283599333.1840_11.cthulhu!2,",
		NULL, &err);
	g_assert (msg);
	g_assert_cmpuint (mu_store_add_msg (store, msg, NULL),
			  !=, MU_STORE_INVALID_DOCID);
	g_assert_cmpuint (1,==,mu_store_count (store, NULL));
	mu_msg_unref (msg);

	/* remove one */
	mu_store_remove_path (store,
			      MU_TESTMAILDIR "/cur/1283599333.1840_11.cthulhu!2,");
	g_assert_cmpuint (0,==,mu_store_count (store, NULL));
	g_assert_cmpuint (FALSE,==,mu_store_contains_message
			  (store,
			   MU_TESTMAILDIR "/cur/1283599333.1840_11.cthulhu!2,", NULL));
	g_free (tmpdir);
	mu_store_unref (store);
}


int
main (int argc, char *argv[])
{
	g_test_init (&argc, &argv, NULL);

	/* mu_runtime_init/uninit */
	g_test_add_func ("/mu-store/mu-store-new-destroy",
			 test_mu_store_new_destroy);
	g_test_add_func ("/mu-store/mu-store-version",
			 test_mu_store_version);
	g_test_add_func ("/mu-store/mu-store-store-and-count",
			 test_mu_store_store_msg_and_count);
	g_test_add_func ("/mu-store/mu-store-store-remove-and-count",
			 test_mu_store_store_msg_remove_and_count);

	if (!g_test_verbose())
		g_log_set_handler (NULL,
		G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL| G_LOG_FLAG_RECURSION,
		(GLogFunc)black_hole, NULL);

	return g_test_run ();
}
