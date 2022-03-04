/*
** Copyright (C) 2008-2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "config.h"

#include <glib.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#include <locale.h>

#include "test-mu-common.hh"
#include "mu-store.hh"

static std::string MuTestMaildir  = Mu::canonicalize_filename(MU_TESTMAILDIR, "/");
static std::string MuTestMaildir2 = Mu::canonicalize_filename(MU_TESTMAILDIR2, "/");

static void
test_store_ctor_dtor()
{
	char* tmpdir = test_mu_common_get_random_tmpdir();
	g_assert(tmpdir);

	Mu::Store store{tmpdir, "/tmp", {}, {}};
	g_free(tmpdir);
	g_assert_true(store.empty());
	g_assert_cmpuint(0, ==, store.size());

	g_assert_cmpstr(MU_STORE_SCHEMA_VERSION, ==, store.properties().schema_version.c_str());
}

static void
test_store_add_count_remove()
{
	char* tmpdir = test_mu_common_get_random_tmpdir();
	g_assert(tmpdir);

	Mu::Store store{tmpdir, MuTestMaildir, {}, {}};
	g_free(tmpdir);

	const auto id1 = store.add_message(MuTestMaildir + "/cur/1283599333.1840_11.cthulhu!2,");

	g_assert_cmpuint(id1, !=, Mu::Store::InvalidId);

	g_assert_cmpuint(store.size(), ==, 1);
	g_assert_true(store.contains_message(MuTestMaildir + "/cur/1283599333.1840_11.cthulhu!2,"));

	g_assert_cmpuint(store.add_message(MuTestMaildir2 + "/bar/cur/mail3"),
			 !=,
			 Mu::Store::InvalidId);

	g_assert_cmpuint(store.size(), ==, 2);
	g_assert_true(store.contains_message(MuTestMaildir2 + "/bar/cur/mail3"));

	store.remove_message(id1);
	g_assert_cmpuint(store.size(), ==, 1);
	g_assert_false(
	    store.contains_message(MuTestMaildir + "/cur/1283599333.1840_11.cthulhu!2,"));

	store.remove_message(MuTestMaildir2 + "/bar/cur/mail3");
	g_assert_true(store.empty());
	g_assert_false(store.contains_message(MuTestMaildir2 + "/bar/cur/mail3"));
}

static void
test_store_add_count_remove_in_memory()
{
	Mu::Store store{MuTestMaildir, {}, {}};

	g_assert_true(store.properties().in_memory);

	const auto id1 = store.add_message(MuTestMaildir + "/cur/1283599333.1840_11.cthulhu!2,");

	g_assert_cmpuint(id1, !=, Mu::Store::InvalidId);

	g_assert_cmpuint(store.size(), ==, 1);
	g_assert_true(store.contains_message(MuTestMaildir + "/cur/1283599333.1840_11.cthulhu!2,"));

	g_assert_cmpuint(store.add_message(MuTestMaildir2 + "/bar/cur/mail3"),
			 !=,
			 Mu::Store::InvalidId);

	g_assert_cmpuint(store.size(), ==, 2);
	g_assert_true(store.contains_message(MuTestMaildir2 + "/bar/cur/mail3"));

	store.remove_message(id1);
	g_assert_cmpuint(store.size(), ==, 1);
	g_assert_false(
	    store.contains_message(MuTestMaildir + "/cur/1283599333.1840_11.cthulhu!2,"));

	store.remove_message(MuTestMaildir2 + "/bar/cur/mail3");
	g_assert_true(store.empty());
	g_assert_false(store.contains_message(MuTestMaildir2 + "/bar/cur/mail3"));
}

int
main(int argc, char* argv[])
{
	g_test_init(&argc, &argv, NULL);

	/* mu_runtime_init/uninit */
	g_test_add_func("/store/ctor-dtor", test_store_ctor_dtor);
	g_test_add_func("/store/add-count-remove", test_store_add_count_remove);
	g_test_add_func("/store/in-memory/add-count-remove", test_store_add_count_remove_in_memory);

	// if (!g_test_verbose())
	//	g_log_set_handler (NULL,
	//	G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL| G_LOG_FLAG_RECURSION,
	//	(GLogFunc)black_hole, NULL);

	return g_test_run();
}
