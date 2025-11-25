/*
** Copyright (C) 2012-2025 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <glib/gstdio.h>

#include <lib/mu-query.hh>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include "utils/mu-test-utils.hh"
#include "utils/mu-utils-file.hh"

#include <lib/mu-store.hh>
#include <utils/mu-utils.hh>

using namespace Mu;

static std::string test_dir;

static std::string
fill_database(void)
{
	{
		const auto res = run_command0({MU_PROGRAM,
				"--quiet", "init",
				"--muhome", test_dir,
				"--maildir", MU_TESTMAILDIR2});
		assert_valid_result(res);
	}

	{
		const auto res = run_command0({MU_PROGRAM, "--quiet",
				"index", "--muhome", test_dir});
		assert_valid_result(res);
	}

	if (g_test_verbose())
		mu_println("\nindexed {} @ {}", MU_TESTMAILDIR2, test_dir);

	return test_dir;
}

static void
test_something(const char* what)
{
	g_setenv("GUILE_AUTO_COMPILE", "0", TRUE);
	g_setenv("GUILE_LOAD_PATH", GUILE_LOAD_PATH, TRUE);
	g_setenv("GUILE_EXTENSIONS_PATH",GUILE_EXTENSIONS_PATH, TRUE);

	if (g_test_verbose())
		g_print("GUILE_LOAD_PATH: %s\n", GUILE_LOAD_PATH);

	const auto dir = fill_database();
	const auto res = run_command0({GUILE_BINARY, "-q", "-e", "main",
			ABS_SRCDIR"/test-mu-guile.scm",
			"--muhome", dir, "--test", what});

	assert_valid_result(res);
}

static void
test_mu_guile_queries(void)
{
	test_something("queries");
}

static void
test_mu_guile_messages(void)
{
	test_something("message");
}

static void
test_mu_guile_stats(void)
{
	test_something("stats");
}

int
main(int argc, char* argv[])
{
	int rv;
	TempDir tempdir;
	test_dir = tempdir.path();

	mu_test_init(&argc, &argv);

	if (!set_en_us_utf8_locale())
		return 0; /* don't error out... */

	g_test_add_func("/guile/queries", test_mu_guile_queries);
	g_test_add_func("/guile/message", test_mu_guile_messages);
	g_test_add_func("/guile/stats", test_mu_guile_stats);

	rv = g_test_run();

	return rv;
}
