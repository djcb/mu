/* -*- mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-
**
** Copyright (C) 2012-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <glib/gstdio.h>

#include "../mu-query.h"

#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include "test-mu-common.h"
#include "mu-store.h"


/* tests for the command line interface, uses testdir2 */

static gchar*
fill_database (void)
{
	gchar *cmdline, *tmpdir;
	GError *err;

	tmpdir = test_mu_common_get_random_tmpdir();
	cmdline = g_strdup_printf ("%s index --muhome=%s --maildir=%s"
				   " --quiet",
				   MU_PROGRAM,
				   tmpdir, MU_TESTMAILDIR2);
	if (g_test_verbose())
		g_print ("%s\n", cmdline);

	err  = NULL;
	if (!g_spawn_command_line_sync (cmdline, NULL, NULL,
					NULL, &err)) {
		g_printerr ("Error: %s\n", err ? err->message : "?");
		g_assert (0);
	}

	g_free (cmdline);
	return tmpdir;
}


static void
test_something (const char *what)
{
	char *dir, *cmdline;
	gint result;

	dir = fill_database ();
	cmdline = g_strdup_printf (
		"LD_LIBRARY_PATH=%s %s -q -L %s -e main %s/test-mu-guile.scm "
		"--muhome=%s --test=%s",
		MU_GUILE_LIBRARY_PATH,
		GUILE_BINARY,
		MU_GUILE_MODULE_PATH,
		ABS_SRCDIR,
		dir,
		what);

	if (g_test_verbose ())
		g_print ("cmdline: %s\n", cmdline);

	result = system (cmdline);
	g_assert (result == 0);

	g_free (dir);
	g_free (cmdline);
}

static void
test_mu_guile_queries (void)
{
	test_something ("queries");
}

static void
test_mu_guile_messages (void)
{
	test_something ("message");
}

static void
test_mu_guile_stats (void)
{
	test_something ("stats");
}


int
main (int argc, char *argv[])
{
	int rv;
	g_test_init (&argc, &argv, NULL);

	if (!set_en_us_utf8_locale())
		return 0; /* don't error out... */

	g_test_add_func ("/guile/queries",  test_mu_guile_queries);
	g_test_add_func ("/guile/message",  test_mu_guile_messages);
	g_test_add_func ("/guile/stats",    test_mu_guile_stats);

	g_log_set_handler (NULL,
			   G_LOG_LEVEL_MASK | G_LOG_LEVEL_WARNING|
			   G_LOG_FLAG_FATAL| G_LOG_FLAG_RECURSION,
			   (GLogFunc)black_hole, NULL);

	rv = g_test_run ();

	return rv;
}
