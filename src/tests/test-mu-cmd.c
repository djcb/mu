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
#include "src/mu-store.h"


/* tests for the command line interface, uses testdir2 */

static gchar*
fill_database (void)
{
	gchar *cmdline, *tmpdir;
	
	tmpdir = test_mu_common_get_random_tmpdir();
	cmdline = g_strdup_printf ("%s index --muhome=%s --maildir=%s"
				   " --quiet",
				   MU_PROGRAM, tmpdir, MU_TESTMAILDIR2);
	
	g_assert (g_spawn_command_line_sync (cmdline, NULL, NULL, NULL, NULL));
	g_free (cmdline);

	return tmpdir;
}

static unsigned
newlines_in_output (const char* str)
{
	int count;

	count = 0;

	while (str && *str) {
		if (*str == '\n')
			++count;
		++str;
	}

	return count;
}

static void
search (const char* query, unsigned expected)
{
        gchar *muhome, *cmdline, *output, *erroutput;

	muhome = fill_database ();
	g_assert (muhome);

	cmdline = g_strdup_printf ("%s --muhome=%s find %s",
				   MU_PROGRAM, muhome, query);
	
	g_assert (g_spawn_command_line_sync (cmdline, &output, &erroutput, NULL, NULL));

	g_assert_cmpuint (newlines_in_output(output),==,expected);

	/* we expect zero lines of error output */
	g_assert_cmpuint (newlines_in_output(erroutput),==,0);
	
	g_free (output);
	g_free (erroutput);
	g_free (cmdline);
	g_free (muhome);
}



/* index testdir2, and make sure it adds two documents */
static void
test_mu_index (void)
{
	MuStore *store;
	gchar *muhome, *xpath;

	muhome = fill_database ();
	g_assert (muhome != NULL);

	xpath = g_strdup_printf ("%s%c%s", muhome, G_DIR_SEPARATOR, "xapian");
	
	store = mu_store_new (xpath);
	g_assert (store);

	g_assert_cmpuint (mu_store_count (store), ==, 3);	
	mu_store_destroy (store);

	g_free (muhome);
	g_free (xpath);
}


/* index testdir2, and make sure it adds two documents */
static void
test_mu_find_01 (void)
{
	search ("f:john fruit", 1);
	search ("f:soc@example.com", 1);	
	search ("t:alki@example.com", 1);
	search ("t:alcibiades", 1);
	search ("f:soc@example.com OR f:john", 2);	
	search ("f:soc@example.com OR f:john OR t:edmond", 3);
	search ("t:julius", 1);	
	search ("s:dude", 1);
	search ("t:dantès", 1);	
}


static void /* error cases */
test_mu_find_02 (void)
{
        gchar *muhome, *cmdline, *erroutput;

	muhome = fill_database ();
	g_assert (muhome);

	cmdline = g_strdup_printf ("%s --muhome=/foo/bar/nonexistent find f:socrates",
				   MU_PROGRAM);
	
	g_assert (g_spawn_command_line_sync (cmdline, NULL, &erroutput,
					     NULL, NULL));
	
	/* we expect multiple lines of error output */
	g_assert_cmpuint (newlines_in_output(erroutput),>=,1);

	g_free (erroutput);
	g_free (cmdline);
	g_free (muhome);
}




int
main (int argc, char *argv[])
{
	g_test_init (&argc, &argv, NULL);
	
	g_test_add_func ("/mu-cmd/test-mu-index", test_mu_index);
	g_test_add_func ("/mu-cmd/test-mu-find-01",  test_mu_find_01); 
	g_test_add_func ("/mu-cmd/test-mu-find-02",  test_mu_find_02);
	
	g_log_set_handler (NULL,
			   G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL| G_LOG_FLAG_RECURSION,
			   (GLogFunc)black_hole, NULL);
	
	return g_test_run ();
}

