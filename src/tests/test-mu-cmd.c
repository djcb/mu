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

	/* g_print ("%s\n", query); */
	g_assert_cmpuint (newlines_in_output(output),==,expected);

	/* we expect zero lines of error output if there is a match;
	 * otherwise there should be one line 'No matches found' */
	g_assert_cmpuint (newlines_in_output(erroutput),==,
			  expected == 0 ? 1 : 0);
	
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

	g_assert_cmpuint (mu_store_count (store), ==, 4);	
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


/* index testdir2, and make sure it adds two documents */
static void
test_mu_find_02 (void)
{
	search ("bull", 1);
	search ("bull m:foo", 0);	
	search ("bull m:/foo", 1);
}



static void /* error cases */
test_mu_find_03 (void)
{
        gchar *muhome, *cmdline, *erroutput;

	muhome = fill_database ();
	g_assert (muhome);

	cmdline = g_strdup_printf ("%s --muhome=%cfoo%cbar%cnonexistent find f:socrates",
				   MU_PROGRAM,
				   G_DIR_SEPARATOR,
				   G_DIR_SEPARATOR,
				   G_DIR_SEPARATOR);
	
	g_assert (g_spawn_command_line_sync (cmdline, NULL, &erroutput,
					     NULL, NULL));
	
	/* we expect multiple lines of error output */
	g_assert_cmpuint (newlines_in_output(erroutput),>=,1);

	g_free (erroutput);
	g_free (cmdline);
	g_free (muhome);
}


static void
test_mu_extract_01 (void)
{
        gchar *cmdline, *output, *erroutput;

	cmdline = g_strdup_printf ("%s extract %s%cfoo%ccur%cmail4",
				   MU_PROGRAM,
				   MU_TESTMAILDIR2,
				   G_DIR_SEPARATOR,
				   G_DIR_SEPARATOR,
				   G_DIR_SEPARATOR);

	output = erroutput = NULL;
	g_assert (g_spawn_command_line_sync (cmdline, &output, &erroutput, NULL, NULL));
	g_assert_cmpstr (output,
			 ==,
			 "MIME-parts in this message:\n"
			 "  0 <none> multipart/mixed [<none>]\n"
			 "  1 <none> text/plain [<none>]\n"
			 "  2 sittingbull.jpg image/jpeg [inline]\n"
			 "  3 custer.jpg image/jpeg [inline]\n");
	
	/* we expect zero lines of error output */
	g_assert_cmpuint (newlines_in_output(erroutput),==,0);

	g_free (output);
	g_free (erroutput);
	g_free (cmdline);
}

static gint64
get_file_size (const char* path)
{
	int rv;
	struct stat statbuf;

	rv = stat (path, &statbuf);
	if (rv != 0)
		return -1;

	return (gint64)statbuf.st_size;
}


static void 
test_mu_extract_02 (void)
{
        gchar *cmdline, *output,  *tmpdir;
	gchar *att1, *att2;
	
	tmpdir = test_mu_common_get_random_tmpdir();

	g_assert (g_mkdir_with_parents (tmpdir, 0700) == 0);
	
	cmdline = g_strdup_printf ("%s extract -a --target-dir=%s %s%cfoo%ccur%cmail4",
				   MU_PROGRAM,
				   tmpdir,
				   MU_TESTMAILDIR2,
				   G_DIR_SEPARATOR,
				   G_DIR_SEPARATOR,
				   G_DIR_SEPARATOR);

	output = NULL;
	g_assert (g_spawn_command_line_sync (cmdline, &output, NULL, NULL, NULL));
	g_assert_cmpstr (output, ==, "");
	
	att1 = g_strdup_printf ("%s%ccuster.jpg", tmpdir, G_DIR_SEPARATOR);
	att2 = g_strdup_printf ("%s%csittingbull.jpg", tmpdir, G_DIR_SEPARATOR);
	
	g_assert_cmpint (get_file_size(att1),==,15960);
	g_assert_cmpint (get_file_size(att2),==,17674);
	
	g_free (output);
	g_free (tmpdir);
	g_free (cmdline);
	g_free (att1);
	g_free (att2);
}


static void 
test_mu_extract_03 (void)
{
        gchar *cmdline, *output,  *tmpdir;
	gchar *att1, *att2;
	
	tmpdir = test_mu_common_get_random_tmpdir();

	g_assert (g_mkdir_with_parents (tmpdir, 0700) == 0);
	
	cmdline = g_strdup_printf ("%s extract --parts 3 "
				   "--target-dir=%s %s%cfoo%ccur%cmail4",
				   MU_PROGRAM,
				   tmpdir,
				   MU_TESTMAILDIR2,
				   G_DIR_SEPARATOR,
				   G_DIR_SEPARATOR,
				   G_DIR_SEPARATOR);

	output = NULL;
	g_assert (g_spawn_command_line_sync (cmdline, &output, NULL, NULL, NULL));
	g_assert_cmpstr (output, ==, "");

	att1 = g_strdup_printf ("%s%ccuster.jpg", tmpdir, G_DIR_SEPARATOR);
	att2 = g_strdup_printf ("%s%csittingbull.jpg", tmpdir, G_DIR_SEPARATOR);
	
	g_assert_cmpint (get_file_size(att1),==,15960); /* should not exist */
	g_assert_cmpint (get_file_size(att2),==,-1);
	
	g_free (output);
	g_free (tmpdir);
	g_free (cmdline);
	g_free (att1);
	g_free (att2);
}




int
main (int argc, char *argv[])
{
	g_test_init (&argc, &argv, NULL);
	
	g_test_add_func ("/mu-cmd/test-mu-index", test_mu_index);
	g_test_add_func ("/mu-cmd/test-mu-find-01",  test_mu_find_01); 
	g_test_add_func ("/mu-cmd/test-mu-find-02",  test_mu_find_02);
 	g_test_add_func ("/mu-cmd/test-mu-find-03",  test_mu_find_03);
	g_test_add_func ("/mu-cmd/test-mu-extract-01",  test_mu_extract_01);
	g_test_add_func ("/mu-cmd/test-mu-extract-02",  test_mu_extract_02);
	g_test_add_func ("/mu-cmd/test-mu-extract-03",  test_mu_extract_03);
	
	g_log_set_handler (NULL,
			   G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL| G_LOG_FLAG_RECURSION,
			   (GLogFunc)black_hole, NULL);
	
	return g_test_run ();
}

