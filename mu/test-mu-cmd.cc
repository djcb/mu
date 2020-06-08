/* -*- mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-
**
** Copyright (C) 2008-2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <string.h>
#include <errno.h>

#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include "test-mu-common.h"
#include "mu-store.hh"
#include "mu-query.h"

/* tests for the command line interface, uses testdir2 */

static gchar *DBPATH; /* global */

static gchar*
fill_database (void)
{
	gchar *cmdline, *tmpdir;
	GError *err;

	tmpdir = test_mu_common_get_random_tmpdir();
	cmdline = g_strdup_printf (
		"/bin/sh -c '"
		"%s init  --muhome=%s --maildir=%s --quiet; "
		"%s index --muhome=%s  --quiet'",
		MU_PROGRAM,  tmpdir, MU_TESTMAILDIR2,
		MU_PROGRAM,  tmpdir);
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
	gchar *cmdline, *output, *erroutput;

	cmdline = g_strdup_printf ("%s find --muhome=%s %s",
				   MU_PROGRAM, DBPATH, query);

	if (g_test_verbose())
		g_printerr ("\n$ %s\n", cmdline);

	g_assert (g_spawn_command_line_sync (cmdline,
					     &output, &erroutput,
					     NULL, NULL));
	if (g_test_verbose())
		g_print ("\nOutput:\n%s", output);

	g_assert_cmpuint (newlines_in_output(output),==,expected);


	/* we expect zero lines of error output if there is a match;
	 * otherwise there should be one line 'No matches found' */
	/* g_assert_cmpuint (newlines_in_output(erroutput),==, */
	/* 		  expected == 0 ? 1 : 0); */

	g_free (output);
	g_free (erroutput);
	g_free (cmdline);
}

/* index testdir2, and make sure it adds two documents */
static void
test_mu_index (void)
{
	MuStore *store;
	gchar *xpath;

	xpath = g_strdup_printf ("%s%c%s", DBPATH, G_DIR_SEPARATOR, "xapian");
	g_printerr ("*** %s\n", DBPATH);
	store = mu_store_new_readable (xpath, NULL);
	g_assert (store);

	g_assert_cmpuint (mu_store_count (store, NULL), ==, 13);
	mu_store_unref (store);

	g_free (xpath);
}


static void
test_mu_find_empty_query (void)
{
	search ("\"\"", 13);
}


static void
test_mu_find_01 (void)
{
	search ("f:john fruit", 1);
	search ("f:soc@example.com", 1);
	search ("t:alki@example.com", 1);
	search ("t:alcibiades", 1);
	search ("http emacs", 1);
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
	search ("bull m:/Foo", 1);
	search ("bull flag:a", 1);
	search ("g:x", 0);
	search ("flag:encrypted", 0);
	search ("flag:attach", 1);
}



static void
test_mu_find_file (void)
{
	search ("file:sittingbull.jpg", 1);
	search ("file:custer.jpg", 1);
	search ("file:custer.*", 1);
	search ("j:sit*", 1);
}


static void
test_mu_find_mime (void)
{
	search ("mime:image/jpeg", 1);
	search ("mime:text/plain", 13);
	search ("y:text*", 13);
	search ("y:image*", 1);
	search ("mime:message/rfc822", 2);
}


static void
test_mu_find_text_in_rfc822 (void)
{
	search ("embed:dancing", 1);
	search ("e:curious", 1);
	search ("embed:with", 2);
	search ("e:karjala", 0);
	search ("embed:navigation", 1);
}


/* some more tests */
static void
test_mu_find_03 (void)
{
	search ("bull", 1);
	search ("bull m:foo", 0);
	search ("bull m:/foo", 1);
	search ("i:3BE9E6535E0D852173@emss35m06.us.lmco.com", 1);
}


static void /* error cases */
test_mu_find_04 (void)
{
	gchar *cmdline, *erroutput;

	cmdline = g_strdup_printf ("find %s --muhome=%cfoo%cbar%cnonexistent "
				   "f:socrates",
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
}


G_GNUC_UNUSED static void
test_mu_find_links (void)
{
	gchar *cmdline, *output, *erroutput, *tmpdir;

	tmpdir = test_mu_common_get_random_tmpdir();

	cmdline = g_strdup_printf (
		"%s find --muhome=%s --format=links --linksdir=%s "
		"mime:message/rfc822", MU_PROGRAM, DBPATH, tmpdir);

	if (g_test_verbose())
		g_print ("cmdline: %s\n", cmdline);

	g_assert (g_spawn_command_line_sync (cmdline,
					     &output, &erroutput,
					     NULL, NULL));
 	/* there should be no errors */
	g_assert_cmpuint (newlines_in_output(output),==,0);
	g_assert_cmpuint (newlines_in_output(erroutput),==,0);
	g_free (output);
	g_free (erroutput);

	/* now we try again, we should get a line of error output,
	 * when we find the first target file already exists */

	if (g_test_verbose())
		g_print ("cmdline: %s\n", cmdline);

	g_assert (g_spawn_command_line_sync (cmdline,
					     &output, &erroutput,
					     NULL, NULL));
 	g_assert_cmpuint (newlines_in_output(output),==,0);
	g_assert_cmpuint (newlines_in_output(erroutput),==,1);
	g_free (output);
	g_free (erroutput);

	/* now we try again with --clearlinks, and the we should be
	 * back to 0 errors */
	g_free (cmdline);
	cmdline = g_strdup_printf (
		"%s find --muhome=%s --format=links --linksdir=%s --clearlinks "
		"mime:message/rfc822", MU_PROGRAM, DBPATH, tmpdir);
	g_assert (g_spawn_command_line_sync (cmdline,
					     &output, &erroutput,
					     NULL, NULL));
	if (g_test_verbose())
		g_print ("cmdline: %s\n", cmdline);
	g_assert_cmpuint (newlines_in_output(output),==,0);
	g_assert_cmpuint (newlines_in_output(erroutput),==,0);
	g_free (output);
	g_free (erroutput);

	g_free (cmdline);
	g_free (tmpdir);
}



/* some more tests */
static void
test_mu_find_maildir_special (void)
{

	search ("\"maildir:/wOm_bàT\"", 3);
	search ("\"maildir:/wOm*\"", 3);
	search ("\"maildir:/wOm_*\"", 3);
	search ("\"maildir:wom_bat\"", 0);
	search ("\"maildir:/wombat\"", 0);
	search ("subject:atoms", 1);
	search ("\"maildir:/wom_bat\" subject:atoms", 1);
}


/* static void */
/* test_mu_find_mime_types (void) */
/* { */
/* 	/\* ensure that maldirs with spaces in their names work... *\/ */
/* 	search ("\"maildir:/wom bat\" subject:atoms", 1); */
/* 	search ("\"maildir:/wOm_bàT\"", 3); */
/* 	search ("subject:atoms", 1); */
/* } */




static void
test_mu_extract_01 (void)
{
	gchar *cmdline, *output, *erroutput, *tmpdir;

	tmpdir = test_mu_common_get_random_tmpdir();
	g_assert (g_mkdir_with_parents (tmpdir, 0700) == 0);

	cmdline = g_strdup_printf ("%s extract --muhome=%s %s%cFoo%ccur%cmail5",
				   MU_PROGRAM,
				   tmpdir,
				   MU_TESTMAILDIR2,
				   G_DIR_SEPARATOR,
				   G_DIR_SEPARATOR,
				   G_DIR_SEPARATOR);

	if (g_test_verbose())
		g_print ("cmd: %s\n", cmdline);

	output = erroutput = NULL;
	g_assert (g_spawn_command_line_sync (cmdline, &output, &erroutput,
					     NULL, NULL));
	g_assert_cmpstr (output,
			 ==,
			 "MIME-parts in this message:\n"
			 "  1 <none> text/plain [<none>] (27 bytes)\n"
			 "  2 sittingbull.jpg image/jpeg [inline] (23.3 KB)\n"
			 "  3 custer.jpg image/jpeg [inline] (21.1 KB)\n");

	/* we expect zero lines of error output */
	g_assert_cmpuint (newlines_in_output(erroutput),==,0);

	g_free (output);
	g_free (erroutput);
	g_free (cmdline);
	g_free (tmpdir);
}

static gint64
get_file_size (const char* path)
{
	int rv;
	struct stat statbuf;

	rv = stat (path, &statbuf);
	if (rv != 0) {
		/* g_warning ("error: %s", strerror (errno)); */
		return -1;
	}

	return (gint64)statbuf.st_size;
}


static void
test_mu_extract_02 (void)
{
	gchar	*cmdline, *output,  *tmpdir;
	gchar	*att1, *att2;
	size_t	 size;

	tmpdir = test_mu_common_get_random_tmpdir();

	g_assert (g_mkdir_with_parents (tmpdir, 0700) == 0);

	cmdline = g_strdup_printf ("%s extract --muhome=%s -a "
				   "--target-dir=%s %s%cFoo%ccur%cmail5",
				   MU_PROGRAM,
				   tmpdir,
				   tmpdir,
				   MU_TESTMAILDIR2,
				   G_DIR_SEPARATOR,
				   G_DIR_SEPARATOR,
				   G_DIR_SEPARATOR);

	if (g_test_verbose ())
		g_print ("$ %s\n", cmdline);

	output = NULL;
	g_assert (g_spawn_command_line_sync (cmdline, &output, NULL,
					     NULL, NULL));
	g_assert_cmpstr (output, ==, "");

	att1 = g_strdup_printf ("%s%ccuster.jpg", tmpdir, G_DIR_SEPARATOR);
	att2 = g_strdup_printf ("%s%csittingbull.jpg", tmpdir, G_DIR_SEPARATOR);

	size = get_file_size(att1);
	g_assert_true (size >= 15958 && size <= 15960);
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
	gchar	*cmdline, *output,  *tmpdir;
	gchar	*att1, *att2;
	size_t	 size;

	tmpdir = test_mu_common_get_random_tmpdir();

	g_assert (g_mkdir_with_parents (tmpdir, 0700) == 0);

	cmdline = g_strdup_printf ("%s extract --muhome=%s --parts 3 "
				   "--target-dir=%s %s%cFoo%ccur%cmail5",
				   MU_PROGRAM,
				   tmpdir,
				   tmpdir,
				   MU_TESTMAILDIR2,
				   G_DIR_SEPARATOR,
				   G_DIR_SEPARATOR,
				   G_DIR_SEPARATOR);
	output = NULL;

	if (g_test_verbose ())
		g_print ("$ %s\n", cmdline);

	g_assert (g_spawn_command_line_sync (cmdline, &output, NULL,
					     NULL, NULL));
	g_assert_cmpstr (output, ==, "");

	att1 = g_strdup_printf ("%s%ccuster.jpg", tmpdir, G_DIR_SEPARATOR);
	att2 = g_strdup_printf ("%s%csittingbull.jpg", tmpdir, G_DIR_SEPARATOR);

	size = get_file_size(att1);
	g_assert_true (size >= 15958 && size <= 15960);
	g_assert_cmpint (get_file_size(att2),==,-1);

	g_free (output);
	g_free (tmpdir);
	g_free (cmdline);
	g_free (att1);
	g_free (att2);
}


static void
test_mu_extract_overwrite (void)
{
	gchar *cmdline, *output, *erroutput, *tmpdir;

	tmpdir = test_mu_common_get_random_tmpdir();

	g_assert (g_mkdir_with_parents (tmpdir, 0700) == 0);

	cmdline = g_strdup_printf ("%s extract --muhome=%s -a "
				   "--target-dir=%s %s%cFoo%ccur%cmail5",
				   MU_PROGRAM, tmpdir, tmpdir,
				   MU_TESTMAILDIR2, G_DIR_SEPARATOR,
				   G_DIR_SEPARATOR, G_DIR_SEPARATOR);

	if (g_test_verbose ())
		g_print ("$ %s\n", cmdline);

	g_assert (g_spawn_command_line_sync (cmdline, &output, &erroutput,
					     NULL, NULL));
	g_assert_cmpstr (output, ==, "");
	g_assert_cmpstr (erroutput, ==, "");
	g_free (erroutput);
	g_free (output);

	if (g_test_verbose ())
		g_print ("$ %s\n", cmdline);

	return; // XXX fix tests

	/* now, it should fail, because we don't allow overwrites
	 * without --overwrite */
	g_assert (g_spawn_command_line_sync (cmdline, &output, &erroutput,
					     NULL, NULL));
	g_assert_cmpstr (output, ==, "");
	g_assert_cmpstr (erroutput, !=, "");
	g_free (erroutput);
	g_free (output);

	g_free (cmdline);
	/* this should work now, because we have specified --overwrite */
	cmdline = g_strdup_printf ("%s extract --muhome=%s -a --overwrite "
				   "--target-dir=%s %s%cFoo%ccur%cmail5",
				   MU_PROGRAM, tmpdir, tmpdir,
				   MU_TESTMAILDIR2, G_DIR_SEPARATOR,
				   G_DIR_SEPARATOR, G_DIR_SEPARATOR);
	if (g_test_verbose ())
		g_print ("$ %s\n", cmdline);

	g_assert (g_spawn_command_line_sync (cmdline, &output, &erroutput,
					     NULL, NULL));
	g_assert_cmpstr (output, ==, "");
	g_assert_cmpstr (erroutput, ==, "");
	g_free (erroutput);
	g_free (output);

	g_free (tmpdir);
	g_free (cmdline);
}


static void
test_mu_extract_by_name (void)
{
	gchar *cmdline, *output, *erroutput, *tmpdir, *path;

	tmpdir = test_mu_common_get_random_tmpdir();

	g_assert (g_mkdir_with_parents (tmpdir, 0700) == 0);

	cmdline = g_strdup_printf ("%s extract --muhome=%s "
				   "--target-dir=%s %s%cFoo%ccur%cmail5 "
				   "sittingbull.jpg",
				   MU_PROGRAM, tmpdir, tmpdir,
				   MU_TESTMAILDIR2, G_DIR_SEPARATOR,
				   G_DIR_SEPARATOR, G_DIR_SEPARATOR);

	if (g_test_verbose ())
		g_print ("$ %s\n", cmdline);

	g_assert (g_spawn_command_line_sync (cmdline, &output, &erroutput,
					     NULL, NULL));
	g_assert_cmpstr (output, ==, "");
	g_assert_cmpstr (erroutput, ==, "");
	path = g_strdup_printf ("%s%c%s", tmpdir, G_DIR_SEPARATOR,
				"sittingbull.jpg");
	g_assert (access (path, F_OK) == 0);
	g_free (path);

	g_free (erroutput);
	g_free (output);

	g_free (tmpdir);
	g_free (cmdline);
}




static void
test_mu_view_01 (void)
{
	gchar *cmdline, *output, *tmpdir;
	int len;

	tmpdir = test_mu_common_get_random_tmpdir();
	g_assert (g_mkdir_with_parents (tmpdir, 0700) == 0);

	cmdline = g_strdup_printf ("%s view --muhome=%s %s%cbar%ccur%cmail4",
				   MU_PROGRAM,
				   tmpdir,
				   MU_TESTMAILDIR2,
				   G_DIR_SEPARATOR,
				   G_DIR_SEPARATOR,
				   G_DIR_SEPARATOR);
	output = NULL;

	if (g_test_verbose ())
		g_print ("$ %s\n", cmdline);

	g_assert (g_spawn_command_line_sync (cmdline, &output, NULL,
					     NULL, NULL));
	g_assert_cmpstr  (output, !=, NULL);

	/*
	 * note: there are two possibilities here; older versions of
	 * GMime will produce:
	 *
	 *    From: "=?iso-8859-1?Q? =F6tzi ?=" <oetzi@web.de>
	 *
	 * while newer ones return something like:
	 *
	 *    From:  ?tzi  <oetzi@web.de>
	 *
	 * or even
	 *
	 *    From:  \xc3\xb6tzi  <oetzi@web.de>
	 *
	 * both are 'okay' from mu's perspective; it'd be even better
	 * to have some #ifdefs for the GMime versions, but this
	 * should work for now
	 *
	 * Added 350 as 'okay', which comes with gmime 2.4.24 (ubuntu 10.04)
	 */
	len = strlen(output);
	/* g_print ("\n[%s] (%d)\n", output, len); */
	g_assert (len > 349);

	g_free (output);
	g_free (cmdline);
	g_free (tmpdir);
}


static void
test_mu_view_multi (void)
{
	gchar *cmdline, *output, *tmpdir;
	int len;

	tmpdir = test_mu_common_get_random_tmpdir();
	g_assert (g_mkdir_with_parents (tmpdir, 0700) == 0);

	cmdline = g_strdup_printf ("%s view --muhome=%s "
				   "%s%cbar%ccur%cmail5 "
				   "%s%cbar%ccur%cmail5",
				   MU_PROGRAM,
				   tmpdir,
				   MU_TESTMAILDIR2,
				   G_DIR_SEPARATOR,
				   G_DIR_SEPARATOR,
				   G_DIR_SEPARATOR,
				   MU_TESTMAILDIR2,
				   G_DIR_SEPARATOR,
				   G_DIR_SEPARATOR,
				   G_DIR_SEPARATOR);
	output = NULL;
	g_assert (g_spawn_command_line_sync (cmdline, &output, NULL,
					     NULL, NULL));
	g_assert_cmpstr  (output, !=, NULL);

	len = strlen(output);
	/* g_print ("\n[%s](%u)\n", output, len); */
	g_assert_cmpuint (len,>,150);

	g_free (output);
	g_free (cmdline);
	g_free (tmpdir);
}


static void
test_mu_view_multi_separate (void)
{
	gchar *cmdline, *output, *tmpdir;
	int len;

	tmpdir = test_mu_common_get_random_tmpdir();
	g_assert (g_mkdir_with_parents (tmpdir, 0700) == 0);

	cmdline = g_strdup_printf ("%s view --terminate --muhome=%s "
				   "%s%cbar%ccur%cmail5 "
				   "%s%cbar%ccur%cmail5",
				   MU_PROGRAM,
				   tmpdir,
				   MU_TESTMAILDIR2,
				   G_DIR_SEPARATOR,
				   G_DIR_SEPARATOR,
				   G_DIR_SEPARATOR,
				   MU_TESTMAILDIR2,
				   G_DIR_SEPARATOR,
				   G_DIR_SEPARATOR,
				   G_DIR_SEPARATOR);
	output = NULL;
	g_assert (g_spawn_command_line_sync (cmdline, &output, NULL,
					     NULL, NULL));
	g_assert_cmpstr  (output, !=, NULL);

	len = strlen(output);
	/* g_print ("\n[%s](%u)\n", output, len); */
	g_assert_cmpuint (len,>,150);

	g_free (output);
	g_free (cmdline);
	g_free (tmpdir);
}




static void
test_mu_view_attach (void)
{
	gchar *cmdline, *output, *tmpdir;
	int len;

	tmpdir = test_mu_common_get_random_tmpdir();
	g_assert (g_mkdir_with_parents (tmpdir, 0700) == 0);

	cmdline = g_strdup_printf ("%s view --muhome=%s %s%cFoo%ccur%cmail5",
				   MU_PROGRAM,
				   tmpdir,
				   MU_TESTMAILDIR2,
				   G_DIR_SEPARATOR,
				   G_DIR_SEPARATOR,
				   G_DIR_SEPARATOR);
	output = NULL;
	g_assert (g_spawn_command_line_sync (cmdline, &output, NULL,
					     NULL, NULL));
	g_assert_cmpstr  (output, !=, NULL);

	len = strlen(output);
	g_assert (len == 168 || len == 166);

	g_free (output);
	g_free (cmdline);
	g_free (tmpdir);
}



static void
test_mu_mkdir_01 (void)
{
	gchar *cmdline, *output,  *tmpdir;
	gchar *dir;

	tmpdir = test_mu_common_get_random_tmpdir();
	g_assert (g_mkdir_with_parents (tmpdir, 0700) == 0);

	cmdline = g_strdup_printf ("%s mkdir --muhome=%s %s%ctest1 %s%ctest2",
				   MU_PROGRAM,tmpdir,
				   tmpdir, G_DIR_SEPARATOR,
				   tmpdir, G_DIR_SEPARATOR);

	output = NULL;
	g_assert (g_spawn_command_line_sync (cmdline, &output, NULL,
					     NULL, NULL));
	g_assert_cmpstr (output, ==, "");

	dir = g_strdup_printf ("%s%ctest1%ccur", tmpdir, G_DIR_SEPARATOR,
			       G_DIR_SEPARATOR);
	g_assert (access (dir, F_OK) == 0);
	g_free (dir);

	dir = g_strdup_printf ("%s%ctest2%ctmp", tmpdir, G_DIR_SEPARATOR,
			       G_DIR_SEPARATOR);
	g_assert (access (dir, F_OK) == 0);
	g_free (dir);

	dir = g_strdup_printf ("%s%ctest1%cnew", tmpdir, G_DIR_SEPARATOR,
			       G_DIR_SEPARATOR);
	g_assert (access (dir, F_OK) == 0);
	g_free (dir);

	g_free (output);
	g_free (tmpdir);
	g_free (cmdline);
}

/* we can only test 'verify' if gpg is installed, and has
 * djcb@djcbsoftware's key in the keyring */
G_GNUC_UNUSED static gboolean
verify_is_testable (void)
{
	gchar *gpg, *cmdline;
	gchar *output, *erroutput;
	int retval;
	gboolean rv;

	/* find GPG or return FALSE */
	if ((gpg = (char*)g_getenv ("MU_GPG_PATH"))) {
		if (access (gpg, X_OK) != 0)
			return FALSE;
		else
			gpg = g_strdup (gpg);

	} else if (!(gpg = g_find_program_in_path ("gpg2")))
		return FALSE;

	cmdline = g_strdup_printf ("%s --list-keys DCC4A036", gpg);
	g_free (gpg);

	output = erroutput = NULL;
	rv = g_spawn_command_line_sync (cmdline, &output, &erroutput,
					&retval, NULL);
	g_free (output);
	g_free (erroutput);
	g_free (cmdline);

	return (rv && retval == 0) ? TRUE:FALSE;
}

G_GNUC_UNUSED static void
test_mu_verify_good (void)
{
	gchar *cmdline, *output;
	int retval;

	if (!verify_is_testable ())
		return;

	cmdline = g_strdup_printf ("%s verify %s/signed!2,S",
				   MU_PROGRAM, MU_TESTMAILDIR4);

	if (g_test_verbose())
		g_print ("$ %s\n", cmdline);

	output = NULL;
	g_assert (g_spawn_command_line_sync (cmdline, &output, NULL,
					     &retval, NULL));
	g_free (output);
	g_assert_cmpuint (retval, ==, 0);
	g_free (cmdline);

}


G_GNUC_UNUSED  static void
test_mu_verify_bad (void)
{
	gchar *cmdline, *output;
	int retval;

	if (!verify_is_testable ())
		return;

	cmdline = g_strdup_printf ("%s verify %s/signed-bad!2,S",
				   MU_PROGRAM,
				   MU_TESTMAILDIR4);

	if (g_test_verbose())
		g_print ("$ %s\n", cmdline);

	output = NULL;
	g_assert (g_spawn_command_line_sync (cmdline, &output, NULL,
					     &retval, NULL));
	g_free (output);
	g_assert_cmpuint (retval, !=, 0);
	g_free (cmdline);
}



int
main (int argc, char *argv[])
{
	int rv;
	g_test_init (&argc, &argv, NULL);

	if (!set_en_us_utf8_locale())
		return 0; /* don't error out... */

	g_test_add_func ("/mu-cmd/test-mu-index", test_mu_index);

	g_test_add_func ("/mu-cmd/test-mu-find-empty-query",
			 test_mu_find_empty_query);
	g_test_add_func ("/mu-cmd/test-mu-find-01", test_mu_find_01);
	g_test_add_func ("/mu-cmd/test-mu-find-02", test_mu_find_02);

	g_test_add_func ("/mu-cmd/test-mu-find-file", test_mu_find_file);
	g_test_add_func ("/mu-cmd/test-mu-find-mime", test_mu_find_mime);

	/* recently, this test breaks _sometimes_ when run on Travis; but it
	 * seems related to the setup there, as nothing has changed in the code.
	 * turn off for now. */
	/* g_test_add_func ("/mu-cmd/test-mu-find-links",
	 * test_mu_find_links); */

	g_test_add_func ("/mu-cmd/test-mu-find-text-in-rfc822",
			 test_mu_find_text_in_rfc822);

	g_test_add_func ("/mu-cmd/test-mu-find-03", test_mu_find_03);
	g_test_add_func ("/mu-cmd/test-mu-find-04", test_mu_find_04);
	g_test_add_func ("/mu-cmd/test-mu-find-maildir-special",
			 test_mu_find_maildir_special);
	g_test_add_func ("/mu-cmd/test-mu-extract-01", test_mu_extract_01);
	g_test_add_func ("/mu-cmd/test-mu-extract-02", test_mu_extract_02);
	g_test_add_func ("/mu-cmd/test-mu-extract-03", test_mu_extract_03);
	g_test_add_func ("/mu-cmd/test-mu-extract-overwrite",
			 test_mu_extract_overwrite);
	g_test_add_func ("/mu-cmd/test-mu-extract-by-name",
			 test_mu_extract_by_name);

	g_test_add_func ("/mu-cmd/test-mu-view-01",  test_mu_view_01);
	g_test_add_func ("/mu-cmd/test-mu-view-multi",
			 test_mu_view_multi);
	g_test_add_func ("/mu-cmd/test-mu-view-multi-separate",
			 test_mu_view_multi_separate);
	g_test_add_func ("/mu-cmd/test-mu-view-attach",  test_mu_view_attach);
	g_test_add_func ("/mu-cmd/test-mu-mkdir-01",  test_mu_mkdir_01);

	g_test_add_func ("/mu-cmd/test-mu-verify-good",  test_mu_verify_good);
	g_test_add_func ("/mu-cmd/test-mu-verify-bad",  test_mu_verify_bad);

	g_log_set_handler (NULL,
			   (GLogLevelFlags)(
			   G_LOG_LEVEL_MASK | G_LOG_LEVEL_WARNING|
			   G_LOG_FLAG_FATAL| G_LOG_FLAG_RECURSION),
			   (GLogFunc)black_hole, NULL);

	DBPATH = fill_database ();
	rv = g_test_run ();
	g_free (DBPATH);

	return rv;
}
