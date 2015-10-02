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
#include "mu-maildir.h"
#include "mu-util.h"

static void
test_mu_maildir_mkdir_01 (void)
{
	int i;
	gchar *tmpdir, *mdir, *tmp;
	const gchar *subs[] = {"tmp", "cur", "new"};

	tmpdir = test_mu_common_get_random_tmpdir ();
	mdir   = g_strdup_printf ("%s%c%s", tmpdir, G_DIR_SEPARATOR,
				  "cuux");

	g_assert_cmpuint (mu_maildir_mkdir (mdir, 0755, FALSE, NULL),
			  ==, TRUE);

	for (i = 0; i != G_N_ELEMENTS(subs); ++i) {
		gchar* dir;

		dir = g_strdup_printf ("%s%c%s", mdir, G_DIR_SEPARATOR,
				       subs[i]);
		g_assert_cmpuint (g_access (dir, R_OK), ==, 0);
		g_assert_cmpuint (g_access (dir, W_OK), ==, 0);
		g_free (dir);
	}

	tmp = g_strdup_printf ("%s%c%s", mdir, G_DIR_SEPARATOR, ".noindex");
	g_assert_cmpuint (g_access (tmp, F_OK), !=, 0);

	g_free (tmp);
	g_free (tmpdir);
	g_free (mdir);

}


static void
test_mu_maildir_mkdir_02 (void)
{
	int i;
	gchar *tmpdir, *mdir, *tmp;
	const gchar *subs[] = {"tmp", "cur", "new"};

	tmpdir = test_mu_common_get_random_tmpdir ();
	mdir   = g_strdup_printf ("%s%c%s", tmpdir, G_DIR_SEPARATOR,
				  "cuux");

	g_assert_cmpuint (mu_maildir_mkdir (mdir, 0755, TRUE, NULL),
			  ==, TRUE);

	for (i = 0; i != G_N_ELEMENTS(subs); ++i) {
		gchar* dir;

		dir = g_strdup_printf ("%s%c%s", mdir, G_DIR_SEPARATOR,
				       subs[i]);
		g_assert_cmpuint (g_access (dir, R_OK), ==, 0);

		g_assert_cmpuint (g_access (dir, W_OK), ==, 0);
		g_free (dir);
	}

	tmp = g_strdup_printf ("%s%c%s", mdir, G_DIR_SEPARATOR, ".noindex");
	g_assert_cmpuint (g_access (tmp, F_OK), ==, 0);

	g_free (tmp);
	g_free (tmpdir);
	g_free (mdir);
}


static void
test_mu_maildir_mkdir_03 (void)
{
	int i;
	gchar *tmpdir, *mdir, *tmp;
	const gchar *subs[] = {"tmp", "cur", "new"};

	tmpdir = test_mu_common_get_random_tmpdir ();
	mdir   = g_strdup_printf ("%s%c%s", tmpdir, G_DIR_SEPARATOR,
				  "cuux");

	/* create part of the structure already... */
	{
		gchar *dir;
		dir = g_strdup_printf ("%s%ccur", mdir, G_DIR_SEPARATOR);
		g_assert_cmpuint (g_mkdir_with_parents (dir, 0755), ==, 0);
		g_free (dir);
	}

	/* this should still work */
	g_assert_cmpuint (mu_maildir_mkdir (mdir, 0755, FALSE, NULL),
			  ==, TRUE);

	for (i = 0; i != G_N_ELEMENTS(subs); ++i) {
		gchar* dir;

		dir = g_strdup_printf ("%s%c%s", mdir, G_DIR_SEPARATOR,
				       subs[i]);
		g_assert_cmpuint (g_access (dir, R_OK), ==, 0);
		g_assert_cmpuint (g_access (dir, W_OK), ==, 0);
		g_free (dir);
	}

	tmp = g_strdup_printf ("%s%c%s", mdir, G_DIR_SEPARATOR, ".noindex");
	g_assert_cmpuint (g_access (tmp, F_OK), !=, 0);

	g_free (tmp);
	g_free (tmpdir);
	g_free (mdir);

}



static void
test_mu_maildir_mkdir_04 (void)
{
	gchar *tmpdir, *mdir;

	tmpdir = test_mu_common_get_random_tmpdir ();
	mdir   = g_strdup_printf ("%s%c%s", tmpdir, G_DIR_SEPARATOR,
				  "cuux");

	/* create part of the structure already... */
	{
		gchar *dir;
		g_assert_cmpuint (g_mkdir_with_parents (mdir, 0755), ==, 0);
		dir = g_strdup_printf ("%s%ccur", mdir, G_DIR_SEPARATOR);
		g_assert_cmpuint (g_mkdir_with_parents (dir, 0000), ==, 0);
		g_free (dir);
	}

	/* this should fail now, because cur is not read/writable  */
	g_assert_cmpuint (mu_maildir_mkdir (mdir, 0755, FALSE, NULL),
			  ==, (geteuid()==0 ? TRUE : FALSE));
	g_free (tmpdir);
	g_free (mdir);
}




static gboolean
ignore_error (const char* log_domain, GLogLevelFlags log_level, const gchar* msg,
	      gpointer user_data)
{
	return FALSE; /* don't abort */
}

static void
test_mu_maildir_mkdir_05 (void)
{
	/* this must fail */
	g_test_log_set_fatal_handler ((GTestLogFatalFunc)ignore_error, NULL);

	g_assert_cmpuint (mu_maildir_mkdir (NULL, 0755, TRUE, NULL),
					    ==, FALSE);
}


static gchar*
copy_test_data (void)
{
	gchar *dir, *cmd;

	dir = test_mu_common_get_random_tmpdir();
	cmd = g_strdup_printf ("mkdir -m 0700 %s", dir);
	if (g_test_verbose())
		g_print ("cmd: %s\n", cmd);
	g_assert (g_spawn_command_line_sync (cmd, NULL, NULL, NULL, NULL));
	g_free (cmd);

	cmd = g_strdup_printf ("cp -R %s %s", MU_TESTMAILDIR, dir);
	if (g_test_verbose())
		g_print ("cmd: %s\n", cmd);
	g_assert (g_spawn_command_line_sync (cmd, NULL, NULL, NULL, NULL));
	g_free (cmd);

	return dir;
}


typedef struct {
	int _file_count;
	int _dir_entered;
	int _dir_left;
} WalkData;

static MuError
dir_cb (const char *fullpath, gboolean enter, WalkData *data)
{
	if (enter)
	 	++data->_dir_entered;
	else
		++data->_dir_left;

	if (g_test_verbose())
		g_print ("%s: %s: %s (%u)\n", __func__, enter ? "entering" : "leaving",
			 fullpath, enter ? data->_dir_entered : data->_dir_left);

	return MU_OK;
}


static MuError
msg_cb (const char *fullpath, const char* mdir, struct stat *statinfo,
	WalkData *data)
{
	++data->_file_count;
	return MU_OK;
}


static void
test_mu_maildir_walk_01 (void)
{
	char *tmpdir;
	WalkData data;
	MuError rv;

	tmpdir = copy_test_data ();
	memset (&data, 0, sizeof(WalkData));

	rv = mu_maildir_walk (tmpdir,
			      (MuMaildirWalkMsgCallback)msg_cb,
			      (MuMaildirWalkDirCallback)dir_cb,
			      TRUE,
			      &data);

	g_assert_cmpuint (MU_OK, ==, rv);
	g_assert_cmpuint (data._file_count, ==, 18);

	g_assert_cmpuint (data._dir_entered,==, 5);
	g_assert_cmpuint (data._dir_left,==, 5);

	g_free (tmpdir);
}


static void
test_mu_maildir_walk (void)
{
	char *tmpdir, *cmd, *dir;
	WalkData data;
	MuError rv;

	tmpdir = copy_test_data ();
	memset (&data, 0, sizeof(WalkData));

	/* mark the 'new' dir with '.noindex', to ignore it */
	dir =  g_strdup_printf ("%s%ctestdir%cnew", tmpdir,
				G_DIR_SEPARATOR, G_DIR_SEPARATOR);
	cmd = g_strdup_printf ("chmod 700 %s", dir);
	g_assert (g_spawn_command_line_sync (cmd, NULL, NULL, NULL, NULL));
	g_free (cmd);

	cmd = g_strdup_printf ("touch %s%c.noindex", dir, G_DIR_SEPARATOR);
	g_assert (g_spawn_command_line_sync (cmd, NULL, NULL, NULL, NULL));
	g_free (cmd);
	g_free (dir);

	rv = mu_maildir_walk (tmpdir,
			      (MuMaildirWalkMsgCallback)msg_cb,
			      (MuMaildirWalkDirCallback)dir_cb,
			      TRUE,
			      &data);

	g_assert_cmpuint (MU_OK, ==, rv);
	g_assert_cmpuint (data._file_count, ==, 14);

	g_assert_cmpuint (data._dir_entered,==, 4);
	g_assert_cmpuint (data._dir_left,==, 4);

	g_free (tmpdir);
}

static void
test_mu_maildir_walk_with_noupdate (void)
{
	char *tmpdir, *cmd, *dir;
	WalkData data;
	MuError rv;

	tmpdir = copy_test_data ();

	/* mark the 'new' dir with '.noindex', to ignore it */
	dir =  g_strdup_printf ("%s%ctestdir%cnew", tmpdir,
				G_DIR_SEPARATOR, G_DIR_SEPARATOR);
	cmd = g_strdup_printf ("chmod 700 %s", dir);
	g_assert (g_spawn_command_line_sync (cmd, NULL, NULL, NULL, NULL));
	g_free (cmd);

	memset (&data, 0, sizeof(WalkData));
	rv = mu_maildir_walk (tmpdir,
			      (MuMaildirWalkMsgCallback)msg_cb,
			      (MuMaildirWalkDirCallback)dir_cb,
			      FALSE, /* ie., non-full update */
			      &data);

	g_assert_cmpuint (MU_OK, ==, rv);
	g_assert_cmpuint (data._file_count, ==, 18);
	g_assert_cmpuint (data._dir_entered,==, 5);
	g_assert_cmpuint (data._dir_left,==, 5);

	/* again, full update. results should be the same, since there
	 * is no noupdate yet */
	memset (&data, 0, sizeof(WalkData));
	rv = mu_maildir_walk (tmpdir,
			      (MuMaildirWalkMsgCallback)msg_cb,
			      (MuMaildirWalkDirCallback)dir_cb,
			      TRUE, /* ie., full update */
			      &data);

	g_assert_cmpuint (MU_OK, ==, rv);
	g_assert_cmpuint (data._file_count, ==, 18);
	g_assert_cmpuint (data._dir_entered,==, 5);
	g_assert_cmpuint (data._dir_left,==, 5);

	/* add a '.noupdate' file; this affects the outcome when the
	 * 4th arg to mu_maildir_walk is FALSE */
	cmd = g_strdup_printf ("touch %s%c.noupdate", dir, G_DIR_SEPARATOR);
	g_assert (g_spawn_command_line_sync (cmd, NULL, NULL, NULL, NULL));
	g_free (cmd);

	memset (&data, 0, sizeof(WalkData));
	rv = mu_maildir_walk (tmpdir,
			      (MuMaildirWalkMsgCallback)msg_cb,
			      (MuMaildirWalkDirCallback)dir_cb,
			      FALSE, /* non-full update */
			      &data);

	g_assert_cmpuint (MU_OK, ==, rv);
	g_assert_cmpuint (data._file_count, ==, 14);

	g_assert_cmpuint (data._dir_entered,==, 4);
	g_assert_cmpuint (data._dir_left,==, 4);

	/* now run again, but do a full update */
	memset (&data, 0, sizeof(WalkData));
	rv = mu_maildir_walk (tmpdir,
			      (MuMaildirWalkMsgCallback)msg_cb,
			      (MuMaildirWalkDirCallback)dir_cb,
			      TRUE, /* full update */
			      &data);

	g_assert_cmpuint (MU_OK, ==, rv);
	g_assert_cmpuint (data._file_count, ==, 18);

	g_assert_cmpuint (data._dir_entered,==, 5);
	g_assert_cmpuint (data._dir_left,==, 5);

	g_free (dir);
	g_free (tmpdir);
}






static void
test_mu_maildir_get_flags_from_path (void)
{
	int i;
	struct {
		const char *path;
		MuFlags flags;
	} paths[] = {
		{
			"/home/foo/Maildir/test/cur/123456:2,FSR",
			MU_FLAG_REPLIED | MU_FLAG_SEEN | MU_FLAG_FLAGGED
		},
		{
			"/home/foo/Maildir/test/new/123456",
			MU_FLAG_NEW
		},
		{
			/* NOTE: when in new/, the :2,.. stuff is ignored */
			"/home/foo/Maildir/test/new/123456:2,FR",
			MU_FLAG_NEW
		},
		{
			"/home/foo/Maildir/test/cur/123456:2,DTP",
			MU_FLAG_DRAFT | MU_FLAG_TRASHED |
			MU_FLAG_PASSED
		},
		{
			"/home/foo/Maildir/test/cur/123456:2,S",
			MU_FLAG_SEEN
		}
	};

	for (i = 0; i != G_N_ELEMENTS(paths); ++i) {
		MuFlags flags;
		flags = mu_maildir_get_flags_from_path(paths[i].path);
		g_assert_cmpuint(flags, ==, paths[i].flags);
	}
}



static void
assert_matches_regexp (const char *str, const char *rx)
{
	if (!g_regex_match_simple (rx, str, 0, 0)) {
		if (g_test_verbose ())
			g_print ("%s does not match %s", str, rx);
		g_assert (0);
	}
}



static void
test_mu_maildir_get_new_path_new (void)
{
	int i;

	struct {
		const char *oldpath;
		MuFlags flags;
		const char *newpath;
	} paths[] = {
		{
			"/home/foo/Maildir/test/cur/123456:2,FR",
			MU_FLAG_REPLIED,
			"/home/foo/Maildir/test/cur/123456:2,R"
		}, {
			"/home/foo/Maildir/test/cur/123456:2,FR",
			MU_FLAG_NEW,
			"/home/foo/Maildir/test/new/123456"
		}, {
			"/home/foo/Maildir/test/new/123456:2,FR",
			MU_FLAG_SEEN | MU_FLAG_REPLIED,
			"/home/foo/Maildir/test/cur/123456:2,RS"
		}, {
			"/home/foo/Maildir/test/new/1313038887_0.697:2,",
			MU_FLAG_SEEN | MU_FLAG_FLAGGED | MU_FLAG_PASSED,
			"/home/foo/Maildir/test/cur/1313038887_0.697:2,FPS"
		}, {
			"/home/djcb/Maildir/trash/new/1312920597.2206_16.cthulhu",
			MU_FLAG_SEEN,
			"/home/djcb/Maildir/trash/cur/1312920597.2206_16.cthulhu:2,S"
		}
	};

	for (i = 0; i != G_N_ELEMENTS(paths); ++i) {
		char	*str, *newbase;
		str	= mu_maildir_get_new_path (paths[i].oldpath, NULL,
						   paths[i].flags, TRUE);
		newbase = g_path_get_basename (str);
		assert_matches_regexp (newbase,
				       "\\d+\\."
				       "[[:xdigit:]]{16}\\."
				       "[[:alpha:]]+(:2,.*)?");
		g_free (newbase);
		g_free(str);
	}
}




static void
test_mu_maildir_get_new_path_01 (void)
{
	int i;

	struct {
		const char *oldpath;
		MuFlags flags;
		const char *newpath;
	} paths[] = {
		{
			"/home/foo/Maildir/test/cur/123456:2,FR",
			MU_FLAG_REPLIED,
			"/home/foo/Maildir/test/cur/123456:2,R"
		}, {
			"/home/foo/Maildir/test/cur/123456:2,FR",
			MU_FLAG_NEW,
			"/home/foo/Maildir/test/new/123456"
		}, {
			"/home/foo/Maildir/test/new/123456:2,FR",
			MU_FLAG_SEEN | MU_FLAG_REPLIED,
			"/home/foo/Maildir/test/cur/123456:2,RS"
		}, {
			"/home/foo/Maildir/test/new/1313038887_0.697:2,",
			MU_FLAG_SEEN | MU_FLAG_FLAGGED | MU_FLAG_PASSED,
			"/home/foo/Maildir/test/cur/1313038887_0.697:2,FPS"
		}, {
			"/home/djcb/Maildir/trash/new/1312920597.2206_16.cthulhu",
			MU_FLAG_SEEN,
			"/home/djcb/Maildir/trash/cur/1312920597.2206_16.cthulhu:2,S"
		}
	};

	for (i = 0; i != G_N_ELEMENTS(paths); ++i) {
		gchar *str;
		str = mu_maildir_get_new_path(paths[i].oldpath, NULL,
					      paths[i].flags, FALSE);
		g_assert_cmpstr(str, ==, paths[i].newpath);
		g_free(str);
	}
}


static void
test_mu_maildir_get_new_path_02 (void)
{
	int i;

	struct {
		const char *oldpath;
		MuFlags flags;
		const char *targetdir;
		const char *newpath;
	} paths[] = {
		{
			"/home/foo/Maildir/test/cur/123456:2,FR",
			MU_FLAG_REPLIED, "/home/foo/Maildir/blabla",
			"/home/foo/Maildir/blabla/cur/123456:2,R"
		}, {
			"/home/foo/Maildir/test/cur/123456:2,FR",
			MU_FLAG_NEW, "/home/bar/Maildir/coffee",
			"/home/bar/Maildir/coffee/new/123456"
		}, {
			"/home/foo/Maildir/test/new/123456",
			MU_FLAG_SEEN | MU_FLAG_REPLIED,
			"/home/cuux/Maildir/tea",
			"/home/cuux/Maildir/tea/cur/123456:2,RS"
		}, {
			"/home/foo/Maildir/test/new/1313038887_0.697:2,",
			MU_FLAG_SEEN | MU_FLAG_FLAGGED | MU_FLAG_PASSED,
			"/home/boy/Maildir/stuff",
			"/home/boy/Maildir/stuff/cur/1313038887_0.697:2,FPS"
		}
	};

	for (i = 0; i != G_N_ELEMENTS(paths); ++i) {
		gchar *str;
		str = mu_maildir_get_new_path(paths[i].oldpath,
					      paths[i].targetdir,
					      paths[i].flags, FALSE);
		g_assert_cmpstr(str, ==, paths[i].newpath);
		g_free(str);
	}
}


static void
test_mu_maildir_get_new_path_custom (void)
{
	int i;

	struct {
		const char *oldpath;
		MuFlags flags;
		const char *targetdir;
		const char *newpath;
	} paths[] = {
		{
			"/home/foo/Maildir/test/cur/123456:2,FR",
			MU_FLAG_REPLIED, "/home/foo/Maildir/blabla",
			"/home/foo/Maildir/blabla/cur/123456:2,R"
		}, {
			"/home/foo/Maildir/test/cur/123456:2,hFeRllo123",
			MU_FLAG_FLAGGED, "/home/foo/Maildir/blabla",
			"/home/foo/Maildir/blabla/cur/123456:2,Fhello123"
		}, {
			"/home/foo/Maildir/test/cur/123456:2,abc",
			MU_FLAG_PASSED, "/home/foo/Maildir/blabla",
			"/home/foo/Maildir/blabla/cur/123456:2,Pabc"
		}
	};

	for (i = 0; i != G_N_ELEMENTS(paths); ++i) {
		gchar *str;
		str = mu_maildir_get_new_path(paths[i].oldpath,
					      paths[i].targetdir,
					      paths[i].flags, FALSE);
		g_assert_cmpstr(str, ==, paths[i].newpath);
		g_free(str);
	}
}



static void
test_mu_maildir_get_maildir_from_path (void)
{
	unsigned u;

	struct {
		const char *path, *exp;
	} cases[] = {
		{"/home/foo/Maildir/test/cur/123456:2,FR",
		 "/home/foo/Maildir/test"},
		{"/home/foo/Maildir/lala/new/1313038887_0.697:2,",
		 "/home/foo/Maildir/lala"}
	};


	for (u = 0; u != G_N_ELEMENTS(cases); ++u) {
		gchar *mdir;
		mdir = mu_maildir_get_maildir_from_path (cases[u].path);
		g_assert_cmpstr(mdir,==,cases[u].exp);
		g_free (mdir);
	}
}


int
main (int argc, char *argv[])
{
	g_test_init (&argc, &argv, NULL);

	/* mu_util_maildir_mkmdir */
 	g_test_add_func ("/mu-maildir/mu-maildir-mkdir-01",
			 test_mu_maildir_mkdir_01);
	g_test_add_func ("/mu-maildir/mu-maildir-mkdir-02",
			 test_mu_maildir_mkdir_02);
	g_test_add_func ("/mu-maildir/mu-maildir-mkdir-03",
			 test_mu_maildir_mkdir_03);
	g_test_add_func ("/mu-maildir/mu-maildir-mkdir-04",
			 test_mu_maildir_mkdir_04);
	g_test_add_func ("/mu-maildir/mu-maildir-mkdir-05",
			 test_mu_maildir_mkdir_05);


	/* mu_util_maildir_walk */
	g_test_add_func ("/mu-maildir/mu-maildir-walk-01",
			 test_mu_maildir_walk_01);
	g_test_add_func ("/mu-maildir/mu-maildir-walk",
			 test_mu_maildir_walk);
	g_test_add_func ("/mu-maildir/mu-maildir-walk-with-noupdate",
			 test_mu_maildir_walk_with_noupdate);

	/* get/set flags */
	g_test_add_func("/mu-maildir/mu-maildir-get-new-path-new",
			test_mu_maildir_get_new_path_new);

	g_test_add_func("/mu-maildir/mu-maildir-get-new-path-01",
			test_mu_maildir_get_new_path_01);
	g_test_add_func("/mu-maildir/mu-maildir-get-new-path-02",
			test_mu_maildir_get_new_path_02);
	g_test_add_func("/mu-maildir/mu-maildir-get-new-path-custom",
			test_mu_maildir_get_new_path_custom);
	g_test_add_func("/mu-maildir/mu-maildir-get-flags-from-path",
			test_mu_maildir_get_flags_from_path);


	g_test_add_func("/mu-maildir/mu-maildir-get-maildir-from-path",
			test_mu_maildir_get_maildir_from_path);

	g_log_set_handler (NULL,
			   G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL|
			   G_LOG_FLAG_RECURSION,
			   (GLogFunc)black_hole, NULL);

	return g_test_run ();
}
