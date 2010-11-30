/* 
** Copyright (C) 2008-2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 3 of the License, or
** (at your option) any later version.
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
#include <config.h>
#endif /*HAVE_CONFIG_H*/

#include "mu-util.h"

#define _XOPEN_SOURCE 500
#include <wordexp.h> /* for shell-style globbing */
#include <stdlib.h>

/* hopefully, the should get us a sane PATH_MAX */
#include <limits.h>
/* not all systems provide PATH_MAX in limits.h */
#ifndef PATH_MAX
#include <sys/param.h>
#ifndef PATH_MAX
#define PATH_MAX MAXPATHLEN
#endif /*!PATH_MAX*/
#endif /*PATH_MAX*/

#include <string.h>
#include <locale.h> /* for setlocale() */

#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <glib-object.h>
#include <glib/gstdio.h>
#include <errno.h>

static char*
do_wordexp (const char *path)
{
	wordexp_t wexp;
	char *dir;

	if (!path) {
		g_debug ("%s: path is empty", __FUNCTION__);
		return NULL;
	}
	
	if (wordexp (path, &wexp, 0) != 0) {
		/* g_debug ("%s: expansion failed for %s", __FUNCTION__, path); */
		return NULL;
	}
	
	/* we just pick the first one */
	dir = g_strdup (wexp.we_wordv[0]);

	/* strangely, below seems to lead to a crash on MacOS (BSD);
	   so we have to allow for a tiny leak here on that
	   platform... maybe instead of __APPLE__ it should be
	   __BSD__?*/
#ifndef __APPLE__     
	wordfree (&wexp);
#endif /*__APPLE__*/

	return dir;
}


/* note, the g_debugs are commented out because this function may be
 * called before the log handler is installed. */
char*
mu_util_dir_expand (const char *path)
{
	char *dir;
	char resolved[PATH_MAX + 1];
	
	g_return_val_if_fail (path, NULL);

	dir = do_wordexp (path);
	if (!dir)
		return NULL; /* error */
	
	/* now, resolve any symlinks, .. etc. */
	if (!realpath (dir, resolved)) {
		/* g_debug ("%s: could not get realpath for '%s': %s", */
		/* 	 __FUNCTION__, dir, strerror(errno)); */
		g_free (dir);
		return NULL;
	} else 
		g_free (dir);
	
	return g_strdup (resolved);
}

gboolean
mu_util_init_system (void)
{
	/* without setlocale, non-ascii cmdline params (like search
	 * terms) won't work */
	setlocale (LC_ALL, "");

	/* on FreeBSD, it seems g_slice_new and friends lead to
	 * segfaults. So we shut if off */
#ifdef 	__FreeBSD__
	if (!g_setenv ("G_SLICE", "always-malloc", TRUE)) {
		g_critical ("cannot set G_SLICE");
		return FALSE;
	}
	MU_LOG_FILE("setting G_SLICE to always-malloc");
#endif /*__FreeBSD__*/

	g_type_init ();

	return TRUE;
}


gboolean
mu_util_check_dir (const gchar* path, gboolean readable, gboolean writeable)
{
	int mode;
	struct stat statbuf;
	
	if (!path) 
		return FALSE;
	
	mode = F_OK | (readable ? R_OK : 0) | (writeable ? W_OK : 0);

	if (access (path, mode) != 0) {
		g_debug ("Cannot access %s: %s", path, strerror (errno));
		return FALSE;
	}

	if (stat (path, &statbuf) != 0) {
		g_debug ("Cannot stat %s: %s", path, strerror (errno));
		return FALSE;
	}
	
	return S_ISDIR(statbuf.st_mode) ? TRUE: FALSE;
}


gchar*
mu_util_guess_maildir (void)
{
	const gchar *mdir1;
	gchar *mdir2;
	
	/* first, try MAILDIR */
	mdir1 = g_getenv ("MAILDIR");

	if (mdir1 && mu_util_check_dir (mdir1, TRUE, FALSE))
		return g_strdup (mdir1);
	
	/* then, try ~/Maildir */
	mdir2 = mu_util_dir_expand ("~/Maildir");
	if (mu_util_check_dir (mdir2, TRUE, FALSE))
		return mdir2;
	
	/* nope; nothing found */
	return NULL;
}


gchar*
mu_util_guess_mu_homedir (void)
{
	const char* home;

	home = g_getenv ("HOME");
	if (!home)
		home = g_get_home_dir ();

	if (!home)
		MU_WRITE_LOG ("failed to determine homedir");
	
	return g_strdup_printf ("%s%c%s", home ? home : ".", G_DIR_SEPARATOR,
				".mu");
}

gboolean
mu_util_create_dir_maybe (const gchar *path)
{
	struct stat statbuf;
	
	g_return_val_if_fail (path, FALSE);

	/* if it exists, it must be a readable dir */ 
	if (stat (path, &statbuf) == 0) {
		if ((!S_ISDIR(statbuf.st_mode)) ||
		    (access (path, W_OK|R_OK) != 0)) {
			g_warning ("not a rw-directory: %s", path);
			return FALSE;
		}
	}		
		
	if (g_mkdir_with_parents (path, 0700) != 0) {
		g_warning ("failed to create %s: %s",
			   path, strerror(errno));
		return FALSE;
	}

	return TRUE;
}


gchar*
mu_util_str_from_strv (const gchar **params)
{
	GString *str;
	int i;
	
	g_return_val_if_fail (params, NULL);
	
	if (!params[0])
		return g_strdup ("");
	
	str = g_string_sized_new (64); /* just a guess */
	
	for (i = 0; params[i]; ++i) {

		if (i>0)
			g_string_append_c (str, ' ');

		g_string_append (str, params[i]);
	}		
	
	return g_string_free (str, FALSE);
}

int
mu_util_create_writeable_fd (const char* filename, const char* dir,
			     gboolean overwrite)
{
	int fd;
	char *fullpath;
	
	errno = 0; /* clear! */
	g_return_val_if_fail (filename, -1);

	fullpath = g_strdup_printf ("%s%s%s",
				    dir ? dir : "",
				    dir ? G_DIR_SEPARATOR_S : "",
				    filename);

	if (overwrite)
		fd = open (fullpath, O_WRONLY|O_CREAT|O_TRUNC, 0644);
	else
		fd = open (fullpath, O_WRONLY|O_CREAT, 0644);

	if (fd < 0)
		g_debug ("%s: cannot open %s for writing: %s",
			 __FUNCTION__, fullpath, strerror(errno));

	g_free (fullpath);	
	return fd;
}


