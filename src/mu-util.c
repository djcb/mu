/*
** Copyright (C) 2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#define _XOPEN_SOURCE
#include <wordexp.h> /* for shell-style globbing */

#include <stdlib.h>
#include <string.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <glib/gstdio.h>
#include <errno.h>

#include "mu-util.h"

char*
mu_util_dir_expand (const char *path)
{
	wordexp_t wexp;
	char *dir;
	int rv;
	
	g_return_val_if_fail (path, NULL);

	dir = NULL;

	rv = wordexp (path, &wexp, 0);
	if (rv != 0) {
		g_debug ("%s: expansion failed for '%s' [%d]",
			   __FUNCTION__, path, rv);
		return NULL;
	} else if (wexp.we_wordc != 1) {
		g_debug ("%s: expansion ambiguous for '%s'",
			 __FUNCTION__, path);
	} else	
		dir = g_strdup (wexp.we_wordv[0]);

	/* strangely, below seems to load to a crash on MacOS (BSD);
	   so we have to allow for a tiny leak here on that
	   platform... maybe instead of __APPLE__ it should be
	   __BSD__?*/
#ifndef __APPLE__     
	wordfree (&wexp);
#endif /*__APPLE__*/
	
	return dir;
}


gboolean
mu_util_check_dir (const gchar* path, gboolean readable, gboolean writeable)
{
	mode_t mode;
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



gboolean
mu_util_create_dir_maybe (const gchar *path)
{
	struct stat statbuf;
	
	g_return_val_if_fail (path, FALSE);

	/* if it exists, it must be a readable dir */ 
	if (stat (path, &statbuf) == 0) {
		if ((!S_ISDIR(statbuf.st_mode)) ||
		    (access (path, W_OK|R_OK) != 0)) {
			g_warning ("Not a rw-directory: %s", path);
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
