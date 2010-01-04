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
	
	g_return_val_if_fail (path, NULL);

	dir = NULL;
	wordexp (path, &wexp, 0);
	if (wexp.we_wordc != 1) 
		g_warning ("error expanding dir '%s'", path);
	else 
		dir = g_strdup (wexp.we_wordv[0]);
	
	wordfree (&wexp);

	return dir;
}




static gboolean
_is_readable_dir (const gchar* path)
{
	struct stat statbuf;

	return path &&
		access (path, R_OK) == 0 &&
		stat (path, &statbuf) == 0 &&
		S_ISDIR(statbuf.st_mode);
}

gchar*
mu_util_guess_maildir (void)
{
	char *dir;

	/* first, try MAILDIR */
	dir = getenv ("MAILDIR");
	if (_is_readable_dir (dir))
		return g_strdup (dir);

	/* then, try ~/Maildir */
	dir = mu_util_dir_expand ("~/Maildir");
	if (_is_readable_dir (dir))
		return dir;

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
