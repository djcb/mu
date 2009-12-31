/* 
** Copyright (C) 2009 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <string.h>
#include <errno.h>

#include "mu-maildir.h"

static gboolean
_create_maildir (const char *path, int mode, GError **err)
{
	int i;
	const gchar* subdirs[] = {"new", "cur", "tmp"};

	/* make sure it does not exist yet */
	if (access (path, F_OK) == 0)
		errno = EEXIST;
	if (errno != ENOENT) {
		g_set_error (err, 0, 0, "%s", strerror (errno));
		return FALSE;
	}
	
	for (i = 0; i != G_N_ELEMENTS(subdirs); ++i) {

		gchar *fullpath;
		int rv;
		
		fullpath = g_strdup_printf ("%s%c%s", path,
					    G_DIR_SEPARATOR,
					    subdirs[i]);
		rv = g_mkdir_with_parents (fullpath, mode);
		g_free (fullpath);
		
		if (rv != 0) {
			g_set_error (err, 0, 0, "%s",
				     strerror (errno));
			return FALSE;
		}
	}
	
	return TRUE;
}

static gboolean
_create_noindex (const char *path, GError **err)
{	
	/* create a noindex file if requested */
	int fd;
	gchar *noindexpath;
	noindexpath = g_strdup_printf ("%s%c%s", path,
					       G_DIR_SEPARATOR,
					       ".noindex");
	fd = creat (noindexpath, 0644);
	g_free (noindexpath);
	if (fd < 0 || close (fd) != 0) {
		g_set_error (err, 0, 0, "%s", strerror (errno));
		return FALSE;
	}

	return TRUE;
}


gboolean
mu_maildir_mkmdir (const char* path, int mode,
		   gboolean noindex, GError **err)
{
	
	g_return_val_if_fail (path, FALSE);
	
	if (!_create_maildir (path, mode, err))
		return FALSE;

	if (noindex && !_create_noindex (path, err))
		return FALSE;
	
	return TRUE;
}

/* determine whether the source message is in 'new' or in 'cur';
 * we ignore messages in 'tmp' for obvious reasons */
static gboolean
_check_subdir (const char *src, gboolean *in_cur, GError **err)
{
	gchar *srcpath;

	srcpath = g_path_get_dirname (src);

	if (g_str_has_suffix (srcpath, "new"))
		*in_cur = FALSE;
	else if (g_str_has_suffix (srcpath, "cur"))
		*in_cur = TRUE;
	else {
		g_set_error (err, 0, 0, "%s",
			     "Invalid source message");
		return FALSE;
	}
	g_free (srcpath);
	
	return TRUE;
}

static gchar*
_get_target_fullpath (const char* src,  const gchar *targetpath,
		      GError **err)
{
	gchar *targetfullpath;
	gchar *srcfile;
	gboolean in_cur;
	
	if (!_check_subdir (src, &in_cur, err))
		return NULL;
	
	srcfile = g_path_get_basename (src);

	/* create & test targetpath  */
	targetfullpath = g_strdup_printf ("%s%c%s%c%s",
					  targetpath,
					  G_DIR_SEPARATOR,
					  in_cur ? "cur" : "new",
					  G_DIR_SEPARATOR,
					  srcfile);
	g_free (srcfile);
	
	return targetfullpath;
}



gboolean
mu_maildir_link (const char* src, const char *targetpath,
		 GError **err)
{
	gchar *targetfullpath;
	int rv;
	
	g_return_val_if_fail (src, FALSE);
	g_return_val_if_fail (targetpath, FALSE);

	/* just a basic check */
	if (access (src, R_OK) != 0) {
		g_set_error (err, 0, 0, "Cannot read source message: %s",
			     strerror (errno));
		return FALSE;
	}
	
	targetfullpath = _get_target_fullpath (src, targetpath, err);
	if (!targetfullpath)
		return FALSE;
	
	rv = symlink (src, targetfullpath);
	g_free (targetfullpath);
	if (rv != 0) {
		g_set_error (err, 0, 0, "Error creating link: %s",
			     strerror (errno));
		return FALSE;
	}

	return TRUE;
}

