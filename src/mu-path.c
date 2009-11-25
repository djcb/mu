/* 
** Copyright (C) 2008 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <dirent.h>
#include <errno.h>
#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <glib/gstdio.h>
#include <string.h>

#include "mu-path.h"
#include "mu-msg-flags.h"

static MuResult process_dir (const char* path, MuPathWalkMsgCallback msg_cb, 
			     MuPathWalkDirCallback dir_cb, void *data);




static MuResult 
process_file (const char* fullpath, MuPathWalkMsgCallback cb, void *data)
{
	MuResult result;
	struct stat statbuf;
	
	if (!cb)
		return MU_OK;

	/* FIXME: may remove this access, and only use 'stat'? may be faster */
	if (G_UNLIKELY(access(fullpath, R_OK) != 0)) {
		g_warning ("cannot access %s: %s", fullpath, strerror(errno));
		return MU_ERROR;
	}
	
	if (G_UNLIKELY(stat (fullpath, &statbuf) != 0)) {
		g_warning ("cannot stat %s: %s", fullpath, strerror(errno));
		return MU_ERROR;
	}

	result = (cb)(fullpath,statbuf.st_mtime,data);
	
	if (G_LIKELY(result == MU_OK || result == MU_STOP))
		return result;
	else {
		g_warning ("%s: failed %d in callback (%s)",  
			   __FUNCTION__, result, fullpath);
		return result;
	}
}


/* determine if path is a maildir leaf-dir; ie. if it's 'cur' or 'new'
 * (we're ignoring 'tmp' for obvious reasons)
 */
static gboolean
is_maildir_new_or_cur (const char *path)
{
	size_t len;
	const char *sfx;

	/* path is the full path; it cannot possibly be shorter
	 * than 4 for a maildir (/cur or /new)
	 */
	if (!path||(len = strlen(path)) < 4)
		return FALSE;
	
	sfx = &path[len - 4];

	if (sfx[0] != G_DIR_SEPARATOR) /* small optimization */
		return FALSE;
	else if (sfx[1] != 'c' && sfx[1] != 'n')
		return FALSE; /* optimization */
	else
		return (strcmp (sfx + 1, "cur") == 0 || 
			strcmp (sfx + 1, "new") == 0);
}

static MuResult
process_dir_entry (const char* path,struct dirent *entry,
		   MuPathWalkMsgCallback cb_msg, MuPathWalkDirCallback cb_dir, 
		   void *data)
{
	char* fullpath;
	
	/* ignore anything starting with a dot */
	if (G_UNLIKELY(entry->d_name[0] == '.')) 
			return MU_OK;

	fullpath = g_newa (char, strlen(path) + 1 + strlen(entry->d_name) + 1);
	sprintf (fullpath, "%s%c%s", path, G_DIR_SEPARATOR, entry->d_name);
	
	switch (entry->d_type) {
	case DT_REG:
		/* we only want files in cur/ and new/ */
		if (!is_maildir_new_or_cur (path)) 
			return MU_OK; 

		return process_file (fullpath, cb_msg, data);
		
	case DT_DIR:
		return process_dir (fullpath, cb_msg, cb_dir, data);
	
	default:
		return MU_OK; /* ignore other types */
	}
}

static struct dirent* 
dirent_copy (struct dirent *entry)
{
	struct dirent *d = g_slice_new (struct dirent);
	/* NOTE: simply memcpy'ing sizeof(struct dirent) bytes will
	 * give memory errors*/
	return (struct dirent*)memcpy (d, entry, entry->d_reclen);
}

static void
dirent_destroy (struct dirent *entry)
{
	g_slice_free(struct dirent, entry);
}

static gint
dirent_cmp (struct dirent *d1, struct dirent *d2)
{
	return d1->d_ino - d2->d_ino;
}

static MuResult
process_dir (const char* path, MuPathWalkMsgCallback msg_cb, 
	     MuPathWalkDirCallback dir_cb, void *data)
{
	MuResult result = MU_OK;
	GList *lst, *c;
	struct dirent *entry;
	DIR* dir;
	
	dir = opendir (path);		
	if (G_UNLIKELY(!dir)) {
		g_warning ("failed to open %s: %s", path, strerror(errno));
		return MU_ERROR;
	}
	
	if (dir_cb) {
		MuResult rv = dir_cb (path, TRUE, data);
		if (rv != MU_OK) {
			closedir (dir);
			return rv;
		}
	}

	/* we sort the inodes, which makes file-access much faster on 
	   some filesystems, such as ext3fs */
	lst = NULL;
	while ((entry = readdir (dir)))
		lst = g_list_prepend (lst, dirent_copy(entry));

	c = lst = g_list_sort (lst, (GCompareFunc)dirent_cmp);
	for (c = lst; c && result == MU_OK; c = c->next) 
		result = process_dir_entry (path, (struct dirent*)c->data, 
					    msg_cb, dir_cb, data);

	g_list_foreach (lst, (GFunc)dirent_destroy, NULL);
	g_list_free (lst);
	
	closedir (dir);

	if (dir_cb) 
		return dir_cb (path, FALSE, data);
	
	return result;
}

MuResult
mu_path_walk_maildir (const char *path, MuPathWalkMsgCallback cb_msg, 
		      MuPathWalkDirCallback cb_dir, void *data)
{
	struct stat statbuf;

	g_return_val_if_fail (path && cb_msg, MU_ERROR);
	
	if (access(path, R_OK) != 0) {
		g_warning ("cannot access %s: %s", path, strerror(errno));
		return MU_ERROR;
	}
	
	if (stat (path, &statbuf) != 0) {
		g_warning ("cannot stat %s: %s", path, strerror(errno));
		return MU_ERROR;
	}
	
	if ((statbuf.st_mode & S_IFMT) == S_IFREG)
		return process_file (path, cb_msg, data);
	
	if ((statbuf.st_mode & S_IFMT) == S_IFDIR) 
		return process_dir (path, cb_msg, cb_dir, data);

	g_warning ("%s: unsupported file type for %s", 
		   __FUNCTION__, path);
	return MU_ERROR;
}


