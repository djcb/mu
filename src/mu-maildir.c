/* 
** Copyright (C) 2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <dirent.h>

#include <string.h>
#include <errno.h>
#include <glib/gprintf.h>

#include "mu-maildir.h"

#define MU_MAILDIR_WALK_MAX_FILE_SIZE (15*1000*1000)
#define MU_MAILDIR_NOINDEX_FILE  ".noindex"


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
					       MU_MAILDIR_NOINDEX_FILE);
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


static MuResult process_dir (const char* path, MuMaildirWalkMsgCallback msg_cb, 
			     MuMaildirWalkDirCallback dir_cb, void *data);

static MuResult 
process_file (const char* fullpath, MuMaildirWalkMsgCallback cb, void *data)
{
	MuResult result;
	struct stat statbuf;
	
	if (!cb)
		return MU_OK;

	if (G_UNLIKELY(access(fullpath, R_OK) != 0)) {
		g_warning ("cannot access %s: %s", fullpath, strerror(errno));
		return MU_ERROR;
	}
	
	if (G_UNLIKELY(stat (fullpath, &statbuf) != 0)) {
		g_warning ("cannot stat %s: %s", fullpath, strerror(errno));
		return MU_ERROR;
	}
	
	if (G_UNLIKELY(statbuf.st_size > MU_MAILDIR_WALK_MAX_FILE_SIZE)) {
		g_warning ("ignoring because bigger than %d bytes: %s",
			   MU_MAILDIR_WALK_MAX_FILE_SIZE, fullpath);
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


/*
 * determine if path is a maildir leaf-dir; ie. if it's 'cur' or 'new'
 * (we're skipping 'tmp' for obvious reasons)
 */
G_GNUC_CONST static gboolean 
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

/* check if there is a noindex file (MU_WALK_NOINDEX_FILE) in this dir; */
static gboolean
has_noindex_file (const char *path)
{
	char *fname;

	fname = g_newa (char, strlen(path) + 1 +
			strlen(MU_MAILDIR_NOINDEX_FILE) + 1);
	g_sprintf (fname, "%s%c%s", path, G_DIR_SEPARATOR, MU_MAILDIR_NOINDEX_FILE);

	if (access (fname, F_OK) == 0)
		return TRUE;
	else if (errno != ENOENT)
		g_warning ("error testing for noindex file: %s",
			   strerror(errno));

	return FALSE;
}

static gboolean
_ignore_dir_entry (struct dirent *entry)
{
	const char *name;

	/* if it's not a dir and not a file, ignore it.
	 * note, this means also symlinks (DT_LNK) are ignored,
	 * maybe make this optional. Also note that entry->d_type is
	 * defined on Linux, BSDs is not part of POSIX; this needs a
	 * configure check */
	if (entry->d_type != DT_REG &&
	    entry->d_type != DT_DIR)
		return TRUE;

	name = entry->d_name;
	
	if (name[0] != '.')
		return FALSE;

	/* ignore . and .. */
	if (name[1] == '\0' ||
	    (name[1] == '.' && name[2] == '\0'))
		return TRUE;

	/* ignore .notmuch, .nnmaildir */
	if (name[1] == 'n')  {/* optimization */
		if (strcmp (name, ".notmuch") == 0 ||
		    strcmp (name, ".nnmaildir") == 0)
			return TRUE;
	}
	
	return FALSE;
}

static MuResult
process_dir_entry (const char* path,struct dirent *entry,
		   MuMaildirWalkMsgCallback cb_msg, MuMaildirWalkDirCallback cb_dir, 
		   void *data)
{
	char* fullpath;
	
	/* ignore special dirs: */
	if (_ignore_dir_entry (entry)) 
		return MU_OK;
	
	fullpath = g_newa (char, strlen(path) + 1 + strlen(entry->d_name) + 1);
	sprintf (fullpath, "%s%c%s", path, G_DIR_SEPARATOR, entry->d_name);
	
	switch (entry->d_type) {
	case DT_REG:
		/* we only want files in cur/ and new/ */
		if (!is_maildir_new_or_cur (path)) 
			return MU_OK; 

		return process_file (fullpath, cb_msg, data);
		
	case DT_DIR: {
		/* if it has a noindex file, we ignore this dir */
		if (has_noindex_file (fullpath)) {
			g_message ("ignoring dir %s", fullpath);
			return MU_OK;
		}
		
		return process_dir (fullpath, cb_msg, cb_dir, data);
	}

		
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
process_dir (const char* path, MuMaildirWalkMsgCallback msg_cb, 
	     MuMaildirWalkDirCallback dir_cb, void *data)
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
mu_maildir_walk (const char *path, MuMaildirWalkMsgCallback cb_msg, 
		 MuMaildirWalkDirCallback cb_dir, void *data)
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
	
	if (S_ISREG(statbuf.st_mode))
		return process_file (path, cb_msg, data);
	else if (S_ISDIR(statbuf.st_mode)) 
		return process_dir (path, cb_msg, cb_dir, data);
	else
		g_warning ("%s: unsupported file type for %s", 
			   __FUNCTION__, path);

	return MU_ERROR;
}



