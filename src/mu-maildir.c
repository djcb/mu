/* 
** Copyright (C) 2008-2011 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#if HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <string.h>
#include <errno.h>
#include <glib/gprintf.h>

#include "mu-util.h"
#include "mu-maildir.h"
#include "mu-str.h"

#define MU_MAILDIR_NOINDEX_FILE       ".noindex"

/* On Linux (and some BSD), we have entry->d_type, but some file
 * systems (XFS, ReiserFS) do not support it, and set it DT_UNKNOWN.
 * On other OSs, notably Solaris, entry->d_type is not present at all.
 * For these cases, we use lstat (in get_dtype) as a slower fallback,
 * and return it in the d_type parameter
 */
#ifdef HAVE_STRUCT_DIRENT_D_TYPE
#define GET_DTYPE(DE,FP) \
	((DE)->d_type == DT_UNKNOWN ? mu_util_get_dtype_with_lstat((FP)) : (DE)->d_type)
#else
#define GET_DTYPE(DE,FP)			\
	mu_util_get_dtype_with_lstat((FP))
#endif /*HAVE_STRUCT_DIRENT_D_TYPE*/


static gboolean
create_maildir (const char *path, mode_t mode, GError **err)
{
	int i;
	const gchar* subdirs[] = {"new", "cur", "tmp"};

	/* make sure it does not exist yet */
	if (access (path, F_OK) == 0)
		errno = EEXIST;

	if (errno != ENOENT) {
		g_set_error (err, 0, MU_ERROR_FILE, "%s", strerror (errno));
		return FALSE;
	}
	
	for (i = 0; i != G_N_ELEMENTS(subdirs); ++i) {
		const char *fullpath;
		int rv;

		/* static buffer */
		fullpath = mu_str_fullpath_s (path, subdirs[i]);
		rv = g_mkdir_with_parents (fullpath, (int)mode);
		if (rv != 0) {
			g_set_error (err, 0, MU_ERROR_FILE_CANNOT_MKDIR,
				     "g_mkdir_with_parents failed: %s",
				     strerror (errno));
			return FALSE;
		}
	}
	
	return TRUE;
}

static gboolean
create_noindex (const char *path, GError **err)
{	
	/* create a noindex file if requested */
	int fd;
	const char *noindexpath;

	/* static buffer */
	noindexpath = mu_str_fullpath_s (path, MU_MAILDIR_NOINDEX_FILE);

	fd = creat (noindexpath, 0644);

	if (fd < 0 || close (fd) != 0) {
		g_set_error (err, 0, MU_ERROR_FILE_CANNOT_CREATE,
			     "error in create_noindex: %s",
			     strerror (errno));
		return FALSE;
	}
	
	return TRUE;
}

gboolean
mu_maildir_mkdir (const char* path, mode_t mode, gboolean noindex, GError **err)
{	
	g_return_val_if_fail (path, FALSE);
	
	MU_WRITE_LOG ("%s (%s, %o, %s)", __FUNCTION__,
		      path, mode, noindex ? "TRUE" : "FALSE");
	
	if (!create_maildir (path, mode, err))
		return FALSE;

	if (noindex && !create_noindex (path, err))
		return FALSE;
	
	return TRUE;
}

/* determine whether the source message is in 'new' or in 'cur';
 * we ignore messages in 'tmp' for obvious reasons */
static gboolean
check_subdir (const char *src, gboolean *in_cur, GError **err)
{
	gchar *srcpath;

	srcpath = g_path_get_dirname (src);

	if (g_str_has_suffix (srcpath, "new"))
		*in_cur = FALSE;
	else if (g_str_has_suffix (srcpath, "cur"))
		*in_cur = TRUE;
	else {
		g_set_error(err, 0, MU_ERROR_FILE_INVALID_SOURCE,
			    "invalid source message '%s'", src);
		return FALSE;
	}
	g_free (srcpath);
	
	return TRUE;
}

static gchar*
get_target_fullpath (const char* src, const gchar *targetpath, GError **err)
{
	gchar *targetfullpath, *srcfile, *srcpath, *c;
	gboolean in_cur;
	
	if (!check_subdir (src, &in_cur, err))
		return NULL;
	
	/* note: make the filename *cough* unique by making the pathname
	 * part of the file name. This helps if there are copies of a
	 * message (which all have the same basename)*/
	c = srcpath = g_path_get_dirname (src);
	while (c && *c) {
		if (*c == G_DIR_SEPARATOR)
			*c = '_';
		++c;
	}
	srcfile = g_path_get_basename (src);

	/* create & test targetpath  */
	targetfullpath = g_strdup_printf ("%s%c%s%c%s_%s",
					  targetpath,
					  G_DIR_SEPARATOR,
					  in_cur ? "cur" : "new",
					  G_DIR_SEPARATOR,
					  srcpath,
					  srcfile);
	g_free (srcfile);
	g_free (srcpath);
	
	return targetfullpath;
}


gboolean
mu_maildir_link (const char* src, const char *targetpath, GError **err)
{
	gchar *targetfullpath;
	int rv;
	
	g_return_val_if_fail (src, FALSE);
	g_return_val_if_fail (targetpath, FALSE);
	
	targetfullpath = get_target_fullpath (src, targetpath, err);
	if (!targetfullpath)
		return FALSE;
	
	rv = symlink (src, targetfullpath);
	
	if (rv != 0) {
		g_set_error (err, 0, MU_ERROR_FILE_CANNOT_LINK,
			     "error creating link %s => %s: %s",
			     targetfullpath, src, strerror (errno));
		g_free (targetfullpath);
		return FALSE;
	}

	g_free (targetfullpath);
	return TRUE;
}


static MuResult process_dir (const char* path, const gchar *mdir,
			     MuMaildirWalkMsgCallback msg_cb, 
			     MuMaildirWalkDirCallback dir_cb, void *data);

static MuResult 
process_file (const char* fullpath, const gchar* mdir,
	      MuMaildirWalkMsgCallback msg_cb, void *data)
{
	MuResult result;
	struct stat statbuf;
	
	if (!msg_cb)
		return MU_OK;

	if (G_UNLIKELY(access(fullpath, R_OK) != 0)) {
		g_warning ("cannot access %s: %s", fullpath,
			   strerror(errno));
		return MU_ERROR;
	}
	
	if (G_UNLIKELY(stat (fullpath, &statbuf) != 0)) {
		g_warning ("cannot stat %s: %s", fullpath, strerror(errno));
		return MU_ERROR;
	}
	

	result = (msg_cb)(fullpath, mdir, &statbuf, data);
	if (result == MU_STOP) 
		g_debug ("callback said 'MU_STOP' for %s", fullpath);
	else if (result == MU_ERROR)
		g_warning ("%s: failed %d in callback (%s)",  
			   __FUNCTION__, result, fullpath);

	return result;
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
	 * than 4 for a maildir (/cur or /new) */
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
	const char* noindexpath;

	/* static buffer */
	noindexpath = mu_str_fullpath_s (path, MU_MAILDIR_NOINDEX_FILE);

	if (access (noindexpath, F_OK) == 0)
		return TRUE;
	
	else if (G_UNLIKELY(errno != ENOENT))
		g_warning ("error testing for noindex file %s: %s",
			   noindexpath, strerror(errno));

	return FALSE;
}


static gboolean
ignore_dir_entry (struct dirent *entry, unsigned char d_type)
{
	const char *name;
	
	/* if it's not a dir and not a file, ignore it.
	 * note, this means also symlinks (DT_LNK) are ignored,
	 * maybe make this optional */
	if (d_type != DT_REG && d_type != DT_DIR)
		return TRUE;

	name = entry->d_name;
	
	if (name[0] != '.')
		return FALSE;

	/* ignore . and .. */
	if (name[1] == '\0' ||
	    (name[1] == '.' && name[2] == '\0'))
		return TRUE;

	/* ignore .notmuch, .nnmaildir */
	if ((name[1] == 'n') && /* optimization */
	    (strcmp (name, ".notmuch") == 0 ||
	     strcmp (name, ".nnmaildir") == 0)) 
		return TRUE;
	
	return FALSE;
}


static gchar*
get_mdir_for_path (const gchar *old_mdir, const gchar *dir)
{
	if (dir[0] != 'n' && dir[0] != 'c' &&
	    strcmp(dir, "cur") != 0 && strcmp(dir, "new") != 0)
		return g_strconcat (old_mdir ? old_mdir : "",
				    G_DIR_SEPARATOR_S, dir, NULL);
	else
		return strdup (old_mdir ? old_mdir : G_DIR_SEPARATOR_S);
}


static MuResult
process_dir_entry (const char* path, const char* mdir, struct dirent *entry,
		   MuMaildirWalkMsgCallback cb_msg,
		   MuMaildirWalkDirCallback cb_dir, 
		   void *data)
{
	const char *fp;
	char* fullpath;
	unsigned char d_type;
	
	/* we have to copy the buffer from fullpath_s, because it
	 * returns a static buffer */
	fp = mu_str_fullpath_s (path, entry->d_name);
	fullpath = g_newa (char, strlen(fp) + 1);
	strcpy (fullpath, fp);

	d_type = GET_DTYPE(entry, fullpath);
	
	/* ignore special files/dirs */
	if (ignore_dir_entry (entry, d_type)) 
		return MU_OK;
	
	switch (d_type) {
	case DT_REG: /* we only want files in cur/ and new/ */
		if (!is_maildir_new_or_cur (path)) 
			return MU_OK;
		
		return process_file (fullpath, mdir, cb_msg, data);
		
	case DT_DIR: {
		char *my_mdir;
		MuResult rv;

		my_mdir = get_mdir_for_path (mdir, entry->d_name);
		rv = process_dir (fullpath, my_mdir, cb_msg, cb_dir, data);
		g_free (my_mdir);

		return rv;
	}
		
	default:
		return MU_OK; /* ignore other types */
	}
}


static struct dirent* 
dirent_copy (struct dirent *entry)
{
	struct dirent *d;

	d = g_slice_new (struct dirent);

	/* NOTE: simply memcpy'ing sizeof(struct dirent) bytes will
	 * give memory errors. */
	return (struct dirent*) memcpy (d, entry, entry->d_reclen);
}

static void
dirent_destroy (struct dirent *entry)
{
	g_slice_free (struct dirent, entry);
}

#ifdef HAVE_STRUCT_DIRENT_D_INO	
static int
dirent_cmp (struct dirent *d1, struct dirent *d2)
{
	/* we do it his way instead of a simple d1->d_ino - d2->d_ino
	 * because this way, we don't need 64-bit numbers for the
	 * actual sorting */
	if (d1->d_ino < d2->d_ino)
		return -1;
	else if (d1->d_ino > d2->d_ino)
		return 1;
	else
		return 0;
}
#endif /*HAVE_STRUCT_DIRENT_D_INO*/

static MuResult
process_dir_entries (DIR *dir, const char* path, const char* mdir,
		     MuMaildirWalkMsgCallback msg_cb,
		     MuMaildirWalkDirCallback dir_cb, void *data)
{
	MuResult result;
	GList *lst, *c;
	struct dirent *entry;
	
	lst = NULL;
	while ((entry = readdir (dir)))
		lst = g_list_prepend (lst, dirent_copy(entry));
	
	/* we sort by inode; this makes things much faster on
	 * extfs2,3 */
#if HAVE_STRUCT_DIRENT_D_INO		
	c = lst = g_list_sort (lst, (GCompareFunc)dirent_cmp);
#endif /*HAVE_STRUCT_DIRENT_D_INO*/	

	for (c = lst, result = MU_OK; c && result == MU_OK; c = c->next) {
		result = process_dir_entry (path, mdir,
					    (struct dirent*)c->data, 
					    msg_cb, dir_cb, data);
		/* hmmm, break on MU_ERROR as well? */
		if (result == MU_STOP)
			break;
	}

	g_list_foreach (lst, (GFunc)dirent_destroy, NULL);
	g_list_free (lst);
	
	return result;
}


static MuResult
process_dir (const char* path, const char* mdir,
	     MuMaildirWalkMsgCallback msg_cb, 
	     MuMaildirWalkDirCallback dir_cb, void *data)
{
	MuResult result;
	DIR* dir;
	
	/* if it has a noindex file, we ignore this dir */
	if (has_noindex_file (path)) {
		g_debug ("found .noindex: ignoring dir %s", path);
		return MU_OK;
	}

	dir = opendir (path);		
	if (G_UNLIKELY(!dir)) {
		g_warning ("%s: ignoring  %s: %s",  __FUNCTION__,
			   path, strerror(errno));
		return MU_OK;
	}
	
	if (dir_cb) {
		MuResult rv;
		rv = dir_cb (path, TRUE, data);
		if (rv != MU_OK) {
			closedir (dir);
			return rv;
		}
	}
	
	result = process_dir_entries (dir, path, mdir, msg_cb, dir_cb, data);
	closedir (dir);

	/* only run dir_cb if it exists and so far, things went ok */
	if (dir_cb && result == MU_OK)
		return dir_cb (path, FALSE, data);
	
	return result;
}


MuResult
mu_maildir_walk (const char *path, MuMaildirWalkMsgCallback cb_msg, 
		 MuMaildirWalkDirCallback cb_dir, void *data)
{
	MuResult rv;
	char *mypath;
	
	g_return_val_if_fail (path && cb_msg, MU_ERROR);
	g_return_val_if_fail (mu_util_check_dir(path, TRUE, FALSE), MU_ERROR);	
	
	/* strip the final / or \ */
	mypath = g_strdup (path);
	if (mypath[strlen(mypath)-1] == G_DIR_SEPARATOR)
		mypath[strlen(mypath)-1] = '\0';
	
	rv = process_dir (mypath, NULL, cb_msg, cb_dir, data);
	g_free (mypath);
	
	return rv;
}


static gboolean
clear_links (const gchar* dirname, DIR *dir, GError **err)
{
	struct dirent *entry;
	gboolean rv;
	
	rv = TRUE;
	errno = 0;
	while ((entry = readdir (dir))) {
		
		const char *fp;
		char *fullpath;
		unsigned char d_type;
		
		/* ignore empty, dot thingies */
		if (!entry->d_name || entry->d_name[0] == '.')
			continue;
		
		/* we have to copy the buffer from fullpath_s, because
		 * it returns a static buffer and we are
		 * recursive*/
		fp = mu_str_fullpath_s (dirname, entry->d_name);
		fullpath = g_newa (char, strlen(fp) + 1);
		strcpy (fullpath, fp);
		
		d_type = GET_DTYPE (entry, fullpath);
		
		/* ignore non-links / non-dirs */
		if (d_type != DT_LNK && d_type != DT_DIR)
			continue; 
				
		if (d_type == DT_LNK) {
			if (unlink (fullpath) != 0) {
				/* don't use err */
				g_warning  ("error unlinking %s: %s",
					    fullpath, strerror(errno));
				rv = FALSE;
			}
		} else /* DT_DIR, see check before*/
			rv = mu_maildir_clear_links (fullpath, err);
	}

	if (errno != 0)
		g_set_error (err, 0, MU_ERROR_FILE,"file error: %s", strerror(errno));
	
	return (rv == FALSE && errno == 0);
}


gboolean
mu_maildir_clear_links (const gchar* path, GError **err)
{
	DIR *dir;
	gboolean rv;
	
	g_return_val_if_fail (path, FALSE);
	
	dir = opendir (path);		
	if (!dir) {
		g_set_error (err, 0, MU_ERROR_FILE_CANNOT_OPEN,
			     "failed to open %s: %s", path,
			     strerror(errno));
		return FALSE;
	}

	rv = clear_links (path, dir, err);
	closedir (dir);

	return rv;
}
