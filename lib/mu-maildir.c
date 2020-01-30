/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

/*
** Copyright (C) 2008-2016 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
** along with this program; if not, write to 59the Free Software Foundation,
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
#include <stdlib.h>

#include <string.h>
#include <errno.h>
#include <glib/gprintf.h>

#include "mu-maildir.h"
#include "utils/mu-str.h"

#define MU_MAILDIR_NOINDEX_FILE       ".noindex"
#define MU_MAILDIR_NOUPDATE_FILE      ".noupdate"


/* On Linux (and some BSD), we have entry->d_type, but some file
 * systems (XFS, ReiserFS) do not support it, and set it DT_UNKNOWN.
 * On other OSs, notably Solaris, entry->d_type is not present at all.
 * For these cases, we use lstat (in get_dtype) as a slower fallback,
 * and return it in the d_type parameter
 */
#ifdef HAVE_STRUCT_DIRENT_D_TYPE
#define GET_DTYPE(DE,FP)						   \
	((DE)->d_type == DT_UNKNOWN ? mu_util_get_dtype_with_lstat((FP)) : \
	 (DE)->d_type)
#else
#define GET_DTYPE(DE,FP)			                           \
	mu_util_get_dtype_with_lstat((FP))
#endif /*HAVE_STRUCT_DIRENT_D_TYPE*/


static gboolean
create_maildir (const char *path, mode_t mode, GError **err)
{
	int i;
	const gchar* subdirs[] = {"new", "cur", "tmp"};

	for (i = 0; i != G_N_ELEMENTS(subdirs); ++i) {

		const char *fullpath;
		int rv;

		/* static buffer */
		fullpath = mu_str_fullpath_s (path, subdirs[i]);

		/* if subdir already exists, don't try to re-create
		 * it */
		if (mu_util_check_dir (fullpath, TRUE, TRUE))
			continue;

		rv = g_mkdir_with_parents (fullpath, (int)mode);

		/* note, g_mkdir_with_parents won't detect an error if
		 * there's already such a dir, but with the wrong
		 * permissions; so we need to check */
		if (rv != 0 || !mu_util_check_dir(fullpath, TRUE, TRUE))
			return mu_util_g_set_error
				(err,MU_ERROR_FILE_CANNOT_MKDIR,
				 "creating dir failed for %s: %s",
				 fullpath, strerror (errno));
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

	/* note, if the 'close' failed, creation may still have
	 * succeeded...*/
	if (fd < 0 || close (fd) != 0)
		return mu_util_g_set_error (err, MU_ERROR_FILE_CANNOT_CREATE,
					    "error in create_noindex: %s",
					    strerror (errno));
	return TRUE;
}

gboolean
mu_maildir_mkdir (const char* path, mode_t mode, gboolean noindex, GError **err)
{
	g_return_val_if_fail (path, FALSE);

	MU_WRITE_LOG ("%s (%s, %o, %s)", __func__,
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
	gboolean rv;
	gchar *srcpath;

	srcpath = g_path_get_dirname (src);
	*in_cur = FALSE;
	rv = TRUE;

	if (g_str_has_suffix (srcpath, "cur"))
		*in_cur = TRUE;
	else if (!g_str_has_suffix (srcpath, "new"))
		rv = mu_util_g_set_error (err,
					  MU_ERROR_FILE_INVALID_SOURCE,
					  "invalid source message '%s'",
					  src);
	g_free (srcpath);
	return rv;
}

static gchar*
get_target_fullpath (const char* src, const gchar *targetpath, GError **err)
{
	gchar *targetfullpath, *srcfile;
	gboolean in_cur;

	if (!check_subdir (src, &in_cur, err))
		return NULL;

	srcfile = g_path_get_basename (src);

	/* create targetpath; note: make the filename *cough* unique
	 * by including a hash of the srcname in the targetname. This
	 * helps if there are copies of a message (which all have the
	 * same basename)
	 */
	targetfullpath = g_strdup_printf ("%s%c%s%c%u_%s",
					  targetpath,
					  G_DIR_SEPARATOR,
					  in_cur ? "cur" : "new",
					  G_DIR_SEPARATOR,
					  g_str_hash(src),
					  srcfile);
	g_free (srcfile);

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

	if (rv != 0)
		mu_util_g_set_error (err, MU_ERROR_FILE_CANNOT_LINK,
				     "error creating link %s => %s: %s",
				     targetfullpath, src, strerror (errno));
	g_free (targetfullpath);

	return rv == 0 ? TRUE: FALSE;
}


static MuError
process_dir (const char* path, const gchar *mdir,
	     MuMaildirWalkMsgCallback msg_cb,
	     MuMaildirWalkDirCallback dir_cb, gboolean full,
	     void *data);

static MuError
process_file (const char* fullpath, const gchar* mdir,
	      MuMaildirWalkMsgCallback msg_cb, void *data)
{
	MuError result;
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
		g_warning ("%s: error in callback (%s)",
			   __func__, fullpath);

	return result;
}


/*
 * determine if path is a maildir leaf-dir; ie. if it's 'cur' or 'new'
 * (we're skipping 'tmp' for obvious reasons)
 */
gboolean
mu_maildir_is_leaf_dir (const char *path)
{
	size_t len;

	/* path is the full path; it cannot possibly be shorter
	 * than 4 for a maildir (/cur or /new) */
	len = path ? strlen (path) : 0;
	if (G_UNLIKELY(len < 4))
		return FALSE;

	/* optimization; one further idea would be cast the 4 bytes to an
	 * integer and compare that -- need to think about alignment,
	 * endianness */

	if (path[len - 4] == G_DIR_SEPARATOR &&
	    path[len - 3] == 'c' &&
	    path[len - 2] == 'u' &&
	    path[len - 1] == 'r')
		return TRUE;

	if (path[len - 4] == G_DIR_SEPARATOR &&
	    path[len - 3] == 'n' &&
	    path[len - 2] == 'e' &&
	    path[len - 1] == 'w')
		return TRUE;

	return FALSE;
}


/* check if there path contains file; used for checking if there is
 * MU_MAILDIR_NOINDEX_FILE or MU_MAILDIR_NOUPDATE_FILE in this
 * dir; */
static gboolean
dir_contains_file (const char *path, const char *file)
{
	const char* fullpath;

	/* static buffer */
	fullpath = mu_str_fullpath_s (path, file);

	if (access (fullpath, F_OK) == 0)
		return TRUE;
	else if (G_UNLIKELY(errno != ENOENT && errno != EACCES))
		g_warning ("error testing for %s/%s: %s",
			   fullpath, file, strerror(errno));
	return FALSE;
}

static gboolean
is_dotdir_to_ignore (const char* dir)
{
	int i;
	const char* ignore[] = {
		".notmuch",
		".nnmaildir",
		".#evolution"
	}; /* when adding names, check the optimization below */

	if (dir[0] != '.')
		return FALSE; /* not a dotdir */

	if (dir[1] == '\0' || (dir[1] == '.' && dir[2] == '\0'))
		return TRUE; /* ignore '.' and '..' */

	/* optimization: special dirs have 'n' or '#' in pos 1 */
	if (dir[1] != 'n' && dir[1] != '#')
		return FALSE; /* not special: don't ignore */

	for (i = 0; i != G_N_ELEMENTS(ignore); ++i)
		if (strcmp(dir, ignore[i]) == 0)
			return TRUE;

	return FALSE; /* don't ignore */
}

static gboolean
ignore_dir_entry (struct dirent *entry, unsigned char d_type)
{
	if (G_LIKELY(d_type == DT_REG)) {

		guint u;

		/* ignore emacs tempfiles */
		if (entry->d_name[0] == '#')
			return TRUE;
		/* ignore dovecot metadata */
		if (entry->d_name[0] == 'd' &&
		    strncmp (entry->d_name, "dovecot", 7) == 0)
			return TRUE;
		/* ignore special files */
		if (entry->d_name[0] == '.')
			return TRUE;
		/* ignore core files */
		if (entry->d_name[0] == 'c' &&
		    strncmp (entry->d_name, "core", 4) == 0)
			return TRUE;
		/* ignore tmp/backup files; find the last char */
		for (u = 0; entry->d_name[u] != '\0'; ++u) {
			switch (entry->d_name[u]) {
			case '#':
			case '~':
				/* looks like a backup / tempsave file */
				if (entry->d_name[u + 1] == '\0')
					return TRUE;
				continue;
			default:
				continue;
			}
		}
		return FALSE; /* other files: don't ignore */

	} else if (d_type == DT_DIR)
		return is_dotdir_to_ignore (entry->d_name);
	else
		return TRUE; /* ignore non-normal files, non-dirs */
}

/*
 * return the maildir value for the the path - this is the directory
 * for the message (with the top-level dir as "/"), and without the
 * leaf "/cur" or "/new". In other words, contatenate old_mdir + "/" + dir,
 * unless dir is either 'new' or 'cur'. The value will be used in queries.
 */
static gchar*
get_mdir_for_path (const gchar *old_mdir, const gchar *dir)
{
	/* if the current dir is not 'new' or 'cur', contatenate
	 * old_mdir an dir */
	if ((dir[0] == 'n' && strcmp(dir, "new") == 0) ||
	    (dir[0] == 'c' && strcmp(dir, "cur") == 0) ||
	    (dir[0] == 't' && strcmp(dir, "tmp") == 0))
		return strdup (old_mdir ? old_mdir : G_DIR_SEPARATOR_S);
	else
		return g_strconcat (old_mdir ? old_mdir : "",
				    G_DIR_SEPARATOR_S, dir, NULL);

}


static MuError
process_dir_entry (const char* path, const char* mdir, struct dirent *entry,
		   MuMaildirWalkMsgCallback cb_msg,
		   MuMaildirWalkDirCallback cb_dir,
		   gboolean full, void *data)
{
	const char *fp;
	char* fullpath;
	unsigned char d_type;

	/* we have to copy the buffer from fullpath_s, because it
	 * returns a static buffer, and we maybe called reentrantly */
	fp = mu_str_fullpath_s (path, entry->d_name);
	fullpath = g_newa (char, strlen(fp) + 1);
	strcpy (fullpath, fp);

	d_type = GET_DTYPE(entry, fullpath);

	/* ignore special files/dirs */
	if (ignore_dir_entry (entry, d_type)) {
		/* g_debug ("ignoring %s\n", entry->d_name); */
		return MU_OK;
	}

	switch (d_type) {
	case DT_REG: /* we only want files in cur/ and new/ */
		if (!mu_maildir_is_leaf_dir (path))
			return MU_OK;

		return process_file (fullpath, mdir, cb_msg, data);

	case DT_DIR: {
		char *my_mdir;
		MuError rv;
		/* my_mdir is the search maildir (the dir starting
		 * with the top-level maildir as /, and without the
		 * /tmp, /cur, /new  */
		my_mdir = get_mdir_for_path (mdir, entry->d_name);
		rv = process_dir (fullpath, my_mdir, cb_msg, cb_dir, full, data);
		g_free (my_mdir);

		return rv;
	}

	default:
		return MU_OK; /* ignore other types */
	}
}


static const size_t DIRENT_ALLOC_SIZE =
	offsetof (struct dirent, d_name) + PATH_MAX;

static struct dirent*
dirent_new (void)
{
	return (struct dirent*) g_slice_alloc (DIRENT_ALLOC_SIZE);
}


static void
dirent_destroy (struct dirent *entry)
{
	g_slice_free1 (DIRENT_ALLOC_SIZE, entry);
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

static MuError
process_dir_entries (DIR *dir, const char* path, const char* mdir,
		     MuMaildirWalkMsgCallback msg_cb,
		     MuMaildirWalkDirCallback dir_cb,
		     gboolean full, void *data)
{
	MuError result;
	GSList *lst, *c;

	for (lst = NULL;;) {
		int rv;
		struct dirent *entry, *res;
		entry = dirent_new ();
		rv = readdir_r (dir, entry, &res);
		if (rv == 0) {
			if (res)
				lst = g_slist_prepend (lst, entry);
			else {
				dirent_destroy (entry);
				break; /* last direntry reached */
			}
		} else {
			dirent_destroy (entry);
			g_warning ("error scanning dir: %s", strerror(rv));
			return MU_ERROR_FILE;
		}
	}

	/* we sort by inode; this makes things much faster on
	 * extfs2,3 */
#if HAVE_STRUCT_DIRENT_D_INO
	c = lst = g_slist_sort (lst, (GCompareFunc)dirent_cmp);
#endif /*HAVE_STRUCT_DIRENT_D_INO*/

	for (c = lst, result = MU_OK; c && result == MU_OK; c = g_slist_next(c))
		result = process_dir_entry (path, mdir, (struct dirent*)c->data,
					    msg_cb, dir_cb, full, data);

	g_slist_foreach (lst, (GFunc)dirent_destroy, NULL);
	g_slist_free (lst);

	return result;
}


static MuError
process_dir (const char* path, const char* mdir,
	     MuMaildirWalkMsgCallback msg_cb, MuMaildirWalkDirCallback dir_cb,
	     gboolean full, void *data)
{
	MuError result;
	DIR*	dir;

	/* if it has a noindex file, we ignore this dir */
	if (dir_contains_file (path, MU_MAILDIR_NOINDEX_FILE) ||
	    (!full && dir_contains_file (path, MU_MAILDIR_NOUPDATE_FILE))) {
		g_debug ("found noindex/noupdate: ignoring dir %s", path);
		return MU_OK;
	}

	if (dir_cb) {
		MuError rv;
		rv = dir_cb (path, TRUE/*enter*/, data);
		/* ignore this dir; not necessarily an _error_, dir might
		 * be up-to-date and return MU_IGNORE */
		if (rv == MU_IGNORE)
			return MU_OK;
		else if (rv != MU_OK)
			return rv;
	}

	dir = opendir (path);
	if (!dir) {
		g_warning ("cannot access %s: %s", path, strerror(errno));
		return MU_OK;
	}

	result = process_dir_entries (dir, path, mdir, msg_cb, dir_cb,
				      full, data);
	closedir (dir);

	/* only run dir_cb if it exists and so far, things went ok */
	if (dir_cb && result == MU_OK)
		return dir_cb (path, FALSE/*leave*/, data);

	return result;
}


MuError
mu_maildir_walk (const char *path, MuMaildirWalkMsgCallback cb_msg,
		 MuMaildirWalkDirCallback cb_dir, gboolean full,
		 void *data)
{
	MuError rv;
	char *mypath;

	g_return_val_if_fail (path && cb_msg, MU_ERROR);
	g_return_val_if_fail (mu_util_check_dir(path, TRUE, FALSE), MU_ERROR);

	/* strip the final / or \ */
	mypath = g_strdup (path);
	if (mypath[strlen(mypath)-1] == G_DIR_SEPARATOR)
		mypath[strlen(mypath)-1] = '\0';

	rv = process_dir (mypath, NULL, cb_msg, cb_dir, full, data);
	g_free (mypath);

	return rv;
}


static gboolean
clear_links (const char *path, DIR *dir)
{
	gboolean	 rv;
	struct dirent	*dentry;

	rv    = TRUE;
	errno = 0;

	while ((dentry = readdir (dir))) {

		guint8	 d_type;
		char	*fullpath;

		if (dentry->d_name[0] == '.')
			continue; /* ignore .,.. other dotdirs */

		fullpath = g_build_path ("/", path, dentry->d_name, NULL);
		d_type	 = GET_DTYPE (dentry, fullpath);

		if (d_type == DT_LNK) {
			if (unlink (fullpath) != 0 ) {
				g_warning ("error unlinking %s: %s",
					   fullpath, strerror(errno));
				rv = FALSE;
			}
		} else if (d_type == DT_DIR) {
			DIR *subdir;
			subdir = opendir (fullpath);
			if (!subdir) {
				g_warning ("failed to open dir %s: %s",
					   fullpath, strerror(errno));
				rv = FALSE;
				goto next;
			}

			if (!clear_links (fullpath, subdir))
				rv = FALSE;

			closedir (subdir);
		}

	next:
		g_free (fullpath);
	}

	return rv;
}

gboolean
mu_maildir_clear_links (const char *path, GError **err)
{
	DIR		*dir;
	gboolean	 rv;

	g_return_val_if_fail (path, FALSE);

	dir = opendir (path);
	if (!dir) {
		g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_FILE_CANNOT_OPEN,
			     "failed to open %s: %s", path, strerror(errno));
		return FALSE;
	}

	rv = clear_links (path, dir);

	closedir (dir);

	return rv;
}




MuFlags
mu_maildir_get_flags_from_path (const char *path)
{
	g_return_val_if_fail (path, MU_FLAG_INVALID);

	/* try to find the info part */
	/* note that we can use either the ':' or '!' as separator;
	 * the former is the official, but as it does not work on e.g. VFAT
	 * file systems, some Maildir implementations use the latter instead
	 * (or both). For example, Tinymail/modest does this. The python
	 * documentation at http://docs.python.org/lib/mailbox-maildir.html
	 * mentions the '!' as well as a 'popular choice'
	 */

	/* we check the dir -- */
	if (strstr (path, G_DIR_SEPARATOR_S "new" G_DIR_SEPARATOR_S)) {

		char *dir, *dir2;
		MuFlags flags;

		dir  = g_path_get_dirname (path);
		dir2 = g_path_get_basename (dir);

		flags = MU_FLAG_NONE;

		if (g_strcmp0 (dir2, "new") == 0)
			flags = MU_FLAG_NEW;

		g_free (dir);
		g_free (dir2);

		/* NOTE: new/ message should not have :2,-stuff, as
		 * per http://cr.yp.to/proto/maildir.html. If they, do
		 * we ignore it
		 */
		if (flags == MU_FLAG_NEW)
			return flags;
	}

	/*  get the file flags */
	{
		char *info;

		info = strrchr (path, '2');
		if (!info || info == path ||
		    (info[-1] != ':' && info[-1] != '!') ||
		    (info[1] != ','))
			return MU_FLAG_NONE;
		else
			return mu_flags_from_str
				(&info[2], MU_FLAG_TYPE_MAILFILE,
				 TRUE /*ignore invalid */);
	}
}


/*
 * take an existing message path, and return a new path, based on
 * whether it should be in 'new' or 'cur'; ie.
 *
 * /home/user/Maildir/foo/bar/cur/abc:2,F  and flags == MU_FLAG_NEW
 *     => /home/user/Maildir/foo/bar/new
 * and
 * /home/user/Maildir/foo/bar/new/abc  and flags == MU_FLAG_REPLIED
 *    => /home/user/Maildir/foo/bar/cur
 *
 * so the difference is whether MU_FLAG_NEW is set or not; and in the
 * latter case, no other flags are allowed.
 *
 */
static gchar*
get_new_path (const char *mdir, const char *mfile, MuFlags flags,
	      const char* custom_flags)
{
	if (flags & MU_FLAG_NEW)
		return g_strdup_printf ("%s%cnew%c%s",
					mdir, G_DIR_SEPARATOR, G_DIR_SEPARATOR,
					mfile);
	else {
		const char *flagstr;
		flagstr = mu_flags_to_str_s (flags, MU_FLAG_TYPE_MAILFILE);

		return g_strdup_printf ("%s%ccur%c%s:2,%s%s",
					mdir, G_DIR_SEPARATOR, G_DIR_SEPARATOR,
					mfile, flagstr,
					custom_flags ? custom_flags : "");
	}
}


char*
mu_maildir_get_maildir_from_path (const char* path)
{
	gchar *mdir;

	/* determine the maildir */
	mdir = g_path_get_dirname (path);
	if (!g_str_has_suffix (mdir, "cur") &&
	    !g_str_has_suffix (mdir, "new")) {
		g_warning ("%s: not a valid maildir path: %s",
			   __func__, path);
		g_free (mdir);
		return NULL;
	}

	/* remove the 'cur' or 'new' */
	mdir[strlen(mdir) - 4] = '\0';

	return mdir;
}


static char*
get_new_basename (void)
{
	return g_strdup_printf ("%u.%08x%08x.%s",
				(guint)time(NULL),
				g_random_int(),
				(gint32)g_get_monotonic_time (),
				g_get_host_name ());
}


char*
mu_maildir_get_new_path (const char *oldpath, const char *new_mdir,
			 MuFlags newflags, gboolean new_name)
{
	char *mfile, *mdir, *custom_flags, *newpath;

	g_return_val_if_fail (oldpath, NULL);

	mfile = newpath = custom_flags = NULL;

	/* determine the maildir */
	mdir = mu_maildir_get_maildir_from_path (oldpath);
	if (!mdir)
		return NULL;

	if (new_name)
		mfile = get_new_basename ();
	else {
		/* determine the name of the mailfile, stripped of its flags, as
		 * well as any custom (non-standard) flags */
		char *cur;
		mfile = g_path_get_basename (oldpath);
		for (cur = &mfile[strlen(mfile)-1]; cur > mfile; --cur) {
			if ((*cur == ':' || *cur == '!') &&
			    (cur[1] == '2' && cur[2] == ',')) {
				/* get the custom flags (if any) */
				custom_flags =
					mu_flags_custom_from_str (cur + 3);
				cur[0] = '\0'; /* strip the flags */
				break;
			}
		}
	}

	newpath = get_new_path (new_mdir ? new_mdir : mdir,
				mfile, newflags, custom_flags);
	g_free (mfile);
	g_free (mdir);
	g_free (custom_flags);

	return newpath;
}


static gint64
get_file_size (const char* path)
{
	int		rv;
	struct stat	statbuf;

	rv = stat (path, &statbuf);
	if (rv != 0) {
		/* g_warning ("error: %s", strerror (errno)); */
		return -1;
	}

	return (gint64)statbuf.st_size;
}


static gboolean
msg_move_check_pre (const gchar *src, const gchar *dst, GError **err)
{
	gint size1, size2;

	if (!g_path_is_absolute(src))
		return mu_util_g_set_error
			(err, MU_ERROR_FILE,
			 "source is not an absolute path: '%s'", src);

	if (!g_path_is_absolute(dst))
		return mu_util_g_set_error
			(err, MU_ERROR_FILE,
			 "target is not an absolute path: '%s'", dst);

	if (access (src, R_OK) != 0)
		return mu_util_g_set_error (err, MU_ERROR_FILE,
					    "cannot read %s",  src);

	if (access (dst, F_OK) != 0)
		return TRUE;

	/* target exist; we simply overwrite it, unless target has a different
	 * size. ignore the exceedingly rare case where have duplicate message
	 * file names with different content yet the same length. (md5 etc. is a
	 * bit slow) */
	size1 = get_file_size (src);
	size2 = get_file_size (dst);
	if (size1 != size2)
		return mu_util_g_set_error (err, MU_ERROR_FILE,
					    "%s already exists", dst);

	return TRUE;
}

static gboolean
msg_move_check_post (const char *src, const char *dst, GError **err)
{
	/* double check -- is the target really there? */
	if (access (dst, F_OK) != 0)
		return mu_util_g_set_error
			(err, MU_ERROR_FILE, "can't find target (%s)",  dst);

	if (access (src, F_OK) == 0)
		return mu_util_g_set_error
			(err, MU_ERROR_FILE, "source still there (%s)", src);

	return TRUE;
}


static gboolean
msg_move (const char* src, const char *dst, GError **err)
{
	if (!msg_move_check_pre (src, dst, err))
		return FALSE;

	if (rename (src, dst) != 0)
		return mu_util_g_set_error
			(err, MU_ERROR_FILE,"error moving %s to %s", src, dst);

	return msg_move_check_post (src, dst, err);
}

gchar*
mu_maildir_move_message (const char* oldpath, const char* targetmdir,
			 MuFlags newflags, gboolean ignore_dups,
			 gboolean new_name, GError **err)
{
	char *newfullpath;
	gboolean rv;
	gboolean src_is_target;

	g_return_val_if_fail (oldpath, FALSE);

	newfullpath = mu_maildir_get_new_path (oldpath, targetmdir,
					       newflags, new_name);
	if (!newfullpath) {
		mu_util_g_set_error (err, MU_ERROR_FILE,
				     "failed to determine targetpath");
		return NULL;
	}

	src_is_target = (g_strcmp0 (oldpath, newfullpath) == 0);

	if (!ignore_dups && src_is_target) {
		mu_util_g_set_error (err, MU_ERROR_FILE_TARGET_EQUALS_SOURCE,
				     "target equals source");
		return NULL;
	}

	if (!src_is_target) {
		rv = msg_move (oldpath, newfullpath, err);
		if (!rv) {
			g_free (newfullpath);
			return NULL;
		}
	}

	return newfullpath;
}
