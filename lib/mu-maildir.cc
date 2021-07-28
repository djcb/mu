/*
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
** along with this program; if not, write to 59the Free Software Foundation,
** Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
**
*/

#include "config.h"

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>

#include <string.h>
#include <errno.h>
#include <glib/gprintf.h>
#include <gio/gio.h>

#include "mu-maildir.hh"
#include "utils/mu-str.h"

using namespace Mu;

#define MU_MAILDIR_NOINDEX_FILE       ".noindex"
#define MU_MAILDIR_NOUPDATE_FILE      ".noupdate"

/* On Linux (and some BSD), we have entry->d_type, but some file
 * systems (XFS, ReiserFS) do not support it, and set it DT_UNKNOWN.
 * On other OSs, notably Solaris, entry->d_type is not present at all.
 * For these cases, we use lstat (in get_dtype) as a slower fallback,
 * and return it in the d_type parameter
 */
static unsigned char
get_dtype (struct dirent* dentry, const char *path, gboolean use_lstat)
{
#ifdef HAVE_STRUCT_DIRENT_D_TYPE

	if (dentry->d_type == DT_UNKNOWN)
		goto slowpath;
	if (dentry->d_type == DT_LNK && !use_lstat)
		goto slowpath;

	return dentry->d_type; /* fastpath */

slowpath:
#endif /*HAVE_STRUCT_DIRENT_D_TYPE*/
	return mu_util_get_dtype (path, use_lstat);
}

static gboolean
create_maildir (const char *path, mode_t mode, GError **err)
{
	int i;
	const char* subdirs[] = {"new", "cur", "tmp"};

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
				 fullpath, g_strerror (errno));
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
					    g_strerror (errno));
	return TRUE;
}

gboolean
Mu::mu_maildir_mkdir (const char* path, mode_t mode, gboolean noindex, GError **err)
{
	g_return_val_if_fail (path, FALSE);

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
	char *srcpath;

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

static char*
get_target_fullpath (const char* src, const char *targetpath, GError **err)
{
	char *targetfullpath, *srcfile;
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
Mu::mu_maildir_link (const char* src, const char *targetpath, GError **err)
{
	char *targetfullpath;
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
				     targetfullpath, src, g_strerror (errno));
	g_free (targetfullpath);

	return rv == 0 ? TRUE: FALSE;
}



/*
 * determine if path is a maildir leaf-dir; ie. if it's 'cur' or 'new'
 * (we're skipping 'tmp' for obvious reasons)
 */
gboolean
Mu::mu_maildir_is_leaf_dir (const char *path)
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
		d_type	 = get_dtype (dentry, fullpath, TRUE/*lstat*/);

		if (d_type == DT_LNK) {
			if (unlink (fullpath) != 0 ) {
				g_warning ("error unlinking %s: %s",
					   fullpath, g_strerror(errno));
				rv = FALSE;
			}
		} else if (d_type == DT_DIR) {
			DIR *subdir;
			subdir = opendir (fullpath);
			if (!subdir) {
				g_warning ("failed to open dir %s: %s",
					   fullpath, g_strerror(errno));
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
Mu::mu_maildir_clear_links (const char *path, GError **err)
{
	DIR		*dir;
	gboolean	 rv;

	g_return_val_if_fail (path, FALSE);

	dir = opendir (path);
	if (!dir) {
		g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_FILE_CANNOT_OPEN,
			     "failed to open %s: %s", path, g_strerror(errno));
		return FALSE;
	}

	rv = clear_links (path, dir);

	closedir (dir);

	return rv;
}

MuFlags
Mu::mu_maildir_get_flags_from_path (const char *path)
{
	g_return_val_if_fail (path, MU_FLAG_INVALID);

	/* try to find the info part */
	/* note that we can use either the ':', ';', or '!' as separator;
	 * the former is the official, but as it does not work on e.g. VFAT
	 * file systems, some Maildir implementations use the latter instead
	 * (or both). For example, Tinymail/modest does this. The python
	 * documentation at http://docs.python.org/lib/mailbox-maildir.html
	 * mentions the '!' as well as a 'popular choice'. Isync uses ';' by
	 * default on Windows.
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
		const char *info;

		info = strrchr (path, '2');
		if (!info || info == path ||
		    (info[-1] != ':' && info[-1] != '!' && info[-1] != ';') ||
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
static char*
get_new_path (const char *mdir, const char *mfile, MuFlags flags,
	      const char* custom_flags, char flags_sep)
{
	if (flags & MU_FLAG_NEW)
		return g_strdup_printf ("%s%cnew%c%s",
					mdir, G_DIR_SEPARATOR, G_DIR_SEPARATOR,
					mfile);
	else {
		const char *flagstr;
		flagstr = mu_flags_to_str_s (flags, MU_FLAG_TYPE_MAILFILE);

		return g_strdup_printf ("%s%ccur%c%s%c2,%s%s",
					mdir, G_DIR_SEPARATOR, G_DIR_SEPARATOR,
					mfile, flags_sep, flagstr,
					custom_flags ? custom_flags : "");
	}
}


char*
Mu::mu_maildir_get_maildir_from_path (const char* path)
{
	char *mdir;

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

static char*
find_path_separator(const char *path)
{
	const char *cur;
	for (cur = &path[strlen(path)-1]; cur > path; --cur) {
		if ((*cur == ':' || *cur == '!' || *cur == ';') &&
		    (cur[1] == '2' && cur[2] == ',')) {
			return (char*)cur;
		}
	}
	return NULL;
}

char*
Mu::mu_maildir_get_new_path (const char *oldpath, const char *new_mdir,
			 MuFlags newflags, gboolean new_name)
{
	char *mfile, *mdir, *custom_flags, *cur, *newpath, flags_sep = ':';

	g_return_val_if_fail (oldpath, NULL);

	mfile = newpath = custom_flags = NULL;

	/* determine the maildir */
	mdir = mu_maildir_get_maildir_from_path (oldpath);
	if (!mdir)
		return NULL;

        /* determine the name of the location of the flag separator */

	if (new_name) {
		mfile = get_new_basename ();
		cur = find_path_separator (oldpath);
		if (cur) {
			/* preserve the existing flags separator
			 * in the new file name */
			flags_sep = *cur;
		}
	} else {
		mfile = g_path_get_basename (oldpath);
		cur = find_path_separator (mfile);
		if (cur) {
			/* get the custom flags (if any) */
			custom_flags = mu_flags_custom_from_str (cur + 3);
			/* preserve the existing flags separator
			 * in the new file name */
			flags_sep = *cur;
			cur[0] = '\0'; /* strip the flags */
		}
	}

	newpath = get_new_path (new_mdir ? new_mdir : mdir,
				mfile, newflags, custom_flags, flags_sep);
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
		/* g_warning ("error: %s", g_strerror (errno)); */
		return -1;
	}

	return (gint64)statbuf.st_size;
}


static gboolean
msg_move_check_pre (const char *src, const char *dst, GError **err)
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
			(err, MU_ERROR_FILE, "can't find target (%s->%s)", src, dst);

	if (access (src, F_OK) == 0) {

                if (g_strcmp0(src, dst) == 0) {
                        g_warning ("moved %s to itself", src);
                        return TRUE;
                }

                /* this could happen if some other tool (for mail syncing) is
                 * interfering */
                g_debug ("the source is still there (%s->%s)",
                         src, dst);
        }

	return TRUE;
}

/* use GIO to move files; this is slower than rename() so only use
 * this when needed: when moving across filesystems */
static gboolean
msg_move_g_file (const char* src, const char *dst, GError **err)
{
	GFile		*srcfile, *dstfile;
	gboolean	 res;

	srcfile = g_file_new_for_path (src);
	dstfile = g_file_new_for_path (dst);

	res = g_file_move (srcfile, dstfile, G_FILE_COPY_NONE,
			   NULL, NULL, NULL, err);

	g_clear_object (&srcfile);
	g_clear_object (&dstfile);

	return res;
}

static gboolean
msg_move (const char* src, const char *dst, GError **err)
{
	if (!msg_move_check_pre (src, dst, err))
		return FALSE;

	if (rename (src, dst) == 0) /* seems it worked. */
		return msg_move_check_post (src, dst, err);

	if (errno != EXDEV) /* some unrecoverable error occurred */
		return mu_util_g_set_error
			(err, MU_ERROR_FILE, "error moving %s -> %s", src, dst);

	/* he EXDEV case -- source and target live on different filesystems */
	return msg_move_g_file (src, dst, err);
}

char*
Mu::mu_maildir_move_message (const char* oldpath, const char* targetmdir,
			 MuFlags newflags, gboolean ignore_dups,
			 gboolean new_name, GError **err)
{
	char		*newfullpath;
	gboolean	 rv;
	gboolean	 src_is_target;

	g_return_val_if_fail (oldpath, FALSE);

	/* first try *without* changing the name (as per new_name), since
	 * src_is_target shouldn't use a changed name */
	newfullpath = mu_maildir_get_new_path (oldpath, targetmdir,
					       newflags, FALSE);
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

	/* if we generated file is not the same (modulo flags), create a fully
	 * new name in the new_name case */
	if (!src_is_target && new_name) {
		g_free(newfullpath);
		newfullpath = mu_maildir_get_new_path (oldpath, targetmdir,
						       newflags, new_name);
		if (!newfullpath) {
			mu_util_g_set_error (err, MU_ERROR_FILE,
					     "failed to determine targetpath");
			return NULL;
		}
	}

	if (!src_is_target) {
                g_debug ("moving %s (%s, %x, %d) --> %s",
                         oldpath, targetmdir, newflags, new_name,
                         newfullpath);
		rv = msg_move (oldpath, newfullpath, err);
		if (!rv) {
			g_free (newfullpath);
			return NULL;
		}
	}

	return newfullpath;
}
