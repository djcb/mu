/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/
/*
**
** Copyright (C) 2008-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#if HAVE_CONFIG_H
#include <config.h>
#endif /*HAVE_CONFIG_H*/

#include "mu-util.h"
#define _XOPEN_SOURCE 500

#ifdef HAVE_WORDEXP_H
#include <wordexp.h> /* for shell-style globbing */
#endif /*HAVE_WORDEXP_H*/

#include <stdlib.h>

#include <string.h>
#include <locale.h> /* for setlocale() */

#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <glib-object.h>
#include <glib/gstdio.h>
#include <errno.h>

#include <langinfo.h>


static char*
do_wordexp (const char *path)
{
#ifdef HAVE_WORDEXP_H
	wordexp_t wexp;
	char *dir;

	if (!path) {
		/* g_debug ("%s: path is empty", __func__); */
		return NULL;
	}

	if (wordexp (path, &wexp, 0) != 0) {
		/* g_debug ("%s: expansion failed for %s", __func__, path); */
		return NULL;
	}

	/* we just pick the first one */
	dir = g_strdup (wexp.we_wordv[0]);

	/* strangely, below seems to lead to a crash on MacOS (BSD);
	   so we have to allow for a tiny leak here on that
	   platform... maybe instead of __APPLE__ it should be
	   __BSD__?

	   Hmmm., cannot reproduce that crash anymore, so commenting
	   it out for now...
	   */
/* #ifndef __APPLE__ */
	wordfree (&wexp);
/* #endif /\*__APPLE__*\/ */
	return dir;

# else /*!HAVE_WORDEXP_H*/
/* E.g. OpenBSD does not have wordexp.h, so we ignore it */
	return path ? g_strdup (path) : NULL;
#endif /*HAVE_WORDEXP_H*/
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

	/* don't try realpath if the dir does not exist */
	if (access (dir, F_OK) != 0)
		return dir;

	/* now resolve any symlinks, .. etc. */
	if (realpath (dir, resolved) == NULL) {
		/* g_debug ("%s: could not get realpath for '%s': %s", */
		/* 	 __func__, dir, strerror(errno)); */
		g_free (dir);
		return NULL;
	} else
		g_free (dir);

	return g_strdup (resolved);
}


char*
mu_util_create_tmpdir (void)
{
	gchar *dirname;

        dirname =  g_strdup_printf ("%s%cmu-%d%c%x",
				    g_get_tmp_dir(),
				    G_DIR_SEPARATOR,
				    getuid(),
				    G_DIR_SEPARATOR,
				    (int)random()*getpid()*(int)time(NULL));

	if (!mu_util_create_dir_maybe (dirname, 0700, FALSE)) {
		g_free (dirname);
		return NULL;
	}

	return dirname;
}


GQuark
mu_util_error_quark (void)
{
	static GQuark error_domain = 0;

	if (G_UNLIKELY(error_domain == 0))
		error_domain = g_quark_from_static_string
			("mu-error-quark");

	return error_domain;
}


const char*
mu_util_cache_dir (void)
{
	static char cachedir [PATH_MAX];

	snprintf (cachedir, sizeof(cachedir), "%s%cmu-%u",
		  g_get_tmp_dir(), G_DIR_SEPARATOR,
		  getuid());

	return cachedir;
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
		/* g_debug ("Cannot access %s: %s", path, strerror (errno)); */
		return FALSE;
	}

	if (stat (path, &statbuf) != 0) {
		/* g_debug ("Cannot stat %s: %s", path, strerror (errno)); */
		return FALSE;
	}

	return S_ISDIR(statbuf.st_mode) ? TRUE: FALSE;
}


gchar*
mu_util_guess_maildir (void)
{
        const gchar *mdir1, *home;

        /* first, try MAILDIR */
        mdir1 = g_getenv ("MAILDIR");

        if (mdir1 && mu_util_check_dir (mdir1, TRUE, FALSE))
                return g_strdup (mdir1);

        /* then, try <home>/Maildir */
        home = g_get_home_dir();
        if (home) {
                char *mdir2;
                mdir2 = g_strdup_printf ("%s%cMaildir",
                        home, G_DIR_SEPARATOR);
                if (mu_util_check_dir (mdir2, TRUE, FALSE))
                        return mdir2;
                g_free (mdir2);
        }

        /* nope; nothing found */
        return NULL;
}


gchar*
mu_util_guess_mu_homedir (void)
{
        const char* home;

        /* g_get_home_dir use /etc/passwd, not $HOME; this is better,
         * as HOME may be wrong when using 'sudo' etc.*/
        home = g_get_home_dir ();

        if (!home) {
                MU_WRITE_LOG ("failed to determine homedir");
                return NULL;
        }

        return g_strdup_printf ("%s%c%s", home ? home : ".",
				G_DIR_SEPARATOR, ".mu");
}

gboolean
mu_util_create_dir_maybe (const gchar *path, mode_t mode, gboolean nowarn)
{
	struct stat statbuf;

	g_return_val_if_fail (path, FALSE);

	/* if it exists, it must be a readable dir */
	if (stat (path, &statbuf) == 0) {
		if ((!S_ISDIR(statbuf.st_mode)) ||
		    (access (path, W_OK|R_OK) != 0)) {
			if (!nowarn)
				g_warning ("not a read-writable"
					   "directory: %s", path);
			return FALSE;
		}
	}

	if (g_mkdir_with_parents (path, mode) != 0) {
		if (!nowarn)
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

		if (i > 0)
			g_string_append_c (str, ' ');

		g_string_append (str, params[i]);
	}

	return g_string_free (str, FALSE);
}


int
mu_util_create_writeable_fd (const char* path, mode_t mode,
			     gboolean overwrite)
{
	errno = 0; /* clear! */
	g_return_val_if_fail (path, -1);

	if (overwrite)
		return open (path, O_WRONLY|O_CREAT|O_TRUNC, mode);
	else
		return open (path, O_WRONLY|O_CREAT|O_EXCL, mode);
}


gboolean
mu_util_is_local_file (const char* path)
{
	/* if it starts with file:// it's a local file (for the
	 * purposes of this function -- if it's on a remote FS it's
	 * still considered local) */
	if (g_ascii_strncasecmp ("file://", path, strlen("file://")) == 0)
		return TRUE;

	if (access (path, R_OK) == 0)
		return TRUE;

	return FALSE;
}


gboolean
mu_util_supports (MuFeature feature)
{

	/* check for Guile support */
#ifndef BUILD_GUILE
	if (feature & MU_FEATURE_GUILE)
		return FALSE;
#endif /*BUILD_GUILE*/

	/* check for Gnuplot */
	if (feature & MU_FEATURE_GNUPLOT)
		if (!mu_util_program_in_path ("gnuplot"))
			return FALSE;

	return TRUE;
}


gboolean
mu_util_program_in_path (const char *prog)
{
	gchar *path;

	g_return_val_if_fail (prog, FALSE);

	path = g_find_program_in_path (prog);
	g_free (path);

	return (path != NULL) ? TRUE : FALSE;
}



gboolean
mu_util_play (const char *path, gboolean allow_local, gboolean allow_remote,
	      GError **err)
{
	gboolean rv;
	const gchar *argv[3];
	const char *prog;

	g_return_val_if_fail (path, FALSE);
	g_return_val_if_fail (mu_util_is_local_file (path) || allow_remote,
			      FALSE);
	g_return_val_if_fail (!mu_util_is_local_file (path) || allow_local,
			      FALSE);

	prog = g_getenv ("MU_PLAY_PROGRAM");
	if (!prog) {
#ifdef __APPLE__
		prog = "open";
#else
		prog = "xdg-open";
#endif /*!__APPLE__*/
	}

	if (!mu_util_program_in_path (prog)) {
		mu_util_g_set_error (err, MU_ERROR_FILE_CANNOT_EXECUTE,
				     "cannot find '%s' in path", prog);
		return FALSE;
	}

	argv[0] = prog;
	argv[1] = path;
	argv[2] = NULL;

	err = NULL;
	rv = g_spawn_async (NULL, (gchar**)&argv, NULL,
			    G_SPAWN_SEARCH_PATH, NULL, NULL, NULL,
			    err);
	return rv;
}


unsigned char
mu_util_get_dtype_with_lstat (const char *path)
{
	struct stat statbuf;

	g_return_val_if_fail (path, DT_UNKNOWN);

	if (lstat (path, &statbuf) != 0) {
		g_warning ("stat failed on %s: %s", path, strerror(errno));
		return DT_UNKNOWN;
	}

	/* we only care about dirs, regular files and links */
	if (S_ISREG (statbuf.st_mode))
		return DT_REG;
	else if (S_ISDIR (statbuf.st_mode))
		return DT_DIR;
	else if (S_ISLNK (statbuf.st_mode))
		return DT_LNK;

	return DT_UNKNOWN;
}


gboolean
mu_util_locale_is_utf8 (void)
{
	const gchar *dummy;
	static int is_utf8 = -1;

	if (G_UNLIKELY(is_utf8 == -1))
	    	is_utf8 = g_get_charset(&dummy) ? 1 : 0;

	return is_utf8 ? TRUE : FALSE;
}

gboolean
mu_util_fputs_encoded (const char *str, FILE *stream)
{
	int rv;
	unsigned	 bytes;
	char		*conv;

	g_return_val_if_fail (str, FALSE);
	g_return_val_if_fail (stream, FALSE);

	/* g_get_charset return TRUE when the locale is UTF8 */
	if (mu_util_locale_is_utf8())
		return fputs (str, stream) == EOF ? FALSE : TRUE;

	 /* charset is _not_ utf8, so we actually have to convert
	  * it
	  */
	conv = NULL;
	if (g_utf8_validate (str, -1, NULL))
		/* it _seems_ that on the bsds, the final err param
		 * may receive garbage... so we don't use it */
		conv = g_locale_from_utf8
			(str, -1, (gsize*)&bytes, NULL, NULL);

	/* conversion failed; this happens because is some cases GMime
	 * may gives us non-UTF-8 strings from e.g. wrongly encoded
	 * message-subjects; if so, we escape the string
	 */
	if (!conv)
		conv = g_strescape (str, "\n\t");

	rv  = conv ? fputs (conv, stream) : EOF;
	g_free (conv);

	return (rv == EOF) ? FALSE : TRUE;
}



gboolean
mu_util_g_set_error (GError **err, MuError errcode, const char *frm, ...)
{
	va_list ap;
	char *msg;

	/* don't bother with NULL errors, or errors already set */
	if (!err || *err)
		return FALSE;

	msg = NULL;
	va_start (ap, frm);
	g_vasprintf (&msg, frm, ap);
	va_end (ap);

	g_set_error (err, MU_ERROR_DOMAIN, errcode, "%s", msg);

	g_free (msg);

	return FALSE;
}


static gboolean
print_args (FILE *stream, const char *frm, va_list args)
{
	gchar *str;
	gboolean rv;

	str = g_strdup_vprintf (frm, args);

	rv = mu_util_fputs_encoded (str, stream);

	g_free (str);

	return rv;
}


gboolean
mu_util_print_encoded (const char *frm, ...)
{
	va_list args;
	gboolean rv;

	g_return_val_if_fail (frm, FALSE);

	va_start (args, frm);
	rv = print_args (stdout, frm, args);
	va_end (args);

	return rv;
}

gboolean
mu_util_printerr_encoded (const char *frm, ...)
{
	va_list args;
	gboolean rv;

	g_return_val_if_fail (frm, FALSE);

	va_start (args, frm);
	rv = print_args (stderr, frm, args);
	va_end (args);

	return rv;
}


char*
mu_util_read_password (const char *prompt)
{
	char *pass;

	g_return_val_if_fail (prompt, NULL);

	/* note: getpass is obsolete; replace with something better */

	pass = getpass (prompt); /* returns static mem, don't free */
	if (!pass) {
		if (errno)
			g_warning ("error: %s", strerror(errno));
		return NULL;
	}

	return g_strdup (pass);
}


const char*
mu_util_get_hash (const char* str)
{
	unsigned	djbhash, bkdrhash, bkdrseed;
	unsigned	u;
	static char	hex[18];

	g_return_val_if_fail (str, NULL);

	djbhash  = 5381;
	bkdrhash = 0;
	bkdrseed = 1313;

	for(u = 0; str[u]; ++u) {
		djbhash  = ((djbhash << 5) + djbhash) + str[u];
		bkdrhash = bkdrhash * bkdrseed + str[u];
	}

	snprintf (hex, sizeof(hex), "%08x%08x", djbhash, bkdrhash);

	return hex;
}
