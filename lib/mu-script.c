/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

/*
** Copyright (C) 2012-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifdef BUILD_GUILE
#include <libguile.h>
#endif /*BUILD_GUILE*/

#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <dirent.h>
#include <errno.h>
#include <unistd.h>

#include "mu-str.h"
#include "mu-script.h"
#include "mu-util.h"

/**
 * Structure with information about a certain script.
 * the values will be *freed* when MuScriptInfo is freed
 */
struct _MuScriptInfo {
	char *_name;    /* filename-sans-extension */
	char *_path;    /* full path to script */
	char *_oneline; /* one-line description */
	char *_descr;   /* longer description */
};


/* create a new MuScriptInfo* object*/
static MuScriptInfo*
script_info_new (void)
{
	return g_slice_new0 (MuScriptInfo);
}

/* destroy a MuScriptInfo* object */
static void
script_info_destroy (MuScriptInfo *msi)
{
	if (!msi)
		return;

	g_free (msi->_name);
	g_free (msi->_path);
	g_free (msi->_oneline);
	g_free (msi->_descr);

	g_slice_free (MuScriptInfo, msi);
}

/* compare two MuScripInfo* objects (for sorting) */
static int
script_info_cmp (MuScriptInfo *msi1, MuScriptInfo *msi2)
{
	return strcmp (msi1->_name, msi2->_name);

}

const char*
mu_script_info_name (MuScriptInfo *msi)
{
	g_return_val_if_fail (msi, NULL);
	return msi->_name;
}

const char*
mu_script_info_path (MuScriptInfo *msi)
{
	g_return_val_if_fail (msi, NULL);
	return msi->_path;
}

const char*
mu_script_info_one_line (MuScriptInfo *msi)
{
	g_return_val_if_fail (msi, NULL);
	return msi->_oneline;
}

const char*
mu_script_info_description (MuScriptInfo *msi)
{
	g_return_val_if_fail (msi, NULL);
	return msi->_descr;
}


gboolean
mu_script_info_matches_regex (MuScriptInfo *msi, const char *rxstr,
			      GError **err)
{
	GRegex *rx;
	gboolean match;

	g_return_val_if_fail (msi, FALSE);
	g_return_val_if_fail (rxstr, FALSE);

	rx = g_regex_new (rxstr, G_REGEX_CASELESS|G_REGEX_OPTIMIZE, 0, err);
	if (!rx)
		return FALSE;

	match = FALSE;
	if (msi->_name)
		match = g_regex_match (rx, msi->_name, 0, NULL);
	if (!match && msi->_oneline)
		match = g_regex_match (rx, msi->_oneline, 0, NULL);

	return match;
}

void
mu_script_info_list_destroy (GSList *lst)
{
	g_slist_foreach (lst, (GFunc)script_info_destroy, NULL);
	g_slist_free    (lst);
}


static GIOChannel *
open_channel (const char *path)
{
	GError *err;
	GIOChannel *io_chan;

	err = NULL;

	io_chan = g_io_channel_new_file (path, "r", &err);
	if (!io_chan) {
		g_warning ("failed to open '%s': %s", path,
			   err ? err->message : "something went wrong");
		g_clear_error (&err);
		return NULL;
	}

	return io_chan;
}

static void
end_channel (GIOChannel *io_chan)
{
	GIOStatus status;
	GError *err;

	err    = NULL;
	status = g_io_channel_shutdown (io_chan, FALSE,
					&err);
	if (status != G_IO_STATUS_NORMAL) {
		g_warning ("failed to shutdown io-channel: %s",
			   err ? err->message : "something went wrong");
		g_clear_error (&err);
	}

	g_io_channel_unref (io_chan);
}



static gboolean
get_descriptions (MuScriptInfo *msi, const char *prefix)
{
	GIOStatus  io_status;
	GIOChannel *script_io;
	GError *err;

	char *line, *descr, *oneline;

	if (!prefix)
		return TRUE; /* not an error */

	if (!(script_io = open_channel (msi->_path)))
		return FALSE;

	err  = NULL;
	line = descr = oneline = NULL;

	do {
		g_free (line);
		io_status = g_io_channel_read_line (script_io, &line,
						    NULL, NULL, &err);
		if (io_status != G_IO_STATUS_NORMAL)
			break;

		if (!g_str_has_prefix (line, prefix))
			continue;

		if (!oneline)
			oneline = g_strdup (line + strlen (prefix));
		else {
			char *tmp;
			tmp = descr;
			descr = g_strdup_printf
				("%s%s", descr ? descr : "",
				 line + strlen(prefix));
				g_free (tmp);
		}

	} while (TRUE);

	if (io_status != G_IO_STATUS_EOF) {
		g_warning ("error reading %s: %s", msi->_path,
			   err ? err->message : "something went wrong");
		g_clear_error (&err);
	}

	end_channel (script_io);
	msi->_oneline = oneline;
	msi->_descr   = descr;

	return TRUE;
}



GSList*
mu_script_get_script_info_list (const char *path, const char *ext,
				const char *descprefix, GError **err)
{
	DIR *dir;
	GSList *lst;
	struct dirent *dentry;

	g_return_val_if_fail (path, NULL);

	dir = opendir (path);
	if (!dir) {
		mu_util_g_set_error (err, MU_ERROR_FILE_CANNOT_OPEN,
				     "failed to open '%s': %s",
				     path, strerror(errno));
		return NULL;
	}

	/* create a list of names, paths */
	lst = NULL;
	while ((dentry = readdir (dir))) {
		MuScriptInfo *msi;
		/* only consider files with certain extensions,
		 * if ext != NULL */
		if (ext && !g_str_has_suffix (dentry->d_name, ext))
			continue;
		msi = script_info_new ();
		msi->_name = g_strdup (dentry->d_name);
		if (ext) /* strip the extension */
			msi->_name[strlen(msi->_name) - strlen(ext)] = '\0';
		msi->_path = g_strdup_printf ("%s%c%s", path, G_DIR_SEPARATOR,
					      dentry->d_name);
		/* set the one-line and long description */
		get_descriptions (msi, descprefix);
		lst = g_slist_prepend (lst, msi);
	}

	closedir (dir); /* ignore error checking... */

	return g_slist_sort (lst, (GCompareFunc)script_info_cmp);
}


MuScriptInfo*
mu_script_find_script_with_name (GSList *lst, const char *name)
{
	GSList *cur;

	g_return_val_if_fail (name, NULL);

	for (cur = lst; cur; cur = g_slist_next (cur)) {

		MuScriptInfo *msi;
		msi = (MuScriptInfo*)cur->data;

		if (g_strcmp0 (name, mu_script_info_name (msi)) == 0)
			return msi;
	}

	return NULL;
}

#ifdef BUILD_GUILE
static void
guile_shell (void *closure, int argc, char **argv)
{
	scm_shell (argc, argv);
}

gboolean
mu_script_guile_run (MuScriptInfo *msi, const char *muhome,
		     const char **args, GError **err)
{
	char *mainargs, *expr;
	char *argv[] = {
		"guile", "-l", NULL, "-c", NULL, NULL
	};

	g_return_val_if_fail (msi, FALSE);
	g_return_val_if_fail (muhome, FALSE);

	if (access (mu_script_info_path (msi), R_OK) != 0) {
		mu_util_g_set_error (err, MU_ERROR_FILE_CANNOT_READ,
				     "failed to read script: %s",
				     strerror(errno));
		 return FALSE;
	 }
	argv[2] = (char*)mu_script_info_path (msi);

	mainargs = mu_str_quoted_from_strv (args);
	expr = g_strdup_printf (
	"(main '(\"%s\" \"--muhome=%s\" %s))",
		mu_script_info_name (msi),
		muhome,
		mainargs ? mainargs : "");

	g_free (mainargs);
	argv[4] = expr;

	scm_boot_guile (5, argv, guile_shell, NULL);

	/* never reached but let's be correct(TM)*/
	g_free (expr);
	return TRUE;
}
#else /*!BUILD_GUILE*/
gboolean
mu_script_guile_run (MuScriptInfo *msi, const char *muhome,
		     const char **args, GError **err)
{
	mu_util_g_set_error (err, MU_ERROR_INTERNAL,
			     "this mu does not have guile support");
	return FALSE;
}
#endif /*!BUILD_GUILE*/
