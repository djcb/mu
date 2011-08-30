/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/
/*
** Copyright (C) 2010-2011 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

#include "mu-msg.h"
#include "mu-msg-part.h"
#include "mu-cmd.h"
#include "mu-util.h"
#include "mu-str.h"
#include "mu-date.h"
#include "mu-maildir.h"
#include "mu-contacts.h"
#include "mu-runtime.h"
#include "mu-flags.h"
#include "mu-store.h"

#define VIEW_TERMINATOR '\f' /* form-feed */


static gboolean
view_msg_sexp (MuMsg *msg)
{
	char *sexp;

	sexp = mu_msg_to_sexp (msg, NULL, FALSE);
	fputs (sexp, stdout);
	g_free (sexp);

	return TRUE;
}


static void
each_part (MuMsg *msg, MuMsgPart *part, gchar **attach)
{
	if (mu_msg_part_looks_like_attachment (part, TRUE) &&
	    (part->file_name)) {

		char *tmp = *attach;

		*attach = g_strdup_printf ("%s%s'%s'",
					   *attach ? *attach : "",
					   *attach ? ", " : "",
					   part->file_name);
		g_free (tmp);
	}
}

/* return comma-sep'd list of attachments */
static gchar *
get_attach_str (MuMsg *msg)
{
	gchar *attach;

	attach = NULL;
	mu_msg_part_foreach (msg, (MuMsgPartForeachFunc)each_part, &attach);

	return attach;
}

#define color_maybe(C)	do{ if (color) fputs ((C),stdout);}while(0)

static void
print_field (const char* field, const char *val, gboolean color)
{
	if (!val)
		return;

	color_maybe (MU_COLOR_MAGENTA);
	mu_util_fputs_encoded (field, stdout);
	color_maybe (MU_COLOR_DEFAULT);
	fputs (": ", stdout);

	if (val) {
		color_maybe (MU_COLOR_GREEN);
		mu_util_fputs_encoded (val, stdout);
	}

	color_maybe (MU_COLOR_DEFAULT);
	fputs ("\n", stdout);
}


static void
body_or_summary (MuMsg *msg, gboolean summary, gboolean color)
{
	const char* field;
	const int SUMMARY_LEN = 5;

	field = mu_msg_get_body_text (msg);
	if (!field)
		return; /* no body -- nothing more to do */

	if (summary) {
		gchar *summ;
		summ = mu_str_summarize (field, SUMMARY_LEN);
		print_field ("Summary", summ, color);
		g_free (summ);
	} else {
		color_maybe (MU_COLOR_YELLOW);
		mu_util_print_encoded ("\n%s\n", field);
		color_maybe (MU_COLOR_DEFAULT);
	}
}


/* we ignore fields for now */
static gboolean
view_msg_plain (MuMsg *msg, const gchar *fields, gboolean summary,
	  gboolean color)
{
	gchar *attachs;
	time_t date;
	const GSList *lst;

	print_field ("From", mu_msg_get_from (msg), color);
	print_field ("To",   mu_msg_get_to (msg), color);
	print_field ("Cc",   mu_msg_get_cc (msg), color);
	print_field ("Bcc",  mu_msg_get_bcc (msg), color);
	print_field ("Subject",  mu_msg_get_subject (msg), color);

	if ((date = mu_msg_get_date (msg)))
		print_field ("Date", mu_date_str_s ("%c", date),
			     color);

	if ((lst = mu_msg_get_tags (msg))) {
		gchar *tags;
		tags = mu_str_from_list (lst,',');
		print_field ("Tags", tags, color);
		g_free (tags);
	}

	if ((attachs = get_attach_str (msg))) {
		print_field ("Attachments", attachs, color);
		g_free (attachs);
	}

	body_or_summary (msg, summary, color);

	return TRUE;
}


static gboolean
handle_msg (const char *fname, MuConfig *opts, MuError *code)
{
	GError *err;
	MuMsg *msg;
	gboolean rv;

	err = NULL;
	msg = mu_msg_new_from_file (fname, NULL, &err);

	if (!msg) {
		if (err && err->message) {
			g_warning ("%s", err->message);
			g_error_free (err);
		}
		*code = MU_ERROR;
		return FALSE;
	}

	switch (opts->format) {
	case MU_CONFIG_FORMAT_PLAIN:
		rv = view_msg_plain (msg, NULL, opts->summary, opts->color);
		break;
	case MU_CONFIG_FORMAT_SEXP:
		rv = view_msg_sexp (msg);
		break;
	default:
		g_critical ("bug: should not be reached");
		*code = MU_ERROR_INTERNAL;
		rv = FALSE;
	}

	mu_msg_unref (msg);

	return rv;
}

static gboolean
view_params_valid (MuConfig *opts)
{
	/* note: params[0] will be 'view' */
	if (!opts->params[0] || !opts->params[1]) {
		g_warning ("usage: mu view [options] <file> [<files>]");
		return FALSE;
	}

	switch (opts->format) {
	case MU_CONFIG_FORMAT_PLAIN:
	case MU_CONFIG_FORMAT_SEXP:
		break;
	default:
		g_warning ("invalid output format %s",
			   opts->formatstr ? opts->formatstr : "<none>");
		return FALSE;
	}

	return TRUE;
}


MuError
mu_cmd_view (MuConfig *opts)
{
	int i;
	gboolean rv;
	MuError code;

	g_return_val_if_fail (opts, MU_ERROR_INTERNAL);
	g_return_val_if_fail (opts->cmd == MU_CONFIG_CMD_VIEW,
			      MU_ERROR_INTERNAL);

	if (!view_params_valid(opts))
		return MU_ERROR_IN_PARAMETERS;

	for (i = 1, code = MU_OK; opts->params[i]; ++i) {

		rv = handle_msg (opts->params[i], opts, &code);
		if (!rv)
			break;
		/* add a separator between two messages? */
		if (opts->terminator)
			g_print ("%c", VIEW_TERMINATOR);
	}

	return code;
}


MuError
mu_cmd_mkdir (MuConfig *opts)
{
	int i;

	g_return_val_if_fail (opts, MU_ERROR_INTERNAL);
	g_return_val_if_fail (opts->cmd == MU_CONFIG_CMD_MKDIR,
			      MU_ERROR_INTERNAL);

	if (!opts->params[1]) {
		g_warning ("usage: mu mkdir [-u,--mode=<mode>] "
			   "<dir> [more dirs]");
		return MU_ERROR_IN_PARAMETERS;
	}

	for (i = 1; opts->params[i]; ++i) {

		GError *err;
		err = NULL;

		if (!mu_maildir_mkdir (opts->params[i], opts->dirmode,
				       FALSE, &err)) {
			if (err && err->message) {
				g_warning ("%s", err->message);
				g_error_free (err);
			}
			return MU_ERROR;
		}
	}

	return MU_OK;
}


static gboolean
mv_check_params (MuConfig *opts, MuFlags *flags)
{
	if (!opts->params[1]) {
		g_warning ("usage: mu mv [--flags=<flags>] <mailfile> "
			   "[<maildir>]");
		return FALSE;
	}

	/* FIXME: check for invalid flags */
	if (!opts->flagstr)
		*flags = MU_FLAG_INVALID; /* ie., ignore flags */
	else {
		/* if there's a '+' or '-' sign in the string, it must
		 * be a flag-delta */
		if (strstr (opts->flagstr, "+") || strstr (opts->flagstr, "-")) {
			MuFlags oldflags;
			oldflags = mu_maildir_get_flags_from_path (opts->params[1]);
			*flags = mu_flags_from_str_delta (opts->flagstr,
							  oldflags,
							  MU_FLAG_TYPE_MAILDIR|
							  MU_FLAG_TYPE_MAILFILE);
		} else
			*flags = mu_flags_from_str (opts->flagstr,
						    MU_FLAG_TYPE_MAILDIR |
						    MU_FLAG_TYPE_MAILFILE);
	}

	return TRUE;
}


static MuError
cmd_mv_dev_null (MuConfig *opts)
{
	if (unlink (opts->params[1]) != 0) {
		g_warning ("unlink failed: %s", strerror (errno));
		return MU_ERROR_FILE;
	}

	if (opts->print_target)
		g_print ("/dev/null\n"); /* /dev/null */

	return MU_OK;
}


MuError
mu_cmd_mv (MuConfig *opts)
{
	GError *err;
	gchar *fullpath;
	MuFlags flags;

	if (!mv_check_params (opts, &flags))
		return MU_ERROR_IN_PARAMETERS;

	/* special case: /dev/null */
	if (g_strcmp0 (opts->params[2], "/dev/null") == 0)
		return cmd_mv_dev_null (opts);

	err = NULL;
	fullpath = mu_maildir_move_message (opts->params[1],
					    opts->params[2],
					    flags, opts->ignore_dups,
					    &err);
	if (!fullpath) {
		if (err) {
			MuError code;
			code = err->code;
			g_warning ("move failed: %s", err->message);
			g_error_free (err);
			return code;
		}
		return MU_ERROR_FILE;

	} else {
		if (opts->print_target)
			g_print ("%s\n", fullpath);
		return MU_OK;
	}

	g_free (fullpath);

	return MU_OK;
}


static gboolean
check_file_okay (const char *path, gboolean cmd_add)
{
 	if (!g_path_is_absolute (path)) {
		g_warning ("path is not absolute: %s", path);
		return FALSE;
	}

	if (cmd_add && access(path, R_OK) != 0) {
		g_warning ("path is not readable: %s: %s",
			   path, strerror (errno));
		return FALSE;
	}

	return TRUE;
}




MuError
mu_cmd_add (MuStore *store, MuConfig *opts)
{
	gboolean allok;
	int i;

	g_return_val_if_fail (store, MU_ERROR_INTERNAL);
	g_return_val_if_fail (opts, MU_ERROR_INTERNAL);
	g_return_val_if_fail (opts->cmd == MU_CONFIG_CMD_ADD,
			      MU_ERROR_INTERNAL);

	/* note: params[0] will be 'add' */
	if (!opts->params[0] || !opts->params[1]) {
		g_warning ("usage: mu add <file> [<files>]");
		return MU_ERROR_IN_PARAMETERS;
	}

	for (i = 1, allok = TRUE; opts->params[i]; ++i) {

		const char* src;
		src = opts->params[i];

		if (!check_file_okay (src, TRUE)) {
			allok = FALSE;
			continue;
		}

		if (!mu_store_store_path (store, src)) {
			allok = FALSE;
			g_warning ("failed to store %s", src);
		}
	}

	return allok ? MU_OK : MU_ERROR;
}


MuError
mu_cmd_remove (MuStore *store, MuConfig *opts)
{
	gboolean allok;
	int i;

	g_return_val_if_fail (opts, MU_ERROR_INTERNAL);
	g_return_val_if_fail (opts->cmd == MU_CONFIG_CMD_REMOVE,
			      MU_ERROR_INTERNAL);

	/* note: params[0] will be 'add' */
	if (!opts->params[0] || !opts->params[1]) {
		g_warning ("usage: mu remove <file> [<files>]");
		return MU_ERROR_IN_PARAMETERS;
	}

	for (i = 1, allok = TRUE; opts->params[i]; ++i) {

		const char* src;
		src = opts->params[i];

		if (!check_file_okay (src, FALSE)) {
			allok = FALSE;
			continue;
		}

		if (!mu_store_remove_path (store, src)) {
			allok = FALSE;
			g_warning ("failed to remove %s", src);
		}
	}

	return allok ? MU_OK : MU_ERROR;
}


static void
show_usage (gboolean noerror)
{
	const char* usage=
		"usage: mu command [options] [parameters]\n"
		"where command is one of index, find, cfind, view, mkdir, cleanup, "
		"extract, mv, add, remove or server\n\n"
		"see the mu, mu-<command> or mu-easy manpages for "
		"more information\n";

	if (noerror)
		g_print ("%s", usage);
	else
		g_printerr ("%s", usage);
}

static void
show_version (void)
{
	g_print ("mu (mail indexer/searcher) version " VERSION "\n"
		 "Copyright (C) 2008-2011 Dirk-Jan C. Binnema (GPLv3+)\n");
}

typedef MuError (*store_func) (MuStore *, MuConfig *);

MuError
with_store (store_func func, MuConfig *opts, gboolean read_only)
{
	MuStore *store;
	GError *err;

	err = NULL;

	if (read_only)
		store = mu_store_new_read_only
			(mu_runtime_path(MU_RUNTIME_PATH_XAPIANDB),
			 &err);
	else
		store = mu_store_new_writable
			(mu_runtime_path(MU_RUNTIME_PATH_XAPIANDB),
			 mu_runtime_path(MU_RUNTIME_PATH_CONTACTS),
			 &err);
	if (!store) {
		if (err) {
			g_warning ("store error: %s", err->message);
			g_error_free (err);
		}
		return MU_ERROR_XAPIAN;

	} else {
		MuError merr;

		merr = func (store, opts);
		mu_store_unref (store);

		return merr;
	}
}


MuError
mu_cmd_execute (MuConfig *opts)
{
	g_return_val_if_fail (opts, MU_ERROR_INTERNAL);

	if (opts->version) {
		show_version ();
		return MU_OK;
	}

	if (!opts->params||!opts->params[0]) {/* no command? */
		show_version ();
		show_usage (TRUE);
		return MU_ERROR_IN_PARAMETERS;
	}

	switch (opts->cmd) {
	case MU_CONFIG_CMD_CFIND:   return mu_cmd_cfind (opts);
	case MU_CONFIG_CMD_MKDIR:   return mu_cmd_mkdir (opts);
	case MU_CONFIG_CMD_MV:	    return mu_cmd_mv (opts);
	case MU_CONFIG_CMD_VIEW:    return mu_cmd_view (opts);
	case MU_CONFIG_CMD_EXTRACT: return mu_cmd_extract (opts);

	case MU_CONFIG_CMD_CLEANUP:
		return with_store (mu_cmd_cleanup, opts, FALSE);
	case MU_CONFIG_CMD_FIND:
		return with_store (mu_cmd_find, opts, TRUE);
	case MU_CONFIG_CMD_INDEX:
		return with_store (mu_cmd_index, opts, FALSE);
	case MU_CONFIG_CMD_ADD:
		return with_store (mu_cmd_add, opts, FALSE);
	case MU_CONFIG_CMD_REMOVE:
		return with_store (mu_cmd_remove, opts, FALSE);
	case MU_CONFIG_CMD_SERVER:
		return with_store (mu_cmd_server, opts, FALSE);
	default:
		show_usage (FALSE);
		return MU_ERROR_IN_PARAMETERS;
	}
}
