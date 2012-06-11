/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/
/*
** Copyright (C) 2010-2012 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

	sexp = mu_msg_to_sexp (msg, 0, NULL, FALSE, FALSE);
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
	mu_msg_part_foreach (msg, FALSE,
			     (MuMsgPartForeachFunc)each_part, &attach);

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


/* a summary_len of 0 mean 'don't show summary, show body */
static void
body_or_summary (MuMsg *msg, unsigned summary_len, gboolean color)
{
	const char* field;

	field = mu_msg_get_body_text (msg);
	if (!field)
		return; /* no body -- nothing more to do */

	if (summary_len != 0) {
		gchar *summ;
		summ = mu_str_summarize (field, summary_len);
		print_field ("Summary", summ, color);
		g_free (summ);
	} else {
		color_maybe (MU_COLOR_YELLOW);
		mu_util_print_encoded ("\n%s\n", field);
		color_maybe (MU_COLOR_DEFAULT);
	}
}


/* we ignore fields for now */
/* summary_len == 0 means "no summary */
static gboolean
view_msg_plain (MuMsg *msg, const gchar *fields, unsigned summary_len,
	  gboolean color)
{
	gchar *attachs;
	time_t date;
	const GSList *lst;

	print_field ("From",    mu_msg_get_from (msg),    color);
	print_field ("To",      mu_msg_get_to (msg),      color);
	print_field ("Cc",      mu_msg_get_cc (msg),      color);
	print_field ("Bcc",     mu_msg_get_bcc (msg),     color);
	print_field ("Subject", mu_msg_get_subject (msg), color);

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

	body_or_summary (msg, summary_len, color);

	return TRUE;
}


static gboolean
handle_msg (const char *fname, MuConfig *opts, GError **err)
{
	MuMsg *msg;
	gboolean rv;

	err = NULL;
	msg = mu_msg_new_from_file (fname, NULL, err);
	if (!msg)
		return FALSE;

	switch (opts->format) {
	case MU_CONFIG_FORMAT_PLAIN:
		rv = view_msg_plain
			(msg, NULL,
			 opts->summary ? opts->summary_len : 0,
			 !opts->nocolor);
		break;
	case MU_CONFIG_FORMAT_SEXP:
		rv = view_msg_sexp (msg);
		break;
	default:
		g_critical ("bug: should not be reached");
		rv = FALSE;
	}

	mu_msg_unref (msg);

	return rv;
}

static gboolean
view_params_valid (MuConfig *opts, GError **err)
{
	/* note: params[0] will be 'view' */
	if (!opts->params[0] || !opts->params[1]) {
		mu_util_g_set_error (err, MU_ERROR_IN_PARAMETERS,
				     "error in parameters");
		return FALSE;
	}

	switch (opts->format) {
	case MU_CONFIG_FORMAT_PLAIN:
	case MU_CONFIG_FORMAT_SEXP:
		break;
	default:
		mu_util_g_set_error (err, MU_ERROR_IN_PARAMETERS,
				     "invalid output format");
		return FALSE;
	}

	return TRUE;
}


MuError
mu_cmd_view (MuConfig *opts, GError **err)
{
	int i;
	gboolean rv;

	g_return_val_if_fail (opts, MU_ERROR_INTERNAL);
	g_return_val_if_fail (opts->cmd == MU_CONFIG_CMD_VIEW,
			      MU_ERROR_INTERNAL);

	rv = view_params_valid(opts, err);
	if (!rv)
		goto leave;

	for (i = 1; opts->params[i]; ++i) {

		rv = handle_msg (opts->params[i], opts, err);
		if (!rv)
			break;

		/* add a separator between two messages? */
		if (opts->terminator)
			g_print ("%c", VIEW_TERMINATOR);
	}

leave:
	if (!rv)
		return err && *err ? (*err)->code : MU_ERROR;

	return MU_OK;
}


MuError
mu_cmd_mkdir (MuConfig *opts, GError **err)
{
	int i;

	g_return_val_if_fail (opts, MU_ERROR_INTERNAL);
	g_return_val_if_fail (opts->cmd == MU_CONFIG_CMD_MKDIR,
			      MU_ERROR_INTERNAL);

	if (!opts->params[1]) {
		g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_IN_PARAMETERS,
			     "missing directory parameter");
		return MU_ERROR_IN_PARAMETERS;
	}

	for (i = 1; opts->params[i]; ++i)
		if (!mu_maildir_mkdir (opts->params[i], opts->dirmode,
				       FALSE, err))
			return err && *err ? (*err)->code :
				MU_ERROR_FILE_CANNOT_MKDIR;

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

gboolean
check_add_params (MuConfig *opts, GError **err)
{

	return TRUE;
}



MuError
mu_cmd_add (MuStore *store, MuConfig *opts, GError **err)
{
	gboolean allok;
	int i;

	g_return_val_if_fail (store, MU_ERROR_INTERNAL);
	g_return_val_if_fail (opts, MU_ERROR_INTERNAL);
	g_return_val_if_fail (opts->cmd == MU_CONFIG_CMD_ADD,
			      MU_ERROR_INTERNAL);

	/* note: params[0] will be 'add' */
	if (!opts->params[0] || !opts->params[1]) {
		g_print ("usage: mu add <file> [<files>]\n");
		g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_IN_PARAMETERS,
			     "missing source and/or target");
		return MU_ERROR_IN_PARAMETERS;
	}

	for (i = 1, allok = TRUE; opts->params[i]; ++i) {

		const char* src;
		src = opts->params[i];

		if (!check_file_okay (src, TRUE) ||
		    mu_store_add_path (store, src, NULL, err) ==
		    MU_STORE_INVALID_DOCID) {
			MU_WRITE_LOG ("failed to add %s", src);
			allok = FALSE;
		}
	}

	if (!allok) {
		g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_XAPIAN_STORE_FAILED,
			     "store failed for some message(s)");
		return MU_ERROR_XAPIAN_STORE_FAILED;
	}

	return MU_OK;
}


MuError
mu_cmd_remove (MuStore *store, MuConfig *opts, GError **err)
{
	gboolean allok;
	int i;

	g_return_val_if_fail (opts, MU_ERROR_INTERNAL);
	g_return_val_if_fail (opts->cmd == MU_CONFIG_CMD_REMOVE,
			      MU_ERROR_INTERNAL);

	/* note: params[0] will be 'add' */
	if (!opts->params[0] || !opts->params[1]) {
		g_warning ("usage: mu remove <file> [<files>]");
		g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_IN_PARAMETERS,
				     "missing source and/or target");
		return MU_ERROR_IN_PARAMETERS;
	}

	for (i = 1, allok = TRUE; opts->params[i]; ++i) {

		const char* src;
		src = opts->params[i];

		if (!check_file_okay (src, FALSE) ||
		    !mu_store_remove_path (store, src)) {
			allok = FALSE;
			MU_WRITE_LOG ("failed to remove %s", src);
		}
	}

	if (!allok) {
		g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_XAPIAN_STORE_FAILED,
			     "remove failed for some message(s)");
		return MU_ERROR_XAPIAN_REMOVE_FAILED;
	}

	return MU_OK;
}


static void
show_usage (void)
{
	g_print ("usage: mu command [options] [parameters]\n");
	g_print ("where command is one of index, find, cfind, view, mkdir, "
		   "extract, add, remove or server\n");
	g_print ("see the mu, mu-<command> or mu-easy manpages for "
		   "more information\n");
}

static void
show_version (void)
{
	g_print ("mu (mail indexer/searcher) version " VERSION "\n"
		 "Copyright (C) 2008-2011 Dirk-Jan C. Binnema (GPLv3+)\n");
}

typedef MuError (*store_func) (MuStore *, MuConfig *, GError **err);

MuError
with_store (store_func func, MuConfig *opts, gboolean read_only,
	    GError **err)
{
	MuStore *store;

	if (read_only)
		store = mu_store_new_read_only
			(mu_runtime_path(MU_RUNTIME_PATH_XAPIANDB),
			 err);
	else
		store = mu_store_new_writable
			(mu_runtime_path(MU_RUNTIME_PATH_XAPIANDB),
			 mu_runtime_path(MU_RUNTIME_PATH_CONTACTS),
			 opts->rebuild, err);
	if (!store)
		return MU_G_ERROR_CODE(err);
	else {
		MuError merr;
		merr = func (store, opts, err);
		mu_store_unref (store);
		return merr;
	}
}


gboolean
check_params (MuConfig *opts, GError **err)
{
	if (!opts->params||!opts->params[0]) {/* no command? */
		show_usage ();
		mu_util_g_set_error (err, MU_ERROR_IN_PARAMETERS,
				     "error in parameters");
		return FALSE;
	}

	return TRUE;
}

MuError
mu_cmd_execute (MuConfig *opts, GError **err)
{
	g_return_val_if_fail (opts, MU_ERROR_INTERNAL);

	if (opts->version) {
		show_version ();
		return MU_OK;
	}

	if (!check_params(opts, err))
		return MU_G_ERROR_CODE(err);

	switch (opts->cmd) {
	case MU_CONFIG_CMD_CFIND:   return mu_cmd_cfind (opts, err);
	case MU_CONFIG_CMD_MKDIR:   return mu_cmd_mkdir (opts, err);
	case MU_CONFIG_CMD_VIEW:    return mu_cmd_view (opts, err);
	case MU_CONFIG_CMD_EXTRACT: return mu_cmd_extract (opts, err);

	case MU_CONFIG_CMD_FIND:
		return with_store (mu_cmd_find, opts, TRUE, err);
	case MU_CONFIG_CMD_INDEX:
		return with_store (mu_cmd_index, opts, FALSE, err);
	case MU_CONFIG_CMD_ADD:
		return with_store (mu_cmd_add, opts, FALSE, err);
	case MU_CONFIG_CMD_REMOVE:
		return with_store (mu_cmd_remove, opts, FALSE, err);
	case MU_CONFIG_CMD_SERVER:
		return with_store (mu_cmd_server, opts, FALSE, err);
	default:
		show_usage ();
		g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_IN_PARAMETERS,
			     "unknown command '%s'", opts->cmdstr);
		return MU_ERROR_IN_PARAMETERS;
	}
}
