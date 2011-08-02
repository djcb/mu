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
#include "mu-msg-flags.h"

#define VIEW_TERMINATOR '\f' /* form-feed */


static gboolean
view_msg_sexp (MuMsg *msg)
{
	fputs (mu_msg_to_sexp (msg, FALSE), stdout);
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


static MuExitCode
handle_msg (const char *fname, MuConfig *opts)
{
	GError *err;
	MuMsg *msg;
	MuExitCode rv;
	
	err = NULL;
	msg = mu_msg_new_from_file (fname, NULL, &err);

	if (!msg) {
		g_warning ("error: %s", err->message);
		g_error_free (err);
		return MU_EXITCODE_ERROR;
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
		rv = MU_EXITCODE_ERROR;
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


MuExitCode
mu_cmd_view (MuConfig *opts)
{
	int rv, i;
	
	g_return_val_if_fail (opts, MU_EXITCODE_ERROR);
	g_return_val_if_fail (opts->cmd == MU_CONFIG_CMD_VIEW,
			      MU_EXITCODE_ERROR);

	if (!view_params_valid(opts))
		return MU_EXITCODE_ERROR;
	
	for (i = 1; opts->params[i]; ++i) {

		rv = handle_msg (opts->params[i], opts);
		if (rv != MU_EXITCODE_OK)
			break;
		/* add a separator between two messages? */
		if (opts->terminator)
			g_print ("%c", VIEW_TERMINATOR);
	}
	
	return rv;
}


MuExitCode
mu_cmd_mkdir (MuConfig *opts)
{
	int i;
	
	g_return_val_if_fail (opts, MU_EXITCODE_ERROR);
	g_return_val_if_fail (opts->cmd == MU_CONFIG_CMD_MKDIR,
			      MU_EXITCODE_ERROR);
	
	if (!opts->params[1]) {
		g_warning ("usage: mu mkdir [-u,--mode=<mode>] "
			   "<dir> [more dirs]");
		return MU_EXITCODE_ERROR;
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
			return MU_EXITCODE_ERROR;
		}
	}

	return MU_EXITCODE_OK;
}


static gboolean
mv_check_params (MuConfig *opts, MuMsgFlags *flags)
{
	if (!opts->params[1] || !opts->params[2]) {
		g_warning ("usage: mu mv [--flags=<flags>] <sourcefile> "
			   "<targetmaildir>");
		return FALSE;
	}

	if (!opts->flagstr)
		*flags = MU_MSG_FLAG_NONE;
	else {
		*flags = mu_msg_flags_from_str (opts->flagstr);
		if (*flags == MU_MSG_FLAG_NONE) {
			g_warning ("error in flags");
			return FALSE;
		}
	}

	return TRUE;
}



MuExitCode
mu_cmd_mv (MuConfig *opts)
{
	GError *err;
	gchar *fullpath;
	MuMsgFlags flags;

	if (!mv_check_params (opts, &flags))
		return MU_EXITCODE_ERROR;
	
	err = NULL;

	/* special case: /dev/null */
	if (g_strcmp0 (opts->params[2], "/dev/null") == 0) {
		if (unlink (opts->params[1]) != 0) {
			g_warning ("unlink failed: %s", strerror (errno));
			return MU_EXITCODE_ERROR;
		} else
			return MU_EXITCODE_OK;
	}
		
	fullpath = mu_msg_file_move_to_maildir (opts->params[1], opts->params[2],
						flags, &err);
	if (!fullpath) {
		if (err) {
			g_warning ("move failed: %s", err->message);
			g_error_free (err);
		}
		return MU_EXITCODE_ERROR;	
	}
	
	return MU_EXITCODE_OK;
}
