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

#include "mu-msg.h"
#include "mu-msg-part.h"
#include "mu-cmd.h"
#include "mu-util.h"
#include "mu-str.h"
#include "mu-maildir.h"
#include "mu-contacts.h"
#include "mu-runtime.h"


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


static void
print_field (const char* field, const char *val, gboolean color)
{
	if (!val)
		return;

	if (color) {
		mu_util_color_print (MU_COLOR_MAGENTA, field);
		mu_util_color_print (MU_COLOR_BLUE, val);
	} else {
		fputs (field, stdout);
		fputs (val, stdout);
	}	
	fputs ("\n", stdout);
}



/* we ignore fields for now */
static gboolean
view_msg (MuMsg *msg, const gchar *fields, gboolean summary,
	  gboolean color)
{
	const char *field;
	gchar *attachs;
	time_t date;
	const int SUMMARY_LEN = 5;

	print_field ("From: ", mu_msg_get_from (msg), color);
	print_field ("To: ",   mu_msg_get_to (msg), color);
	print_field ("Cc: ",   mu_msg_get_cc (msg), color);
	print_field ("Bcc: ",  mu_msg_get_bcc (msg), color);
	print_field ("Subject: ",  mu_msg_get_subject (msg), color);
	
	if ((date = mu_msg_get_date (msg))) 
		print_field ("Date: ", mu_str_date_s ("%c", date),
			     color);

	if ((attachs = get_attach_str (msg))) {
		print_field ("Attachments: ", attachs, color);
		g_free (attachs);
	}
	
	if (!(field = mu_msg_get_body_text (msg)))
		return TRUE; /* no body -- nothing more to do */
	
	if (summary) {
		gchar *summ;
		summ = mu_str_summarize (field, SUMMARY_LEN);
		print_field ("Summary: ", summ, color);
		g_free (summ);
	} else
		g_print ("\n%s\n", field);

	return TRUE;
}

MuExitCode
mu_cmd_view (MuConfig *opts)
{
	int rv, i;
	
	g_return_val_if_fail (opts, MU_EXITCODE_ERROR);
	g_return_val_if_fail (opts->cmd == MU_CONFIG_CMD_VIEW,
			      MU_EXITCODE_ERROR);
	
	/* note: params[0] will be 'view' */
	if (!opts->params[0] || !opts->params[1]) {
		g_warning ("usage: mu view [options] <file> [<files>]");
		return MU_EXITCODE_ERROR;
	}
	
	;
	for (i = 1, rv = MU_EXITCODE_OK;
	     opts->params[i] && rv == MU_EXITCODE_OK; ++i) {
		GError *err = NULL;
		MuMsg  *msg = mu_msg_new_from_file (opts->params[i], NULL, &err);
		if (!msg) {
			g_warning ("error: %s", err->message);
			g_error_free (err);
			return MU_EXITCODE_ERROR;
		}
		if (!view_msg (msg, NULL, opts->summary, opts->color))
			rv = MU_EXITCODE_ERROR;
		
		mu_msg_unref (msg);
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

