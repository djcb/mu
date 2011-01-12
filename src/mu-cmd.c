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

#include "mu-msg.h"
#include "mu-msg-part.h"
#include "mu-cmd.h"
#include "mu-util.h"
#include "mu-str.h"
#include "mu-maildir.h"


/* we ignore fields for now */
static gboolean
view_msg (MuMsg *msg, const gchar *fields, size_t summary_len)
{
	const char *field;
	time_t date;

	if ((field = mu_msg_get_from (msg)))
		g_print ("From: %s\n", field);
	
	if ((field = mu_msg_get_to (msg)))
		g_print ("To: %s\n", field);

	if ((field = mu_msg_get_cc (msg)))
		g_print ("Cc: %s\n", field);

	if ((field = mu_msg_get_subject (msg)))
		g_print ("Subject: %s\n", field);
	
	if ((date = mu_msg_get_date (msg)))
		g_print ("Date: %s\n", mu_str_date_s ("%c", date));

	if (summary_len > 0) {
		field = mu_msg_get_summary (msg, summary_len);
		g_print ("Summary: %s\n", field ? field : "<none>");
	} else if ((field = mu_msg_get_body_text (msg))) 
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
		MuMsg  *msg = mu_msg_new (opts->params[i], NULL, &err);
		if (!msg) {
			g_warning ("error: %s", err->message);
			g_error_free (err);
			return MU_EXITCODE_ERROR;
		}
		if (!view_msg (msg, NULL, opts->summary_len))
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
