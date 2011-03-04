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
#include "mu-contacts.h"
#include "mu-runtime.h"

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


enum _OutputFormat {
	FORMAT_PLAIN,
	FORMAT_MUTT,
	FORMAT_WL,
	FORMAT_CSV,
	FORMAT_ORG_CONTACT,

	FORMAT_NONE
};
typedef enum _OutputFormat OutputFormat;

static OutputFormat
get_output_format (const char *formatstr)
{
	int i;
	struct {
		const char*	name;
		OutputFormat	format;
	} formats [] = {
		{MU_CONFIG_FORMAT_PLAIN,	 FORMAT_PLAIN},
		{MU_CONFIG_FORMAT_MUTT,		 FORMAT_MUTT},
		{MU_CONFIG_FORMAT_WL,		 FORMAT_WL},
		{MU_CONFIG_FORMAT_CSV,	         FORMAT_CSV},
		{MU_CONFIG_FORMAT_ORG_CONTACT,	 FORMAT_ORG_CONTACT}
	};

	for (i = 0; i != G_N_ELEMENTS(formats); i++)
		if (g_strcmp0 (formats[i].name, formatstr) == 0)
			return formats[i].format;

	return FORMAT_NONE;
}



static void
each_contact (const char *email, const char *name, time_t tstamp,
	      OutputFormat format)
{
	switch (format) {
	case FORMAT_MUTT:
		if (name)
			g_print ("alias %s <%s>\n", name, email);
		break;
	case FORMAT_WL:
		if (name)
			g_print ("%s \"%s\"\n", email, name);
		break;
	case FORMAT_ORG_CONTACT:
		if (name)
			g_print ("* %s\n:PROPERTIES:\n:EMAIL: %s\n:END:\n\n",
				 name, email);
		break;
	default:
		g_print ("%u: %s %s\n", (unsigned)tstamp, email, name ? name : "");
	}
}




MuExitCode
mu_cmd_cfind (MuConfig *opts)
{
	OutputFormat format;
	MuContacts *contacts;
	
	g_return_val_if_fail (opts, MU_EXITCODE_ERROR);
	g_return_val_if_fail (opts->cmd == MU_CONFIG_CMD_CFIND,
			      MU_EXITCODE_ERROR);

	format = get_output_format (opts->formatstr);
	if (format == FORMAT_NONE) {
		g_warning ("invalid output format %s",
			   opts->formatstr ? opts->formatstr : "<none>");
		return FALSE;
	}
	
	
	/* if (!opts->params[1]) { */
	/* 	g_warning ("usage: mu cfind [OPTIONS] [<ptrn>]"); */
	/* 	return MU_EXITCODE_ERROR; */
	/* } */

	contacts = mu_contacts_new (mu_runtime_contacts_cache_file());
	if (!contacts) {
		g_warning ("could not retrieve contacts");
		return MU_EXITCODE_ERROR;
	}
	
	mu_contacts_foreach (contacts, (MuContactsForeachFunc)each_contact,
			     GINT_TO_POINTER(format), opts->params[1]);
	mu_contacts_destroy (contacts);
	
	return MU_OK;
}

