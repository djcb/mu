/*
** Copyright (C) 2011 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-cmd.h"
#include "mu-util.h"
#include "mu-str.h"
#include "mu-date.h"
#include "mu-contacts.h"
#include "mu-runtime.h"

static void
print_header (MuConfigFormat format)
{
	switch (format) {
	case MU_CONFIG_FORMAT_BBDB:
		g_print (";; -*-coding: utf-8-emacs;-*-\n"			 
			 ";;; file-version: 6\n");
		break;
	case MU_CONFIG_FORMAT_MUTT_AB:
		g_print ("Matching addresses in the mu database:\n");
		break;
	default:
		break;
	}
}

static void
each_contact_bbdb (const char *email, const char *name, time_t tstamp)
{
	char *fname, *lname, *now, *timestamp;

	fname	  = mu_str_guess_first_name (name);
	lname	  = mu_str_guess_last_name (name);
	now	  = mu_date_str ("%Y-%m-%d", time(NULL));
	timestamp = mu_date_str ("%Y-%m-%d", tstamp);
			
	g_print ("[\"%s\" \"%s\" nil nil nil nil (\"%s\") "
		 "((creation-date . \"%s\") (time-stamp . \"%s\")) nil]\n",
		 fname, lname, email, now, timestamp);

	g_free (now);
	g_free (timestamp);
	g_free (fname);
	g_free (lname);	
}


static void
each_contact_mutt_alias (const char *email, const char *name)
{
	gchar *nick;

	if (!name)
		return;

	nick = mu_str_guess_nick (name);
	mu_util_print_encoded ("alias %s %s <%s>\n",
			       nick, name, email);	
	g_free (nick);

}


static void
each_contact_wl (const char *email, const char *name)
{
	gchar *nick;

	if (!name)
		return;

	nick = mu_str_guess_nick (name);
	mu_util_print_encoded ("%s \"%s\" \"%s\"\n",
			       email, nick, name);
	g_free (nick);
}


static void
each_contact_org_contact (const char *email, const char *name)
{
	if (name)
		mu_util_print_encoded (
			"* %s\n:PROPERTIES:\n:EMAIL: %s\n:END:\n\n",
			name, email);
}

static void
print_plain (const char *email, const char *name, gboolean color)
{
	if (name) {
		if (color) fputs (MU_COLOR_MAGENTA, stdout);
		mu_util_fputs_encoded (name, stdout);
		fputs (" ", stdout);
	}

	if (color)
		fputs (MU_COLOR_GREEN, stdout);

	mu_util_fputs_encoded (email, stdout);

	if (color)
		fputs (MU_COLOR_DEFAULT, stdout);

	fputs ("\n", stdout);
}

struct _ECData {
	MuConfigFormat format;
	gboolean color;
};
typedef struct _ECData ECData;


static void
each_contact (const char *email, const char *name, time_t tstamp,
	      ECData *ecdata)
{
	switch (ecdata->format) {
	case MU_CONFIG_FORMAT_MUTT_ALIAS:
		each_contact_mutt_alias (email, name);
		break;
	case MU_CONFIG_FORMAT_MUTT_AB:
		mu_util_print_encoded ("%s\t%s\t\n",
				       email, name ? name : "");
		break;
	case MU_CONFIG_FORMAT_WL:
		each_contact_wl (email, name);
		break;
	case MU_CONFIG_FORMAT_ORG_CONTACT:
		each_contact_org_contact (email, name);
		break;
	case MU_CONFIG_FORMAT_BBDB:
		each_contact_bbdb (email, name, tstamp);
		break;	
        case MU_CONFIG_FORMAT_CSV:
		mu_util_print_encoded ("%s,%s\n", name ? name : "", email);
		break;
	default:
		print_plain (email, name, ecdata->color);
	}
}


static MuExitCode
run_cmd_cfind (const char* pattern, MuConfigFormat format,
	       gboolean color)
{
	gboolean rv;
	MuContacts *contacts;
	size_t num;
	ECData ecdata = {format, color};
	
	contacts = mu_contacts_new (mu_runtime_path(MU_RUNTIME_PATH_CONTACTS));
	if (!contacts) {
		g_warning ("could not retrieve contacts");
		return MU_EXITCODE_ERROR;
	}

	print_header (format);
	rv = mu_contacts_foreach (contacts,
				  (MuContactsForeachFunc)each_contact,
				  &ecdata, pattern, &num);
	
	mu_contacts_destroy (contacts);

	if (num == 0) {
		g_warning ("no matching contacts found");
		return MU_EXITCODE_NO_MATCHES;
	}

	return rv ? MU_EXITCODE_OK : MU_EXITCODE_ERROR;

	
}

static gboolean
cfind_params_valid (MuConfig *opts)
{
	switch (opts->format) {
	case MU_CONFIG_FORMAT_PLAIN:
	case MU_CONFIG_FORMAT_MUTT_ALIAS:
	case MU_CONFIG_FORMAT_MUTT_AB:
	case MU_CONFIG_FORMAT_WL:
	case MU_CONFIG_FORMAT_BBDB:
	case MU_CONFIG_FORMAT_CSV:
	case MU_CONFIG_FORMAT_ORG_CONTACT:
		break;
	default:
		g_warning ("invalid output format %s",
			   opts->formatstr ? opts->formatstr : "<none>");
		return FALSE;
	}

	/* only one pattern allowed */
	if (opts->params[1] && opts->params[2]) {
		g_warning ("usage: mu cfind [options] [<ptrn>]");
		return FALSE;
	}

	return TRUE;
}


MuExitCode
mu_cmd_cfind (MuConfig *opts)
{
	g_return_val_if_fail (opts, MU_EXITCODE_ERROR);
	g_return_val_if_fail (opts->cmd == MU_CONFIG_CMD_CFIND,
			      MU_EXITCODE_ERROR);

	if (!cfind_params_valid (opts))
		return MU_EXITCODE_ERROR;

	return run_cmd_cfind (opts->params[1], opts->format, opts->color);
}

