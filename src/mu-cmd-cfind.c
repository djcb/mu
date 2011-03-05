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

#include "mu-cmd.h"
#include "mu-util.h"
#include "mu-str.h"
#include "mu-contacts.h"
#include "mu-runtime.h"

enum _OutputFormat {
	FORMAT_PLAIN,
	FORMAT_MUTT,
	FORMAT_WL,
	FORMAT_BBDB,
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
		{MU_CONFIG_FORMAT_BBDB,		 FORMAT_BBDB},
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
	case FORMAT_BBDB: /* FIXME */
		break;

	case FORMAT_CSV: /* FIXME */
		break;
	default:
                g_print ("%s%s%s\n", name ? name : "", name ? " " : "", email);
	}
}



MuExitCode
mu_cmd_cfind (MuConfig *opts)
{
	OutputFormat format;
	MuContacts *contacts;
	gboolean rv;
	size_t num;
	
	g_return_val_if_fail (opts, MU_EXITCODE_ERROR);
	g_return_val_if_fail (opts->cmd == MU_CONFIG_CMD_CFIND,
			      MU_EXITCODE_ERROR);

	format = get_output_format (opts->formatstr);
	if (format == FORMAT_NONE) {
		g_warning ("invalid output format %s",
			   opts->formatstr ? opts->formatstr : "<none>");
		return FALSE;
	}

	/* only one pattern allowed */
	if (opts->params[1] && opts->params[2]) {
		g_warning ("usage: mu cfind [OPTIONS] [<ptrn>]");
		return MU_EXITCODE_ERROR;
	}

	contacts = mu_contacts_new (mu_runtime_contacts_cache_file());
	if (!contacts) {
		g_warning ("could not retrieve contacts");
		return MU_EXITCODE_ERROR;
	}
	
	rv = mu_contacts_foreach (contacts, (MuContactsForeachFunc)each_contact,
				  GINT_TO_POINTER(format), opts->params[1], &num);
	
	mu_contacts_destroy (contacts);

	if (rv) 
		return (num == 0) ? MU_EXITCODE_NO_MATCHES : MU_EXITCODE_OK;
	else
		return MU_EXITCODE_ERROR;
}

