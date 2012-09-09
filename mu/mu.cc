/* -*-mode: c++; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8-*- */

/*
** Copyright (C) 2008-2011 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /*HAVE_CONFIG_H*/

#include <glib.h>
#include <glib-object.h>
#include <locale.h>

#include "mu-config.h"
#include "mu-cmd.h"
#include "mu-runtime.h"


static void
show_version (void)
{
	g_print ("mu (mail indexer/searcher) version " VERSION "\n"
		 "Copyright (C) 2008-2012 Dirk-Jan C. Binnema (GPLv3+)\n");
}


static void
handle_error (MuConfig *conf, GError *err)
{
	const char *advise;
	char *dynadvise;

	if (!err)
		return; /* nothing to do */

	dynadvise = NULL;
	advise    = NULL;

	switch (err->code) {

	case MU_ERROR_XAPIAN_CANNOT_GET_WRITELOCK:
		advise = "maybe mu is already running?"; break;

	case MU_ERROR_XAPIAN_CORRUPTION:
	case MU_ERROR_XAPIAN_NOT_UP_TO_DATE:
		advise = "please try 'mu index --rebuild'"; break;
	case MU_ERROR_XAPIAN_IS_EMPTY:
		advise = "please try 'mu index'";
		break;
	case MU_ERROR_IN_PARAMETERS:
		if (conf->cmd != MU_CONFIG_CMD_UNKNOWN)
			dynadvise = g_strdup_printf ("see 'mu help %s'",
						     conf->cmdstr);
		break;
	default:break; /* nothing to do */
	}

	g_warning ("%s", err->message);
	if (advise)
		g_print ("%s\n", advise);
	if (dynadvise) {
		g_print ("%s\n", dynadvise);
		g_free (dynadvise);
	}
}


int
main (int argc, char *argv[])
{
	GError *err;
	MuError rv;
	MuConfig *conf;

	setlocale (LC_ALL, "");
	g_type_init ();

	conf = mu_config_init (&argc, &argv);
	if (!conf)
		return 1;
	else if (conf->version) {
		show_version ();
		return 0;
	}

	if (!mu_runtime_init (conf->muhome, PACKAGE_NAME)) {
		mu_config_uninit (conf);
		return 1;
	}

	err = NULL;
	rv = mu_cmd_execute (conf, &err);

	handle_error (conf, err);
	g_clear_error (&err);


	mu_config_uninit (conf);
	mu_runtime_uninit ();

	return rv;
}
