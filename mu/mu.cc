/* -*-mode: c++; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8-*- */

/*
** Copyright (C) 2008-2012 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
	const char* blurb =
		"mu (mail indexer/searcher) version " VERSION "\n"
		"Copyright (C) 2008-2012 Dirk-Jan C. Binnema\n"
		"License GPLv3+: GNU GPL version 3 or later "
		"<http://gnu.org/licenses/gpl.html>.\n"
		"This is free software: you are free to change "
		"and redistribute it.\n"
		"There is NO WARRANTY, to the extent permitted by law.";

	g_print ("%s\n", blurb);
}


static void
handle_error (MuConfig *conf, GError *err)
{
	if (!err)
		return; /* nothing to do */

	switch (err->code) {
	case MU_ERROR_XAPIAN_CANNOT_GET_WRITELOCK:
		g_print ("maybe mu is already running?\n");
		break;
	case MU_ERROR_XAPIAN_CORRUPTION:
	case MU_ERROR_XAPIAN_NOT_UP_TO_DATE:
		g_print ("database needs update; try 'mu index --rebuild'\n");
		break;
	case MU_ERROR_XAPIAN_IS_EMPTY:
		g_print ("database is empty; try 'mu index'");
		break;
	case MU_ERROR_IN_PARAMETERS:
		if (mu_config_cmd_is_valid(conf->cmd))
			mu_config_show_help (conf->cmd);
		break;
	default:break; /* nothing to do */
	}

	g_warning ("%s", err->message);
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

	/* nothing to do */
	if (conf->cmd == MU_CONFIG_CMD_NONE)
		return 0;

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
