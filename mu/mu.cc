/*
** Copyright (C) 2008-2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <config.h>

#include <glib.h>
#include <glib-object.h>
#include <locale.h>

#include "mu-config.hh"
#include "mu-cmd.hh"
#include "mu-runtime.h"


static void
show_version (void)
{
	const char* blurb =
		"mu (mail indexer/searcher) version " VERSION "\n"
		"Copyright (C) 2008-2020 Dirk-Jan C. Binnema\n"
		"License GPLv3+: GNU GPL version 3 or later "
		"<http://gnu.org/licenses/gpl.html>.\n"
		"This is free software: you are free to change "
		"and redistribute it.\n"
		"There is NO WARRANTY, to the extent permitted by law.";

	g_print ("%s\n", blurb);
}


static void
handle_error (MuConfig *conf, MuError merr, GError **err)
{
	if (!(err && *err))
		return;

	if (*err)
		g_printerr ("error: %s (%u)\n",
			    (*err)->message,
			    (*err)->code);

	switch ((*err)->code) {
	case MU_ERROR_XAPIAN_CANNOT_GET_WRITELOCK:
		g_printerr ("maybe mu is already running?\n");
		break;
	case MU_ERROR_XAPIAN_NEEDS_REINDEX:
		g_printerr ("database needs (re)indexing.\n"
			    "try 'mu index' "
			    "(see mu-index(1) for details)\n");
		return;
	case MU_ERROR_IN_PARAMETERS:
		if (conf && mu_config_cmd_is_valid(conf->cmd))
			mu_config_show_help (conf->cmd);
		break;
	case MU_ERROR_SCRIPT_NOT_FOUND:
		g_printerr ("see the mu manpage for commands, or "
			    "'mu script' for the scripts\n");
		break;
	case MU_ERROR_XAPIAN_CANNOT_OPEN:
		g_printerr("Please (re)initialize mu with 'mu init' "
			   "see mu-init(1) for details\n");
		return;
	case MU_ERROR_XAPIAN_SCHEMA_MISMATCH:
		g_printerr("Please (re)initialize mu with 'mu init' "
			   "see mu-init(1) for details\n");
		return;
	default:
		break; /* nothing to do */
	}
}


int
main (int argc, char *argv[])
{
	GError	 *err;
	MuError	  rv;
	MuConfig *conf;

	setlocale (LC_ALL, "");

	err = NULL;
	rv  = MU_OK;

	conf = mu_config_init (&argc, &argv, &err);
	if (!conf) {
		rv = err ? (MuError)err->code : MU_ERROR;
		goto cleanup;
	} else if (conf->version) {
		show_version ();
		goto cleanup;
	}

	/* nothing to do */
	if (conf->cmd == MU_CONFIG_CMD_NONE)
		return 0;

	if (!mu_runtime_init (conf->muhome, PACKAGE_NAME, conf->debug)) {
		mu_config_uninit (conf);
		return 1;
	}

	rv = mu_cmd_execute (conf, &err);

cleanup:
	handle_error (conf, rv, &err);
	g_clear_error (&err);

	mu_config_uninit (conf);
	mu_runtime_uninit ();

	return rv;
}
