/*
** Copyright (C) 2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>

#include "mu-maildir.h"
#include "mu-cmd.h"

static MuCmd 
cmd_from_string (const char* cmd)
{
	int i;
	typedef struct {
		const gchar* _name;
		MuCmd        _cmd;
	} Cmd;

	Cmd cmd_map[]= {
		{ "index",   MU_CMD_INDEX },
		{ "find",    MU_CMD_FIND },
		{ "cleanup", MU_CMD_CLEANUP },
		{ "mkdir",   MU_CMD_MKDIR },
		{ "view",    MU_CMD_VIEW },
		{ "index",   MU_CMD_INDEX }};
	
	for (i = 0; i != G_N_ELEMENTS(cmd_map); ++i) 
		if (strcmp (cmd, cmd_map[i]._name) == 0)
			return cmd_map[i]._cmd;

	return MU_CMD_UNKNOWN;
}

static gboolean
show_usage (gboolean noerror)
{
	const char* usage=
		"usage: mu [options] command [parameters]\n"
		"\twhere command is one of index, find, mkdir, cleanup or view\n\n"
		"see mu(1) (the mu manpage) for more information, or try "
		"mu --help\n";

	if (noerror)
		g_print ("%s", usage);
	else
		g_printerr ("%s", usage);

	return noerror;
}

static gboolean
show_version (void)
{
	const char* msg =
		"mu (mail indexer / searcher version) " VERSION "\n\n"
		"Copyright (C) 2008-2010 Dirk-Jan C. Binnema\n"
		"License GPLv3+: GNU GPL version 3 or later "
		"<http://gnu.org/licenses/gpl.html>.\n\n"
		"This is free software: you are free to change "
		"and redistribute it.\n"
		"There is NO WARRANTY, to the extent permitted by law.";

	g_print ("%s\n", msg);

	return TRUE;
}




gboolean
mu_cmd_execute (MuConfigOptions *opts)
{
	MuCmd cmd;
	
	if (opts->version)
		return show_version ();
	
	if (!opts->params||!opts->params[0]) {/* no command? */
		show_version ();
		g_print ("\n");
		return show_usage (FALSE);
	}
	
	cmd = cmd_from_string (opts->params[0]);

	switch (cmd) {

	case MU_CMD_INDEX:   return mu_cmd_index (opts);
	case MU_CMD_FIND:    return mu_cmd_find (opts);
	case MU_CMD_MKDIR:   return mu_cmd_mkdir (opts);
	case MU_CMD_CLEANUP: return mu_cmd_cleanup (opts);
	case MU_CMD_VIEW:    return mu_cmd_view (opts);

	case MU_CMD_UNKNOWN:
		return show_usage (FALSE);
	default:
		g_return_val_if_reached (FALSE);
	}	
}
