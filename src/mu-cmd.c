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

#if HAVE_CONFIG_H
#include <config.h>
#endif /*HAVE_CONFIG_H*/

#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>

#include "mu-maildir.h"
#include "mu-cmd.h"

gboolean
mu_cmd_equals (MuConfigOptions *config, const gchar *cmd)
{
	g_return_val_if_fail (config, FALSE);
	g_return_val_if_fail (cmd, FALSE);
	
	if (!config->params || !config->params[0])
		return FALSE;

	return (strcmp (config->params[0], cmd) == 0);
}

static void
show_usage (gboolean noerror)
{
	const char* usage=
		"usage: mu command [options] [parameters]\n"
		"where command is one of index, find, view, mkdir, cleanup "
		"or extract\n\n"
		"see the mu, mu-<command> or mu-easy manpages for "
		"more information\n";

	if (noerror)
		g_print ("%s", usage);
	else
		g_printerr ("%s", usage);
}

static void
show_version (void)
{
	g_print ("mu (mail indexer/searcher) " VERSION "\n"
		 "Copyright (C) 2008-2011 Dirk-Jan C. Binnema (GPLv3+)\n");
}



gboolean
mu_cmd_execute (MuConfigOptions *opts)
{
	if (opts->version) {
		show_version ();
		return TRUE;
	}
	
	if (!opts->params||!opts->params[0]) {/* no command? */
		show_version ();
		g_print ("\n");
		show_usage (TRUE);
		return FALSE;
	}

	switch (opts->cmd) {

	case MU_CONFIG_CMD_CLEANUP:    return mu_cmd_cleanup (opts);
	case MU_CONFIG_CMD_EXTRACT:    return mu_cmd_extract (opts);
	case MU_CONFIG_CMD_FIND:       return mu_cmd_find (opts);
	case MU_CONFIG_CMD_INDEX:      return mu_cmd_index (opts);
	case MU_CONFIG_CMD_MKDIR:      return mu_cmd_mkdir (opts);
	case MU_CONFIG_CMD_VIEW:       return mu_cmd_view (opts);
		
	case MU_CONFIG_CMD_UNKNOWN:
		g_printerr ("mu: unknown command '%s'\n\n", opts->cmdstr);
		show_usage (FALSE);
		return FALSE;
	default:
		g_return_val_if_reached (FALSE);
	}	
}
