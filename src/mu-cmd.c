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

#include <string.h>
#include "mu-cmd.h"

MuCmd 
mu_cmd_from_string (const char* cmd)
{
	if (!cmd)
		return MU_CMD_UNKNOWN;

	if (strcmp (cmd, "index") == 0)
		return MU_CMD_INDEX;

	/* support some synonyms... */
	if ((strcmp (cmd, "query") == 0) ||
	    (strcmp (cmd, "find")  == 0) ||
	    (strcmp (cmd, "search") == 0))
		return MU_CMD_QUERY;

	if ((strcmp (cmd, "mkmdir") == 0) ||
	    (strcmp (cmd, "mkdir") == 0)) 
		return MU_CMD_MKDIR;

	if (strcmp (cmd, "link") == 0)
		return MU_CMD_LINK;
	
	if ((strcmp (cmd, "help") == 0) ||
	    (strcmp (cmd, "info") == 0))
		return MU_CMD_HELP;
	
	return MU_CMD_UNKNOWN;
}



static gboolean
_check_query_params (MuConfigOptions *opts)
{
	if (opts->linksdir) 
		if (opts->fields || opts->sortfield || opts->xquery) {
			g_warning ("Invalid option for '--linksdir'");
			return FALSE;
		}
	
	if (opts->xquery) 
		if (opts->fields || opts->sortfield) {
			g_warning ("Invalid option for '--xquery'");
			return FALSE;
		}
	
	if (opts->ascending && opts->descending) {
		g_warning ("Cannot specify both '--ascending'"
			   " and '--descending'");
		return FALSE;
	}
	
	if (!opts->params[0] || !opts->params[1]) {
		g_warning ("Missing search expression");
		return FALSE;
	}
	
	return TRUE;
}


static gboolean
_check_index_params (MuConfigOptions *opts)
{
	if (opts->linksdir) 
		if (opts->linksdir  || opts->fields ||
		    opts->sortfield || opts->xquery ||
		    opts->ascending || opts->descending||
		    opts->xquery) {
			g_warning ("Invalid option(s) for command");
			return FALSE;
		}
	
	return TRUE;
}
	


gboolean
mu_cmd_check_parameters (MuCmd cmd, MuConfigOptions *opts)
{
	g_return_val_if_fail (cmd > 0 && cmd < MU_CMD_UNKNOWN,
			  FALSE);
	switch (cmd) {
	case MU_CMD_INDEX:
		return _check_index_params (opts);
	case MU_CMD_QUERY:
		return _check_query_params (opts);
	default:
		return TRUE;
	}
}
