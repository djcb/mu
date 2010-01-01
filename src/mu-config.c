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

#include <glib.h>
#include <string.h> /* memset */

#include "mu-util.h"
#include "mu-config.h"


GOptionGroup*
mu_config_options_group_mu (MuConfigOptions *opts)
{
	GOptionGroup *og;
	GOptionEntry entries[] = {
		{ "debug", 'g', 0, G_OPTION_ARG_NONE, &opts->debug,
		  "print debug output to standard error", NULL },
		{ "quiet", 'q', 0, G_OPTION_ARG_NONE, &opts->quiet,
		  "don't give any progress information", NULL },
		{"version", 'v', 0, G_OPTION_ARG_NONE, &opts->version,
		 "display version and copyright information", NULL},
		{"muhome", 'h', 0, G_OPTION_ARG_FILENAME, &opts->muhome,
		 "mu directory", NULL},
		{"log-stderr", 'e', 0, G_OPTION_ARG_NONE, &opts->log_stderr,
		 "log to standard error", NULL},
		{"log-append", 'a', 0, G_OPTION_ARG_NONE, &opts->log_append,
		 "append to the logfile (instead of overwriting)", 
		 NULL},
		{ G_OPTION_REMAINING, 0, 0, G_OPTION_ARG_STRING_ARRAY,
		  &opts->params, "parameters", NULL },
		{ NULL, 0, 0, 0, NULL, NULL, NULL }
	};
	
	og = g_option_group_new ("mu",
				 "general mu options",
				 "", NULL, NULL);	
	g_option_group_add_entries (og, entries);
	
	return og;
}



GOptionGroup*
mu_config_options_group_index (MuConfigOptions *opts)
{
	GOptionGroup *og;
	GOptionEntry entries[] = {
		{"maildir", 'm', 0, G_OPTION_ARG_FILENAME, &opts->maildir,
		 "top of the maildir", NULL},
		{"reindex", 'r', 0, G_OPTION_ARG_NONE, &opts->reindex,
		 "index even already indexed messages ", NULL},
		{ NULL, 0, 0, 0, NULL, NULL, NULL }
	};
	
	og = g_option_group_new ("index",
				 "options for the 'index' command",
				 "", NULL, NULL);	
	g_option_group_add_entries (og, entries);
	
	return og;
}

GOptionGroup*
mu_config_options_group_query (MuConfigOptions *opts)
{
	GOptionGroup *og;
	GOptionEntry entries[] = {
		{"xquery", 'x', 0, G_OPTION_ARG_NONE, &opts->xquery,
		 "print the Xapian query", NULL},
		{"fields", 'f', 0, G_OPTION_ARG_STRING, &opts->fields,
		 "fields to display in the output", NULL},
		{"sortfield", 's', 0, G_OPTION_ARG_STRING, &opts->sortfield,
		 "field to sort on", NULL},
		{"ascending", 'u', 0, G_OPTION_ARG_NONE, &opts->ascending,
		 "sort ascending (up)", NULL},
		{"descending", 'd', 0, G_OPTION_ARG_NONE, &opts->descending,
		 "sort descending (down)", NULL},
		{"linksdir", 't', 0, G_OPTION_ARG_STRING, &opts->linksdir,
		 "output as symbolic links to a target maildir", NULL },
		{ NULL, 0, 0, 0, NULL, NULL, NULL }
	};

	og = g_option_group_new ("query",
				 "options for the 'query' command",
				 "", NULL, NULL);	
	g_option_group_add_entries (og, entries);
	
	return og;
}


void
mu_config_init (MuConfigOptions *opts)
{
	g_return_if_fail (opts);

	/* start from zero */
	memset (opts, 0, sizeof(MuConfigOptions));

	/* general */
	opts->quiet	 = FALSE;
	opts->debug	 = FALSE;
	opts->muhome	 = mu_util_dir_expand ("~/.mu");
	opts->log_append = TRUE;
	opts->log_stderr = FALSE;
	
	/* indexing */		
	opts->maildir = mu_util_guess_maildir();
	opts->cleanup = FALSE;
	opts->reindex = FALSE;

	/* querying */
	opts->xquery     = FALSE;
	opts->fields     = "d f s";
	opts->sortfield  = "d";
	opts->ascending  = FALSE;
	opts->descending = TRUE;
	opts->linksdir   = NULL;
}


void
mu_config_uninit (MuConfigOptions *opts)
{
	g_return_if_fail (opts);

	g_free (opts->muhome);
	g_free (opts->maildir);
	g_free (opts->linksdir);

	g_strfreev (opts->params);
}


