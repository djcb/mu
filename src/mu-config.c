/*
** Copyright (C) 2008, 2009 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
		{ "debug", 'd', 0, G_OPTION_ARG_NONE, &opts->debug,
		  "print debug output to standard-error", NULL },
		{ "quiet", 'q', 0, G_OPTION_ARG_NONE, &opts->quiet,
		  "don't give any progress information", NULL },
		{"version", 'v', 0, G_OPTION_ARG_NONE, &opts->version,
		 "display version and copyright information", NULL},
		{"muhome", 'h', 0, G_OPTION_ARG_FILENAME, &opts->muhome,
		 "mu directory", NULL},
		{"log-stderr", 's', 0, G_OPTION_ARG_NONE, &opts->log_stderr,
		 "log to standard error", NULL},
		{"log-append", 'a', 0, G_OPTION_ARG_NONE, &opts->log_append,
		 "append to the current logfile (instead of overwriting it)", 
		 NULL},
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

		/* FIXME: implement this */
		{"reindex", 'r', 0, G_OPTION_ARG_NONE, &opts->reindex,
		 "re-index already indexed messages ", NULL},
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
		 "print a string representation of the Xapian query", NULL},
		{"fields", 'f', 0, G_OPTION_ARG_STRING, &opts->fields,
		 "fields to display in output", NULL},
		{"sortfield", 's', 0, G_OPTION_ARG_STRING, &opts->sortfield_str,
		 "field to sort on", NULL},
		{"ascending", 'a', 0, G_OPTION_ARG_NONE, &opts->ascending_flag,
		 "sort ascending", NULL},
		{"descending", 'c', 0, G_OPTION_ARG_NONE, &opts->descending_flag,
		 "sort ascending", NULL},
		{ NULL, 0, 0, 0, NULL, NULL, NULL }
	};

	og = g_option_group_new ("querying",
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
	opts->maildir = mu_util_dir_expand ("~/Maildir");
	opts->cleanup = FALSE;
	opts->reindex = FALSE;

	/* querying */
	opts->xquery        = FALSE;
	opts->fields        = "d f s";
}


void
mu_config_uninit (MuConfigOptions *opts)
{
	g_return_if_fail (opts);

	/* yes, this is fine -- they are only const
	 * for the 'outside world' */
	g_free ((gchar*)opts->muhome);
	g_free ((gchar*)opts->maildir);
}
