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
		{ NULL }
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
		{NULL}
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
		{NULL}
	};

	og = g_option_group_new ("querying",
				 "options for the 'query' command",
				 "", NULL, NULL);	
	g_option_group_add_entries (og, entries);
	
	return og;
}



void
mu_config_set_defaults (MuConfigOptions *opts)
{
	g_return_if_fail (opts);

	/* start from zero */
	memset (opts, 0, sizeof(MuConfigOptions));

	/* general */
	opts->quiet = FALSE;
	opts->debug = FALSE;
	
	/* indexing */
	opts->maildir = "/home/djcb/Maildir";
	opts->cleanup = FALSE;
	opts->reindex = FALSE;

	/* querying */
	opts->xquery        = FALSE;
	opts->fields        = "d f s";
}
