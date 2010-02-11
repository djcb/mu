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

#include "config.h"

#include <glib.h>
#include <string.h> /* memset */

#include "mu-util.h"
#include "mu-config.h"


static GOptionGroup*
config_options_group_mu (MuConfigOptions *opts)
{
	GOptionGroup *og;
	GOptionEntry entries[] = {
		{ "debug", 'd', 0, G_OPTION_ARG_NONE, &opts->debug,
		  "print debug output to standard error", NULL },
		{ "quiet", 'q', 0, G_OPTION_ARG_NONE, &opts->quiet,
		  "don't give any progress information", NULL },
		{"version", 'v', 0, G_OPTION_ARG_NONE, &opts->version,
		 "display version and copyright information", NULL},
		{"muhome", 'a', 0, G_OPTION_ARG_FILENAME, &opts->muhome,
		 "specify an alternative mu directory", NULL},
		{"log-stderr", 'e', 0, G_OPTION_ARG_NONE, &opts->log_stderr,
		 "log to standard error", NULL},
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



static GOptionGroup*
config_options_group_index (MuConfigOptions *opts)
{
	GOptionGroup *og;
	GOptionEntry entries[] = {
		{"maildir", 'm', 0, G_OPTION_ARG_FILENAME, &opts->maildir,
		 "top of the maildir", NULL},
		{"reindex", 'r', 0, G_OPTION_ARG_NONE, &opts->reindex,
		 "index already indexed messages too", NULL},
		{"rebuild", 'y', 0, G_OPTION_ARG_NONE, &opts->rebuild,
		 "rebuild the database from scratch", NULL},
		{"autoupgrade", 'u', 0, G_OPTION_ARG_NONE, &opts->autoupgrade,
		 "automatically upgrade the database with new mu versions", NULL},
		{"nocleanup", 'n', 0, G_OPTION_ARG_NONE, &opts->nocleanup,
		 "don't clean up the database after indexing", NULL},
		{ NULL, 0, 0, 0, NULL, NULL, NULL }
	};
	
	og = g_option_group_new ("index",
				 "options for the 'index' command",
				 "", NULL, NULL);	
	g_option_group_add_entries (og, entries);
	
	return og;
}

static GOptionGroup*
config_options_group_find (MuConfigOptions *opts)
{
	GOptionGroup *og;
	GOptionEntry entries[] = {
		{"xquery", 'x', 0, G_OPTION_ARG_NONE, &opts->xquery,
		 "print the Xapian query", NULL},
		{"fields", 'f', 0, G_OPTION_ARG_STRING, &opts->fields,
		 "fields to display in the output", NULL},
		{"sortfield", 's', 0, G_OPTION_ARG_STRING, &opts->sortfield,
		 "field to sort on", NULL},
		{"descending", 'z', 0, G_OPTION_ARG_NONE, &opts->descending,
		 "sort in descending order (z -> a)", NULL},
		{"linksdir", 'l', 0, G_OPTION_ARG_STRING, &opts->linksdir,
		 "output as symbolic links to a target maildir", NULL },
		{"clearlinks", 'c', 0, G_OPTION_ARG_NONE, &opts->clearlinks,
		 "clear old links before filling a linksdir", NULL},
		{ NULL, 0, 0, 0, NULL, NULL, NULL }
	};

	og = g_option_group_new ("find",
				 "options for the 'find' command",
				 "", NULL, NULL);	
	g_option_group_add_entries (og, entries);
	
	return og;
}


static GOptionGroup*
config_options_group_mkdir (MuConfigOptions *opts)
{
	GOptionGroup *og;
	GOptionEntry entries[] = {
		{"mode", 'p', 0, G_OPTION_ARG_INT, &opts->dirmode,
		 "set the mode (as in chmod), in octal notation", NULL},
		{ NULL, 0, 0, 0, NULL, NULL, NULL }
	};

	og = g_option_group_new ("mkdir",
				 "options for the 'mkdir' command",
				 "", NULL, NULL);	
	g_option_group_add_entries (og, entries);
	
	return og;
}


static gchar*
guess_muhome (void)
{
	const char* home;

	home = g_getenv ("HOME");
	if (!home)
		home = g_get_home_dir ();

	if (!home)
		MU_WRITE_LOG ("failed to determine homedir");
	
	return g_strdup_printf ("%s%c%s", home ? home : ".", G_DIR_SEPARATOR,
				".mu");
}


static gboolean
parse_params (MuConfigOptions *config, int *argcp, char ***argvp)
{
	GError *error = NULL;
	GOptionContext *context;
	gboolean rv;
	
	context = g_option_context_new ("- maildir utilities");

	g_option_context_set_main_group (context,
					 config_options_group_mu (config));
	g_option_context_add_group (context,
				    config_options_group_index (config));
	g_option_context_add_group (context,
				    config_options_group_find (config));
	g_option_context_add_group (context,
				    config_options_group_mkdir (config));

	rv = g_option_context_parse (context, argcp, argvp, &error);
	if (!rv) {
		g_printerr ("error in options: %s\n", error->message);
		g_error_free (error);
	} else
		g_option_context_free (context);
	
	return rv;
}


gboolean
mu_config_init (MuConfigOptions *opts, int *argcp, char ***argvp)
{
	gchar *old;

	g_return_val_if_fail (opts, FALSE);	
	memset (opts, 0, sizeof(MuConfigOptions));

	/* set dirmode before, because '0000' is a valid mode */
	opts->dirmode = 0755;

	if (argcp && argvp)
		if (!parse_params (opts, argcp, argvp))
			return FALSE;
	
	if (!opts->muhome)
		opts->muhome = guess_muhome ();
	
	/* note: xpath is is *not* settable from the cmdline */
	opts->xpath = g_strdup_printf ("%s%c%s", opts->muhome,G_DIR_SEPARATOR,
				       MU_XAPIAN_DIR_NAME);

	/* indexing */
	old = opts->maildir;
	if (opts->maildir)
		opts->maildir = mu_util_dir_expand (opts->maildir);
	else
		opts->maildir = mu_util_guess_maildir();
	g_free (old);
		      
	/* querying */
	
	/* note, when no fields are specified, we use
	 * date-from-subject, and sort descending by date. If fields
	 * *are* specified, we sort in ascending order. */
	if (!opts->fields) {
		opts->descending = TRUE;
		opts->fields     = "d f s";
		opts->sortfield  = "d";
	}

	if (opts->linksdir) {
		gchar *old = opts->linksdir;
		opts->linksdir = mu_util_dir_expand (opts->linksdir);
		g_free(old);
	}

	return TRUE;
}

void
mu_config_uninit (MuConfigOptions *opts)
{
	g_return_if_fail (opts);

	g_free (opts->muhome);
	g_free (opts->xpath);
	g_free (opts->maildir);
	g_free (opts->linksdir);

	g_strfreev (opts->params);
}


