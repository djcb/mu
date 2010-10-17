/*
** Copyright (C) 2008-2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include <glib.h>
#include <string.h> /* memset */

#include "mu-util.h"
#include "mu-config.h"


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

static void
set_group_mu_defaults (MuConfigOptions *opts)
{
	if (!opts->muhome)
		opts->muhome = guess_muhome ();
	
	/* note: xpath is is *not* settable from the cmdline */
	opts->xpath = g_strdup_printf ("%s%c%s", opts->muhome,G_DIR_SEPARATOR,
				       MU_XAPIAN_DIR_NAME);
}


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
		{"muhome", 0, 0, G_OPTION_ARG_FILENAME, &opts->muhome,
		 "specify an alternative mu directory", NULL},
		{"log-stderr", 0, 0, G_OPTION_ARG_NONE, &opts->log_stderr,
		 "log to standard error", NULL},
		{ G_OPTION_REMAINING, 0, 0, G_OPTION_ARG_STRING_ARRAY,
		  &opts->params, "parameters", NULL },
		{ NULL, 0, 0, 0, NULL, NULL, NULL }
	};

	og = g_option_group_new ("mu", "general mu options",
				 "", NULL, NULL);	
	g_option_group_add_entries (og, entries);
	
	return og;
}


static void
set_group_index_defaults (MuConfigOptions *opts)
{
	gchar *old;
	
	old = opts->maildir;
	if (opts->maildir)
		opts->maildir = mu_util_dir_expand (opts->maildir);
	else
		opts->maildir = mu_util_guess_maildir();

	/* note, this may be an invalid dir, but we're checking for
	 * validity of the dir later */
	if (!opts->maildir)
		opts->maildir = old;
	else
		g_free (old);
}



static GOptionGroup*
config_options_group_index (MuConfigOptions *opts)
{
	GOptionGroup *og;
	GOptionEntry entries[] = {
		{"maildir", 'm', 0, G_OPTION_ARG_FILENAME, &opts->maildir,
		 "top of the maildir", NULL},
		{"reindex", 0, 0, G_OPTION_ARG_NONE, &opts->reindex,
		 "index even already indexed messages", NULL},
		{"rebuild", 0, 0, G_OPTION_ARG_NONE, &opts->rebuild,
		 "rebuild the database from scratch", NULL},
		{"autoupgrade", 0, 0, G_OPTION_ARG_NONE, &opts->autoupgrade,
		 "automatically upgrade the database with new mu versions", NULL},
		{"nocleanup", 0, 0, G_OPTION_ARG_NONE, &opts->nocleanup,
		 "don't clean up the database after indexing", NULL},
		{ NULL, 0, 0, 0, NULL, NULL, NULL }
	};
	
	og = g_option_group_new ("index",
				 "options for the 'index' command",
				 "", NULL, NULL);	
	g_option_group_add_entries (og, entries);
	
	return og;
}


static void
set_group_find_defaults (MuConfigOptions *opts)
{
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
		if (!opts->linksdir) /* we'll check the dir later */
			opts->linksdir = old;
		else
			g_free(old);
	}

	/* FIXME: some warning when summary_len < 0? */
	if (opts->summary_len < 1)
		opts->summary_len = 0;
}


static GOptionGroup*
config_options_group_find (MuConfigOptions *opts)
{
	GOptionGroup *og;
	GOptionEntry entries[] = {
		{"xquery", 0, 0, G_OPTION_ARG_NONE, &opts->xquery,
		 "print the Xapian query (for debugging)", NULL},
		{"fields", 'f', 0, G_OPTION_ARG_STRING, &opts->fields,
		 "fields to display in the output", NULL},
		{"sortfield", 's', 0, G_OPTION_ARG_STRING, &opts->sortfield,
		 "field to sort on", NULL},
		{"descending", 'z', 0, G_OPTION_ARG_NONE, &opts->descending,
		 "sort in descending order (z -> a)", NULL},
		{"summary-len", 'k', 0, G_OPTION_ARG_INT, &opts->summary_len,
		 "max number of lines for summary", NULL},
		{"linksdir", 0, 0, G_OPTION_ARG_STRING, &opts->linksdir,
		 "output as symbolic links to a target maildir", NULL },
		{"clearlinks", 0, 0, G_OPTION_ARG_NONE, &opts->clearlinks,
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
		{"mode", 0, 0, G_OPTION_ARG_INT, &opts->dirmode,
		 "set the mode (as in chmod), in octal notation", NULL},
		{ NULL, 0, 0, 0, NULL, NULL, NULL }
	};

	/* set dirmode before, because '0000' is a valid mode */
	opts->dirmode = 0755;
	
	og = g_option_group_new ("mkdir",
				 "options for the 'mkdir' command",
				 "", NULL, NULL);	
	g_option_group_add_entries (og, entries);
	
	return og;
}


static GOptionGroup*
config_options_group_extract (MuConfigOptions *opts)
{
	GOptionGroup *og;
	GOptionEntry entries[] = {
		{"save-attachments", 'a', 0, G_OPTION_ARG_NONE, &opts->save_attachments,
		 "save all attachments", NULL},
		{"save-all", 0, 0, G_OPTION_ARG_NONE, &opts->save_all,
		 "save all parts (incl. non-attachments)", NULL},
		{"parts", 0, 0, G_OPTION_ARG_STRING, &opts->parts,
		 "save specific parts", NULL},
		{"target-dir", 0, 0, G_OPTION_ARG_FILENAME, &opts->targetdir,
		 "target directory for saving", NULL},
		{"overwrite", 0, 0, G_OPTION_ARG_NONE, &opts->overwrite,
		 "overwrite existing files", NULL},
		{ NULL, 0, 0, 0, NULL, NULL, NULL }
	};

	opts->targetdir = g_strdup("."); /* default is the current dir */
	
	og = g_option_group_new ("extract",
				 "options for the 'extract' command",
				 "", NULL, NULL);	
	g_option_group_add_entries (og, entries);
	
	return og;
}





static gboolean
parse_params (MuConfigOptions *opts, int *argcp, char ***argvp)
{
	GError *error = NULL;
	GOptionContext *context;
	gboolean rv;
	
	context = g_option_context_new ("- mu general option");

	g_option_context_set_main_group (context,
					 config_options_group_mu (opts));
	g_option_context_add_group (context,
				    config_options_group_index (opts));
	g_option_context_add_group (context,
				    config_options_group_find (opts));
	g_option_context_add_group (context,
				    config_options_group_mkdir (opts));
	g_option_context_add_group (context,
				    config_options_group_extract (opts));
	
	rv = g_option_context_parse (context, argcp, argvp, &error);
	if (!rv) {
		g_printerr ("error in options: %s\n", error->message);
		g_error_free (error);
	} else
		g_option_context_free (context);

	/* fill in the defaults if user did not specify */
	set_group_mu_defaults (opts);
	set_group_index_defaults (opts);
	set_group_find_defaults (opts);
	/* set_group_mkdir_defaults (opts); */
	
	return rv;
}


gboolean
mu_config_init (MuConfigOptions *opts, int *argcp, char ***argvp)
{
	g_return_val_if_fail (opts, FALSE);	
	memset (opts, 0, sizeof(MuConfigOptions));	

	/* defaults are set in parse_params */
	
	if (argcp && argvp)
		if (!parse_params (opts, argcp, argvp))
			return FALSE;
		
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


