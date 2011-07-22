/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/
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
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include <glib.h>
#include <string.h>		/* memset */
#include <unistd.h>
#include <stdio.h>

#include "mu-util.h"
#include "mu-config.h"
#include "mu-cmd.h"

static void
set_group_mu_defaults (MuConfig *opts)
{
	gchar *exp;

	if (!opts->muhome)
		opts->muhome = mu_util_guess_mu_homedir();

	exp = mu_util_dir_expand(opts->muhome);
	if (exp) {
		g_free(opts->muhome);
		opts->muhome = exp;
	}


	/* check for the MU_COLORS env var; but in any case don't use
	 * colors unless we're writing to a tty */
	
	if (g_getenv (MU_COLORS) != NULL)
		opts->color = TRUE;
	
	if (!isatty(fileno(stdout)))
		opts->color = FALSE;

}

static GOptionGroup*
config_options_group_mu (MuConfig *opts)
{
	GOptionGroup *og;
	GOptionEntry entries[] = {
		{"debug", 'd', 0, G_OPTION_ARG_NONE, &opts->debug,
		 "print debug output to standard error (false)", NULL},
		{"quiet", 'q', 0, G_OPTION_ARG_NONE, &opts->quiet,
		 "don't give any progress information (false)", NULL},
		{"version", 'v', 0, G_OPTION_ARG_NONE, &opts->version,
		 "display version and copyright information (false)", NULL},
		{"muhome", 0, 0, G_OPTION_ARG_FILENAME, &opts->muhome,
		 "specify an alternative mu directory", NULL},
		{"log-stderr", 0, 0, G_OPTION_ARG_NONE, &opts->log_stderr,
		 "log to standard error (false)", NULL},
		{"color", 0, 0, G_OPTION_ARG_NONE, &opts->color,
		 "use ANSI-colors in some output (false)", NULL},
		
		{G_OPTION_REMAINING, 0, 0, G_OPTION_ARG_STRING_ARRAY,
		 &opts->params, "parameters", NULL},
		{NULL, 0, 0, 0, NULL, NULL, NULL}
	};

	og = g_option_group_new("mu", "general mu options", "", NULL, NULL);
	g_option_group_add_entries(og, entries);

	return og;
}

static void
set_group_index_defaults (MuConfig * opts)
{
	/* note: opts->maildir is handled in mu-cmd, as we need to
	 * have a MuIndex entry for mu_index_last_used_maildir ()
	 */
	opts->xbatchsize = 0;
}

static GOptionGroup*
config_options_group_index (MuConfig * opts)
{
	GOptionGroup *og;
	GOptionEntry entries[] = {
		{"maildir", 'm', 0, G_OPTION_ARG_FILENAME, &opts->maildir,
		 "top of the maildir", NULL},
		{"reindex", 0, 0, G_OPTION_ARG_NONE, &opts->reindex,
		 "index even already indexed messages (false)", NULL},
		{"rebuild", 0, 0, G_OPTION_ARG_NONE, &opts->rebuild,
		 "rebuild the database from scratch (false)", NULL},
		{"autoupgrade", 0, 0, G_OPTION_ARG_NONE, &opts->autoupgrade,
		 "auto-upgrade the database with new mu versions (false)",
		 NULL},
		{"nocleanup", 0, 0, G_OPTION_ARG_NONE, &opts->nocleanup,
		 "don't clean up the database after indexing (false)", NULL},
		{"xbatchsize", 0, 0, G_OPTION_ARG_INT, &opts->xbatchsize,
		 "set transaction batchsize for xapian commits (0)", NULL},
		{"max-msg-size", 0, 0, G_OPTION_ARG_INT, &opts->max_msg_size,
		 "set the maximum size for message files", NULL},
		{NULL, 0, 0, 0, NULL, NULL, NULL}
	};

	og = g_option_group_new("index",
				"options for the 'index' command",
				"", NULL, NULL);
	g_option_group_add_entries(og, entries);

	return og;
}

static void
set_group_find_defaults (MuConfig *opts)
{
	/* note, when no fields are specified, we use
	 * date-from-subject, and sort descending by date. If fields
	 * *are* specified, we sort in ascending order. */
	if (!opts->fields) {
		opts->fields = "d f s";
		if (!opts->sortfield) {
			opts->sortfield = "d";
			opts->descending = TRUE;
		}
	}

	if (!opts->formatstr) /* by default, use plain output */
		opts->formatstr = MU_CONFIG_FORMAT_PLAIN;
		
	if (opts->linksdir) {
		gchar *old = opts->linksdir;
		opts->linksdir = mu_util_dir_expand(opts->linksdir);
		if (!opts->linksdir)	/* we'll check the dir later */
			opts->linksdir = old;
		else
			g_free(old);
	}
}

static GOptionGroup*
config_options_group_find (MuConfig *opts)
{
	GOptionGroup *og;
	GOptionEntry entries[] = {
		{"fields", 'f', 0, G_OPTION_ARG_STRING, &opts->fields,
		 "fields to display in the output", NULL},
		{"sortfield", 's', 0, G_OPTION_ARG_STRING, &opts->sortfield,
		 "field to sort on", NULL},
		{"threads", 't', 0, G_OPTION_ARG_NONE, &opts->threads,
		 "show message threads", NULL},
		{"bookmark", 'b', 0, G_OPTION_ARG_STRING, &opts->bookmark,
		 "use a bookmarked query", NULL},
		{"descending", 'z', 0, G_OPTION_ARG_NONE, &opts->descending,
		 "sort in descending order (z -> a)", NULL},
		{"summary", 'k', 0, G_OPTION_ARG_NONE, &opts->summary,
		 "include a short summary of the message (false)", NULL},
		{"linksdir", 0, 0, G_OPTION_ARG_STRING, &opts->linksdir,
		 "output as symbolic links to a target maildir", NULL},
		{"clearlinks", 0, 0, G_OPTION_ARG_NONE, &opts->clearlinks,
		 "clear old links before filling a linksdir (false)", NULL},
		{"format", 'o', 0, G_OPTION_ARG_STRING, &opts->formatstr,
		 "output format ('plain'(*), 'links', 'xml',"
		 "'json', 'sexp', 'xquery')", NULL},
		{"exec", 'e', 0, G_OPTION_ARG_STRING, &opts->exec,
		 "execute command on each match message", NULL},
		{NULL, 0, 0, 0, NULL, NULL, NULL}
	};

	og = g_option_group_new("find",
				"options for the 'find' command",
				"", NULL, NULL);
	g_option_group_add_entries(og, entries);

	return og;
}

static GOptionGroup *
config_options_group_mkdir (MuConfig *opts)
{
	GOptionGroup *og;
	GOptionEntry entries[] = {
		{"mode", 0, 0, G_OPTION_ARG_INT, &opts->dirmode,
		 "set the mode (as in chmod), in octal notation", NULL},
		{NULL, 0, 0, 0, NULL, NULL, NULL}
	};

	/* set dirmode before, because '0000' is a valid mode */
	opts->dirmode = 0755;

	og = g_option_group_new("mkdir", "options for the 'mkdir' command",
				"", NULL, NULL);
	g_option_group_add_entries(og, entries);

	return og;
}


static void
set_group_cfind_defaults (MuConfig *opts)
{
	if (!opts->formatstr) /* by default, use plain output */
		opts->formatstr = MU_CONFIG_FORMAT_PLAIN;
}


static GOptionGroup *
config_options_group_cfind (MuConfig *opts)
{
	GOptionGroup *og;
	GOptionEntry entries[] = {
		{"format", 'o', 0, G_OPTION_ARG_STRING, &opts->formatstr,
		 "output format ('plain'(*), 'mutt', 'wanderlust',"
		 "'org-contact', 'csv')", NULL},
		{NULL, 0, 0, 0, NULL, NULL, NULL}
	};
		
	og = g_option_group_new("cfind", "options for the 'cfind' command",
				"", NULL, NULL);
	g_option_group_add_entries(og, entries);

	return og;
}


static GOptionGroup *
config_options_group_view (MuConfig *opts)
{
	GOptionGroup *og;
	GOptionEntry entries[] = {
		{"summary", 0, 0, G_OPTION_ARG_NONE, &opts->summary,
		 "only show a short summary of the message (false)", NULL},
		{"terminate", 0, 0, G_OPTION_ARG_NONE, &opts->terminator,
		 "terminate messages with ascii-0x07 (\\f, form-feed)", NULL},
		{NULL, 0, 0, 0, NULL, NULL, NULL}
	};
		
	og = g_option_group_new("view", "options for the 'view' command",
				"", NULL, NULL);
	g_option_group_add_entries(og, entries);

	return og;
}



static GOptionGroup*
config_options_group_extract (MuConfig *opts)
{
	GOptionGroup *og;
	GOptionEntry entries[] = {
		{"save-attachments", 'a', 0, G_OPTION_ARG_NONE,
		 &opts->save_attachments,
		 "save all attachments (false)", NULL},
		{"save-all", 0, 0, G_OPTION_ARG_NONE, &opts->save_all,
		 "save all parts (incl. non-attachments) (false)", NULL},
		{"parts", 0, 0, G_OPTION_ARG_STRING, &opts->parts,
		 "save specific parts (comma-separated list)", NULL},
		{"target-dir", 0, 0, G_OPTION_ARG_FILENAME, &opts->targetdir,
		 "target directory for saving", NULL},
		{"overwrite", 0, 0, G_OPTION_ARG_NONE, &opts->overwrite,
		 "overwrite existing files (false)", NULL},
		{"play", 0, 0, G_OPTION_ARG_NONE, &opts->play,
		 "try to 'play' (open) the extracted parts", NULL},
		{NULL, 0, 0, 0, NULL, NULL, NULL}
	};

	opts->targetdir = g_strdup(".");	/* default is the current dir */

	og = g_option_group_new("extract",
				"options for the 'extract' command",
				"", NULL, NULL);
	g_option_group_add_entries(og, entries);

	return og;
}


static gboolean  
parse_cmd (MuConfig *opts, int *argcp, char ***argvp)
{
	int i;
	typedef struct {
		const gchar*	_name;
		MuConfigCmd	_cmd;
	} Cmd;
		
	Cmd cmd_map[] = {
		{ "index",   MU_CONFIG_CMD_INDEX },
		{ "find",    MU_CONFIG_CMD_FIND },
		{ "cleanup", MU_CONFIG_CMD_CLEANUP },
		{ "mkdir",   MU_CONFIG_CMD_MKDIR },
		{ "view",    MU_CONFIG_CMD_VIEW },
		{ "extract", MU_CONFIG_CMD_EXTRACT },
		{ "cfind",   MU_CONFIG_CMD_CFIND },
	};
		
	opts->cmd	 = MU_CONFIG_CMD_NONE;
	opts->cmdstr = NULL;
		
	if (*argcp < 2) /* no command found at all */
		return TRUE;
	else if ((**argvp)[1] == '-') 
		/* if the first param starts with '-', there is no
		 * command, just some option (like --version, --help
		 * etc.)*/
		return TRUE;
		
	opts->cmd    = MU_CONFIG_CMD_UNKNOWN;
	opts->cmdstr = (*argvp)[1];

	for (i = 0; i != G_N_ELEMENTS(cmd_map); ++i) 
		if (strcmp (opts->cmdstr, cmd_map[i]._name) == 0)
			opts->cmd = cmd_map[i]._cmd;

	return TRUE;
}


static void
add_context_group (GOptionContext *context, MuConfig *opts)
{
	GOptionGroup *group;

	group = NULL;
		
	switch (opts->cmd) {
	case MU_CONFIG_CMD_INDEX:
		group = config_options_group_index (opts);
		break;
	case MU_CONFIG_CMD_FIND:
		group = config_options_group_find (opts);
		break;
	case MU_CONFIG_CMD_MKDIR:
		group = config_options_group_mkdir (opts);
		break;
	case MU_CONFIG_CMD_EXTRACT:
		group = config_options_group_extract (opts);
		break;
	case MU_CONFIG_CMD_CFIND:
		group = config_options_group_cfind (opts);
		break;
	case MU_CONFIG_CMD_VIEW:
		group = config_options_group_view (opts);
		break;
	default: break;
	}

	if (group)
		g_option_context_add_group(context, group);
}


static gboolean
parse_params (MuConfig *opts, int *argcp, char ***argvp)
{
	GError *err = NULL;
	GOptionContext *context;
	gboolean rv;
		
	context = g_option_context_new("- mu general option");
	g_option_context_set_main_group(context,
					config_options_group_mu(opts));

	add_context_group (context, opts);
		
	rv = g_option_context_parse (context, argcp, argvp, &err);
	g_option_context_free (context);
	if (!rv) {
		g_printerr ("mu: error in options: %s\n", err->message);
		g_error_free (err);
		return FALSE;
	}
	return TRUE;
}
				

MuConfig*
mu_config_new (int *argcp, char ***argvp)
{
	MuConfig *config;

	g_return_val_if_fail (argcp && argvp, NULL);
		
	config = g_new0 (MuConfig, 1);
			
	if (!parse_cmd (config, argcp, argvp) ||
	    !parse_params(config, argcp, argvp)) {
		mu_config_destroy (config);
		return NULL;
	}
		
	/* fill in the defaults if user did not specify */
	set_group_mu_defaults (config);
	set_group_index_defaults (config);
	set_group_find_defaults (config);
	set_group_cfind_defaults (config);
	/* set_group_mkdir_defaults (config); */
		
	return config;
}

void
mu_config_destroy (MuConfig *opts)
{
	if (!opts)
		return;
		
	g_free (opts->muhome);
	g_free (opts->maildir);
	g_free (opts->linksdir);
	g_free (opts->targetdir);
	g_strfreev (opts->params);
	g_free (opts);
}


static void
show_usage (gboolean noerror)
{
	const char* usage=
		"usage: mu command [options] [parameters]\n"
		"where command is one of index, find, cfind, view, mkdir, cleanup "
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


MuExitCode
mu_config_execute (MuConfig *opts)
{
	g_return_val_if_fail (opts, MU_EXITCODE_ERROR);
		
	if (opts->version) {
		show_version ();
		return MU_EXITCODE_OK;
	}
		
	if (!opts->params||!opts->params[0]) {/* no command? */
		show_version ();
		g_print ("\n");
		show_usage (TRUE);
		return MU_EXITCODE_ERROR;
	}
		
	switch (opts->cmd) {
	case MU_CONFIG_CMD_CLEANUP:    return mu_cmd_cleanup (opts);
	case MU_CONFIG_CMD_EXTRACT:    return mu_cmd_extract (opts);
	case MU_CONFIG_CMD_FIND:       return mu_cmd_find (opts);
	case MU_CONFIG_CMD_INDEX:      return mu_cmd_index (opts);
	case MU_CONFIG_CMD_MKDIR:      return mu_cmd_mkdir (opts);
	case MU_CONFIG_CMD_VIEW:       return mu_cmd_view (opts);
	case MU_CONFIG_CMD_CFIND:      return mu_cmd_cfind (opts);
	case MU_CONFIG_CMD_UNKNOWN:
		g_printerr ("mu: unknown command '%s'\n\n", opts->cmdstr);
		show_usage (FALSE);
		return MU_EXITCODE_ERROR;
	default:
		g_return_val_if_reached (MU_EXITCODE_ERROR);
	}
}

guint
mu_config_param_num (MuConfig *conf)
{
	guint u;
	
	g_return_val_if_fail (conf, 0);
	
	for (u = 0; conf->params[u]; ++u);

	return u;
}
