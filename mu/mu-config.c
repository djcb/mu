/* -*-Mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

/*
** Copyright (C) 2008-2012 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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


static MuConfig MU_CONFIG;

#define DEFAULT_SUMMARY_LEN 5


static MuConfigFormat
get_output_format (const char *formatstr)
{
	int i;
	struct {
		const char*	name;
		MuConfigFormat	format;
	} formats [] = {
		{"mutt-alias",  MU_CONFIG_FORMAT_MUTT_ALIAS},
		{"mutt-ab",	MU_CONFIG_FORMAT_MUTT_AB},
		{"wl",		MU_CONFIG_FORMAT_WL},
		{"csv",		MU_CONFIG_FORMAT_CSV},
		{"org-contact", MU_CONFIG_FORMAT_ORG_CONTACT},
		{"bbdb",	MU_CONFIG_FORMAT_BBDB},
		{"links",	MU_CONFIG_FORMAT_LINKS},
		{"plain",	MU_CONFIG_FORMAT_PLAIN},
		{"sexp",	MU_CONFIG_FORMAT_SEXP},
		{"xml",		MU_CONFIG_FORMAT_XML},
		{"xquery",	MU_CONFIG_FORMAT_XQUERY}
	};

	for (i = 0; i != G_N_ELEMENTS(formats); i++)
		if (strcmp (formats[i].name, formatstr) == 0)
			return formats[i].format;

	return MU_CONFIG_FORMAT_UNKNOWN;
}


static void
set_group_mu_defaults (void)
{
	gchar *exp;

	if (!MU_CONFIG.muhome)
		MU_CONFIG.muhome = mu_util_guess_mu_homedir();

	exp = mu_util_dir_expand(MU_CONFIG.muhome);
	if (exp) {
		g_free(MU_CONFIG.muhome);
		MU_CONFIG.muhome = exp;
	}

	/* check for the MU_NOCOLOR env var; but in any case don't
	 * use colors unless we're writing to a tty */
	if (g_getenv (MU_NOCOLOR) != NULL)
		MU_CONFIG.nocolor = TRUE;

	if (!isatty(fileno(stdout)))
		MU_CONFIG.nocolor = TRUE;

}

static GOptionGroup*
config_options_group_mu (void)
{
	GOptionGroup *og;
	GOptionEntry entries[] = {
		{"debug", 'd', 0, G_OPTION_ARG_NONE, &MU_CONFIG.debug,
		 "print debug output to standard error (false)", NULL},
		{"quiet", 'q', 0, G_OPTION_ARG_NONE, &MU_CONFIG.quiet,
		 "don't give any progress information (false)", NULL},
		{"version", 'v', 0, G_OPTION_ARG_NONE, &MU_CONFIG.version,
		 "display version and copyright information (false)", NULL},
		{"muhome", 0, 0, G_OPTION_ARG_FILENAME, &MU_CONFIG.muhome,
		 "specify an alternative mu directory", NULL},
		{"log-stderr", 0, 0, G_OPTION_ARG_NONE, &MU_CONFIG.log_stderr,
		 "log to standard error (false)", NULL},
		{"nocolor", 0, 0, G_OPTION_ARG_NONE, &MU_CONFIG.nocolor,
		 "don't use ANSI-colors in some output (false)", NULL},

		{G_OPTION_REMAINING, 0, 0, G_OPTION_ARG_STRING_ARRAY,
		 &MU_CONFIG.params, "parameters", NULL},
		{NULL, 0, 0, 0, NULL, NULL, NULL}
	};

	og = g_option_group_new("mu", "general mu options", "", NULL, NULL);
	g_option_group_add_entries(og, entries);

	return og;
}

static void
set_group_index_defaults (void)
{
	char *exp;

	if (!MU_CONFIG.maildir)
		MU_CONFIG.maildir = mu_util_guess_maildir ();

	if (MU_CONFIG.maildir) {
		exp = mu_util_dir_expand(MU_CONFIG.maildir);
		if (exp) {
			g_free(MU_CONFIG.maildir);
			MU_CONFIG.maildir = exp;
		}
	}
}

static GOptionGroup*
config_options_group_index (void)
{
	GOptionGroup *og;
	GOptionEntry entries[] = {
		{"maildir", 'm', 0, G_OPTION_ARG_FILENAME, &MU_CONFIG.maildir,
		 "top of the maildir", NULL},
		{"reindex", 0, 0, G_OPTION_ARG_NONE, &MU_CONFIG.reindex,
		 "index even already indexed messages (false)", NULL},
		{"rebuild", 0, 0, G_OPTION_ARG_NONE, &MU_CONFIG.rebuild,
		 "rebuild the database from scratch (false)", NULL},
		{"my-address", 0, 0, G_OPTION_ARG_STRING_ARRAY,&MU_CONFIG.my_addresses,
		 "my e-mail address (regexp); can be used multiple times", NULL},
		{"autoupgrade", 0, 0, G_OPTION_ARG_NONE, &MU_CONFIG.autoupgrade,
		 "auto-upgrade the database with new mu versions (false)",
		 NULL},
		{"nocleanup", 0, 0, G_OPTION_ARG_NONE, &MU_CONFIG.nocleanup,
		 "don't clean up the database after indexing (false)", NULL},
		{"xbatchsize", 0, 0, G_OPTION_ARG_INT, &MU_CONFIG.xbatchsize,
		 "set transaction batchsize for xapian commits (0)", NULL},
		{"max-msg-size", 0, 0, G_OPTION_ARG_INT, &MU_CONFIG.max_msg_size,
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
set_group_find_defaults (void)
{
	/* note, when no fields are specified, we use
	 * date-from-subject, and sort descending by date. If fields
	 * *are* specified, we sort in ascending order. */
	if (!MU_CONFIG.fields) {
		MU_CONFIG.fields = "d f s";
		if (!MU_CONFIG.sortfield)
			MU_CONFIG.sortfield = "d";
	}

	if (!MU_CONFIG.formatstr) /* by default, use plain output */
		MU_CONFIG.format = MU_CONFIG_FORMAT_PLAIN;
	else
		MU_CONFIG.format =
			get_output_format (MU_CONFIG.formatstr);

	if (MU_CONFIG.linksdir) {
		gchar *old = MU_CONFIG.linksdir;
		MU_CONFIG.linksdir = mu_util_dir_expand(MU_CONFIG.linksdir);
		if (!MU_CONFIG.linksdir)	/* we'll check the dir later */
			MU_CONFIG.linksdir = old;
		else
			g_free(old);
	}

	if ((MU_CONFIG.summary && !MU_CONFIG.summary_len)||
	    (MU_CONFIG.summary_len < 1))
		MU_CONFIG.summary_len = DEFAULT_SUMMARY_LEN;
}

static GOptionGroup*
config_options_group_find (void)
{
	GOptionGroup *og;
	GOptionEntry entries[] = {
		{"fields", 'f', 0, G_OPTION_ARG_STRING, &MU_CONFIG.fields,
		 "fields to display in the output", NULL},
		{"sortfield", 's', 0, G_OPTION_ARG_STRING, &MU_CONFIG.sortfield,
		 "field to sort on", NULL},
		{"threads", 't', 0, G_OPTION_ARG_NONE, &MU_CONFIG.threads,
		 "show message threads", NULL},
		{"bookmark", 'b', 0, G_OPTION_ARG_STRING, &MU_CONFIG.bookmark,
		 "use a bookmarked query", NULL},
		{"reverse", 'z', 0, G_OPTION_ARG_NONE, &MU_CONFIG.reverse,
		 "sort in reverse (descending) order (z -> a)", NULL},
		{"summary", 'k', 0, G_OPTION_ARG_NONE, &MU_CONFIG.summary,
		 "include a short summary of the message (false)", NULL},
		{"summary-len", 0, 0, G_OPTION_ARG_INT, &MU_CONFIG.summary_len,
		 "use up to <n> lines for the summary (5)", NULL},
		{"linksdir", 0, 0, G_OPTION_ARG_STRING, &MU_CONFIG.linksdir,
		 "output as symbolic links to a target maildir", NULL},
		{"clearlinks", 0, 0, G_OPTION_ARG_NONE, &MU_CONFIG.clearlinks,
		 "clear old links before filling a linksdir (false)", NULL},
		{"format", 'o', 0, G_OPTION_ARG_STRING, &MU_CONFIG.formatstr,
		 "output format ('plain'(*), 'links', 'xml',"
		 "'sexp', 'xquery')", NULL},
		{"exec", 'e', 0, G_OPTION_ARG_STRING, &MU_CONFIG.exec,
		 "execute command on each match message", NULL},
		{"include-unreable", 0, 0, G_OPTION_ARG_NONE,
		 &MU_CONFIG.include_unreadable,
		 "don't ignore messages without a disk file (false)", NULL},
		{NULL, 0, 0, 0, NULL, NULL, NULL}
	};

	og = g_option_group_new("find",
				"options for the 'find' command",
				"", NULL, NULL);
	g_option_group_add_entries(og, entries);

	return og;
}

static GOptionGroup *
config_options_group_mkdir (void)
{
	GOptionGroup *og;
	GOptionEntry entries[] = {
		{"mode", 0, 0, G_OPTION_ARG_INT, &MU_CONFIG.dirmode,
		 "set the mode (as in chmod), in octal notation", NULL},
		{NULL, 0, 0, 0, NULL, NULL, NULL}
	};

	/* set dirmode before, because '0000' is a valid mode */
	MU_CONFIG.dirmode = 0755;

	og = g_option_group_new("mkdir", "options for the 'mkdir' command",
				"", NULL, NULL);
	g_option_group_add_entries(og, entries);

	return og;
}


static void
set_group_cfind_defaults (void)
{
	if (!MU_CONFIG.formatstr) /* by default, use plain output */
		MU_CONFIG.format = MU_CONFIG_FORMAT_PLAIN;
	else
		MU_CONFIG.format  = get_output_format (MU_CONFIG.formatstr);

}


static GOptionGroup *
config_options_group_cfind (void)
{
	GOptionGroup *og;
	GOptionEntry entries[] = {
		{"format", 'o', 0, G_OPTION_ARG_STRING, &MU_CONFIG.formatstr,
		 "output format ('plain'(*), 'mutt', 'wanderlust',"
		 "'org-contact', 'csv')", NULL},
		{NULL, 0, 0, 0, NULL, NULL, NULL}
	};

	og = g_option_group_new("cfind", "options for the 'cfind' command",
				"", NULL, NULL);
	g_option_group_add_entries(og, entries);

	return og;
}


static void
set_group_view_defaults (void)
{
	if (!MU_CONFIG.formatstr) /* by default, use plain output */
		MU_CONFIG.format = MU_CONFIG_FORMAT_PLAIN;
	else
		MU_CONFIG.format  = get_output_format (MU_CONFIG.formatstr);

	if ((MU_CONFIG.summary && !MU_CONFIG.summary_len)||
	    (MU_CONFIG.summary_len < 1))
		MU_CONFIG.summary_len = DEFAULT_SUMMARY_LEN;
}

static GOptionGroup *
config_options_group_view (void)
{
	GOptionGroup *og;
	GOptionEntry entries[] = {
		{"summary", 0, 0, G_OPTION_ARG_NONE, &MU_CONFIG.summary,
		 "only show a short summary of the message (false)", NULL},
		{"summary-len", 0, 0, G_OPTION_ARG_INT, &MU_CONFIG.summary_len,
		 "use up to <n> lines for the summary (5)", NULL},
		{"terminate", 0, 0, G_OPTION_ARG_NONE, &MU_CONFIG.terminator,
		 "terminate messages with ascii-0x07 (\\f, form-feed)", NULL},
		{"format", 'o', 0, G_OPTION_ARG_STRING, &MU_CONFIG.formatstr,
		 "output format ('plain'(*), 'sexp')", NULL},
		{NULL, 0, 0, 0, NULL, NULL, NULL}
	};

	og = g_option_group_new("view", "options for the 'view' command",
				"", NULL, NULL);
	g_option_group_add_entries(og, entries);

	return og;
}



static GOptionGroup*
config_options_group_extract (void)
{
	GOptionGroup *og;
	GOptionEntry entries[] = {
		{"save-attachments", 'a', 0, G_OPTION_ARG_NONE,
		 &MU_CONFIG.save_attachments,
		 "save all attachments (false)", NULL},
		{"save-all", 0, 0, G_OPTION_ARG_NONE, &MU_CONFIG.save_all,
		 "save all parts (incl. non-attachments) (false)", NULL},
		{"parts", 0, 0, G_OPTION_ARG_STRING, &MU_CONFIG.parts,
		 "save specific parts (comma-separated list)", NULL},
		{"target-dir", 0, 0, G_OPTION_ARG_FILENAME, &MU_CONFIG.targetdir,
		 "target directory for saving", NULL},
		{"overwrite", 0, 0, G_OPTION_ARG_NONE, &MU_CONFIG.overwrite,
		 "overwrite existing files (false)", NULL},
		{"play", 0, 0, G_OPTION_ARG_NONE, &MU_CONFIG.play,
		 "try to 'play' (open) the extracted parts", NULL},
		{NULL, 0, 0, 0, NULL, NULL, NULL}
	};

	MU_CONFIG.targetdir = g_strdup(".");	/* default is the current dir */

	og = g_option_group_new("extract",
				"options for the 'extract' command",
				"", NULL, NULL);
	g_option_group_add_entries(og, entries);

	return og;
}


static GOptionGroup*
config_options_group_server (void)
{
	GOptionGroup *og;
	GOptionEntry entries[] = {
		{"maildir", 'm', 0, G_OPTION_ARG_FILENAME, &MU_CONFIG.maildir,
		 "top of the maildir", NULL},
		{NULL, 0, 0, 0, NULL, NULL, NULL}
	};

	og = g_option_group_new("server",
				"options for the 'server' command",
				"", NULL, NULL);
	g_option_group_add_entries(og, entries);

	return og;
}



static gboolean
parse_cmd (int *argcp, char ***argvp)
{
	int i;
	struct {
		const gchar*	_name;
		MuConfigCmd	_cmd;
	} cmd_map[] = {
		{ "cfind",   MU_CONFIG_CMD_CFIND },
		{ "extract", MU_CONFIG_CMD_EXTRACT },
		{ "find",    MU_CONFIG_CMD_FIND },
		{ "index",   MU_CONFIG_CMD_INDEX },
		{ "mkdir",   MU_CONFIG_CMD_MKDIR },
		{ "view",    MU_CONFIG_CMD_VIEW },
		{ "add",     MU_CONFIG_CMD_ADD },
		{ "remove",  MU_CONFIG_CMD_REMOVE },
		{ "server",  MU_CONFIG_CMD_SERVER }
	};

	MU_CONFIG.cmd	 = MU_CONFIG_CMD_NONE;
	MU_CONFIG.cmdstr = NULL;

	if (*argcp < 2) /* no command found at all */
		return TRUE;
	else if ((**argvp)[1] == '-')
		/* if the first param starts with '-', there is no
		 * command, just some option (like --version, --help
		 * etc.)*/
		return TRUE;

	MU_CONFIG.cmd    = MU_CONFIG_CMD_UNKNOWN;
	MU_CONFIG.cmdstr = (*argvp)[1];

	for (i = 0; i != G_N_ELEMENTS(cmd_map); ++i)
		if (strcmp (MU_CONFIG.cmdstr, cmd_map[i]._name) == 0)
			MU_CONFIG.cmd = cmd_map[i]._cmd;

	return TRUE;
}


static void
add_context_group (GOptionContext *context)
{
	GOptionGroup *group;

	switch (MU_CONFIG.cmd) {
	case MU_CONFIG_CMD_INDEX:
		group = config_options_group_index();
		break;
	case MU_CONFIG_CMD_FIND:
		group = config_options_group_find();
		break;
	case MU_CONFIG_CMD_MKDIR:
		group = config_options_group_mkdir();
		break;
	case MU_CONFIG_CMD_EXTRACT:
		group = config_options_group_extract();
		break;
	case MU_CONFIG_CMD_CFIND:
		group = config_options_group_cfind();
		break;
	case MU_CONFIG_CMD_VIEW:
		group = config_options_group_view();
		break;
	case MU_CONFIG_CMD_SERVER:
		group = config_options_group_server();
		break;
	default:
		return; /* no group to add */
	}

	g_option_context_add_group(context, group);
}


static gboolean
parse_params (int *argcp, char ***argvp)
{
	GError *err = NULL;
	GOptionContext *context;
	gboolean rv;

	context = g_option_context_new("- mu general option");
	g_option_context_set_main_group(context, config_options_group_mu());

	add_context_group (context);

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
mu_config_init (int *argcp, char ***argvp)
{
	g_return_val_if_fail (argcp && argvp, NULL);

	memset (&MU_CONFIG, 0, sizeof(MU_CONFIG));

	if (!parse_cmd (argcp, argvp) ||
	    !parse_params(argcp, argvp)) {
		mu_config_uninit (&MU_CONFIG);
		return NULL;
	}

	/* fill in the defaults if user did not specify */
	set_group_mu_defaults();
	set_group_index_defaults();
	set_group_find_defaults();
	set_group_cfind_defaults();
	set_group_view_defaults();
	/* set_group_mkdir_defaults (config); */

	return &MU_CONFIG;
}


void
mu_config_uninit (MuConfig *opts)
{
	if (!opts)
		return;

	g_free (opts->muhome);
	g_free (opts->maildir);
	g_free (opts->linksdir);
	g_free (opts->targetdir);

	g_strfreev (opts->params);

	memset (opts, 0, sizeof(MU_CONFIG));
}


size_t
mu_config_param_num (MuConfig *opts)
{
	size_t n;

	g_return_val_if_fail (opts && opts->params, 0);
	for (n = 0; opts->params[n]; ++n);

	return n;
}
