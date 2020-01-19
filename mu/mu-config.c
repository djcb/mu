/*
** Copyright (C) 2008-2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-config.h"
#include "mu-cmd.h"

static MuConfig MU_CONFIG;

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
		{"json",	MU_CONFIG_FORMAT_JSON},
		{"xml",		MU_CONFIG_FORMAT_XML},
		{"xquery",	MU_CONFIG_FORMAT_XQUERY},
		{"mquery",	MU_CONFIG_FORMAT_MQUERY},
		{"debug",	MU_CONFIG_FORMAT_DEBUG}
	};

	for (i = 0; i != G_N_ELEMENTS(formats); i++)
		if (strcmp (formats[i].name, formatstr) == 0)
			return formats[i].format;

	return MU_CONFIG_FORMAT_UNKNOWN;
}


static void
set_group_mu_defaults (void)
{
	/* If muhome is not set, we use the XDG Base Directory Specification
	 * locations. */

	if (MU_CONFIG.muhome) {
		gchar *exp;
		exp = mu_util_dir_expand(MU_CONFIG.muhome);
		if (exp) {
			g_free(MU_CONFIG.muhome);
			MU_CONFIG.muhome = exp;
		}
	}

	/* check for the MU_NOCOLOR or NO_COLOR env vars; but in any case don't
	 * use colors unless we're writing to a tty */
	if (g_getenv (MU_NOCOLOR) != NULL || g_getenv ("NO_COLOR") != NULL)
		MU_CONFIG.nocolor = TRUE;

	if (!isatty(fileno(stdout)) || !isatty(fileno(stderr)))
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
		{"version", 0, 0, G_OPTION_ARG_NONE, &MU_CONFIG.version,
		 "display version and copyright information (false)", NULL},
		{"muhome", 0, 0, G_OPTION_ARG_FILENAME, &MU_CONFIG.muhome,
		 "specify an alternative mu directory", "<dir>"},
		{"log-stderr", 0, 0, G_OPTION_ARG_NONE, &MU_CONFIG.log_stderr,
		 "log to standard error (false)", NULL},
		{"nocolor", 0, 0, G_OPTION_ARG_NONE, &MU_CONFIG.nocolor,
		 "don't use ANSI-colors in output (false)", NULL},
		{"verbose", 'v', 0, G_OPTION_ARG_NONE, &MU_CONFIG.verbose,
		 "verbose output (false)", NULL},

		{G_OPTION_REMAINING, 0, 0, G_OPTION_ARG_STRING_ARRAY,
		 &MU_CONFIG.params, "parameters", NULL},
		{NULL, 0, 0, 0, NULL, NULL, NULL}
	};

	og = g_option_group_new("mu", "general mu options", "", NULL, NULL);
	g_option_group_add_entries(og, entries);

	return og;
}


#define expand_dir(D)			                     	\
	if ((D)) {						\
		char *exp;					\
		exp = mu_util_dir_expand((D));			\
		if (exp) {					\
			g_free((D));				\
			(D) = exp;				\
		}						\
	}

static void
set_group_index_defaults (void)
{
	if (!MU_CONFIG.maildir)
		MU_CONFIG.maildir = mu_util_guess_maildir ();

	expand_dir (MU_CONFIG.maildir);
}

static GOptionGroup*
config_options_group_index (void)
{
	GOptionGroup *og;
	GOptionEntry entries[] = {
		{"maildir", 'm', 0, G_OPTION_ARG_FILENAME, &MU_CONFIG.maildir,
		 "top of the maildir", "<maildir>"},
		{"rebuild", 0, 0, G_OPTION_ARG_NONE, &MU_CONFIG.rebuild,
		 "rebuild the database from scratch (false)", NULL},
		{"lazy-check", 0, 0, G_OPTION_ARG_NONE, &MU_CONFIG.lazycheck,
		 "only check dir-timestamps (false)", NULL},
		{"my-address", 0, 0, G_OPTION_ARG_STRING_ARRAY,
		 &MU_CONFIG.my_addresses,
		 "my e-mail address; can be used multiple times",
		 "<address>"},
		{"nocleanup", 0, 0, G_OPTION_ARG_NONE, &MU_CONFIG.nocleanup,
		 "don't clean up the database after indexing (false)", NULL},
		{"max-msg-size", 0, 0, G_OPTION_ARG_INT,
		 &MU_CONFIG.max_msg_size,
		 "set the maximum size for message files", "<size>"},
		{NULL, 0, 0, 0, NULL, NULL, NULL}
	};

	og = g_option_group_new("index",
				"Options for the 'index' command",
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
	if (!MU_CONFIG.fields || !*MU_CONFIG.fields) {
		MU_CONFIG.fields = g_strdup ("d f s");
		if (!MU_CONFIG.sortfield)
			MU_CONFIG.sortfield = g_strdup ("d");
	}

	if (!MU_CONFIG.formatstr) /* by default, use plain output */
		MU_CONFIG.format = MU_CONFIG_FORMAT_PLAIN;
	else
		MU_CONFIG.format =
			get_output_format (MU_CONFIG.formatstr);

	expand_dir (MU_CONFIG.linksdir);

	if (MU_CONFIG.cmd == MU_CONFIG_CMD_MFIND) {
		/* 'mfind' --> find with mu4e defaults */
		MU_CONFIG.include_related = TRUE;
		MU_CONFIG.skip_dups	  = TRUE;
		MU_CONFIG.cmd		  = MU_CONFIG_CMD_FIND;
	}
}

static GOptionGroup*
config_options_group_find (void)
{
	GOptionGroup *og;
	GOptionEntry entries[] = {
		{"fields", 'f', 0, G_OPTION_ARG_STRING, &MU_CONFIG.fields,
		 "fields to display in the output", "<fields>"},
		{"sortfield", 's', 0, G_OPTION_ARG_STRING,
		 &MU_CONFIG.sortfield,
		 "field to sort on", "<field>"},
		{"maxnum", 'n', 0, G_OPTION_ARG_INT, &MU_CONFIG.maxnum,
		 "number of entries to display in the output", "<number>"},
		{"threads", 't', 0, G_OPTION_ARG_NONE, &MU_CONFIG.threads,
		 "show message threads", NULL},
		{"bookmark", 'b', 0, G_OPTION_ARG_STRING, &MU_CONFIG.bookmark,
		 "use a bookmarked query", "<bookmark>"},
		{"reverse", 'z', 0, G_OPTION_ARG_NONE, &MU_CONFIG.reverse,
		 "sort in reverse (descending) order (z -> a)", NULL},
		{"skip-dups", 'u', 0, G_OPTION_ARG_NONE,
		 &MU_CONFIG.skip_dups,
		 "show only the first of messages duplicates (false)", NULL},
		{"include-related", 'r', 0, G_OPTION_ARG_NONE,
		 &MU_CONFIG.include_related,
		 "include related messages in results (false)", NULL},
		{"linksdir", 0, 0, G_OPTION_ARG_STRING, &MU_CONFIG.linksdir,
		 "output as symbolic links to a target maildir", "<dir>"},
		{"clearlinks", 0, 0, G_OPTION_ARG_NONE, &MU_CONFIG.clearlinks,
		 "clear old links before filling a linksdir (false)", NULL},
		{"format", 'o', 0, G_OPTION_ARG_STRING, &MU_CONFIG.formatstr,
		 "output format ('plain'(*), 'links', 'xml',"
		 "'sexp', 'xquery')", "<format>"},
		{"summary-len", 0, 0, G_OPTION_ARG_INT, &MU_CONFIG.summary_len,
		 "use up to <n> lines for the summary, or 0 for none (0)",
		 "<len>"},
		{"exec", 'e', 0, G_OPTION_ARG_STRING, &MU_CONFIG.exec,
		 "execute command on each match message", "<command>"},
		{"after", 0, 0, G_OPTION_ARG_INT, &MU_CONFIG.after,
		 "only show messages whose m_time > T (t_time)",
		 "<timestamp>"},
		{NULL, 0, 0, 0, NULL, NULL, NULL}
	};

	og = g_option_group_new("find",
				"Options for the 'find' command",
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
		 "set the mode (as in chmod), in octal notation", "<mode>"},
		{NULL, 0, 0, 0, NULL, NULL, NULL}
	};

	/* set dirmode before, because '0000' is a valid mode */
	MU_CONFIG.dirmode = 0755;

	og = g_option_group_new("mkdir", "Options for the 'mkdir' command",
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
		 "output format (plain(*), mutt-alias, mutt-ab, wl, "
		 "org-contact, bbdb, csv)", "<format>"},
		{"personal", 0, 0, G_OPTION_ARG_NONE, &MU_CONFIG.personal,
		 "whether to only get 'personal' contacts", NULL},
		{"after", 0, 0, G_OPTION_ARG_INT, &MU_CONFIG.after,
		 "only get addresses last seen after T", "<timestamp>"},
		{NULL, 0, 0, 0, NULL, NULL, NULL}
	};

	og = g_option_group_new("cfind", "Options for the 'cfind' command",
				"", NULL, NULL);
	g_option_group_add_entries(og, entries);

	return og;
}



static GOptionGroup *
config_options_group_script (void)
{
	GOptionGroup *og;
	GOptionEntry entries[] = {
		{G_OPTION_REMAINING, 0,0, G_OPTION_ARG_STRING_ARRAY,
		 &MU_CONFIG.params, "script parameters", NULL},
		{NULL, 0, 0, 0, NULL, NULL, NULL}
	};

	og = g_option_group_new("script", "Options for the 'script' command",
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
}


/* crypto options are used in a few different commands */
static GOptionEntry*
crypto_option_entries (void)
{
	static GOptionEntry entries[] = {
		{"auto-retrieve", 'r', 0, G_OPTION_ARG_NONE,
		 &MU_CONFIG.auto_retrieve,
		 "attempt to retrieve keys online (false)", NULL},
		{"use-agent", 'a', 0, G_OPTION_ARG_NONE, &MU_CONFIG.use_agent,
		 "attempt to use the GPG agent (false)", NULL},
		{"decrypt", 0, 0, G_OPTION_ARG_NONE, &MU_CONFIG.decrypt,
		 "attempt to decrypt the message", NULL},
		{NULL, 0, 0, 0, NULL, NULL, NULL}
	};

	return entries;
}


static GOptionGroup *
config_options_group_view (void)
{
	GOptionGroup *og;
	GOptionEntry entries[] = {
		{"summary-len", 0, 0, G_OPTION_ARG_INT, &MU_CONFIG.summary_len,
		 "use up to <n> lines for the summary, or 0 for none (0)",
		 "<len>"},
		{"terminate", 0, 0, G_OPTION_ARG_NONE, &MU_CONFIG.terminator,
		 "terminate messages with ascii-0x07 (\\f, form-feed)",
		 "<term>"},
		{"format", 'o', 0, G_OPTION_ARG_STRING, &MU_CONFIG.formatstr,
		 "output format ('plain'(*), 'sexp')", "<format>"},
		{NULL, 0, 0, 0, NULL, NULL, NULL}
	};

	og = g_option_group_new("view", "Options for the 'view' command",
				"", NULL, NULL);

	g_option_group_add_entries(og, entries);
	g_option_group_add_entries(og, crypto_option_entries());

	return og;
}



static void
set_group_extract_defaults (void)
{
	if (!MU_CONFIG.targetdir)
		MU_CONFIG.targetdir = g_strdup (".");

	expand_dir (MU_CONFIG.targetdir);
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
		 "save specific parts (comma-separated list)", "<parts>"},
		{"target-dir", 0, 0, G_OPTION_ARG_FILENAME,
		 &MU_CONFIG.targetdir,
		 "target directory for saving", "<dir>"},
		{"overwrite", 0, 0, G_OPTION_ARG_NONE, &MU_CONFIG.overwrite,
		 "overwrite existing files (false)", NULL},
		{"play", 0, 0, G_OPTION_ARG_NONE, &MU_CONFIG.play,
		 "try to 'play' (open) the extracted parts", NULL},
		{NULL, 0, 0, 0, NULL, NULL, NULL}
	};
	og = g_option_group_new("extract",
				"Options for the 'extract' command",
				"", NULL, NULL);
	g_option_group_add_entries(og, entries);
	g_option_group_add_entries(og, crypto_option_entries());

	return og;
}


static GOptionGroup*
config_options_group_verify (void)
{
	GOptionGroup *og;
	og = g_option_group_new("verify",
				"Options for the 'verify' command",
				"", NULL, NULL);
	g_option_group_add_entries(og, crypto_option_entries());

	return og;
}


static GOptionGroup*
config_options_group_server (void)
{
	GOptionGroup *og;
	GOptionEntry entries[] = {
		{"maildir", 'm', 0, G_OPTION_ARG_FILENAME, &MU_CONFIG.maildir,
		 "top of the maildir", "<maildir>"},
                {"commands", 0, 0, G_OPTION_ARG_NONE, &MU_CONFIG.commands,
		 "list the available command and their parameters, then exit", NULL},
		{NULL, 0, 0, 0, NULL, NULL, NULL}
	};

	og = g_option_group_new("server",
				"Options for the 'server' command",
				"", NULL, NULL);
	g_option_group_add_entries(og, entries);

	return og;
}


static MuConfigCmd
cmd_from_string (const char *str)
{
	int i;
	struct {
		const gchar*	name;
		MuConfigCmd	cmd;
	} cmd_map[] = {
		{ "add",     MU_CONFIG_CMD_ADD     },
		{ "cfind",   MU_CONFIG_CMD_CFIND   },
		{ "extract", MU_CONFIG_CMD_EXTRACT },
		{ "find",    MU_CONFIG_CMD_FIND    },
		{ "help",    MU_CONFIG_CMD_HELP    },
		{ "index",   MU_CONFIG_CMD_INDEX   },
		{ "mfind",   MU_CONFIG_CMD_MFIND   },
		{ "mkdir",   MU_CONFIG_CMD_MKDIR   },
		{ "remove",  MU_CONFIG_CMD_REMOVE  },
		{ "script",  MU_CONFIG_CMD_SCRIPT  },
		{ "server",  MU_CONFIG_CMD_SERVER  },
		{ "tickle",  MU_CONFIG_CMD_TICKLE  },
		{ "verify",  MU_CONFIG_CMD_VERIFY  },
		{ "view",    MU_CONFIG_CMD_VIEW    }
	};

	if (!str)
		return MU_CONFIG_CMD_UNKNOWN;

	for (i = 0; i != G_N_ELEMENTS(cmd_map); ++i)
		if (strcmp (str, cmd_map[i].name) == 0)
			return cmd_map[i].cmd;
#ifdef BUILD_GUILE
	/* if we don't recognize it and it's not an option, it may be
	 * some script */
	if (str[0] != '-')
		return MU_CONFIG_CMD_SCRIPT;
#endif /*BUILD_GUILE*/

	return MU_CONFIG_CMD_UNKNOWN;
}



static gboolean
parse_cmd (int *argcp, char ***argvp, GError **err)
{
	MU_CONFIG.cmd	 = MU_CONFIG_CMD_NONE;
	MU_CONFIG.cmdstr = NULL;

	if (*argcp < 2) /* no command found at all */
		return TRUE;
	else if ((**argvp)[1] == '-')
		/* if the first param starts with '-', there is no
		 * command, just some option (like --version, --help
		 * etc.)*/
		return TRUE;

	MU_CONFIG.cmdstr = g_strdup ((*argvp)[1]);
	MU_CONFIG.cmd    = cmd_from_string (MU_CONFIG.cmdstr);

#ifndef BUILD_GUILE
	if (MU_CONFIG.cmd == MU_CONFIG_CMD_SCRIPT) {
		mu_util_g_set_error (err, MU_ERROR_IN_PARAMETERS,
				     "command 'script' not supported");
		return FALSE;
	}
#endif /*!BUILD_GUILE*/

	if (MU_CONFIG.cmdstr && MU_CONFIG.cmdstr[0] != '-' &&
	    MU_CONFIG.cmd == MU_CONFIG_CMD_UNKNOWN) {
		mu_util_g_set_error (err, MU_ERROR_IN_PARAMETERS,
				     "unknown command '%s'",
				     MU_CONFIG.cmdstr);
		return FALSE;
	}

	return TRUE;
}


static GOptionGroup*
get_option_group (MuConfigCmd cmd)
{
	switch (cmd) {
	case MU_CONFIG_CMD_CFIND:
		return config_options_group_cfind();
	case MU_CONFIG_CMD_EXTRACT:
		return config_options_group_extract();
	case MU_CONFIG_CMD_FIND:
	case MU_CONFIG_CMD_MFIND:
		return config_options_group_find();
	case MU_CONFIG_CMD_INDEX:
		return config_options_group_index();
	case MU_CONFIG_CMD_MKDIR:
		return config_options_group_mkdir();
	case MU_CONFIG_CMD_SERVER:
		return config_options_group_server();
	case MU_CONFIG_CMD_SCRIPT:
		return config_options_group_script();
	case MU_CONFIG_CMD_VERIFY:
		return config_options_group_verify();
	case MU_CONFIG_CMD_VIEW:
		return config_options_group_view();
	default:
		return NULL; /* no group to add */
	}
}



/* ugh yuck massaging the GOption text output; glib prepares some text
 * which has a 'Usage:' for the 'help' command. However, we need the
 * help for the command we're asking help for. So, we remove the Usage:
 * from what glib generates. :-( */
static gchar*
massage_help (const char *help)
{
	GRegex *rx;
	char *str;

	rx = g_regex_new ("^Usage:.*\n.*\n",
			  0, G_REGEX_MATCH_NEWLINE_ANY, NULL);
	str = g_regex_replace (rx, help,
			       -1, 0, "",
			       G_REGEX_MATCH_NEWLINE_ANY, NULL);
	g_regex_unref (rx);
	return str;
}



static const gchar*
get_help_string (MuConfigCmd cmd, gboolean long_help)
{
	unsigned u;

	/* this include gets us MU_HELP_STRINGS */
#include "mu-help-strings.h"

	for (u = 0; u != G_N_ELEMENTS(MU_HELP_STRINGS); ++u)
		if (cmd == MU_HELP_STRINGS[u].cmd) {
			if (long_help)
				return MU_HELP_STRINGS[u].long_help;
			else
				return MU_HELP_STRINGS[u].usage ;
		}

	g_return_val_if_reached ("");
	return "";
}


void
mu_config_show_help (MuConfigCmd cmd)
{
	GOptionContext *ctx;
	GOptionGroup *group;
	char *help, *cleanhelp;

	g_return_if_fail (mu_config_cmd_is_valid(cmd));

	ctx = g_option_context_new ("- mu help");
	g_option_context_set_main_group	(ctx, config_options_group_mu());

	group = get_option_group (cmd);
	if (group)
		g_option_context_add_group (ctx, group);

	g_option_context_set_description (ctx, get_help_string (cmd, TRUE));
	help	  = g_option_context_get_help (ctx, TRUE, group);
	cleanhelp = massage_help (help);

	g_print ("usage:\n\t%s%s",
		 get_help_string (cmd, FALSE), cleanhelp);

	g_free (help);
	g_free (cleanhelp);
	g_option_context_free (ctx);
}

static gboolean
cmd_help (void)
{
	MuConfigCmd cmd;

	if (!MU_CONFIG.params)
		cmd = MU_CONFIG_CMD_UNKNOWN;
	else
		cmd = cmd_from_string (MU_CONFIG.params[1]);

	if (cmd == MU_CONFIG_CMD_UNKNOWN) {
		mu_config_show_help (MU_CONFIG_CMD_HELP);
		return TRUE;
	}

	mu_config_show_help (cmd);

	return TRUE;
}

static gboolean
parse_params (int *argcp, char ***argvp, GError **err)
{
	GOptionContext *context;
	GOptionGroup *group;
	gboolean rv;

	context = g_option_context_new("- mu general options");

	g_option_context_set_help_enabled (context, TRUE);
	rv  = TRUE;

	g_option_context_set_main_group(context,
					config_options_group_mu());
	g_option_context_set_ignore_unknown_options (context, FALSE);

	switch (MU_CONFIG.cmd) {
	case MU_CONFIG_CMD_NONE:
	case MU_CONFIG_CMD_HELP:
		/* 'help' is special; sucks in the options of the
		 * command after it */
		rv = g_option_context_parse (context, argcp, argvp, err) &&
			cmd_help ();
		break;
	case MU_CONFIG_CMD_SCRIPT:
		/* all unknown commands are passed to 'script' */
		g_option_context_set_ignore_unknown_options (context, TRUE);
		group = get_option_group (MU_CONFIG.cmd);
		g_option_context_add_group (context, group);
		rv  = g_option_context_parse (context, argcp, argvp, err);
		MU_CONFIG.script = g_strdup (MU_CONFIG.cmdstr);
		/* argvp contains the script parameters */
		MU_CONFIG.script_params = (const char**)&((*argvp)[1]);
		break;

	default:
		group = get_option_group (MU_CONFIG.cmd);
		if (group)
			g_option_context_add_group (context, group);

		rv = g_option_context_parse (context, argcp, argvp, err);
		break;
	}

	g_option_context_free (context);

	return rv ? TRUE : FALSE;
}


MuConfig*
mu_config_init (int *argcp, char ***argvp, GError **err)
{
	g_return_val_if_fail (argcp && argvp, NULL);

	memset (&MU_CONFIG, 0, sizeof(MU_CONFIG));

	MU_CONFIG.maxnum = -1; /* By default, output all matching entries. */

	if (!parse_cmd (argcp, argvp, err))
		goto errexit;

	if (!parse_params(argcp, argvp, err))
		goto errexit;

	/* fill in the defaults if user did not specify */
	set_group_mu_defaults();
	set_group_index_defaults();
	set_group_find_defaults();
	set_group_cfind_defaults();
	set_group_view_defaults();
	set_group_extract_defaults();
	/* set_group_mkdir_defaults (config); */

	return &MU_CONFIG;

errexit:
	mu_config_uninit (&MU_CONFIG);
	return NULL;
}


void
mu_config_uninit (MuConfig *opts)
{
	if (!opts)
		return;

	g_free (opts->cmdstr);
	g_free (opts->muhome);
	g_free (opts->maildir);
	g_free (opts->fields);
	g_free (opts->sortfield);
	g_free (opts->bookmark);
	g_free (opts->formatstr);
	g_free (opts->exec);
	g_free (opts->linksdir);
	g_free (opts->targetdir);
	g_free (opts->parts);
	g_free (opts->script);

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


MuMsgOptions
mu_config_get_msg_options (MuConfig *muopts)
{
	MuMsgOptions opts;

	opts = MU_MSG_OPTION_NONE;

	if (muopts->decrypt)
		opts |= MU_MSG_OPTION_DECRYPT;
	if (muopts->verify)
		opts |= MU_MSG_OPTION_VERIFY;
	if (muopts->use_agent)
		opts |= MU_MSG_OPTION_USE_AGENT;
	if (muopts->auto_retrieve)
		opts |= MU_MSG_OPTION_AUTO_RETRIEVE;
	if (muopts->overwrite)
		opts |= MU_MSG_OPTION_OVERWRITE;

	return opts;
}
