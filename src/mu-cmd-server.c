/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/
/*
** Copyright (C) 2010-2011 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

#ifdef HAVE_LIBREADLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif /*HAVE_LIBREADLINE*/

#include "mu-msg.h"
#include "mu-msg-part.h"
#include "mu-cmd.h"
#include "mu-util.h"
#include "mu-str.h"
#include "mu-date.h"
#include "mu-maildir.h"
#include "mu-contacts.h"
#include "mu-runtime.h"
#include "mu-flags.h"
#include "mu-store.h"
#include "mu-query.h"

#define EOX ";; eox\n"


static void
print_expr (const char* str)
{
	fputs (str, stdout);
	fputs (EOX, stdout);
}


static gboolean
server_error (MuError err, const char* str)
{
	gchar *errexp;

	errexp = g_strdup_printf ("(:error %u :error-message \"%s\") ",
				  err, str);
	print_expr (errexp);
	g_free (errexp);

	return FALSE;
}

#define MU_PROMPT "mu> "

static gchar*
my_readline (const char *prompt)
{
#ifdef HAVE_LIBREADLINE
	return readline (MU_PROMPT);
#else
	GString *gstr;
	int kar;

	gstr = g_string_sized_new (512);

	g_print ("%s", prompt);

	do {
		kar = fgetc (stdin);
		if (kar == '\n' || kar == EOF)
			break;
		else
			gstr = g_string_append_c (gstr, (char)kar);

	} while (1);

	return g_string_free (gstr, FALSE);
#endif /*!HAVE_LIBREADLINE*/
}

enum _Cmd {
	CMD_ADD,
	CMD_FIND,
	CMD_HELP,
	CMD_QUIT,
	CMD_REMOVE,
	CMD_RENAME,
	CMD_RM,
	CMD_VIEW,
	CMD_VERSION,
	CMD_IGNORE,
	CMD_NUM,
};
typedef enum _Cmd Cmd;

static const Cmd CMD_INVALID = (Cmd)-1;

struct _CmdInfo {
	Cmd cmd;
	const char *cmdstr;
	const char *help;
};
typedef struct _CmdInfo CmdInfo;

static const CmdInfo CMD_INFO[] = {
	{ CMD_ADD,	"add",
	  "add <path> [<path>] ;; add a message to the database" },
	{ CMD_FIND,     "find",
	  "find \"<expr>\";; find messages matching <expr>" },
	{ CMD_HELP,     "help",
	  "help [<command>];; get some help information about <command>" },
	{ CMD_QUIT,	"quit",
	  "quit ;; quit mu server" },
	{ CMD_REMOVE,	"remove",
	  "remove <docid>|<path> [<uid>|<path>]"
	  ";; remove path (with "") or docid from the database" },
	{ CMD_RENAME,	"rename",
	  "rename <docid>|<path> <path>"
	  ";; rename (move) path (with "") or docid to a new <path>" },
	{ CMD_VERSION,	"version", "version ;; get the mu version" },
	{ CMD_VIEW,	"view", "view <path> ;; get the contents of message at <path>" }
};

static Cmd
cmd_from_string (const char *str)
{
	unsigned u;

	for (u = 0; u != G_N_ELEMENTS(CMD_INFO); ++u)
		if (g_strcmp0 (str, CMD_INFO[u].cmdstr) == 0)
			return CMD_INFO[u].cmd;

	/* handle all-blank strings */
	if (g_regex_match_simple ("^ *$", str, 0, 0))
		return CMD_IGNORE;

	return CMD_INVALID;
}


static Cmd
parse_line (const gchar *line, GSList **args)
{
	Cmd cmd;
	GSList *lst;

	lst = mu_str_esc_to_list (line);
	if (!lst)
		return CMD_INVALID;

	cmd = cmd_from_string ((const char*)lst->data);

	if (cmd == CMD_INVALID) {
		mu_str_free_list (lst);
		*args = NULL;
	} else {
		*args = g_slist_next (lst);
		lst->next = NULL;
		mu_str_free_list (lst);
	}

	return cmd;
}


static gboolean
cmd_version (GSList *lst, GError **err)
{
	if (lst) {
		g_set_error (err, 0,MU_ERROR_IN_PARAMETERS,
				     "`version' does not take any parameters");
		return FALSE;
	}

	print_expr ("(:version \"" VERSION "\")\n");
	return TRUE;
}


static MuConfig*
get_opts (MuConfigCmd cmd)
{
	static MuConfig opts;

	memset (&opts, 0, sizeof(opts));
	opts.cmd = cmd;
	opts.format = MU_CONFIG_FORMAT_SEXP;

	return &opts;
}

static MuError
cmd_find (MuStore *store, GSList *lst, GError **err)
{
	MuConfig *opts;
	const gchar *params[3] = { NULL, NULL, NULL };

	if (!lst)
		return server_error (MU_ERROR_IN_PARAMETERS,
				     "find takes 1 parameter");

	opts = get_opts (MU_CONFIG_CMD_FIND);
	params[1] = (const char*)lst->data;
	opts->params = (char**)params;

	return mu_cmd_find (store, opts, err);
}


static MuError
cmd_view (GSList *args, GError **err)
{
	MuConfig *opts;
	const gchar *params[3] = { NULL, NULL, NULL };

	if (g_slist_length(args) != 1)
		return server_error (MU_ERROR_IN_PARAMETERS,
				     "view takes exactly 1 parameter");

	opts = get_opts (MU_CONFIG_CMD_VIEW);
	params[1] = (const char*)args->data;
	opts->params = (char**)params;

	return mu_cmd_view (opts, err);
}



static gboolean
cmd_help (GSList *lst, GError **err)
{
	unsigned u;

	if (g_slist_length (lst) > 1) {
		g_set_error (err, 0, MU_ERROR_IN_PARAMETERS,
				     "`help' takes at most one parameter");
		return FALSE;
	}

	if (!lst) {
		g_print (";; use 'help <command> to get info about a "
			 "particular command\n");
		g_print (";; commands are: ");
		for (u = 0; u != G_N_ELEMENTS(CMD_INFO); ++u)
			g_print ("%s ", CMD_INFO[u].cmdstr);
		g_print ("\n");

	} else {
		Cmd c = cmd_from_string ((const char*)lst->data);
		if (c == CMD_INVALID) {
			g_set_error (err, 0, MU_ERROR_IN_PARAMETERS,
					     "no such command");
			return FALSE;
		}

		for (u = 0; u != G_N_ELEMENTS(CMD_INFO); ++u)
			if (CMD_INFO[u].cmd == c)
				g_print (";; usage: %s\n", CMD_INFO[u].help);
	}

	return TRUE;
}


static gboolean
cmd_quit (GSList *lst, GError **err)
{
	if (lst) {
		g_set_error (err, 0, MU_ERROR_IN_PARAMETERS,
				     "`quit' does not take any parameters");
		return FALSE;
	}

	print_expr (";; quiting");
	return TRUE;
}


static gboolean
handle_command (MuConfigCmd cmd, MuStore *store, GSList *args, GError **err)
{
	MuError rv;

	switch (cmd) {
	case CMD_VERSION: rv = cmd_version (args, err); break;
	case CMD_QUIT: rv = cmd_quit (args, err); break;
	case CMD_HELP: rv = cmd_help (args, err); break;

	case CMD_FIND: rv = cmd_find (store, args, err); break;
	case CMD_VIEW: rv = cmd_view (args, err); break;

	case CMD_IGNORE: return TRUE;
	default:
		g_set_error (err, 0, MU_ERROR_IN_PARAMETERS,
			     "unknown command");
		return FALSE;
	}

	return rv == MU_OK ? TRUE : FALSE;
}


MuError
mu_cmd_server (MuStore *store, MuConfig *opts, GError **err/*unused*/)
{
	g_return_val_if_fail (store, MU_ERROR_INTERNAL);

	g_print (";; welcome to mu\n");
	g_print (";; type your commands, and press Enter to execute them\n");

	while (1) {
		char *line;
		Cmd cmd;
		GSList *args;
		GError *err;

		line = my_readline (MU_PROMPT);
		cmd  = parse_line (line, &args);
		g_free (line);

		err = NULL;
		if (!handle_command (cmd, store, args, &err)) {
			server_error (err->code, err->message);
			g_clear_error (&err);
		}

		mu_str_free_list (args);

		if (cmd == CMD_QUIT)
			break;
	}

	return MU_OK;
}
