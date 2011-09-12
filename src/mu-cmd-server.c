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

/* hopefully, the should get us a sane PATH_MAX */
#include <limits.h>
/* not all systems provide PATH_MAX in limits.h */
#ifndef PATH_MAX
#include <sys/param.h>
#ifndef PATH_MAX
#define PATH_MAX MAXPATHLEN
#endif 	/*!PATH_MAX */
#endif 	/*PATH_MAX */

#include "mu-str.h"
#include "mu-cmd.h"
#include "mu-maildir.h"

#define EOX "\n;;eox\n"


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


#define MU_PROMPT ";; mu> "

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
	CMD_FIND,
	CMD_HELP,
	CMD_QUIT,
	CMD_REMOVE,
	CMD_MOVE,
	CMD_MKDIR,
	CMD_FLAG,
	CMD_VIEW,
	CMD_VERSION,
	CMD_IGNORE
};
typedef enum _Cmd Cmd;
static const Cmd CMD_INVALID = (Cmd)-1;

static Cmd
cmd_from_string (const char *str)
{
	unsigned u;
	struct {
		Cmd		 cmd;
		const char	*name;
	} commands[] = {
		{ CMD_FIND,	"find" },
		{ CMD_QUIT,	"quit"},
		{ CMD_REMOVE,	"remove" },
		{ CMD_MOVE,	"move"},
		{ CMD_MKDIR,	"mkdir"},
		{ CMD_VIEW,	"view"},
		{ CMD_VERSION,	"version"},
		{ CMD_FLAG,     "flag"}
	};

	for (u = 0; u != G_N_ELEMENTS(commands); ++u)
		if (g_strcmp0 (str, commands[u].name) == 0)
			return commands[u].cmd;

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

	*args = NULL;

	if (!line)
		return CMD_IGNORE;

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


/*
 * creating a message object just to get a path seems a bit excessive
 * maybe mu_store_get_path could be added if this turns out to be a problem
 *
 * NOTE: not re-entrant.
 */
static const char*
get_path_from_docid (MuStore *store, unsigned docid, GError **err)
{
	MuMsg *msg;
	const char* msgpath;
	static char path[PATH_MAX + 1];

	msg = mu_store_get_msg (store, docid, err);
	if (!msg)
		return NULL;

	msgpath = mu_msg_get_path (msg);
	if (!msgpath) {
		mu_msg_unref (msg);
		return NULL;
	}

	strncpy (path, msgpath, sizeof(path));

	mu_msg_unref (msg);
	return path;
}


static gboolean
check_param_num (GSList *lst, unsigned min, unsigned max)
{
	unsigned len;

	len = g_slist_length (lst);

	return len >= min && len <= max;
}



static MuError
cmd_version (GSList *lst, GError **err)
{
	if (lst) {
		g_set_error (err, 0,MU_ERROR_IN_PARAMETERS,
				     "usage: version");
		return MU_ERROR;
	}

	print_expr ("(:version \"" VERSION "\")\n");
	return MU_OK;
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
	MuError mer;
	MuConfig *opts;
	const gchar *params[3] = { NULL, NULL, NULL };

	if (!check_param_num (lst, 1, 1))
		return server_error (MU_ERROR_IN_PARAMETERS,
				     "usage: find <searchexpr>");

	opts = get_opts (MU_CONFIG_CMD_FIND);
	params[1] = (const char*)lst->data;
	opts->params = (char**)params;

	mer = mu_cmd_find (store, opts, err);

	return mer;
}

static MuError
cmd_mkdir (GSList *lst, GError **err)
{
	if (!check_param_num (lst, 1, 1))
		return server_error (MU_ERROR_IN_PARAMETERS,
				     "usage: mkdir <maildir>");

	if (!mu_maildir_mkdir ((const char*)lst->data, 0755, FALSE, err))
		return server_error (MU_G_ERROR_CODE (err),
				     "failed to create maildir");
	return MU_OK;
}


static unsigned
get_docid (const char* str)
{
	unsigned docid;
	char *endptr;

	docid = strtol (str, &endptr, 10);
	if (*endptr != '\0')
		return 0; /* something wrong with the docid */

	return docid;
}


static MuFlags
get_flags (const char *path, const char *flagstr)
{
	if (!flagstr)
		return MU_FLAG_INVALID; /* ie., ignore flags */
	else {
		/* if there's a '+' or '-' sign in the string, it must
		 * be a flag-delta */
		if (strstr (flagstr, "+") || strstr (flagstr, "-")) {
			MuFlags oldflags;
			oldflags = mu_maildir_get_flags_from_path (path);
			return mu_flags_from_str_delta (flagstr, oldflags,
							MU_FLAG_TYPE_MAILDIR|
							MU_FLAG_TYPE_MAILFILE);
		} else
			return  mu_flags_from_str (flagstr,
						   MU_FLAG_TYPE_MAILDIR |
						   MU_FLAG_TYPE_MAILFILE);
	}
}

static MuError
do_move (MuStore *store, unsigned docid, MuMsg *msg, const char* targetdir,
	 MuFlags flags, GError **err)
{
	gchar *pathstr;

	/* NOTE: we remove the message at its old path, then add the
	 * message at its new path. Instead, we could update the path
	 * and flag parameter in the database */

	if (!mu_store_remove_path (store, mu_msg_get_path(msg))) {
		mu_msg_unref (msg);
		return server_error (MU_ERROR_XAPIAN_REMOVE_FAILED,
				     "failed to remove from database");
	}

	if (!mu_msg_move_to_maildir (msg, targetdir, flags, FALSE, err)) {
		mu_msg_unref (msg);
		return server_error (MU_G_ERROR_CODE (err),
				     "failed to move message");
	}

	/* note, after mu_msg_move_to_maildir, path will be the *new* path */
	if (!mu_store_add_path (store, mu_msg_get_path(msg), err)) {
		mu_msg_unref (msg);
		return server_error (MU_G_ERROR_CODE (err),
				     "failed to add to database");
	}

	pathstr = mu_str_escape_c_literal (mu_msg_get_path (msg), TRUE);
	g_print ("(:update move :docid %u :path %s :flags %s)%s",
		 docid, pathstr,
		 mu_flags_to_str_s (mu_msg_get_flags (msg), MU_FLAG_TYPE_ANY),
		 EOX);
	g_free (pathstr);

	mu_msg_unref (msg);
	fputs (EOX, stdout);

	return MU_OK;
}



static MuError
move_or_flag (MuStore *store, GSList *lst, gboolean is_move, GError **err)
{
	MuError merr;
	unsigned docid;
	MuMsg *msg;
	MuFlags flags;

	docid = get_docid ((const char*)lst->data);
	if (docid == 0)
		return server_error (MU_ERROR_IN_PARAMETERS,
				     "invalid docid");

	msg = mu_store_get_msg (store, docid, err);
	if (!msg)
		return server_error (MU_G_ERROR_CODE (err),
				     "failed to get message");

	/* note, if there is no flags arg, this works still */
	flags = get_flags (mu_msg_get_path(msg),
			   (const gchar*)g_slist_nth(lst, is_move ? 3 : 2)->data);

	merr = do_move (store, docid, msg,
			is_move ? (const gchar*)g_slist_nth(lst, 2)->data : NULL,
			flags, err);

	mu_msg_unref (msg);

	return merr;
}


static MuError
cmd_move (MuStore *store, GSList *lst, GError **err)
{
	if (!check_param_num (lst, 2, 3))
		return server_error (MU_ERROR_IN_PARAMETERS,
				     "usage: move <docid> <targetdir> [<flags>]");

	return move_or_flag (store, lst, TRUE, err);
}

static MuError
cmd_flag (MuStore *store, GSList *lst, GError **err)
{
	if (!check_param_num (lst, 2, 2))
		return server_error (MU_ERROR_IN_PARAMETERS,
				     "usage: flag <docid> <flags>");

	return move_or_flag (store, lst, FALSE, err);
}




static MuError
cmd_remove (MuStore *store, GSList *lst, GError **err)
{
	unsigned docid;
	const char *path;

	if (!check_param_num (lst, 1, 1))
		return server_error (MU_ERROR_IN_PARAMETERS,
				     "usage: remove <docid>");

	docid = get_docid ((const char*)lst->data);
	if (docid == 0)
		return server_error (MU_ERROR_IN_PARAMETERS,
				     "invalid docid");

	path = get_path_from_docid (store, docid, err);
	if (!path)
		return server_error (MU_ERROR_IN_PARAMETERS,
				     "no valid doc for docid");

	if (unlink (path) != 0)
		return server_error (MU_ERROR_FILE_CANNOT_UNLINK,
				     strerror (errno));

	if (!mu_store_remove_path (store, path))
		return server_error (MU_ERROR_XAPIAN_REMOVE_FAILED,
				     "failed to remove from database");

	g_print ("(:update remove :docid %u)%s", docid, EOX);
	return MU_OK;
}



static MuError
cmd_view (MuStore *store, GSList *args, GError **err)
{
	MuMsg *msg;
	unsigned docid;
	char *sexp;

	if (!check_param_num (args, 1, 1))
		return server_error (MU_ERROR_IN_PARAMETERS,
				     "view <docid>");

	docid = get_docid ((const char*)args->data);
	if (docid == 0)
		return server_error (MU_ERROR_IN_PARAMETERS,
				     "invalid docid");

	msg = mu_store_get_msg (store, docid, err);
	if (!msg)
		return server_error (MU_G_ERROR_CODE (err),
				     "failed to get message");

	sexp = mu_msg_to_sexp (msg, docid, NULL, FALSE);
	mu_msg_unref (msg);

	/* FIXME: this breaks for the case where the message has the
	 * EOX marker in its body... */
	fputs (sexp, stdout);
	g_free (sexp);

	return MU_OK;
}



static MuError
cmd_quit (GSList *lst, GError **err)
{
	if (lst) {
		g_set_error (err, 0, MU_ERROR_IN_PARAMETERS,
				     "`quit' does not take any parameters");
		return FALSE;
	}

	print_expr (";; quiting");
	return MU_OK;
}


static gboolean
handle_command (MuConfigCmd cmd, MuStore *store, GSList *args, GError **err)
{
	MuError rv;

	switch (cmd) {
	case CMD_VERSION:	rv = cmd_version (args, err); break;
	case CMD_QUIT:		rv = cmd_quit (args, err); break;
	case CMD_MOVE:		rv = cmd_move (store, args, err); break;
	case CMD_REMOVE:	rv = cmd_remove (store, args, err); break;
	case CMD_MKDIR:		rv = cmd_mkdir (args, err); break;
	case CMD_FIND:		rv = cmd_find (store, args, err); break;
	case CMD_VIEW:		rv = cmd_view (store, args, err); break;
	case CMD_FLAG:		rv = cmd_flag (store, args, err); break;

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
			server_error (err ? err->code    : MU_ERROR_INTERNAL,
				      err ? err->message : "internal error");
			g_clear_error (&err);
		}

		mu_str_free_list (args);

		if (cmd == CMD_QUIT)
			break;
	}

	return MU_OK;
}
