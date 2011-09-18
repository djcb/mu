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
#include "mu-query.h"
#include "mu-index.h"


/* we include \376 (0xfe) and \377, (0xff) because it cannot occur as
 * part of the other output, as 0xfe, 0xff are not valid UTF-8
 */
/* #define BOX ";;box\376\n" /\* just before the sexp starts *\/ */
/* #define EOX ";;eox\377\n"   /\* after the sexp has ended *\/ */

#define BOX "\376"

static void
send_expr (const char* str)
{
	char *hdr;

	hdr = g_strdup_printf (BOX "%u" BOX, strlen(str));
	fputs (hdr, stdout);
	fputs (str, stdout);
	g_free (hdr);
}


static MuError
server_error (GError **err, MuError merr, const char* str)
{
	gchar *errexp;
	gboolean has_err;

	has_err = err && *err;
	errexp = g_strdup_printf ("(:error %u :error-message \"%s\") ",
				  has_err ? (unsigned)(*err)->code : merr,
				  has_err ? (*err)->message : str);
	send_expr (errexp);
	g_free (errexp);

	return has_err ? (unsigned)(*err)->code : merr;
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

	fputs (prompt, stdout);

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
	CMD_COMPOSE,
	CMD_MOVE,
	CMD_MKDIR,
	CMD_FLAG,
	CMD_VIEW,
	CMD_VERSION,
	CMD_INDEX,

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
		{ CMD_COMPOSE,	"compose"},
		{ CMD_VERSION,	"version"},
		{ CMD_FLAG,     "flag"},
		{ CMD_INDEX,    "index"}
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
	if (!check_param_num (lst, 0, 0))
		return server_error (NULL, MU_ERROR_IN_PARAMETERS,
				     "usage: version");

	send_expr ("(:info version :version \"" VERSION "\")");
	return MU_OK;
}


static MuError
cmd_find (MuStore *store, MuQuery *query, GSList *lst, GError **err)
{
	MuMsgIter *iter;
	unsigned u;

	if (!check_param_num (lst, 1, 1))
		return server_error (NULL, MU_ERROR_IN_PARAMETERS,
				     "usage: find <searchexpr>");

	iter = mu_query_run (query, (const char*)lst->data, FALSE,
			     MU_MSG_FIELD_ID_NONE, TRUE, err);
	if (!iter)
		return server_error (err, MU_ERROR_INTERNAL,
				     "couldn't get iterator");

	u = 0;
	while (!mu_msg_iter_is_done (iter)) {
		MuMsg *msg;
		msg = mu_msg_iter_get_msg_floating (iter);
		if (mu_msg_is_readable (msg)) {
			char *sexp;
			sexp = mu_msg_to_sexp (msg,
					       mu_msg_iter_get_docid (iter),
					       NULL, TRUE);
			send_expr (sexp);
			g_free (sexp);
			++u;
		}
		mu_msg_iter_next (iter);
	}

	mu_msg_iter_destroy (iter);

	if (u == 0)
		return server_error (NULL, MU_ERROR_NO_MATCHES, "No matches");
	else
		return MU_OK;
}

static MuError
cmd_mkdir (GSList *lst, GError **err)
{
	if (!check_param_num (lst, 1, 1))
		return server_error (NULL, MU_ERROR_IN_PARAMETERS,
				     "usage: mkdir <maildir>");

	if (!mu_maildir_mkdir ((const char*)lst->data, 0755, FALSE, err))
		return server_error (err, MU_G_ERROR_CODE (err),
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
		return MU_FLAG_NONE; /* ie., ignore flags */
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
do_move (MuStore *store, unsigned docid, MuMsg *msg, const char *maildir,
	 MuFlags flags, gboolean is_move, GError **err)
{
	gchar *sexp, *str;
	unsigned rv;

	if (!mu_msg_move_to_maildir (msg, maildir, flags, TRUE, err))
		return MU_G_ERROR_CODE (err);

	/* note, after mu_msg_move_to_maildir, path will be the *new*
	 * path, and flags and maildir fields will be updated as
	 * wel */
	rv = mu_store_update_msg (store, docid, msg, err);
	if (rv == MU_STORE_INVALID_DOCID)
		return server_error (err, MU_ERROR_XAPIAN, "failed to update message");

	sexp = mu_msg_to_sexp (msg, docid, NULL, TRUE);
	str  = g_strdup_printf ("(:update %s :move %s)", sexp, is_move ? "t" : "nil");

	send_expr (str);

	g_free (sexp);
	g_free (str);

	return MU_OK;
}



static MuError
move_or_flag (MuStore *store, GSList *lst, gboolean is_move, GError **err)
{
	MuError merr;
	unsigned docid;
	MuMsg *msg;
	MuFlags flags;
	GSList *flagitem;
	const char *mdir;

	docid = get_docid ((const char*)lst->data);
	if (docid == 0)
		return server_error (err, MU_ERROR_IN_PARAMETERS,
				     "invalid docid");

	msg = mu_store_get_msg (store, docid, err);
	if (!msg)
		return server_error (err, MU_ERROR, "failed to get message");

	if (is_move) {
		mdir     = (const char*)g_slist_nth (lst, 1)->data;
		flagitem = g_slist_nth (lst, 2);
	} else { /* flag */
		mdir     = mu_msg_get_maildir (msg);
		flagitem = g_slist_nth (lst, 1);
	}


	flags = get_flags (mu_msg_get_path(msg),
			   flagitem ? (const gchar*)flagitem->data : NULL);
	if (flags == MU_FLAG_INVALID) {
		mu_msg_unref (msg);
		return server_error (err, MU_ERROR_IN_PARAMETERS,
				     "invalid flags");
	}

	merr = do_move (store, docid, msg, mdir, flags, is_move, err);

	mu_msg_unref (msg);

	return merr;
}


static MuError
cmd_move (MuStore *store, GSList *lst, GError **err)
{
	if (!check_param_num (lst, 2, 3))
		return server_error
			(NULL, MU_ERROR_IN_PARAMETERS,
			 "usage: flag <docid> <maildir> [<flags>]");

	return move_or_flag (store, lst, TRUE, err);

}

static MuError
cmd_flag (MuStore *store, GSList *lst, GError **err)
{
	if (!check_param_num (lst, 2, 2))
		return server_error (NULL, MU_ERROR_IN_PARAMETERS,
				     "usage: flag <docid> <flags>");

	return move_or_flag (store, lst, FALSE, err);
}




static MuError
cmd_remove (MuStore *store, GSList *lst, GError **err)
{
	unsigned docid;
	const char *path;
	char *str;

	if (!check_param_num (lst, 1, 1))
		return server_error (NULL, MU_ERROR_IN_PARAMETERS,
				     "usage: remove <docid>");

	docid = get_docid ((const char*)lst->data);
	if (docid == 0)
		return server_error (NULL, MU_ERROR_IN_PARAMETERS,
				     "invalid docid");

	path = get_path_from_docid (store, docid, err);
	if (!path)
		return server_error (err, MU_ERROR_IN_PARAMETERS,
				     "no valid doc for docid");

	if (unlink (path) != 0)
		return server_error (err, MU_ERROR_FILE_CANNOT_UNLINK,
				     strerror (errno));

	if (!mu_store_remove_path (store, path))
		return server_error (NULL, MU_ERROR_XAPIAN_REMOVE_FAILED,
				     "failed to remove from database");

	str = g_strdup_printf ("(:remove %u)", docid);
	send_expr (str);
	g_free (str);

	return MU_OK;
}



static MuError
cmd_view (MuStore *store, GSList *args, GError **err)
{
	MuMsg *msg;
	unsigned docid;
	char *sexp, *str;

	if (!check_param_num (args, 1, 1))
		return server_error (NULL, MU_ERROR_IN_PARAMETERS,
				     "message <docid> <view|reply|forward>");

	docid = get_docid ((const char*)args->data);
	if (docid == 0)
		return server_error (NULL, MU_ERROR_IN_PARAMETERS,
				     "invalid docid");

	msg = mu_store_get_msg (store, docid, err);
	if (!msg)
		return server_error (err, MU_ERROR,
				     "failed to get message");

	sexp = mu_msg_to_sexp (msg, docid, NULL, FALSE);
	mu_msg_unref (msg);

	str = g_strdup_printf ("(:view %s)", sexp);
	send_expr (str);

	g_free (sexp);
	g_free (str);

	return MU_OK;
}



static MuError
cmd_compose (MuStore *store, GSList *args, GError **err)
{
	MuMsg *msg;
	unsigned docid;
	char *sexp, *str;
	const char* action;

	if ((!check_param_num (args, 2, 2)))
		return server_error (NULL, MU_ERROR_IN_PARAMETERS,
				     "compose <reply|forward> <docid>");

	action = (const char*)args->data;
	if (strcmp (action, "reply") != 0 && strcmp(action, "forward") != 0)
		return server_error (NULL, MU_ERROR_IN_PARAMETERS,
				     "compose <reply|forward> <docid>");

	docid = get_docid ((const char*)g_slist_nth(args, 1)->data);
	if (docid == 0)
		return server_error (NULL, MU_ERROR_IN_PARAMETERS,
				     "invalid docid");

	msg = mu_store_get_msg (store, docid, err);
	if (!msg)
		return server_error (err, MU_ERROR,"failed to get message");

	sexp = mu_msg_to_sexp (msg, docid, NULL, FALSE);
	mu_msg_unref (msg);

	str = g_strdup_printf ("(:compose %s :action %s)", sexp, action);
	send_expr (str);

	g_free (sexp);
	g_free (str);

	return MU_OK;
}

static MuError
index_msg_cb (MuIndexStats *stats, void *user_data)
{
	char *info;

	if (stats->_processed % 500)
		return MU_OK;

	info = g_strdup_printf ("(:info index :status running "
				":processed %u :updated %u)",
				stats->_processed, stats->_updated);
	send_expr (info);
	g_free (info);

	return MU_OK;
}


static MuError
cmd_index (MuStore *store, GSList *lst, GError **err)
{
	MuIndex *index;
	const char *maildir;
	MuIndexStats stats, stats2;
	MuError rv;
	char *info;

	if (!check_param_num (lst, 1, 1))
		return server_error (NULL, MU_ERROR_IN_PARAMETERS,
				     "usage: index <maildir>");

	maildir = (const char*)lst->data;
	index = mu_index_new (store, err);
	if (!index)
		return server_error (err, MU_ERROR, "failed to create index");

	mu_index_stats_clear (&stats);
	rv = mu_index_run (index, maildir, FALSE, &stats, index_msg_cb, NULL, NULL);

	if (rv != MU_OK && rv != MU_STOP) {
		mu_index_destroy (index);
		return server_error (NULL, rv, "indexing failed");
	}

	mu_index_stats_clear (&stats2);
	rv = mu_index_cleanup (index, &stats2, NULL, NULL, err);
	if (rv != MU_OK && rv != MU_STOP) {
		mu_index_destroy (index);
		return server_error (err, rv, "cleanup failed");
	}

	info = g_strdup_printf ("(:info index :status complete "
				":processed %u :updated %u :cleaned-up %u)",
				stats._processed, stats._updated, stats2._cleaned_up);
	send_expr (info);
	g_free (info);

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

	send_expr (";; quiting");
	return MU_OK;
}




static gboolean
handle_command (MuConfigCmd cmd, MuStore *store, MuQuery *query,
		GSList *args, GError **err)
{
	MuError rv;

	switch (cmd) {
	case CMD_VERSION:	rv = cmd_version (args, err); break;
	case CMD_QUIT:		rv = cmd_quit (args, err); break;
	case CMD_MOVE:		rv = cmd_move (store, args, err); break;
	case CMD_REMOVE:	rv = cmd_remove (store, args, err); break;
	case CMD_MKDIR:		rv = cmd_mkdir (args, err); break;
	case CMD_FIND:		rv = cmd_find (store, query, args, err); break;
	case CMD_VIEW:		rv = cmd_view (store, args, err); break;
	case CMD_COMPOSE:	rv = cmd_compose (store, args, err); break;
	case CMD_FLAG:		rv = cmd_flag (store, args, err); break;
	case CMD_INDEX:		rv = cmd_index (store, args, err); break;

	case CMD_IGNORE: return TRUE;
	default:
		g_set_error (err, 0, MU_ERROR_IN_PARAMETERS,
			     "unknown command");
		return FALSE;
	}

	return rv == MU_OK ? TRUE : FALSE;
}


MuError
mu_cmd_server (MuStore *store, MuConfig *opts, GError **err)
{
	MuQuery *query;

	g_return_val_if_fail (store, MU_ERROR_INTERNAL);

	fputs (";; welcome to mu\n", stdout);
	fputs (";; type your commands, and press Enter to execute them\n", stdout);

	query = mu_query_new (store, err);
	if (!query)
		return MU_G_ERROR_CODE (err);

	while (1) {
		char *line;
		Cmd cmd;
		GSList *args;
		GError *my_err;

		line = my_readline (MU_PROMPT);
		cmd  = parse_line (line, &args);
		g_free (line);

		my_err = NULL;
		if (!handle_command (cmd, store, query, args, &my_err)) {
			server_error (&my_err, MU_ERROR_INTERNAL, "internal error");
			g_clear_error (&my_err);
		}

		mu_str_free_list (args);

		if (cmd == CMD_QUIT)
			break;
	}

	mu_query_destroy (query);

	return MU_OK;
}
