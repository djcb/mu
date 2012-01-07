/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/
/*
** Copyright (C) 2011-2012 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <stdarg.h>

#include <glib/gprintf.h>

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
#include "mu-msg-part.h"


static gboolean MU_CAUGHT_SIGNAL;

static void
sig_handler (int sig)
{
        MU_CAUGHT_SIGNAL = TRUE;
}

static void
install_sig_handler (void)
{
        struct sigaction action;
        int i, sigs[] = { SIGINT, SIGHUP, SIGTERM };

        MU_CAUGHT_SIGNAL = FALSE;

        action.sa_handler = sig_handler;
        sigemptyset(&action.sa_mask);
        action.sa_flags = SA_RESETHAND;

        for (i = 0; i != G_N_ELEMENTS(sigs); ++i)
                if (sigaction (sigs[i], &action, NULL) != 0)
                        g_critical ("set sigaction for %d failed: %s",
				    sigs[i], strerror (errno));;
}


/* BOX - beginning-of-expression */
#define BOX "\376"

static void  send_expr (const char* frm, ...) G_GNUC_PRINTF(1, 2);

static void
send_expr (const char* frm, ...)
{
	char *expr;
	va_list ap;
	char hdr[16];
	size_t exprlen, hdrlen;

	va_start (ap, frm);

	expr    = NULL;
	exprlen = g_vasprintf (&expr, frm, ap);
	hdrlen  = snprintf (hdr, sizeof(hdr), BOX "%u" BOX, exprlen);

	if (write (fileno(stdout), hdr, hdrlen) < 0)
		MU_WRITE_LOG ("error writing output: %s", strerror(errno));

	if (write (fileno(stdout), expr, exprlen) < 0)
		MU_WRITE_LOG ("error writing output: %s", strerror(errno));

	g_free (expr);
	va_end (ap);
}


static MuError server_error (GError **err, MuError merr, const char* frm, ...)
	G_GNUC_PRINTF(3, 4);

static MuError
server_error (GError **err, MuError merr, const char* frm, ...)
{
	gboolean has_err;
	char *errmsg;
	va_list ap;

	va_start (ap, frm);
	errmsg = g_strdup_vprintf (frm, ap);

	has_err = err && *err;
	send_expr ("(:error %u :error-message \"%s\") ",
		   has_err ? (unsigned)(*err)->code : merr,
		   has_err ? (*err)->message : errmsg);

	g_free (errmsg);
	va_end (ap);

	return has_err ? (unsigned)(*err)->code : merr;
}



#define MU_PROMPT ";; mu> "

static gchar*
my_readline (const char *prompt)
{
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
}

enum _Cmd {
	CMD_ADD,
	CMD_COMPOSE,
	CMD_FIND,
	CMD_FLAG,
	CMD_HELP,
	CMD_INDEX,
	CMD_MKDIR,
	CMD_MOVE,
	CMD_OPEN,
	CMD_QUIT,
	CMD_REMOVE,
	CMD_SAVE,
	CMD_PING,
	CMD_VIEW,

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
		{ CMD_ADD,      "add"},
		{ CMD_COMPOSE,	"compose"},
		{ CMD_FIND,	"find" },
		{ CMD_FLAG,     "flag"},
		{ CMD_INDEX,    "index"},
		{ CMD_MKDIR,	"mkdir"},
		{ CMD_MOVE,	"move"},
		{ CMD_OPEN,     "open" },
		{ CMD_QUIT,	"quit"},
		{ CMD_REMOVE,	"remove" },
		{ CMD_SAVE,     "save"},
		{ CMD_PING,	"ping"},
		{ CMD_VIEW,	"view"}
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
check_param_num (GSList *args, unsigned min, unsigned max)
{
	unsigned len;

	len = g_slist_length (args);

	return len >= min && len <= max;
}


#define return_if_fail_param_num(ARGS,MN,MX,USAGE)			 \
	do {								 \
		if (!check_param_num((ARGS),(MN),(MX)))			 \
			return server_error(NULL,MU_ERROR_IN_PARAMETERS, \
					    (USAGE));			 \
	} while (0)




/* -> ping
 * <- (:pong mu :version <version> :doccount <doccount>)
 */
static MuError
cmd_ping (MuStore *store, GSList *args, GError **err)
{
	return_if_fail_param_num (args, 0, 0, "usage: version");

	send_expr ("(:pong \"" PACKAGE_NAME "\" "
		    ":version \"" VERSION "\" "
		    ":doccount %u"
		   ")\n",
		   mu_store_count (store, err));

	return MU_OK;
}


static unsigned
output_found_sexps (MuMsgIter *iter, int maxnum)
{
	unsigned u, max;

	u = 0;
	if (maxnum > 0)
		max = (unsigned) maxnum;
	else
		max = G_MAXUINT32;

	while (!mu_msg_iter_is_done (iter) && u <= max &&
	       !MU_CAUGHT_SIGNAL) {

		MuMsg *msg;

		msg = mu_msg_iter_get_msg_floating (iter);

		if (mu_msg_is_readable (msg)) {
			char *sexp;
			sexp = mu_msg_to_sexp (msg, mu_msg_iter_get_docid (iter),
					       mu_msg_iter_get_thread_info (iter),
					       TRUE);
			send_expr ("%s", sexp);
			g_free (sexp);
			++u;
		}
		mu_msg_iter_next (iter);
	}

	return u;
}


/*
 * find  <query> <maxnum>
 * => list of s-expression, each describing a message
 * => (:found <number of found messages>)
 */
static MuError
cmd_find (MuStore *store, MuQuery *query, GSList *args, GError **err)
{
	MuMsgIter *iter;
	int maxnum;
	const char* usage;

	usage = "usage: find <searchexpr> <maxnum>";

	return_if_fail_param_num (args, 2, 2, usage);

	if ((maxnum = atoi((const char*)args->next->data)) == 0)
		return server_error (NULL, MU_ERROR_IN_PARAMETERS, usage);

	/* TODO: ask for *all* results, then, get the <maxnum> newest
	 * ones; it seems we cannot get a sorted list of a subset of
	 * the result --> needs investigation, this is a
	 * work-around */
	iter = mu_query_run (query, (const char*)args->data, TRUE,
			     MU_MSG_FIELD_ID_DATE, TRUE, -1, err);
	if (!iter)
		return server_error (err, MU_ERROR_INTERNAL,
				     "couldn't get iterator");

	/* if (!(iter = mu_query_run (query, (const char*)args->data, TRUE, */
	/* 			   MU_MSG_FIELD_ID_DATE, TRUE, maxnum, err))) */


	/* return results + the number of results found */
	send_expr ("(:found %u)\n", output_found_sexps (iter, maxnum));
	mu_msg_iter_destroy (iter);

	return MU_OK;
}



static MuError
cmd_mkdir (GSList *args, GError **err)
{
	const char *path;

	return_if_fail_param_num (args, 1, 1, "usage: mkdir <path>");

	path = (const char*)args->data;

	if (!mu_maildir_mkdir (path, 0755, FALSE, err))
		return server_error (err, MU_G_ERROR_CODE (err),
				     "failed to create maildir '%s'",
				     path);

	send_expr ("(:info mkdir :message \"%s has been created\")",
		   path);

	return MU_OK;
}


static unsigned
get_docid_from_msgid (MuQuery *query, const char *str, GError **err)
{
	gchar *querystr;
	unsigned docid;
	MuMsgIter *iter;

	querystr = g_strdup_printf ("msgid:%s", str);
	iter = mu_query_run (query, querystr, FALSE,
			     MU_MSG_FIELD_ID_NONE, FALSE, 1, err);
	g_free (querystr);
	docid = MU_STORE_INVALID_DOCID;
	if (!iter || mu_msg_iter_is_done (iter))
		if (err && *err == NULL)
			g_set_error (err, 0, MU_ERROR_NO_MATCHES,
				     "could not find message %s", str);
		else
			return docid;
	else {
		MuMsg *msg;
		msg = mu_msg_iter_get_msg_floating (iter);
		if (!mu_msg_is_readable(msg)) {
			g_set_error (err, 0, MU_ERROR_FILE_CANNOT_READ,
				     "'%s' is not readable",
				     mu_msg_get_path(msg));
		} else
			docid = mu_msg_iter_get_docid (iter);

		mu_msg_iter_destroy (iter);
	}

	return docid;
}


/* the string contains either a number (docid) or a message-id if it
 * doesn't look like a number, and the query param is non-nil, try to
 * locale the message with message-id in the database, and return its
 * docid */
static unsigned
get_docid (MuQuery *query, const char *str, GError **err)
{
	unsigned docid;
	char *endptr;

	docid = strtol (str, &endptr, 10);
	if (*endptr != '\0') {
		if (!query) {
			g_set_error (err, 0, MU_ERROR_IN_PARAMETERS,
				     "invalid docid '%s'", str);
			return MU_STORE_INVALID_DOCID;
		} else
			return get_docid_from_msgid (query, str, err);
	}

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
	gchar *sexp;
	unsigned rv;

	if (!mu_msg_move_to_maildir (msg, maildir, flags, TRUE, err))
		return MU_G_ERROR_CODE (err);

	/* note, after mu_msg_move_to_maildir, path will be the *new*
	 * path, and flags and maildir fields will be updated as
	 * wel */
	rv = mu_store_update_msg (store, docid, msg, err);
	if (rv == MU_STORE_INVALID_DOCID)
		return server_error (err, MU_ERROR_XAPIAN,
				     "failed to update message");

	sexp = mu_msg_to_sexp (msg, docid, NULL, TRUE);
	send_expr ("(:update %s :move %s)", sexp, is_move ? "t" : "nil");

	g_free (sexp);

	return MU_OK;
}



static MuError
move_or_flag (MuStore *store, MuQuery *query, GSList *args, gboolean is_move,
	      GError **err)
{
	MuError merr;
	unsigned docid;
	MuMsg *msg;
	MuFlags flags;
	GSList *flagitem;
	const char *mdir;

	if ((docid = get_docid (query, (const char*)args->data, err)) == 0)
		return server_error (err, MU_ERROR_IN_PARAMETERS,
				     "invalid docid '%s'", (char*)args->data);

	if (!(msg = mu_store_get_msg (store, docid, err)))
		return server_error (err, MU_ERROR, "failed to get message");

	if (is_move) {
		mdir     = (const char*)g_slist_nth (args, 1)->data;
		flagitem = g_slist_nth (args, 2);
	} else { /* flag */
		mdir     = mu_msg_get_maildir (msg);
		flagitem = g_slist_nth (args, 1);
	}

	flags = get_flags (mu_msg_get_path(msg),
			   flagitem ? (gchar*)flagitem->data : NULL);
	if (flags == MU_FLAG_INVALID) {
		mu_msg_unref (msg);
		return server_error (err, MU_ERROR_IN_PARAMETERS, "invalid flags");
	}

	merr = do_move (store, docid, msg, mdir, flags, is_move, err);
	mu_msg_unref (msg);

	return (merr == MU_OK) ? MU_OK :
		server_error (err, merr, "error moving/flagging file");
}


static MuError
cmd_move (MuStore *store, GSList *args, GError **err)
{
	return_if_fail_param_num (args, 2, 3,
				  "usage: move <docid> <maildir> [<flags>]");

	return move_or_flag (store, NULL, args, TRUE, err);
}

static MuError
cmd_flag (MuStore *store, MuQuery *query, GSList *args, GError **err)
{
	return_if_fail_param_num (args, 2, 2,
				  "usage: flag <docid>|<msgid> <flags>");

	return move_or_flag (store, query, args, FALSE, err);
}



static MuError
cmd_remove (MuStore *store, GSList *args, GError **err)
{
	unsigned docid;
	const char *path;

	return_if_fail_param_num (args, 1, 1, "usage: remove <docid>");

	docid = get_docid (NULL, (const char*)args->data, err);
	if (docid == MU_STORE_INVALID_DOCID)
		return server_error (err, MU_ERROR_IN_PARAMETERS,
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

	send_expr ("(:remove %u)", docid);

	return MU_OK;
}



static MuError
save_part (MuMsg *msg, const char* targetpath, unsigned partindex, GError **err)
{
	gboolean rv;
	gchar *path;

	rv = mu_msg_part_save (msg, targetpath, partindex,
			       TRUE/*overwrite*/, FALSE/*use cache*/, err);
	if (!rv)
		return server_error (err, MU_ERROR_FILE,
				     "failed to save to target path");

	path = mu_str_escape_c_literal (targetpath, FALSE);
	send_expr ("(:info save :message \"%s has been saved\")",
		   path);
	g_free (path);

	return MU_OK;
}


static MuError
open_part (MuMsg *msg, unsigned partindex, GError **err)
{
	char *targetpath;
	gboolean rv;

	targetpath = mu_msg_part_filepath_cache (msg, partindex);

	rv = mu_msg_part_save (msg, targetpath, partindex,
			       FALSE/*overwrite*/, TRUE/*use cache*/, err);
	if (!rv) {
		g_free (targetpath);
		return server_error (err, MU_ERROR_FILE,
				     "failed to save");
	}

	rv = mu_util_play (targetpath, TRUE/*allow local*/,
			   FALSE/*allow remote*/);
	if (!rv) {
		g_free (targetpath);
		return server_error (err, MU_ERROR_FILE, "failed to open");
	} else {
		gchar *path;
		path = mu_str_escape_c_literal (targetpath, FALSE);
		send_expr ("(:info open :message \"%s has been opened\")",
			   path);
		g_free (path);
	}

	g_free (targetpath);

	return MU_OK;
}



static MuError
save_or_open (MuStore *store, GSList *args, gboolean is_save, GError **err)
{
	MuMsg *msg;
	unsigned docid, partindex;
	gboolean rv;

	docid = get_docid (NULL, (const char*)args->data, err);
	if (docid == MU_STORE_INVALID_DOCID)
		return server_error (err, MU_ERROR_IN_PARAMETERS,
				     "invalid docid");

	msg = mu_store_get_msg (store, docid, err);
	if (!msg)
		return server_error (err, MU_ERROR, "failed to get message");

	partindex = atoi((const char*)g_slist_nth (args, 1)->data);

	if (is_save) {
		const char *targetpath;
		targetpath = ((const char*)g_slist_nth (args, 2)->data);
		rv = save_part (msg, targetpath, partindex, err);
	} else
		rv = open_part (msg, partindex, err);

	mu_msg_unref (msg);

	return rv;
}



static MuError
cmd_save (MuStore *store, GSList *args, GError **err)
{
	return_if_fail_param_num (args, 3, 3,
				  "save <docid> <partindex> <targetpath>");

	return save_or_open (store, args, TRUE, err);
}


static MuError
cmd_open (MuStore *store, GSList *args, GError **err)
{
	return_if_fail_param_num (args, 2, 2,
				  "open <docid> <partindex>");

	return save_or_open (store, args, FALSE, err);
}



static MuError
cmd_view (MuStore *store, MuQuery *query, GSList *args, GError **err)
{
	MuMsg *msg;
	unsigned docid;
	char *sexp;

	return_if_fail_param_num (args, 1, 1, "view <docid>|<msgid>");

	docid = get_docid (query, (const char*)args->data, err);
	if (docid == MU_STORE_INVALID_DOCID)
		return server_error (err, MU_ERROR_IN_PARAMETERS,
				     "invalid docid");

	msg = mu_store_get_msg (store, docid, err);
	if (!msg)
		return server_error (err, MU_ERROR,
				     "failed to get message");

	sexp = mu_msg_to_sexp (msg, docid, NULL, FALSE);
	mu_msg_unref (msg);

	send_expr ("(:view %s)\n", sexp);

	g_free (sexp);

	return MU_OK;
}



static MuError
cmd_compose (MuStore *store, GSList *args, GError **err)
{
	MuMsg *msg;
	unsigned docid;
	char *sexp;
	const char* ctype;

	return_if_fail_param_num (args, 2, 2,
				  "compose <reply|forward|edit> <docid>");

	ctype = (const char*)args->data;
	if (strcmp (ctype, "reply") != 0 && strcmp(ctype, "forward") != 0
	    && strcmp (ctype, "edit") != 0)
		return server_error (NULL, MU_ERROR_IN_PARAMETERS,
				     "compose <reply|forward|edit> <docid>");

	docid = get_docid (NULL, (const char*)g_slist_nth(args, 1)->data, err);
	if (docid == MU_STORE_INVALID_DOCID)
		return server_error (err, MU_ERROR_IN_PARAMETERS,
				     "invalid docid");

	msg = mu_store_get_msg (store, docid, err);
	if (!msg)
		return server_error (err, MU_ERROR,
				     "failed to get message");

	sexp = mu_msg_to_sexp (msg, docid, NULL, FALSE);
	mu_msg_unref (msg);

	send_expr ("(:compose %s :compose-type %s)", sexp, ctype);

	g_free (sexp);

	return MU_OK;
}

static MuError
index_msg_cb (MuIndexStats *stats, void *user_data)
{
	if (MU_CAUGHT_SIGNAL)
		return MU_STOP;

	if (stats->_processed % 500)
		return MU_OK;

	send_expr ("(:info index :status running "
		   ":processed %u :updated %u)",
		   stats->_processed, stats->_updated);

	return MU_OK;
}

static MuError
cmd_add (MuStore *store, GSList *args, GError **err)
{
	unsigned docid;
	const char *path, *maildir;
	gchar *escpath;

	return_if_fail_param_num (args, 2, 2,
				  "usage: add <path> <maildir>");

	path    = (const char*)args->data;
	maildir = (const char*)g_slist_nth (args, 1)->data;

	docid = mu_store_add_path (store, path, maildir, err);
	if (docid == MU_STORE_INVALID_DOCID)
		return server_error (err, MU_ERROR_XAPIAN,
				     "failed to add path '%s'", path);

	escpath = mu_str_escape_c_literal (path, TRUE);
	send_expr ("(:info add :path %s :docid %u)", escpath, docid);
	g_free (escpath);

	return MU_OK;
}




static MuError
cmd_index (MuStore *store, GSList *args, GError **err)
{
	MuIndex *index;
	const char *maildir;
	MuIndexStats stats, stats2;
	MuError rv;

	return_if_fail_param_num (args, 1, 1,
				  "usage: index <maildir>");

	index = mu_index_new (store, err);
	if (!index)
		return server_error (err, MU_ERROR,
				     "failed to create index object");

	mu_index_stats_clear (&stats);
	maildir = (const char*)args->data;
	rv = mu_index_run (index, maildir, FALSE, &stats, index_msg_cb, NULL, NULL);

	if (rv != MU_OK && rv != MU_STOP) {
		mu_index_destroy (index);
		return server_error (NULL, rv, "indexing failed");
	}

	mu_index_stats_clear (&stats2);
	rv = mu_index_cleanup (index, &stats2, NULL, NULL, err);
	mu_index_destroy (index);
	if (rv != MU_OK && rv != MU_STOP)
		return server_error (err, rv, "cleanup failed");

	mu_store_flush (store);

	send_expr ("(:info index :status complete "
		   ":processed %u :updated %u :cleaned-up %u)",
		   stats._processed, stats._updated, stats2._cleaned_up);

	return MU_OK;
}


static MuError
cmd_quit (GSList *args, GError **err)
{
	return_if_fail_param_num (args, 0, 0, "usage: quit");

	send_expr (";; quiting");
	return MU_OK;
}


static gboolean
handle_command (Cmd cmd, MuStore *store, MuQuery *query, GSList *args,
		GError **err)
{
	MuError rv;

	switch (cmd) {
	case CMD_ADD:		rv = cmd_add (store, args, err); break;
	case CMD_COMPOSE:	rv = cmd_compose (store, args, err); break;
	case CMD_FIND:		rv = cmd_find (store, query, args, err); break;
	case CMD_FLAG:		rv = cmd_flag (store, query, args, err); break;
	case CMD_INDEX:		rv = cmd_index (store, args, err); break;
	case CMD_MKDIR:		rv = cmd_mkdir (args, err); break;
	case CMD_MOVE:		rv = cmd_move (store, args, err); break;
	case CMD_OPEN:		rv = cmd_open (store, args, err); break;
	case CMD_QUIT:		rv = cmd_quit (args, err); break;
	case CMD_REMOVE:	rv = cmd_remove (store, args, err); break;
	case CMD_SAVE:		rv = cmd_save  (store, args, err); break;
	case CMD_PING:		rv = cmd_ping (store, args, err); break;
	case CMD_VIEW:		rv = cmd_view (store, query, args, err); break;

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

	install_sig_handler ();

	fputs (";; welcome to mu\n", stdout);
	fputs (";; type your commands, and press Enter to execute them\n", stdout);

	query = mu_query_new (store, err);
	if (!query)
		return MU_G_ERROR_CODE (err);

	while (!MU_CAUGHT_SIGNAL) {

		char *line;
		Cmd cmd;
		GSList *args;
		GError *my_err;

		line = my_readline (MU_PROMPT);
		cmd  = parse_line (line, &args);
		g_free (line);

		my_err = NULL;
		if (!handle_command (cmd, store, query, args, &my_err))
			g_clear_error (&my_err);

		mu_str_free_list (args);

		if (cmd == CMD_QUIT) {
			mu_store_flush (store);
			break;
		}
	}

	mu_query_destroy (query);

	return MU_OK;
}
