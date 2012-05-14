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


/* signal handling *****************************************************/
/*
 * when we receive SIGINT, SIGHUP, SIGTERM, set MU_CAUGHT_SIGNAL to
 * TRUE
 * */
static gboolean MU_TERMINATE;

static void
sig_handler (int sig)
{
        MU_TERMINATE = TRUE;
}

static void
install_sig_handler (void)
{
        struct sigaction action;
        int i, sigs[] = { SIGINT, SIGHUP, SIGTERM };

        MU_TERMINATE = FALSE;

        action.sa_handler = sig_handler;
        sigemptyset(&action.sa_mask);
        action.sa_flags = SA_RESETHAND;

        for (i = 0; i != G_N_ELEMENTS(sigs); ++i)
                if (sigaction (sigs[i], &action, NULL) != 0)
                        g_critical ("set sigaction for %d failed: %s",
				    sigs[i], strerror (errno));;
}
/************************************************************************/


/*
 * Markers for/after the lenght cookie that precedes the expression we
 * write to output. We use octal 376, 377 (ie, 0xfe, 0xff) as they
 * will never occur in utf8 */


#define COOKIE_PRE  '\376'
#define COOKIE_POST '\377'

static void G_GNUC_PRINTF(1, 2)
print_expr (const char* frm, ...)
{
	char *expr;
	va_list ap;
	size_t exprlen, lenlen;
	char cookie[16];
	static int outfd = 0;

	if (outfd == 0)
		outfd = fileno (stdout);

	expr    = NULL;

	va_start (ap, frm);
	exprlen = g_vasprintf (&expr, frm, ap);
	va_end (ap);

	/* this cookie tells the frontend where to expect the next
	 * expression */

	cookie[0] = COOKIE_PRE;
	lenlen = sprintf(cookie + 1, "%x",
			 (unsigned)exprlen + 1); /* + 1 for \n */
	cookie[lenlen + 1] = COOKIE_POST;

	/* write the cookie, ie.
	 *   COOKIE_PRE <len-of-following-sexp-in-hex> COOKIE_POST
	 */
	write (outfd, cookie, lenlen + 2);
	write (outfd, expr, exprlen);
	write (outfd, "\n", 1);

	g_free (expr);
}


static MuError
print_error (MuError errcode, const char *msg)
{
	char *str;

	str = mu_str_escape_c_literal (msg, TRUE);
	print_expr ("(:error %u :message %s)", errcode, str);
	g_free (str);

	return errcode;
}

static MuError
print_and_clear_g_error (GError **err)
{
	MuError rv;

	if (err && *err)
		rv = print_error ((*err)->code, (*err)->message);
	else
		rv = print_error (MU_ERROR_INTERNAL, "unknown error");

	g_clear_error (err);

	return rv;
}


static GSList*
read_line_as_list (GError **err)
{
	char *line;
	GSList *lst;
	GString *gstr;

	line = NULL;
	gstr = g_string_sized_new (512);

	fputs (";; mu> ", stdout);

	do {
		int kar;

		kar = fgetc (stdin);
		if (kar == '\n' || kar == EOF)
			break;
		else
			gstr = g_string_append_c (gstr, (char)kar);

	} while (1);

	line = g_string_free (gstr, FALSE);
	lst = mu_str_esc_to_list (line, err);

	g_free (line);

	return lst;
}


const char*
get_string_from_args (GSList *args, const char *param, gboolean optional,
		      GError **err)
{
	size_t param_len;

	param_len = strlen (param);

	while (args) {

		const char *arg;
		arg = (const char*)args->data;

		/* do we have this param */
		if (arg && g_str_has_prefix (arg, param) &&
		    arg[param_len] == ':')
			return (const char*) arg + param_len + 1;

		args = g_slist_next (args);
	}

	if (!optional)
		mu_util_g_set_error (err, MU_ERROR_IN_PARAMETERS,
				     "parameter '%s' not found", param);
	return NULL;
}


#define GET_STRING_OR_ERROR_RETURN(ARGS,PARAM,SP,E)			\
	do {								\
		*(SP) = get_string_from_args ((ARGS),(PARAM),FALSE,(E)); \
		if (!(*(SP)))						\
			return MU_G_ERROR_CODE((E));			\
	} while (0);



/* NOTE: this assumes there is only _one_ docid (message) for the
 * particular message id */
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
		mu_util_g_set_error (err, MU_ERROR_NO_MATCHES,
				     "could not find message %s", str);
	else {
		MuMsg *msg;
		msg = mu_msg_iter_get_msg_floating (iter);
		if (!mu_msg_is_readable(msg)) {
			mu_util_g_set_error (err, MU_ERROR_FILE_CANNOT_READ,
					     "'%s' is not readable",
					     mu_msg_get_path(msg));
		} else
			docid = mu_msg_iter_get_docid (iter);

		mu_msg_iter_destroy (iter);
	}

	return docid;
}


/* get a *list* of all messages with the given message id */
static GSList*
get_docids_from_msgids (MuQuery *query, const char *str, GError **err)
{
	gchar *querystr;
	MuMsgIter *iter;
	GSList *lst;

	querystr = g_strdup_printf ("msgid:%s", str);
	iter = mu_query_run (query, querystr, FALSE,
			     MU_MSG_FIELD_ID_NONE, FALSE,-1 /*unlimited*/,
			     err);
	g_free (querystr);

	if (!iter || mu_msg_iter_is_done (iter)) {
		mu_util_g_set_error (err, MU_ERROR_NO_MATCHES,
				     "could not find message %s", str);
		return NULL;
	}

	lst = NULL;
	do {
		lst = g_slist_prepend
			(lst,
			 GSIZE_TO_POINTER(mu_msg_iter_get_docid (iter)));
	} while (mu_msg_iter_next (iter));

	mu_msg_iter_destroy (iter);

	return lst;
}


/* the string contains either a number (docid) or a message-id if it
 * doesn't look like a number, and the query param is non-nil, try to
 * locale the message with message-id in the database, and return its
 * docid */
static unsigned
determine_docid (MuQuery *query, GSList *args, GError **err)
{
	const char* docidstr, *msgidstr;

	docidstr = get_string_from_args (args, "docid", TRUE, err);
	if (docidstr)
		return atoi (docidstr);

	/* no docid: param; use msgid: instead */
	msgidstr = get_string_from_args (args, "msgid", TRUE, err);
	if (!msgidstr) {
		mu_util_g_set_error (err, MU_ERROR_IN_PARAMETERS,
				     "neither docid nor msgid specified");
		return MU_STORE_INVALID_DOCID;
	}

	return get_docid_from_msgid (query, msgidstr, err);
}



#define DOCID_VALID_OR_ERROR_RETURN(DOCID,E)				\
	if ((DOCID)==MU_STORE_INVALID_DOCID) {				\
		mu_util_g_set_error((E),MU_ERROR_IN_PARAMETERS,		\
			    "invalid docid");				\
		return MU_G_ERROR_CODE((E));				\
	}


#define EQSTR(S1,S2) (g_strcmp0((S1),(S2))==0)

/*************************************************************************/
/* implementation for the commands -- for each command <x>, there is a
 * dedicated function cmd_<x>. These function all are of the type CmdFunc
 *
 * these functions return errors only if they don't handle them
 * themselves, where 'handling' is defined as 'report it using
 * print_and_clear_g_error'
 *
 * if function return non-MU_OK, the repl will print the error instead
 */

typedef MuError (*CmdFunc) (MuStore*,MuQuery*,GSList*,GError**);

/* 'add' adds a message to the database, and takes two parameters:
 * 'path', which is the full path to the message, and 'maildir', which
 * is the maildir this message lives in (e.g. "/inbox"). response with
 * an (:info ...) message with information about the newly added
 * message (details: see code below)
 */
static MuError
cmd_add (MuStore *store, MuQuery *query, GSList *args, GError **err)
{
	unsigned docid;
	const char *maildir, *path;

	GET_STRING_OR_ERROR_RETURN (args, "path", &path, err);
	GET_STRING_OR_ERROR_RETURN (args, "maildir", &maildir, err);

	docid = mu_store_add_path (store, path, maildir, err);
	if (docid == MU_STORE_INVALID_DOCID)
		print_and_clear_g_error (err);
	else {
		gchar *escpath;
		escpath = mu_str_escape_c_literal (path, TRUE);
		print_expr ("(:info add :path %s :docid %u)", escpath, docid);
		g_free (escpath);
	}

	return MU_OK;
}


static void
each_part (MuMsg *msg, MuMsgPart *part, GSList **attlist)
{
	char *att, *cachefile;
	GError *err;

	/* exclude things that don't look like proper attachments */
	if (!mu_msg_part_looks_like_attachment(part, TRUE))
		return;

	err	  = NULL;
	cachefile = mu_msg_part_save_temp (msg, part->index, &err);
	if (!cachefile) {
		print_and_clear_g_error (&err);
		return;
	}

	att = g_strdup_printf (
		"(:file-name \"%s\" :mime-type \"%s/%s\" "
		":disposition \"%s\")",
		cachefile,
		part->type, part->subtype,
		part->disposition ? part->disposition : "attachment");
	*attlist = g_slist_append (*attlist, att);
	g_free (cachefile);
}


/* take the attachments of msg, save them as tmp files, and return
 * as sexp (as a string) describing them
 *
 * ((:name <filename> :mime-type <mime-type> :disposition
 *   <attachment|inline>) ... )
 *
 */
static gchar*
include_attachments (MuMsg *msg)
{
	GSList *attlist, *cur;
	GString *gstr;

	attlist = NULL;
	mu_msg_part_foreach (msg, FALSE, (MuMsgPartForeachFunc)each_part,
			     &attlist);

	gstr = g_string_sized_new (512);
	gstr = g_string_append_c (gstr, '(');
	for (cur = attlist; cur; cur = g_slist_next (cur))
		g_string_append (gstr, (gchar*)cur->data);
	gstr = g_string_append_c (gstr, ')');

	mu_str_free_list (attlist);

	return g_string_free (gstr, FALSE);
}

enum { NEW, REPLY, FORWARD, EDIT, INVALID_TYPE };
static unsigned
compose_type (const char *typestr)
{
	if (EQSTR (typestr, "reply"))
		return REPLY;
	else if (EQSTR (typestr, "forward"))
		return FORWARD;
	else if (EQSTR (typestr, "edit"))
		return EDIT;
	else if (EQSTR (typestr, "new"))
		return NEW;
	else
		return INVALID_TYPE;
}

/* 'compose' produces the un-changed *original* message sexp (ie., the
 * message to reply to, forward or edit) for a new message to
 * compose). It takes two parameters: 'type' with the compose type
 * (either reply, forward or edit), and 'docid' for the message to
 * reply to. Note, type:new does not have an original message, and
 * therefore does not need a docid
 *
 * In returns a (:compose <type> [:original <original-msg>] [:include] )
 * message (detals: see code below)
 *
 * Note ':include' t or nil determines whether to include attachments
 */
static MuError
cmd_compose (MuStore *store, MuQuery *query, GSList *args, GError **err)
{
	const gchar *typestr;
	char *sexp, *atts;
	unsigned ctype;

	GET_STRING_OR_ERROR_RETURN (args, "type", &typestr, err);

	ctype = compose_type (typestr);
	if (ctype == INVALID_TYPE) {
		print_error (MU_ERROR_IN_PARAMETERS, "invalid type to compose");
		return MU_OK;
	}

	if (ctype == REPLY || ctype == FORWARD || ctype == EDIT) {
		MuMsg *msg;
		const char *docidstr;
		GET_STRING_OR_ERROR_RETURN (args, "docid", &docidstr, err);
		msg = mu_store_get_msg (store, atoi(docidstr), err);
		if (!msg) {
			print_and_clear_g_error (err);
			return MU_OK;
		}
		sexp = mu_msg_to_sexp (msg, atoi(docidstr), NULL, FALSE);
		atts = (ctype == FORWARD) ? include_attachments (msg) : NULL;
		mu_msg_unref (msg);
	} else
		atts = sexp = NULL;

	print_expr ("(:compose %s :original %s :include %s)",
		    typestr, sexp ? sexp : "nil", atts ? atts : "nil");

	g_free (sexp);
	g_free (atts);

	return MU_OK;
}


static unsigned
print_sexps (MuMsgIter *iter, int maxnum)
{
	unsigned u, max;

	u = 0;
	max = (maxnum > 0) ? (unsigned)maxnum : G_MAXUINT32;

	while (!mu_msg_iter_is_done (iter) && u < max && !MU_TERMINATE) {

		MuMsg *msg;
		msg = mu_msg_iter_get_msg_floating (iter);

		if (mu_msg_is_readable (msg)) {
			char *sexp;
			sexp = mu_msg_to_sexp (msg, mu_msg_iter_get_docid (iter),
					       mu_msg_iter_get_thread_info (iter),
					       TRUE);
			print_expr ("%s", sexp);
			g_free (sexp);
			++u;
		}
		mu_msg_iter_next (iter);
	}
	return u;
}


static MuError
save_part (MuMsg *msg, unsigned index, GSList *args, GError **err)
{
	gboolean rv;
	const gchar *path;
	gchar *escpath;

	GET_STRING_OR_ERROR_RETURN (args, "path", &path, err);

	rv = mu_msg_part_save (msg, path, index,
			       TRUE/*overwrite*/, FALSE/*use cache*/, err);
	if (!rv) {
		print_and_clear_g_error (err);
		return MU_OK;
	}

	escpath = mu_str_escape_c_literal (path, FALSE);
	print_expr ("(:info save :message \"%s has been saved\")",
		    escpath);

	g_free (escpath);
	return MU_OK;
}


static MuError
open_part (MuMsg *msg, unsigned index, GError **err)
{
	char *targetpath;
	gboolean rv;

	targetpath = mu_msg_part_filepath_cache (msg, index);
	rv = mu_msg_part_save (msg, targetpath, index,
			       FALSE/*overwrite*/, TRUE/*use cache*/, err);
	if (!rv) {
		print_and_clear_g_error (err);
		goto leave;
	}

	rv = mu_util_play (targetpath, TRUE/*allow local*/,
			   FALSE/*allow remote*/, err);
	if (!rv)
		print_and_clear_g_error (err);
	else {
		gchar *path;
		path = mu_str_escape_c_literal (targetpath, FALSE);
		print_expr ("(:info open :message \"%s has been opened\")",
			   path);
		g_free (path);
	}
leave:
	g_free (targetpath);
	return MU_OK;
}


static MuError
temp_part (MuMsg *msg, unsigned index, GSList *args, GError **err)
{
	const char *what, *param;
	char *path;

	GET_STRING_OR_ERROR_RETURN (args, "what", &what, err);
	GET_STRING_OR_ERROR_RETURN (args, "param", &param, err);

	path = mu_msg_part_filepath_cache (msg, index);
	if (!mu_msg_part_save (msg, path, index,
			       FALSE/*overwrite*/, TRUE/*use cache*/, err))
		print_and_clear_g_error (err);
	else {
		gchar *escpath, *escparam;
		escpath = mu_str_escape_c_literal (path, FALSE);
		escparam = mu_str_escape_c_literal (param, FALSE);
		print_expr ("(:temp \"%s\""
			    " :what \"%s\""
			    " :param \"%s\")", escpath, what, escparam);
		g_free (escpath);
		g_free (escparam);
	}

	g_free (path);
	return MU_OK;
}


enum { SAVE, OPEN, TEMP, INVALID_ACTION };
static int
action_type (const char *actionstr)
{
	if (EQSTR (actionstr, "save"))
		return SAVE;
	else if (EQSTR (actionstr, "open"))
		return OPEN;
	else if (EQSTR (actionstr, "temp"))
		return TEMP;
	else
		return INVALID_ACTION;
}

/* 'extract' extracts some mime part from a message */
static MuError
cmd_extract (MuStore *store, MuQuery *query, GSList *args, GError **err)
{
	MuMsg *msg;
	int docid, index, action;
	MuError rv;
	const char* actionstr, *indexstr;

	/* read parameters */
	GET_STRING_OR_ERROR_RETURN (args, "action", &actionstr, err);
	GET_STRING_OR_ERROR_RETURN (args, "index",  &indexstr, err);
	index = atoi (indexstr);
	docid = determine_docid (query, args, err);
	if (docid == MU_STORE_INVALID_DOCID) {
		print_and_clear_g_error (err);
		return MU_OK;
	}

	if ((action = action_type (actionstr)) == INVALID_ACTION) {
		print_error (MU_ERROR_IN_PARAMETERS, "invalid action");
		return MU_OK;
	}
	msg = mu_store_get_msg (store, docid, err);
	if (!msg) {
		print_error (MU_ERROR, "failed to get message");
		return MU_OK;
	}

	switch (action) {
	case SAVE: rv = save_part (msg, index, args, err); break;
	case OPEN: rv = open_part (msg, index, err); break;
	case TEMP: rv = temp_part (msg, index, args, err); break;
	default: print_error (MU_ERROR_INTERNAL, "unknown action");
	}

	if (rv != MU_OK)
		print_and_clear_g_error (err);

	mu_msg_unref (msg);
	return MU_OK;
}



/*
 * 'find' finds a list of messages matching some query, and takes a
 * parameter 'query' with the search query, and (optionally) a
 * parameter 'maxnum' with the maximum number of messages to return.
 *
 * returns:
 * => list of s-expressions, each describing a message =>
 * (:found <number of found messages>)
 */
static MuError
cmd_find (MuStore *store, MuQuery *query, GSList *args, GError **err)
{
	MuMsgIter *iter;
	int maxnum;
	unsigned foundnum;
	const char *querystr, *maxnumstr;

	GET_STRING_OR_ERROR_RETURN (args, "query", &querystr, err);
	/* optional */
	maxnumstr = get_string_from_args (args, "maxnum", TRUE, NULL);
	maxnum = maxnumstr ? atoi (maxnumstr) : 0;

	/* TODO: ask for *all* results, then, get the <maxnum> newest
	 * ones; it seems we cannot get a sorted list of a subset of
	 * the result --> needs investigation, this is a
	 * work-around */
	iter = mu_query_run (query, querystr, TRUE,
			     MU_MSG_FIELD_ID_DATE, TRUE, -1, err);
	if (!iter) {
		print_and_clear_g_error (err);
		return MU_OK;
	}

	/* before sending new results, send an 'erase' message, so the
	 * frontend knows it should erase the headers buffer. this
	 * will ensure that the output of two finds quickly will not
	 * be mixed. */
	print_expr ("(:erase t)");
	foundnum = print_sexps (iter, maxnum);
	print_expr ("(:found %u)", foundnum);

	mu_msg_iter_destroy (iter);

	return MU_OK;
}



static MuError
index_msg_cb (MuIndexStats *stats, void *user_data)
{
	if (MU_TERMINATE)
		return MU_STOP;

	if (stats->_processed % 1000)
		return MU_OK;

	print_expr ("(:info index :status running "
		    ":processed %u :updated %u)",
		   stats->_processed, stats->_updated);

	return MU_OK;
}

/*
 * 'index' (re)indexs maildir at path:<path>, and responds with (:info
 * index ... ) messages while doing so (see the code)
 */
static MuError
cmd_index (MuStore *store, MuQuery *query, GSList *args, GError **err)
{
	MuIndex *index;
	const char *path;
	MuIndexStats stats, stats2;
	MuError rv;

	GET_STRING_OR_ERROR_RETURN (args, "path", &path, err);

	index = mu_index_new (store, err);
	if (!index) {
		print_and_clear_g_error (err);
		return MU_OK;

	}
	mu_index_stats_clear (&stats);
	rv = mu_index_run (index, path, FALSE, &stats, index_msg_cb, NULL, NULL);
	if (rv != MU_OK && rv != MU_STOP) {
		print_error (MU_ERROR_INTERNAL, "indexing failed");
		goto leave;
	}

	mu_index_stats_clear (&stats2);
	rv = mu_index_cleanup (index, &stats2, NULL, NULL, err);
	if (rv != MU_OK && rv != MU_STOP) {
		print_error (MU_ERROR_INTERNAL, "cleanup failed");
		goto leave;
	}

	mu_store_flush (store);
	print_expr ("(:info index :status complete "
		   ":processed %u :updated %u :cleaned-up %u)",
		    stats._processed, stats._updated, stats2._cleaned_up);

leave:
	mu_index_destroy (index);
	return MU_OK;
}



/* 'mkdir' attempts to create a maildir directory at 'path:'; sends an
 * (:info mkdir ...) message when it succeeds */
static MuError
cmd_mkdir (MuStore *store, MuQuery *query, GSList *args, GError **err)
{
	const char *path;

	GET_STRING_OR_ERROR_RETURN (args, "path", &path, err);

	if (!mu_maildir_mkdir (path, 0755, FALSE, err))
		print_and_clear_g_error (err);
	else
		print_expr ("(:info mkdir :message \"%s has been created\")",
			    path);

	return MU_OK;
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
							MU_FLAG_TYPE_ANY);
		} else
			return  mu_flags_from_str (flagstr, MU_FLAG_TYPE_ANY);
	}
}


static MuError
do_move (MuStore *store, unsigned docid, MuMsg *msg, const char *maildir,
	 MuFlags flags, GError **err)
{
	unsigned rv;
	gchar *sexp;
	gboolean different_mdir;

	if (!maildir) {
		maildir = mu_msg_get_maildir (msg);
		different_mdir = FALSE;
	} else {
		/* are we moving to a different mdir, or is it just flags? */
		different_mdir =
			(g_strcmp0 (maildir, mu_msg_get_maildir(msg)) != 0);
	}

	if (!mu_msg_move_to_maildir (msg, maildir, flags, TRUE, err))
		return MU_G_ERROR_CODE (err);

	/* note, after mu_msg_move_to_maildir, path will be the *new*
	 * path, and flags and maildir fields will be updated as
	 * wel */
	rv = mu_store_update_msg (store, docid, msg, err);
	if (rv == MU_STORE_INVALID_DOCID) {
		mu_util_g_set_error (err, MU_ERROR_XAPIAN,
				"failed to store updated message");
		print_and_clear_g_error (err);
	}

	sexp = mu_msg_to_sexp (msg, docid, NULL, FALSE/*include body*/);
	/* note, the :move t thing is a hint to the frontend that it
	 * could remove the particular header */
	print_expr ("(:update %s :move %s)", sexp,
		    different_mdir ? "t" : "nil");
	g_free (sexp);

	return MU_OK;
}

/* when called with a msgid, we need to take care of possibly multiple
 * message with this message id. this is a common case when sending
 * messages to ourselves (maybe through a mailing list), where there
 * would a message in inbox and sentbox with the same id. we set the
 * flag on both */
static gboolean
move_msgid_maybe (MuStore *store, MuQuery *query, GSList *args, GError **err)
{
	const char *maildir, *msgid, *flagstr;
	GSList *docids, *cur;

	maildir	= get_string_from_args (args, "maildir", TRUE, err);
	msgid	= get_string_from_args (args, "msgid", TRUE, err);
	flagstr	= get_string_from_args (args, "flags", TRUE, err);

	/*  you cannot use 'maildir' for multiple messages at once */
	if (!msgid || !flagstr || maildir )
		return FALSE;

	if (!(docids = get_docids_from_msgids (query, msgid, err))) {
		print_and_clear_g_error (err);
		return TRUE;
	}

	for (cur = docids; cur; cur = g_slist_next(cur)) {
		MuMsg *msg;
		MuFlags flags;
		unsigned docid = (GPOINTER_TO_SIZE(cur->data));
		if (!(msg = mu_store_get_msg (store, docid, err))) {
			print_and_clear_g_error (err);
			break;
		}

		flags = flagstr ? get_flags (mu_msg_get_path(msg), flagstr) :
			mu_msg_get_flags (msg);

		if (flags == MU_FLAG_INVALID) {
			print_error (MU_ERROR_IN_PARAMETERS, "invalid flags");
			mu_msg_unref (msg);
			break;
		}

		do_move (store, docid, msg, NULL, flags, err);
		mu_msg_unref (msg);
	}

	g_slist_free (docids);

	return TRUE;
}



/*
 * 'move' moves a message to a different maildir and/or changes its
 * flags. parameters are *either* a 'docid:' or 'msgid:' pointing to
 * the message, a 'maildir:' for the target maildir, and a 'flags:'
 * parameter for the new flags.
 *
 * returns an (:update <new-msg-sexp>)
 *
 */
static MuError
cmd_move (MuStore *store, MuQuery *query, GSList *args, GError **err)
{
	unsigned docid;
	MuMsg *msg;
	MuFlags flags;
	const char *maildir, *flagstr;

	/* check if the move is based on the message id; if so, handle
	 * it in move_msgid_maybe */
	if (move_msgid_maybe (store, query, args, err))
		return MU_OK;

	maildir	= get_string_from_args (args, "maildir", TRUE, err);
	flagstr = get_string_from_args (args, "flags", TRUE, err);

	docid = determine_docid (query, args, err);
	if (docid == MU_STORE_INVALID_DOCID) {
		print_and_clear_g_error (err);
		return MU_OK;
	}

	if (!(msg = mu_store_get_msg (store, docid, err))) {
		print_and_clear_g_error (err);
		return MU_OK;
	}

	/* if maildir was not specified, take the current one */
	if (!maildir)
		maildir = mu_msg_get_maildir (msg);

	/* determine the real target flags, which come from the
	 * flags-parameter we received (ie., flagstr), if any, plus
	 * the existing message flags. */
	if (flagstr)
		flags = get_flags (mu_msg_get_path(msg), flagstr);
	else
		flags = mu_msg_get_flags (msg);

	if (flags == MU_FLAG_INVALID) {
		print_error (MU_ERROR_IN_PARAMETERS, "invalid flags");
		goto leave;
	}

	do_move (store, docid, msg, maildir, flags, err);

leave:
	mu_msg_unref (msg);
	return MU_OK;
}



/* 'ping' takes no parameters, and provides information about this mu
 * server using a (:pong ...) message (details: see code below)
 */
static MuError
cmd_ping (MuStore *store, MuQuery *query, GSList *args, GError **err)
{
	unsigned doccount;
	doccount = mu_store_count (store, err);

	if (doccount == (unsigned)-1)
		return print_and_clear_g_error (err);

	print_expr ("(:pong \"" PACKAGE_NAME "\" "
		    ":version \"" VERSION "\" "
		    ":doccount %u)", doccount);

	return MU_OK;
}


/* 'quit' takes no parameters, terminates this mu server */
static MuError
cmd_quit (MuStore *store, MuQuery *query, GSList *args , GError **err)
{
	print_expr (";; quiting");

	return MU_STOP;
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



/* 'remove' removes the message with either docid: or msgid:, sends a
 * (:remove ...) message when it succeeds
 */
static MuError
cmd_remove (MuStore *store, MuQuery *query, GSList *args, GError **err)
{
	unsigned docid;
	const char *path;

	docid = determine_docid (query, args, err);
	if (docid == MU_STORE_INVALID_DOCID) {
		print_and_clear_g_error (err);
		return MU_OK;
	}

	path = get_path_from_docid (store, docid, err);
	if (!path) {
		if (err && *err)
			print_and_clear_g_error (err);
		else
			print_error (MU_ERROR_IN_PARAMETERS,
				     "no path for docid");
		return MU_OK;
	}

	if (unlink (path) != 0) {
		mu_util_g_set_error (err, MU_ERROR_FILE_CANNOT_UNLINK,
				     "%s", strerror (errno));
		print_and_clear_g_error (err);
		return MU_OK;
	}

	if (!mu_store_remove_path (store, path)) {
		print_error (MU_ERROR_XAPIAN_REMOVE_FAILED,
			     "failed to remove from database");
		return MU_OK;
	}

	print_expr ("(:remove %u)", docid);
	return MU_OK;
}

/* 'add' adds a message to the database, and takes two parameters:
 * 'path', which is the full path to the message, and 'maildir', which
 * is the maildir this message lives in (e.g. "/inbox"). response with
 * an (:info ...) message with information about the newly added
 * message (details: see code below)
 */
static MuError
cmd_sent (MuStore *store, MuQuery *query, GSList *args, GError **err)
{
	unsigned docid;
	const char *maildir, *path;

	GET_STRING_OR_ERROR_RETURN (args, "path", &path, err);
	GET_STRING_OR_ERROR_RETURN (args, "maildir", &maildir, err);

	docid = mu_store_add_path (store, path, maildir, err);
	if (docid == MU_STORE_INVALID_DOCID)
		print_and_clear_g_error (err);
	else {
		gchar *escpath;
		escpath = mu_str_escape_c_literal (path, TRUE);
		print_expr ("(:sent t :path %s :docid %u)",
			    escpath, docid);
		g_free (escpath);
	}

	return MU_OK;
}



/* 'view' gets a full (including body etc.) sexp for some message,
 * identified by either docid: or msgid:; return a (:view <sexp>)
 */
static MuError
cmd_view (MuStore *store, MuQuery *query, GSList *args, GError **err)
{
	MuMsg *msg;
	unsigned docid;
	char *sexp;

	docid = determine_docid (query, args, err);
	if (docid == MU_STORE_INVALID_DOCID) {
		print_and_clear_g_error (err);
		return MU_OK;
	}

	msg = mu_store_get_msg (store, docid, err);
	if (!msg) {
		if (err && *err)
			print_and_clear_g_error (err);
		else
			print_error (MU_ERROR_IN_PARAMETERS,
				     "failed to get message");
		return MU_OK;
	}

	sexp = mu_msg_to_sexp (msg, docid, NULL, FALSE);
	mu_msg_unref (msg);

	print_expr ("(:view %s)\n", sexp);
	g_free (sexp);

	return MU_OK;
}





/*************************************************************************/

static MuError
handle_args (MuStore *store, MuQuery *query, GSList *args, GError **err)
{
	unsigned u;
	const char *cmd;
	struct {
		const char *cmd;
		CmdFunc func;
	} cmd_map[] = {
		{ "add",	cmd_add },
		{ "compose",	cmd_compose },
		{ "extract",    cmd_extract },
		{ "find",	cmd_find },
		{ "index",	cmd_index },
		{ "mkdir",	cmd_mkdir },
		{ "move",	cmd_move },
		{ "ping",	cmd_ping },
		{ "quit",	cmd_quit },
		{ "remove",	cmd_remove },
		{ "sent",	cmd_sent },
		{ "view",	cmd_view }
	};

	cmd = (const char*) args->data;
	for (u = 0; u != G_N_ELEMENTS (cmd_map); ++u)
		if (g_strcmp0(cmd, cmd_map[u].cmd) == 0)
			return cmd_map[u].func (store, query,
						g_slist_next(args),
						err);

	mu_util_g_set_error (err, MU_ERROR_IN_PARAMETERS,
			     "unknown command '%s'", cmd ? cmd : "");
	return MU_G_ERROR_CODE (err);
}



MuError
mu_cmd_server (MuStore *store, MuConfig *opts/*unused*/, GError **err)
{
	MuQuery *query;
	gboolean do_quit;

	g_return_val_if_fail (store, MU_ERROR_INTERNAL);

	query = mu_query_new (store, err);
	if (!query)
		return MU_G_ERROR_CODE (err);

	install_sig_handler ();

	g_print (";; welcome to " PACKAGE_STRING "\n");

	/*  the main REPL */
	do_quit = FALSE;
	while (!MU_TERMINATE && !do_quit) {

		GSList *args;
		GError *my_err = NULL;

		/* args will receive a the command as a list of
		 * strings. returning NULL indicates an error */
		args   = read_line_as_list (&my_err);
		if (!args || my_err) {
			print_and_clear_g_error (&my_err);
			continue;
		}

		switch (handle_args (store, query, args, &my_err)) {
		case MU_OK: break;
		case MU_STOP:
			do_quit = TRUE;
			break;
		default: /* some error occurred */
			print_and_clear_g_error (&my_err);
		}

		mu_str_free_list (args);
	}

	mu_store_flush (store);
	mu_query_destroy (query);

	return MU_OK;
}
