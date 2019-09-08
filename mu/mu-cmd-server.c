/*
** Copyright (C) 2011-2019 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#define _GNU_SOURCE

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <stdarg.h>
#include <sys/time.h>

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

#include "mu-runtime.h"
#include "mu-str.h"
#include "mu-cmd.h"
#include "mu-maildir.h"
#include "mu-query.h"
#include "mu-index.h"
#include "mu-store.hh"
#include "mu-msg-part.h"
#include "mu-contacts.hh"

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
        int i, sigs[] = { SIGINT, SIGHUP, SIGTERM, SIGPIPE };

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
 * Markers for/after the length cookie that precedes the expression we write to
 * output. We use octal 376, 377 (ie, 0xfe, 0xff) as they will never occur in
 * utf8 */


#define COOKIE_PRE  '\376'
#define COOKIE_POST '\377'

static void G_GNUC_PRINTF(1, 2)
print_expr (const char* frm, ...)
{
        char *expr, *expr_orig;
        va_list ap;
        ssize_t rv;
        size_t exprlen, lenlen;
        char cookie[16];
        static int outfd = 0;

#if defined(__CYGWIN__ )&& !defined (_WIN32)
        const size_t writestep = 4096 * 16;
        size_t bytestowrite = 0;
#endif

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
        rv = write (outfd, cookie, lenlen + 2);
        if (rv != -1) {
                expr_orig = expr;
#if defined (__CYGWIN__) && !defined(_WIN32)
                /* CYGWIN doesn't like big packets */
                while (exprlen > 0) {
                        bytestowrite = exprlen > writestep ? writestep : exprlen;
                        rv = write(outfd, expr, bytestowrite);
                        expr += bytestowrite;
                        exprlen -= bytestowrite;
                }
#else
                rv = write (outfd, expr, exprlen);
#endif
                g_free (expr_orig);
        }
        if (rv != -1)
                rv = write (outfd, "\n", 1);
        if (rv == -1) {
                g_critical ("%s: write() failed: %s",
                           __func__, strerror(errno));
                /* terminate ourselves */
                raise (SIGTERM);
        }
}

static MuError
print_error (MuError errcode, const char* frm, ...)
{
        char    *msg, *str;
        va_list  ap;

        va_start (ap, frm);
        g_vasprintf (&msg, frm, ap);
        va_end (ap);

        str = mu_str_escape_c_literal (msg, TRUE);
        print_expr ("(:error %u :message %s)", errcode, str);

        g_free (msg);
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


static GHashTable*
read_command_line (GError **err)
{
        char		*line;
        GHashTable	*hash;
        GString		*gstr;

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

        if (!mu_str_is_empty (line))
                hash = mu_str_parse_arglist (line, err);
        else
                hash = NULL;

        g_free (line);

        return hash;
}


static const char*
get_string_from_args (GHashTable *args, const char *param, gboolean optional,
                      GError **err)
{
        const char *str;

        str = g_hash_table_lookup (args, param);
        if (!str && !optional)
                mu_util_g_set_error (err, MU_ERROR_IN_PARAMETERS,
                                     "parameter '%s' not found", param);
        return str;
}

static gboolean
get_bool_from_args (GHashTable *args, const char *param, gboolean optional,
                    GError **err)
{
        const char *val;

        val = get_string_from_args (args, param, optional, err);
        if (err && (*err != NULL))
                return FALSE;

        if (g_strcmp0 (val, "true") == 0)
                return TRUE;

        if (!val || g_strcmp0 (val, "false") == 0)
                return FALSE;

        mu_util_g_set_error (err, MU_ERROR_IN_PARAMETERS,
                             "invalid value for parameter '%s'", param);
        return FALSE;

}


#define GET_STRING_OR_ERROR_RETURN(ARGS,PARAM,SP,E)			 \
        do {								 \
                *(SP) = get_string_from_args ((ARGS),(PARAM),FALSE,(E)); \
                if (!(*(SP)))						 \
                        return MU_G_ERROR_CODE((E));			 \
        } while (0);


/* get a *list* of all messages with the given message id */
static GSList*
get_docids_from_msgids (MuQuery *query, const char *msgid,
                        int max, GError **err)
{
        char		 xprefix;
        char		*rawq, *tmp;
        MuMsgIter	*iter;
        GSList		*lst;

        if (!msgid || strlen(msgid) > MU_STORE_MAX_TERM_LENGTH - 1) {
                mu_util_g_set_error (err, MU_ERROR_IN_PARAMETERS,
                                     "invalid message-id '%s' (length=%zu)",
                                     msgid, strlen(msgid));
                return NULL;
        }

        xprefix = mu_msg_field_xapian_prefix(MU_MSG_FIELD_ID_MSGID);
        /*XXX this is a bit dodgy */
        tmp	= g_ascii_strdown(msgid, -1);
        rawq    = g_strdup_printf("%c%s", xprefix, tmp);
        g_free(tmp);
        iter	= mu_query_run (query, rawq, MU_MSG_FIELD_ID_NONE,
                                max, MU_QUERY_FLAG_RAW, err);
        g_free (rawq);

        if (!iter || mu_msg_iter_is_done (iter)) {
                mu_util_g_set_error (err, MU_ERROR_NO_MATCHES,
                                     "could not find message(s) for msgid %s",
                                     msgid);
                return NULL;
        }

        lst = NULL;
        do {
                lst = g_slist_prepend
                        (lst, GSIZE_TO_POINTER(mu_msg_iter_get_docid (iter)));
        } while (mu_msg_iter_next (iter));
        mu_msg_iter_destroy (iter);

        return lst;
}


/* the string contains either a number (docid) or a message-id if it
 * doesn't look like a number, and the query param is non-nil, try to
 * locale the message with message-id in the database, and return its
 * docid */
static unsigned
determine_docid (MuQuery *query, GHashTable *args, GError **err)
{
        GSList		*docids;
        unsigned	 docid;
        const char*	 docidstr, *msgidstr;

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

        docids = get_docids_from_msgids (query, msgidstr, 1, err);
        if (!docids)
                return MU_STORE_INVALID_DOCID;

        docid = GPOINTER_TO_UINT(docids->data);
        g_slist_free (docids);

        return docid;
}


#define DOCID_VALID_OR_ERROR_RETURN(DOCID,E)				\
        if ((DOCID)==MU_STORE_INVALID_DOCID) {				\
                mu_util_g_set_error((E),MU_ERROR_IN_PARAMETERS,		\
                            "invalid docid");				\
                return MU_G_ERROR_CODE((E));				\
        }


#define EQSTR(S1,S2) (g_strcmp0((S1),(S2))==0)


struct _ServerContext {
        MuStore *store;
        MuQuery *query;
};
typedef struct _ServerContext ServerContext;

/*************************************************************************/
/* implementation for the commands -- for each command <x>, there is a
 * dedicated function cmd_<x>. These function all are of the type CmdFunc
 *
 * these functions return errors only if they don't handle them themselves,
 * where 'handling' is defined as 'report it using print_and_clear_g_error'
 *
 * if function return non-MU_OK, the repl will print the error instead
 */

typedef MuError (*CmdFunc) (ServerContext*,GHashTable*,GError**);

/* 'add' adds a message to the database, and takes two parameters: 'path', which
 * is the full path to the message, and 'maildir', which is the maildir this
 * message lives in (e.g. "/inbox"). response with an (:info ...) message with
 * information about the newly added message (details: see code below)
 */
static MuError
cmd_add (ServerContext *ctx, GHashTable *args, GError **err)
{
        unsigned docid;
        const char *maildir, *path;
        MuMsg *msg;
        gchar *sexp;

        GET_STRING_OR_ERROR_RETURN (args, "path", &path, err);
        GET_STRING_OR_ERROR_RETURN (args, "maildir", &maildir, err);

        docid = mu_store_add_path (ctx->store, path, maildir, err);
        if (docid == MU_STORE_INVALID_DOCID)
                print_and_clear_g_error (err);
        else {
                gchar *escpath;
                escpath = mu_str_escape_c_literal (path, TRUE);
                print_expr ("(:info add :path %s :docid %u)", escpath, docid);

                msg = mu_store_get_msg (ctx->store, docid, err);
                if (msg) {
                        sexp = mu_msg_to_sexp (msg, docid, NULL,
                                               MU_MSG_OPTION_VERIFY);
                        print_expr ("(:update %s :move nil)", sexp);

                        mu_msg_unref(msg);
                        g_free (sexp);
                }
                g_free (escpath);
        }

        return MU_OK;
}


struct _PartInfo {
        GSList      *attlist;
        MuMsgOptions opts;
};
typedef struct _PartInfo PartInfo;

static void
each_part (MuMsg *msg, MuMsgPart *part, PartInfo *pinfo)
{
        char	*att, *cachefile, *encfile;
        GError	*err;

        /* exclude things that don't look like proper attachments,
         * unless they're images */
        if (!mu_msg_part_maybe_attachment(part))
                return;

        err	  = NULL;
        cachefile = mu_msg_part_save_temp (msg,
                                           pinfo->opts|MU_MSG_OPTION_OVERWRITE,
                                           part->index, &err);
        if (!cachefile) {
                print_and_clear_g_error (&err);
                return;
        }

        encfile = mu_str_escape_c_literal(cachefile, TRUE);
        g_free (cachefile);

        att = g_strdup_printf (
                "(:file-name %s :mime-type \"%s/%s\")",
                encfile, part->type, part->subtype);
        pinfo->attlist = g_slist_append (pinfo->attlist, att);
        g_free (encfile);
}


/* take the attachments of msg, save them as tmp files, and return
 * as sexp (as a string) describing them
 *
 * ((:name <filename> :mime-type <mime-type> :disposition
 *   <attachment|inline>) ... )
 *
 */
static gchar*
include_attachments (MuMsg *msg, MuMsgOptions opts)
{
        GSList  *cur;
        GString *gstr;
        PartInfo pinfo;

        pinfo.attlist = NULL;
        pinfo.opts    = opts;
        mu_msg_part_foreach (msg, opts,
                             (MuMsgPartForeachFunc)each_part,
                             &pinfo);

        gstr = g_string_sized_new (512);
        gstr = g_string_append_c (gstr, '(');
        for (cur = pinfo.attlist; cur; cur = g_slist_next (cur))
                g_string_append (gstr, (gchar*)cur->data);
        gstr = g_string_append_c (gstr, ')');

        mu_str_free_list (pinfo.attlist);

        return g_string_free (gstr, FALSE);
}

static MuMsgOptions
get_encrypted_msg_opts (GHashTable *args)
{
        MuMsgOptions opts;

        opts = MU_MSG_OPTION_NONE;

        if (get_bool_from_args (args, "use-agent", FALSE, NULL))
                opts |= MU_MSG_OPTION_USE_AGENT;
        if (get_bool_from_args (args, "extract-encrypted", FALSE, NULL))
                opts |= MU_MSG_OPTION_DECRYPT;

        return opts;
}

enum { NEW, REPLY, FORWARD, EDIT, RESEND, INVALID_TYPE };
static unsigned
compose_type (const char *typestr)
{
        if (EQSTR (typestr, "reply"))
                return REPLY;
        else if (EQSTR (typestr, "forward"))
                return FORWARD;
        else if (EQSTR (typestr, "edit"))
                return EDIT;
        else if (EQSTR (typestr, "resend"))
                return RESEND;
        else if (EQSTR (typestr, "new"))
                return NEW;
        else
                return INVALID_TYPE;
}

/* 'compose' produces the un-changed *original* message sexp (ie., the message
 * to reply to, forward or edit) for a new message to compose). It takes two
 * parameters: 'type' with the compose type (either reply, forward or
 * edit/resend), and 'docid' for the message to reply to. Note, type:new does
 * not have an original message, and therefore does not need a docid
 *
 * In returns a (:compose <type> [:original <original-msg>] [:include] )
 * message (detals: see code below)
 *
 * Note ':include' t or nil determines whether to include attachments
 */
static MuError
cmd_compose (ServerContext *ctx, GHashTable *args, GError **err)
{
        const gchar	*typestr;
        char		*sexp, *atts;
        unsigned	 ctype;
        MuMsgOptions	 opts;

        opts = get_encrypted_msg_opts (args);

        GET_STRING_OR_ERROR_RETURN (args, "type", &typestr, err);

        ctype = compose_type (typestr);
        if (ctype == INVALID_TYPE) {
                print_error (MU_ERROR_IN_PARAMETERS, "invalid type to compose");
                return MU_OK;
        }

        if (ctype == REPLY || ctype == FORWARD ||
            ctype == EDIT || ctype == RESEND) {
                MuMsg *msg;
                const char *docidstr;
                GET_STRING_OR_ERROR_RETURN (args, "docid", &docidstr, err);
                msg = mu_store_get_msg (ctx->store, atoi(docidstr), err);
                if (!msg) {
                        print_and_clear_g_error (err);
                        return MU_OK;
                }
                sexp = mu_msg_to_sexp (msg, atoi(docidstr), NULL, opts);
                atts = (ctype == FORWARD) ?
                        include_attachments (msg, opts) : NULL;
                mu_msg_unref (msg);
        } else
                atts = sexp = NULL;

        print_expr ("(:compose %s :original %s :include %s)",
                    typestr, sexp ? sexp : "nil", atts ? atts : "nil");

        g_free (sexp);
        g_free (atts);

        return MU_OK;
}


struct _SexpData {
        GString          *gstr;
        gboolean          personal;
        time_t            last_seen;
        gint64            tstamp;
        size_t            rank;
};
typedef struct _SexpData  SexpData;

static void
each_contact_sexp (const char* full_address,
                   const char *email, const char *name, gboolean personal,
                   time_t last_seen, unsigned freq,
                   gint64 tstamp, SexpData *sdata)
{
        char *escaddr;

        sdata->rank++;

        /* since the last time we got some contacts */
        if (sdata->tstamp > tstamp)
                return;

        /* (maybe) only include 'personal' contacts */
        if (sdata->personal && !personal)
                return;

        /* only include newer-than-x contacts */
        if (sdata->last_seen > last_seen)
                return;

        /* only include *real* e-mail addresses (ignore local
         * addresses... there's little to complete there anyway...) */
        if (!email || !strstr (email, "@"))
                return;

        escaddr = mu_str_escape_c_literal (full_address, TRUE);

        g_string_append_printf (sdata->gstr, "(%s . %zu)\n",
                                escaddr, sdata->rank);

        g_free (escaddr);
}


/**
 * get all contacts as an s-expression
 *
 * @param self contacts object
 * @param personal_only whether to restrict the list to 'personal' email
 * addresses
 *
 * @return the sexp
 */
static char*
contacts_to_sexp (const MuContacts *contacts, gboolean personal,
                  time_t last_seen, gint64 tstamp)
{
        SexpData sdata;
        gint64   cutoff;

        g_return_val_if_fail (contacts, NULL);

        memset (&sdata, 0, sizeof(SexpData));

        sdata.personal  = personal;
        sdata.last_seen = last_seen;
        sdata.tstamp    = tstamp;
        sdata.rank      = 0;

        /* make a guess for the initial size */
        sdata.gstr = g_string_sized_new (mu_contacts_count(contacts) * 128);
        g_string_append (sdata.gstr, "(:contacts (");

        cutoff = g_get_monotonic_time();
        mu_contacts_foreach (contacts,
                             (MuContactsForeachFunc)each_contact_sexp, &sdata);
        /* pass a string, elisp doesn't like 64-bit nums */
        g_string_append_printf (sdata.gstr,
                                ") :tstamp \"%" G_GINT64_FORMAT  "\")", cutoff);

        return g_string_free (sdata.gstr, FALSE);
}


static MuError
cmd_contacts (ServerContext *ctx, GHashTable *args, GError **err)
{
        const MuContacts *contacts;
        char             *sexp;
        gboolean          personal;
        time_t            after;
        const char       *str;
        gint64            tstamp;

        personal = get_bool_from_args (args, "personal", TRUE, NULL);
        str      = get_string_from_args (args, "after", TRUE, NULL);
        after    = str ? (time_t)atoi(str) : 0;

        // only get contacts updated since tstamp.
        str    = get_string_from_args (args, "tstamp", TRUE, NULL);
        tstamp = str ? g_ascii_strtoll (str, NULL, 10) : 0;

        contacts = mu_store_contacts(ctx->store);
        if (!contacts) {
                print_error (MU_ERROR_INTERNAL,
                             "failed to get contacts cache");
                return MU_OK;
        }

        /* dump the contacts cache as a giant sexp */
        sexp = contacts_to_sexp (contacts, personal, after, tstamp);
        print_expr ("%s\n", sexp);
        g_free (sexp);

        return MU_OK;
}



static unsigned
print_sexps (MuMsgIter *iter, unsigned maxnum)
{
        unsigned u;
        u = 0;

        while (!mu_msg_iter_is_done (iter) && u < maxnum && !MU_TERMINATE) {

                MuMsg *msg;
                msg = mu_msg_iter_get_msg_floating (iter);

                if (mu_msg_is_readable (msg)) {
                        char *sexp;
                        const MuMsgIterThreadInfo* ti;
                        ti   = mu_msg_iter_get_thread_info (iter);
                        sexp = mu_msg_to_sexp (msg,
                                               mu_msg_iter_get_docid (iter),
                                               ti, MU_MSG_OPTION_HEADERS_ONLY);
                        print_expr ("%s", sexp);
                        g_free (sexp);
                        ++u;
                }
                mu_msg_iter_next (iter);
        }
        return u;
}


static MuError
save_part (MuMsg *msg, unsigned docid, unsigned index,
           MuMsgOptions opts, GHashTable *args, GError **err)
{
        gboolean rv;
        const gchar *path;
        gchar *escpath;

        GET_STRING_OR_ERROR_RETURN (args, "path", &path, err);

        rv = mu_msg_part_save (msg, opts | MU_MSG_OPTION_OVERWRITE,
                               path, index, err);
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
open_part (MuMsg *msg, unsigned docid, unsigned index,
           MuMsgOptions opts, GError **err)
{
        char *targetpath;
        gboolean rv;

        targetpath = mu_msg_part_get_cache_path (msg, opts, index, err);
        if (!targetpath)
                return print_and_clear_g_error (err);

        rv = mu_msg_part_save (msg, opts | MU_MSG_OPTION_USE_EXISTING,
                               targetpath, index, err);
        if (!rv) {
                print_and_clear_g_error (err);
                goto leave;
        }

        rv = mu_util_play (targetpath, TRUE,/*allow local*/
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
temp_part (MuMsg *msg, unsigned docid, unsigned index,
           MuMsgOptions opts, GHashTable *args, GError **err)
{
        const char *what, *param;
        char *path;

        GET_STRING_OR_ERROR_RETURN (args, "what", &what, err);
        param = get_string_from_args (args, "param", TRUE, NULL);

        path = mu_msg_part_get_cache_path (msg, opts, index, err);
        if (!path)
                print_and_clear_g_error (err);
        else if (!mu_msg_part_save (msg, opts | MU_MSG_OPTION_USE_EXISTING,
                                    path, index, err))
                print_and_clear_g_error (err);
        else {
                gchar *escpath;
                escpath = mu_str_escape_c_literal (path, FALSE);

                if (param) {
                        char *escparam;
                        escparam = mu_str_escape_c_literal (param, FALSE);
                        print_expr ("(:temp \"%s\""
                                    " :what \"%s\""
                                    " :docid %u"
                                    " :param \"%s\""")",
                                    escpath, what, docid, escparam);
                        g_free (escparam);
                } else
                        print_expr ("(:temp \"%s\" :what \"%s\" :docid %u)",
                                    escpath, what, docid);

                g_free (escpath);

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
cmd_extract (ServerContext *ctx, GHashTable *args, GError **err)
{
        MuMsg *msg;
        int docid, index, action;
        MuError rv;
        MuMsgOptions opts;
        const char* actionstr, *indexstr;

        opts = get_encrypted_msg_opts (args);
        rv = MU_ERROR;

        /* read parameters */
        GET_STRING_OR_ERROR_RETURN (args, "action", &actionstr, err);
        GET_STRING_OR_ERROR_RETURN (args, "index",  &indexstr, err);
        index = atoi (indexstr);
        docid = determine_docid (ctx->query, args, err);
        if (docid == MU_STORE_INVALID_DOCID) {
                print_and_clear_g_error (err);
                return MU_OK;
        }

        if ((action = action_type (actionstr)) == INVALID_ACTION) {
                print_error (MU_ERROR_IN_PARAMETERS, "invalid action");
                return MU_OK;
        }
        msg = mu_store_get_msg (ctx->store, docid, err);
        if (!msg) {
                print_error (MU_ERROR, "failed to get message");
                return MU_OK;
        }

        switch (action) {
        case SAVE: rv = save_part (msg, docid, index, opts, args, err); break;
        case OPEN: rv = open_part (msg, docid, index, opts, err); break;
        case TEMP: rv = temp_part (msg, docid, index, opts, args, err); break;
        default: print_error (MU_ERROR_INTERNAL, "unknown action");
        }

        if (rv != MU_OK)
                print_and_clear_g_error (err);

        mu_msg_unref (msg);
        return MU_OK;
}

#define MAX_NUM_DEFAULT 500

/* parse the find parameters, and return the values as out params */
static MuError
get_find_params (GHashTable *args, MuMsgFieldId *sortfield,
                 int *maxnum, MuQueryFlags *qflags, GError **err)
{
        const char *maxnumstr, *sortfieldstr;

        /* defaults */
        *maxnum	   = MAX_NUM_DEFAULT;
        *qflags	   = MU_QUERY_FLAG_NONE;
        *sortfield = MU_MSG_FIELD_ID_NONE;

        /* field to sort by */
        sortfieldstr = get_string_from_args (args, "sortfield", TRUE, NULL);
        if (sortfieldstr) {
                *sortfield = mu_msg_field_id_from_name (sortfieldstr, FALSE);
                /* note: shortcuts are not allowed here */
                if (*sortfield == MU_MSG_FIELD_ID_NONE) {
                        mu_util_g_set_error (err, MU_ERROR_IN_PARAMETERS,
                                     "not a valid sort field: '%s'",
                                             sortfield);
                        return MU_G_ERROR_CODE(err);
                }
        } else
                *sortfield = MU_MSG_FIELD_ID_DATE;


        /* maximum number of results */
        maxnumstr = get_string_from_args (args, "maxnum", TRUE, NULL);
        if (maxnumstr)
                *maxnum = atoi (maxnumstr);

        if (get_bool_from_args (args, "reverse", TRUE, NULL))
                *qflags |= MU_QUERY_FLAG_DESCENDING;
        if (get_bool_from_args (args, "skip-dups", TRUE, NULL))
                *qflags |= MU_QUERY_FLAG_SKIP_DUPS;
        if (get_bool_from_args (args, "include-related", TRUE, NULL))
                *qflags |= MU_QUERY_FLAG_INCLUDE_RELATED;
        if (get_bool_from_args (args, "threads", TRUE, NULL))
                *qflags |= MU_QUERY_FLAG_THREADS;

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
cmd_find (ServerContext *ctx, GHashTable *args, GError **err)
{
        MuMsgIter    *iter;
        unsigned      foundnum;
        int           maxnum;
        MuMsgFieldId  sortfield;
        const char   *querystr;
        MuQueryFlags  qflags;
        char         *query;

        GET_STRING_OR_ERROR_RETURN (args, "query", &querystr, err);
        if (get_find_params (args, &sortfield, &maxnum, &qflags, err)
            != MU_OK) {
                print_and_clear_g_error (err);
                return MU_OK;
        }

        {
                char	*s;
                gsize	 len;
                s     = (char*)g_base64_decode (querystr, &len);
                query = g_strndup (s, len);
                g_free (s);
        }

        /* note: when we're threading, we get *all* matching messages,
         * and then only return maxnum; this is so that we maximimize
         * the change of all messages in a thread showing up */

        iter = mu_query_run (ctx->query, query, sortfield,
                             maxnum, qflags, err);
        g_free (query);
        if (!iter) {
                print_and_clear_g_error (err);
                return MU_OK;
        }

        /* before sending new results, send an 'erase' message, so the
         * frontend knows it should erase the headers buffer. this
         * will ensure that the output of two finds will not be
         * mixed. */
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


static void
set_my_addresses (MuStore *store, const char *addrstr)
{
        char **my_addresses;

        if (!addrstr)
                return;

        my_addresses = g_strsplit (addrstr, ",", -1);
        mu_store_set_personal_addresses (store, (const char**)my_addresses);

        g_strfreev (my_addresses);
}


static char*
get_checked_path (const char *path)
{
        char *cpath;

        cpath = mu_util_dir_expand(path);
        if (!cpath ||
            !mu_util_check_dir (cpath, TRUE, FALSE)) {
                char *err;
                print_error (MU_ERROR_IN_PARAMETERS,
                             "not a readable dir: '%s'", cpath);
                g_free (cpath);
                return NULL;
        }

        return cpath;
}


static MuError
index_and_maybe_cleanup (MuIndex *index, const char *path,
                         gboolean cleanup, gboolean lazy_check, GError **err)
{
        MuError      rv;
        MuIndexStats stats, stats2;

        mu_index_stats_clear (&stats);
        rv = mu_index_run (index, path, FALSE, lazy_check, &stats,
                           index_msg_cb, NULL, NULL);

        if (rv != MU_OK && rv != MU_STOP) {
                mu_util_g_set_error (err, MU_ERROR_INTERNAL, "indexing failed");
                return rv;
        }

        mu_index_stats_clear (&stats2);
        if (cleanup) {
                rv = mu_index_cleanup (index, &stats2, NULL, NULL, err);
                if (rv != MU_OK && rv != MU_STOP) {
                        mu_util_g_set_error (err, MU_ERROR_INTERNAL,
                                             "cleanup failed");
                        return rv;
                }
        }

        print_expr ("(:info index :status complete "
                    ":processed %u :updated %u :cleaned-up %u)",
                    stats._processed, stats._updated, stats2._cleaned_up);

        return rv;
}

/*
 * 'index' (re)indexs maildir at path:<path>, and responds with (:info
 * index ... ) messages while doing so (see the code)
 */
static MuError
cmd_index (ServerContext *ctx, GHashTable *args, GError **err)
{
        MuIndex		*index;
        const char	*argpath;
        char		*path;
        gboolean	 cleanup, lazy_check, contacts;

        index = NULL;

        GET_STRING_OR_ERROR_RETURN (args, "path", &argpath, err);
        if (!(path = get_checked_path (argpath)))
                goto leave;

        set_my_addresses (ctx->store, get_string_from_args
                          (args, "my-addresses", TRUE, NULL));

        if (!(index = mu_index_new (ctx->store, err)))
                goto leave;

        cleanup	   = get_bool_from_args (args, "cleanup", TRUE, NULL);
        lazy_check = get_bool_from_args (args, "lazy-check", TRUE, NULL);
        lazy_check = get_bool_from_args (args, "contacts", TRUE, NULL);

        index_and_maybe_cleanup (index, path,
                                 cleanup, lazy_check,
                                 err);
leave:
        g_free (path);

        if (err && *err)
                print_and_clear_g_error (err);

        mu_index_destroy (index);

        return MU_OK;
}



/* 'mkdir' attempts to create a maildir directory at 'path:'; sends an
 * (:info mkdir ...) message when it succeeds */
static MuError
cmd_mkdir (ServerContext *ctx, GHashTable *args, GError **err)
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
                        return  mu_flags_from_str (flagstr, MU_FLAG_TYPE_ANY,
                                                   TRUE /*ignore invalid*/);
        }
}


static MuError
do_move (MuStore *store, unsigned docid, MuMsg *msg, const char *maildir,
         MuFlags flags, gboolean new_name, gboolean no_view, GError **err)
{
        unsigned	 rv;
        gchar		*sexp;
        gboolean	 different_mdir;

        if (!maildir) {
                maildir = mu_msg_get_maildir (msg);
                different_mdir = FALSE;
        } else
                /* are we moving to a different mdir, or is it just flags? */
                different_mdir =
                        (g_strcmp0 (maildir, mu_msg_get_maildir(msg)) != 0);

        if (!mu_msg_move_to_maildir (msg, maildir, flags, TRUE,
                                     new_name, err))
                return MU_G_ERROR_CODE (err);

        /* after mu_msg_move_to_maildir, path will be the *new* path,
         * and flags and maildir fields will be updated as wel */
        rv = mu_store_update_msg (store, docid, msg, err);
        if (rv == MU_STORE_INVALID_DOCID) {
                mu_util_g_set_error (err, MU_ERROR_XAPIAN,
                                     "failed to store updated message");
                print_and_clear_g_error (err);
        }

        sexp = mu_msg_to_sexp (msg, docid, NULL, MU_MSG_OPTION_VERIFY);
        /* note, the :move t thing is a hint to the frontend that it
         * could remove the particular header */
        print_expr ("(:update %s :move %s :maybe-view %s)", sexp,
                    different_mdir ? "t" : "nil",
                    no_view ? "nil" : "t");
        g_free (sexp);

        return MU_OK;
}

static MuError
move_docid (MuStore *store, unsigned docid, const char* flagstr,
            gboolean new_name, gboolean no_view, GError **err)
{
        MuMsg		*msg;
        MuError		 rv;
        MuFlags		 flags;

        rv  = MU_ERROR;
        msg = mu_store_get_msg (store, docid, err);

        if (!msg)
                goto leave;

        flags = flagstr ? get_flags (mu_msg_get_path(msg), flagstr) :
                mu_msg_get_flags (msg);

        if (flags == MU_FLAG_INVALID) {
                mu_util_g_set_error (err, MU_ERROR_IN_PARAMETERS,
                                     "invalid flags");
                goto leave;
        }

        rv = do_move (store, docid, msg, NULL, flags,
                      new_name, no_view, err);

leave:
        if (msg)
                mu_msg_unref (msg);
        if (rv != MU_OK)
                print_and_clear_g_error (err);

        return rv;
}


/* when called with a msgid, we need to take care of possibly multiple
 * messages with this message id. this is a common case when sending
 * messages to ourselves (maybe through a mailing list), where there
 * would a message in the inbox and sent folders with the same id. we
 * set the flag on both */
static gboolean
move_msgid_maybe (ServerContext *ctx, GHashTable *args, GError **err)
{
        GSList		*docids, *cur;
        const char*	 maildir, *msgid, *flagstr;
        gboolean	 new_name, no_view;

        maildir	 = get_string_from_args (args, "maildir", TRUE, err);
        msgid	 = get_string_from_args (args, "msgid", TRUE, err);
        flagstr	 = get_string_from_args (args, "flags", TRUE, err);
        new_name = get_bool_from_args (args, "newname", TRUE, err);
        no_view  = get_bool_from_args (args, "noupdate", TRUE, err);

        /*  you cannot use 'maildir' for multiple messages at once */
        if (!msgid || !flagstr || maildir)
                return FALSE;

        if (!(docids = get_docids_from_msgids (ctx->query, msgid,
                                               -1/*unlimited*/, err))) {
                print_and_clear_g_error (err);
                return TRUE;
        }

        for (cur = docids; cur; cur = g_slist_next(cur))
                if (move_docid (ctx->store, GPOINTER_TO_SIZE(cur->data),
                                flagstr, new_name, no_view, err) != MU_OK)
                        break;

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
cmd_move (ServerContext *ctx, GHashTable *args, GError **err)
{
        unsigned docid;
        MuMsg *msg;
        MuFlags flags;
        const char *maildir, *flagstr;
        gboolean new_name, no_view;

        /* check if the move is based on the message id; if so, handle
         * it in move_msgid_maybe */
        if (move_msgid_maybe (ctx, args, err))
                return MU_OK;

        maildir	 = get_string_from_args (args, "maildir", TRUE, err);
        flagstr	 = get_string_from_args (args, "flags", TRUE, err);
        new_name = get_bool_from_args (args, "newname", TRUE, err);
        no_view  = get_bool_from_args (args, "noupdate", TRUE, err);

        docid = determine_docid (ctx->query, args, err);
        if (docid == MU_STORE_INVALID_DOCID ||
            !(msg = mu_store_get_msg (ctx->store, docid, err))) {
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

        if ((do_move (ctx->store, docid, msg, maildir, flags,
                      new_name, no_view, err)
             != MU_OK))
                print_and_clear_g_error (err);

leave:
        mu_msg_unref (msg);
        return MU_OK;
}



/* 'ping' takes no parameters, and provides information about this mu
 * server using a (:pong ...) message (details: see code below)
 */
static MuError
cmd_ping (ServerContext *ctx, GHashTable *args, GError **err)
{
        unsigned doccount;
        doccount = mu_store_count (ctx->store, err);

        if (doccount == (unsigned)-1)
                return print_and_clear_g_error (err);

        print_expr ("(:pong \"" PACKAGE_NAME "\" "
                    " :props (:crypto %s :guile %s "
                    "  :version \"" VERSION "\" "
                    "  :doccount %u))",
                    mu_util_supports (MU_FEATURE_CRYPTO) ? "t" : "nil",
                    mu_util_supports (MU_FEATURE_GUILE|MU_FEATURE_GNUPLOT)
                    ? "t" : "nil",
                    doccount);

        return MU_OK;
}


/* 'quit' takes no parameters, terminates this mu server */
static MuError
cmd_quit (ServerContext *ctx, GHashTable *args , GError **err)
{
        print_expr (";; quitting");

        return MU_STOP;
}


/*
 * creating a message object just to get a path seems a bit excessive
 * maybe mu_store_get_path could be added if this turns out to be a
 * problem
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

        strncpy (path, msgpath, sizeof(path) - 1);
        path[sizeof(path)-1] = '\0';

        mu_msg_unref (msg);
        return path;
}



/* 'remove' removes the message with either docid: or msgid:, sends a
 * (:remove ...) message when it succeeds
 */
static MuError
cmd_remove (ServerContext *ctx, GHashTable *args, GError **err)
{
        unsigned docid;
        const char *path;

        docid = determine_docid (ctx->query, args, err);
        if (docid == MU_STORE_INVALID_DOCID) {
                print_and_clear_g_error (err);
                return MU_OK;
        }

        path = get_path_from_docid (ctx->store, docid, err);
        if (!path) {
                print_and_clear_g_error (err);
                return MU_OK;
        }

        if (unlink (path) != 0) {
                mu_util_g_set_error (err, MU_ERROR_FILE_CANNOT_UNLINK,
                                     "%s", strerror (errno));
                print_and_clear_g_error (err);
                return MU_OK;
        }

        if (!mu_store_remove_path (ctx->store, path)) {
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
cmd_sent (ServerContext *ctx, GHashTable *args, GError **err)
{
        unsigned docid;
        const char *maildir, *path;

        GET_STRING_OR_ERROR_RETURN (args, "path", &path, err);
        GET_STRING_OR_ERROR_RETURN (args, "maildir", &maildir, err);

        docid = mu_store_add_path (ctx->store, path, maildir, err);
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

static MuMsgOptions
get_view_msg_opts (GHashTable *args)
{
        MuMsgOptions opts;

        opts = MU_MSG_OPTION_VERIFY;

        if (get_bool_from_args (args, "extract-images", FALSE, NULL))
                opts |= MU_MSG_OPTION_EXTRACT_IMAGES;
        if (get_bool_from_args (args, "use-agent", FALSE, NULL))
                opts |= MU_MSG_OPTION_USE_AGENT;
        if (get_bool_from_args (args, "auto-retrieve-key", FALSE, NULL))
                opts |= MU_MSG_OPTION_AUTO_RETRIEVE;
        if (get_bool_from_args (args, "extract-encrypted", FALSE, NULL))
                opts |= MU_MSG_OPTION_DECRYPT;

        return opts;
}

/* 'view' gets a full (including body etc.) sexp for some message,
 * identified by either docid: or msgid:; return a (:view <sexp>)
 */
static MuError
cmd_view (ServerContext *ctx, GHashTable *args, GError **err)
{
        MuMsg *msg;
        const gchar *path;
        char *sexp;
        MuMsgOptions opts;
        unsigned docid;

        opts = get_view_msg_opts (args);

        /* when 'path' is specified, get the message at path */
        path = get_string_from_args (args, "path", FALSE, NULL);

        if (path) {
                docid = 0;
                msg   = mu_msg_new_from_file (path, NULL, err);
        } else {
                docid = determine_docid (ctx->query, args, err);
                if (docid == MU_STORE_INVALID_DOCID) {
                        print_and_clear_g_error (err);
                        return MU_OK;
                }
                msg = mu_store_get_msg (ctx->store, docid, err);
        }

        if (!msg) {
                print_and_clear_g_error (err);
                return MU_OK;
        }

        sexp = mu_msg_to_sexp (msg, docid, NULL, opts);
        mu_msg_unref (msg);

        print_expr ("(:view %s)\n", sexp);
        g_free (sexp);

        return MU_OK;
}

/*************************************************************************/

static MuError
handle_args (ServerContext *ctx, GHashTable *args, GError **err)
{
        unsigned u;
        const char *cmd;
        struct {
                const char *cmd;
                CmdFunc func;
        } cmd_map[] = {
                { "add",	cmd_add },
                { "compose",	cmd_compose },
                { "contacts",   cmd_contacts },
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

        cmd = g_hash_table_lookup (args, "cmd");

        /* ignore empty */
        if (mu_str_is_empty (cmd))
                return MU_OK;

        for (u = 0; u != G_N_ELEMENTS (cmd_map); ++u)
                if (g_strcmp0(cmd, cmd_map[u].cmd) == 0)
                        return cmd_map[u].func (ctx, args, err);

        mu_util_g_set_error (err, MU_ERROR_IN_PARAMETERS,
                             "unknown command '%s'", cmd ? cmd : "");

        return MU_G_ERROR_CODE (err);
}


MuError
mu_cmd_server (MuStore *store, MuConfig *opts/*unused*/, GError **err)
{
        ServerContext ctx;
        gboolean      do_quit;

        g_return_val_if_fail (store, MU_ERROR_INTERNAL);

        ctx.store = store;
        ctx.query = mu_query_new (store, err);
        if (!ctx.query)
                return MU_G_ERROR_CODE (err);

        srand (time(NULL)*getpid());

        install_sig_handler ();

        g_print (";; welcome to " PACKAGE_STRING "\n");

        /*  the main REPL */
        do_quit = FALSE;
        while (!MU_TERMINATE && !do_quit) {

                GHashTable	*args;
                GError		*my_err;

                /* args will receive a the command as a list of
                 * strings. returning NULL indicates an error */
                my_err = NULL;
                args   = read_command_line (&my_err);
                if (!args) {
                        if (feof(stdin))
                                break;
                        if (my_err)
                                print_and_clear_g_error (&my_err);
                        continue;
                }

                switch (handle_args (&ctx, args, &my_err)) {
                case MU_OK:
                        break;
                case MU_STOP:
                        do_quit = TRUE;
                        break;
                default: /* some error occurred */
                        print_and_clear_g_error (&my_err);
                }

                g_hash_table_destroy (args);
        }

        mu_store_flush   (ctx.store);
        mu_query_destroy (ctx.query);

        return MU_OK;
}
