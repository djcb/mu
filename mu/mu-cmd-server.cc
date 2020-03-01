/*
** Copyright (C) 2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "config.h"
#include "mu-cmd.h"


#include <iostream>
#include <string>
#include <algorithm>
#include <atomic>
#include <cstring>
#include <glib.h>
#include <glib/gprintf.h>

#ifdef HAVE_LIBREADLINE
#  if defined(HAVE_READLINE_READLINE_H)
#    include <readline/readline.h>
#  elif defined(HAVE_READLINE_H)
#    include <readline.h>
#  else /* !defined(HAVE_READLINE_H) */
extern char *readline ();
#  endif /* !defined(HAVE_READLINE_H) */
char *cmdline = NULL;
#else /* !defined(HAVE_READLINE_READLINE_H) */
   /* no readline */
#endif /* HAVE_LIBREADLINE */

#ifdef HAVE_READLINE_HISTORY
#  if defined(HAVE_READLINE_HISTORY_H)
#    include <readline/history.h>
#  elif defined(HAVE_HISTORY_H)
#    include <history.h>
#  else /* !defined(HAVE_HISTORY_H) */
extern void add_history ();
extern int write_history ();
extern int read_history ();
#  endif /* defined(HAVE_READLINE_HISTORY_H) */
   /* no history */
#endif /* HAVE_READLINE_HISTORY */


#include "mu-runtime.h"
#include "mu-cmd.h"
#include "mu-maildir.h"
#include "mu-query.h"
#include "mu-index.h"
#include "mu-store.hh"
#include "mu-msg-part.h"
#include "mu-contacts.hh"

#include "utils/mu-str.h"
#include "utils/mu-utils.hh"
#include "utils/mu-command-parser.hh"

using namespace Mu;
using namespace Command;
using namespace Sexp;

using DocId = unsigned;

static std::atomic<bool> MuTerminate{false};

static void
sig_handler (int sig)
{
        MuTerminate = true;
}

static void
install_sig_handler (void)
{
        struct sigaction action;
        int i, sigs[] = { SIGINT, SIGHUP, SIGTERM, SIGPIPE };

        MuTerminate = false;

        action.sa_handler = sig_handler;
        sigemptyset(&action.sa_mask);
        action.sa_flags   = SA_RESETHAND;

        for (i = 0; i != G_N_ELEMENTS(sigs); ++i)
                if (sigaction (sigs[i], &action, NULL) != 0)
                        g_critical ("set sigaction for %d failed: %s",
                                    sigs[i], g_strerror (errno));;
}


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
                           __func__, g_strerror(errno));
                /* terminate ourselves */
                raise (SIGTERM);
        }
}


G_GNUC_PRINTF(2,3) static MuError
print_error (MuError errcode, const char* frm, ...)
{
        char    *msg;
        va_list  ap;

        va_start (ap, frm);
        g_vasprintf (&msg, frm, ap);
        va_end (ap);

        print_expr ("(:error %u :message %s)", errcode, quote(msg).c_str());
        g_free (msg);

        return errcode;
}

static unsigned
print_sexps (MuMsgIter *iter, unsigned maxnum)
{
        unsigned u;
        u = 0;

        while (!mu_msg_iter_is_done (iter) && u < maxnum) {

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


struct Context {
        Context(){}
        Context (MuConfig *opts) {
                const auto dbpath{mu_runtime_path(MU_RUNTIME_PATH_XAPIANDB)};
                GError *gerr{};
                store = mu_store_new_writable (dbpath, NULL);
                if (!store) {
                        const auto mu_init = format("mu init %s%s",
                                                    opts->muhome ? "--muhome=" : "",
                                                    opts->muhome ? opts->muhome : "");

                        if (gerr) {
                                if ((MuError)gerr->code == MU_ERROR_XAPIAN_CANNOT_GET_WRITELOCK)
                                        print_error(MU_ERROR_XAPIAN_CANNOT_GET_WRITELOCK,
                                                    "mu database already locked; "
                                                    "some other mu running?");
                                else
                                        print_error((MuError)gerr->code,
                                                    "cannot open database @ %s:%s; already running? "
                                                    "if not, please try '%s", dbpath,
                                                    gerr->message ? gerr->message : "something went wrong",
                                                    mu_init.c_str());
                        } else
                                print_error(MU_ERROR,
                                            "cannot open database @ %s; already running? if not, please try '%s'",
                                            dbpath, mu_init.c_str());

                        throw Mu::Error (Error::Code::Store, &gerr/*consumed*/,
                                         "failed to open database @ %s; already running? if not, please try '%s'",
                                         dbpath, mu_init.c_str());
                }

                query = mu_query_new (store, &gerr);
                if (!query)
                        throw Error(Error::Code::Store, &gerr, "failed to create query");
        }

        ~Context() {
                if (query)
                        mu_query_destroy(query);
                if (store) {
                        mu_store_flush(store);
                        mu_store_unref(store);
                }
        }

        Context(const Context&) = delete;

        MuStore *store{};
        MuQuery *query{};
        bool do_quit{};

        CommandMap command_map;
};


static MuMsgOptions
message_options (const Parameters& params)
{
        const auto extract_images{get_bool_or(params, "extract-images", false)};
        const auto decrypt{get_bool_or(params, "decrypt", false)};
        const auto verify{get_bool_or(params, "verify", false)};

        int opts{MU_MSG_OPTION_NONE};
        if (extract_images)
                opts |= MU_MSG_OPTION_EXTRACT_IMAGES;
        if (verify)
                opts |= MU_MSG_OPTION_VERIFY  | MU_MSG_OPTION_USE_AGENT;
        if (decrypt)
                opts |= MU_MSG_OPTION_DECRYPT | MU_MSG_OPTION_USE_AGENT;

        return (MuMsgOptions)opts;
}

/* 'add' adds a message to the database, and takes two parameters: 'path', which
 * is the full path to the message, and 'maildir', which is the maildir this
 * message lives in (e.g. "/inbox"). response with an (:info ...) message with
 * information about the newly added message (details: see code below)
 */
static void
add_handler (Context& context, const Parameters& params)
{
        const auto path{get_string_or(params, "path")};

        GError *gerr{};
        const auto docid{mu_store_add_path (context.store, path.c_str(), &gerr)};
        if (docid == MU_STORE_INVALID_DOCID)
                throw Error(Error::Code::Store, &gerr, "failed to add message at %s",
                            path.c_str());

        print_expr ("(:info add :path %s :docid %u)", quote(path).c_str(), docid);

        auto msg{mu_store_get_msg(context.store, docid, &gerr)};
        if (!msg)
                throw Error(Error::Code::Store, &gerr, "failed to get message at %s",
                            path.c_str());

        auto sexp{mu_msg_to_sexp (msg, docid, NULL, MU_MSG_OPTION_VERIFY)};
        print_expr ("(:update %s :move nil)", sexp);
        mu_msg_unref(msg);
        g_free (sexp);
}


struct _PartInfo {
        GSList      *attlist;
        MuMsgOptions opts;
};
typedef struct _PartInfo PartInfo;

static void
each_part (MuMsg *msg, MuMsgPart *part, PartInfo *pinfo)
{
        char	*att, *cachefile;

        /* exclude things that don't look like proper attachments,
         * unless they're images */
        if (!mu_msg_part_maybe_attachment(part))
                return;

        GError *gerr{};
        cachefile = mu_msg_part_save_temp (msg,
                                           (MuMsgOptions)(pinfo->opts|MU_MSG_OPTION_OVERWRITE),
                                           part->index, &gerr);
        if (!cachefile)
                throw Error (Error::Code::File, &gerr, "failed to save part");

        att = g_strdup_printf ("(:file-name %s :mime-type \"%s/%s\")",
                               quote(cachefile).c_str(), part->type, part->subtype);
        pinfo->attlist = g_slist_append (pinfo->attlist, att);

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

enum { NEW, REPLY, FORWARD, EDIT, RESEND, INVALID_TYPE };
static unsigned
compose_type (const char *typestr)
{
        if (g_str_equal (typestr, "reply"))
                return REPLY;
        else if (g_str_equal (typestr, "forward"))
                return FORWARD;
        else if (g_str_equal (typestr, "edit"))
                return EDIT;
        else if (g_str_equal (typestr, "resend"))
                return RESEND;
        else if (g_str_equal (typestr, "new"))
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
static void
compose_handler (Context& context, const Parameters& params)
{
        const auto typestr{get_symbol_or(params, "type")};
        const auto ctype{compose_type(typestr.c_str())};
        if (ctype == INVALID_TYPE)
                throw Error(Error::Code::InvalidArgument, "invalid compose type");

        // message optioss below checks extract-images / extract-encrypted

        char *sexp{}, *atts{};
        if (ctype == REPLY || ctype == FORWARD || ctype == EDIT || ctype == RESEND) {

                GError *gerr{};
                const unsigned docid{(unsigned)get_int_or(params, "docid")};
                auto msg{mu_store_get_msg (context.store, docid, &gerr)};
                if (!msg)
                        throw Error{Error::Code::Store, &gerr, "faile to get message %u", docid};

                const auto opts{message_options(params)};
                sexp = mu_msg_to_sexp (msg, docid, NULL, opts);
                atts = (ctype == FORWARD) ? include_attachments (msg, opts) : NULL;
                mu_msg_unref (msg);
        }
        print_expr ("(:compose %s :original %s :include %s)",
                    typestr.c_str(), sexp ? sexp : "nil", atts ? atts : "nil");

        g_free (sexp);
        g_free (atts);
}


struct SexpData {
        GString  *gstr;
        gboolean  personal;
        time_t    last_seen;
        gint64    tstamp;
        size_t    rank;
};


static void
each_contact_sexp (const char* full_address,
                   const char *email, const char *name, gboolean personal,
                   time_t last_seen, unsigned freq,
                   gint64 tstamp, SexpData *sdata)
{
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

        g_string_append_printf (sdata->gstr, "(%s . %zu)\n",
                                quote(full_address).c_str(), sdata->rank);
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
contacts_to_sexp (const MuContacts *contacts, bool personal,
                  int64_t last_seen, gint64 tstamp)
{

        g_return_val_if_fail (contacts, NULL);

        SexpData sdata{};
        sdata.personal  = personal;
        sdata.last_seen = last_seen;
        sdata.tstamp    = tstamp;
        sdata.rank      = 0;

        /* make a guess for the initial size */
        sdata.gstr = g_string_sized_new (mu_contacts_count(contacts) * 128);
        g_string_append (sdata.gstr, "(:contacts (");

        const auto cutoff{g_get_monotonic_time()};
        mu_contacts_foreach (contacts, (MuContactsForeachFunc)each_contact_sexp, &sdata);
        /* pass a string, elisp doesn't like 64-bit nums */
        g_string_append_printf (sdata.gstr,
                                ") :tstamp \"%" G_GINT64_FORMAT  "\")", cutoff);

        return g_string_free (sdata.gstr, FALSE);
}


static void
contacts_handler (Context& context, const Parameters& params)
{
        const auto personal  = get_bool_or(params, "personal");
        const auto afterstr  = get_string_or(params, "after");
        const auto tstampstr = get_string_or(params, "tstamp");

        const auto after{afterstr.empty() ? 0 :
                        g_ascii_strtoll(date_to_time_t_string(afterstr, true).c_str(), {}, 10)};
        const auto tstamp = g_ascii_strtoll (tstampstr.c_str(), NULL, 10);

        const auto contacts{mu_store_contacts(context.store)};
        if (!contacts)
                throw Error{Error::Code::Internal, "failed to get contacts"};

        /* dump the contacts cache as a giant sexp */
        auto sexp = contacts_to_sexp (contacts, personal, after, tstamp);
        print_expr ("%s\n", sexp);
        g_free (sexp);
}

static void
save_part (MuMsg *msg, unsigned docid, unsigned index,
           MuMsgOptions opts, const Parameters& params)
{
        const auto path{get_string_or(params, "path")};
        if (path.empty())
                throw Error{Error::Code::Command, "missing path"};

        GError *gerr{};
        if (!mu_msg_part_save (msg, (MuMsgOptions)(opts | (int)MU_MSG_OPTION_OVERWRITE),
                               path.c_str(), index, &gerr))
                throw Error{Error::Code::File, &gerr, "failed to save part"};

        print_expr ("(:info save :message %s)", quote(path + " has been saved").c_str());
}


static void
open_part (MuMsg *msg, unsigned docid, unsigned index, MuMsgOptions opts)
{
        GError *gerr{};
        char *targetpath{mu_msg_part_get_cache_path (msg, opts, index, &gerr)};
        if (!targetpath)
                throw Error{Error::Code::File, &gerr, "failed to get cache-path"};

        if (!mu_msg_part_save (msg, (MuMsgOptions)(opts | MU_MSG_OPTION_USE_EXISTING),
                                targetpath, index, &gerr)) {
                g_free(targetpath);
                throw Error{Error::Code::File, &gerr, "failed to save to cache-path"};
        }

        if (!mu_util_play (targetpath, TRUE,/*allow local*/
                           FALSE/*allow remote*/, &gerr)) {
                g_free(targetpath);
                throw Error{Error::Code::File, &gerr, "failed to play"};
        }

        print_expr ("(:info open :message %s)",
                    quote(std::string{targetpath} + " has been opened").c_str());
        g_free (targetpath);
}

static void
temp_part (MuMsg *msg, unsigned docid, unsigned index,
           MuMsgOptions opts, const Parameters& params)
{
        const auto what{get_symbol_or(params, "what")};
        if (what.empty())
                throw Error{Error::Code::Command, "missing 'what'"};

        const auto param{get_string_or(params, "param")};

        GError *gerr{};
        char *path{mu_msg_part_get_cache_path (msg, opts, index, &gerr)};
        if (!path)
                throw Error{Error::Code::File, &gerr, "could not get cache path"};

        if (!mu_msg_part_save (msg, (MuMsgOptions)(opts | MU_MSG_OPTION_USE_EXISTING),
                               path, index, &gerr)) {
                g_free(path);
                throw Error{Error::Code::File, &gerr, "saving failed"};
        }

        const auto qpath{quote(path)};
        g_free(path);

        if (!param.empty())
                print_expr ("(:temp %s"
                            " :what \"%s\""
                            " :docid %u"
                            " :param %s"
                            ")",
                            qpath.c_str(), what.c_str(), docid, quote(param).c_str());
        else
                print_expr ("(:temp %s :what \"%s\" :docid %u)",
                            qpath.c_str(), what.c_str(), docid);
}



/* 'extract' extracts some mime part from a message */
static void
extract_handler (Context& context, const Parameters& params)
{
        const auto docid{get_int_or(params, "docid")};
        const auto index{get_int_or(params, "index")};
        const auto opts{message_options(params)};

        GError *gerr{};
        auto msg{mu_store_get_msg (context.store, docid, &gerr)};
        if (!msg)
                throw Error{Error::Code::Store, "failed to get message"};

        try {
                const auto action{get_symbol_or(params, "action")};
                if (action == "save")
                        save_part (msg, docid, index, opts, params);
                else if (action == "open")
                        open_part (msg, docid, index, opts);
                else if (action == "temp")
                        temp_part (msg, docid, index, opts, params);
                else {
                        throw Error{Error::Code::InvalidArgument,
                                        "unknown action '%s'", action.c_str()};
                }

        } catch (...) {
                mu_msg_unref (msg);
                throw;
        }
}


/* get a *list* of all messages with the given message id */
static std::vector<DocId>
docids_for_msgid (MuQuery *query, const std::string& msgid, size_t max=100)
{
        if (msgid.size() > MU_STORE_MAX_TERM_LENGTH - 1) {
                throw Error(Error::Code::InvalidArgument,
                                  "invalid message-id '%s'", msgid.c_str());
        }

        const auto xprefix{mu_msg_field_xapian_prefix(MU_MSG_FIELD_ID_MSGID)};
        /*XXX this is a bit dodgy */
        auto tmp{g_ascii_strdown(msgid.c_str(), -1)};
        auto rawq{g_strdup_printf("%c%s", xprefix, tmp)};
        g_free(tmp);

        GError *gerr{};
        auto iter{mu_query_run (query, rawq, MU_MSG_FIELD_ID_NONE, max, MU_QUERY_FLAG_RAW, &gerr)};
        g_free (rawq);
        if (!iter)
                throw Error(Error::Code::Store, &gerr, "failed to run msgid-query");
        if (mu_msg_iter_is_done (iter))
                throw Error(Error::Code::NotFound,
                                  "could not find message(s) for msgid %s", msgid.c_str());
        std::vector<DocId> docids;
        do {
                docids.emplace_back(mu_msg_iter_get_docid (iter));
        } while (mu_msg_iter_next (iter));
        mu_msg_iter_destroy (iter);

        return docids;
}

/*
 * creating a message object just to get a path seems a bit excessive maybe
 * mu_store_get_path could be added if this turns out to be a problem
 */
static std::string
path_from_docid (MuStore *store, unsigned docid)
{
        GError *gerr{};
        auto msg{mu_store_get_msg (store, docid, &gerr)};
        if (!msg)
                throw Error(Error::Code::Store, &gerr, "could not get message from store");

        auto p{mu_msg_get_path(msg)};
        if (!p) {
                mu_msg_unref(msg);
                throw Error(Error::Code::Store,
                            "could not get path for message %u", docid);
        }

        std::string msgpath{p};
        mu_msg_unref (msg);

        return msgpath;
}


static std::vector<DocId>
determine_docids (MuQuery *query, const Parameters& params)
{
        auto docid{get_int_or(params, "docid", 0)};
        const auto msgid{get_string_or(params, "msgid")};

        if ((docid == 0) == msgid.empty())
                throw Error(Error::Code::InvalidArgument,
                            "precisely one of docid and msgid must be specified");

        if (docid != 0)
                return { (unsigned)docid };
        else
                return docids_for_msgid (query, msgid.c_str());
}


static void
find_handler (Context& context, const Parameters& params)
{
        const auto query{get_string_or(params, "query")};
        const auto threads{get_bool_or(params, "threads", false)};
        const auto sortfieldstr{get_symbol_or(params, "sortfield")};
        const auto descending{get_bool_or(params, "descending", false)};
        const auto maxnum{get_int_or(params, "maxnum", -1/*unlimited*/)};
        const auto skip_dups{get_bool_or(params, "skip-dups", false)};
        const auto include_related{get_bool_or(params, "include-related", false)};

        MuMsgFieldId sort_field{MU_MSG_FIELD_ID_NONE};
        if (!sortfieldstr.empty()) {
                sort_field = mu_msg_field_id_from_name (
                        sortfieldstr.c_str() + 1, FALSE); // skip ':'
                if (sort_field == MU_MSG_FIELD_ID_NONE)
                        throw Error{Error::Code::InvalidArgument, "invalid sort field %s",
                                        sortfieldstr.c_str()};
        }

        int qflags{MU_QUERY_FLAG_NONE/*UNREADABLE*/};
        if (descending)
                qflags |= MU_QUERY_FLAG_DESCENDING;
        if (skip_dups)
                qflags |= MU_QUERY_FLAG_SKIP_DUPS;
        if (include_related)
                qflags |= MU_QUERY_FLAG_INCLUDE_RELATED;
        if (threads)
                qflags |= MU_QUERY_FLAG_THREADS;

        GError *gerr{};
        auto miter{mu_query_run(context.query, query.c_str(), sort_field, maxnum,
                                (MuQueryFlags)qflags, &gerr)};
        if (!miter)
                throw Error(Error::Code::Query, &gerr, "failed to run query");

        /* before sending new results, send an 'erase' message, so the frontend
         * knows it should erase the headers buffer. this will ensure that the
         * output of two finds will not be mixed. */
        print_expr ("(:erase t)");
        const auto foundnum{print_sexps (miter, maxnum)};
        print_expr ("(:found %u)", foundnum);
        mu_msg_iter_destroy (miter);
}


static void
help_handler (Context& context, const Parameters& params)
{
        const auto command{get_symbol_or(params, "command", "")};
        const auto full{get_bool_or(params, "full")};

        if (command.empty()) {
                std::cout << ";; Commands are s-expressions of the form\n"
                          << ";;   (<command-name> :param1 val1 :param2 val2 ...)\n"
                          << ";; For instance:\n;;  (help :command quit)\n"
                          << ";; to get information about the 'quit' commmand\n;;\n";
                std::cout << ";; The following commands are available:\n";
        }

        std::vector<std::string> names;
        for (auto&& name_cmd: context.command_map)
                names.emplace_back(name_cmd.first);
        std::sort(names.begin(), names.end());

        for (auto&& name: names) {
                const auto& info{context.command_map.find(name)->second};

                if (!command.empty() && name != command)
                        continue;

                if (!command.empty())
                        std::cout << ";;   " << format("%-10s -- %s\n", name.c_str(),
                                                       info.docstring.c_str());
                else
                        std::cout << ";;  " << name.c_str() << ": "
                                  << info.docstring.c_str() << '\n';

                if (!full)
                        continue;

                for (auto&& argname: info.sorted_argnames()) {
                        const auto& arg{info.args.find(argname)};
                        std::cout << ";;        "
                                  << format("%-17s  : %-24s ", arg->first.c_str(),
                                            to_string(arg->second).c_str());
                        std::cout << "  " << arg->second.docstring << "\n";
                }
                std::cout << ";;\n";
        }
}

static MuError
index_msg_cb (MuIndexStats *stats, void *user_data)
{
        if (MuTerminate)
                return MU_STOP;

        if (stats->_processed % 1000)
                return MU_OK;

        print_expr ("(:info index :status running "
                    ":processed %u :updated %u)",
                    stats->_processed, stats->_updated);

        return MU_OK;
}


static MuError
index_and_maybe_cleanup (MuIndex *index, bool cleanup, bool lazy_check)
{
        MuIndexStats stats{}, stats2{};
        mu_index_stats_clear (&stats);
        auto rv = mu_index_run (index, FALSE, lazy_check, &stats,
                                index_msg_cb, NULL, NULL);
        if (rv != MU_OK && rv != MU_STOP)
                throw Error{Error::Code::Store, "indexing failed"};

        mu_index_stats_clear (&stats2);
        if (cleanup) {
                GError *gerr{};
                rv = mu_index_cleanup (index, &stats2, NULL, NULL, &gerr);
                if (rv != MU_OK && rv != MU_STOP)
                        throw Error{Error::Code::Store, &gerr, "cleanup failed"};
        }

        print_expr ("(:info index :status complete "
                    ":processed %u :updated %u :cleaned-up %u)",
                    stats._processed, stats._updated, stats2._cleaned_up);

        return rv;
}


static void
index_handler (Context& context, const Parameters& params)
{
        GError *gerr{};
        const auto cleanup{get_bool_or(params,   "cleanup")};
        const auto lazy_check{get_bool_or(params,  "lazy-check")};
        auto index{mu_index_new (context.store, &gerr)};
        if (!index)
                throw Error(Error::Code::Index, &gerr, "failed to create index object");

        try {
                index_and_maybe_cleanup (index, cleanup, lazy_check);
        } catch (...) {
                mu_index_destroy(index);
                throw;
        }
        mu_index_destroy(index);
        mu_store_flush(context.store);
}

static void
mkdir_handler (Context& context, const Parameters& params)
{
        const auto path{get_string_or(params, "path")};

        GError *gerr{};
        if (!mu_maildir_mkdir(path.c_str(), 0755, FALSE, &gerr))
                throw Error{Error::Code::File, &gerr, "failed to create maildir"};

        print_expr ("(:info mkdir :message \"%s has been created\")", path.c_str());
}


static MuFlags
get_flags (const std::string& path, const std::string& flagstr)
{
        if (flagstr.empty())
                return MU_FLAG_NONE; /* ie., ignore flags */
        else {
                /* if there's a '+' or '-' sign in the string, it must
                 * be a flag-delta */
                if (strstr (flagstr.c_str(), "+") || strstr (flagstr.c_str(), "-")) {
                        auto oldflags = mu_maildir_get_flags_from_path (path.c_str());
                        return mu_flags_from_str_delta (flagstr.c_str(), oldflags, MU_FLAG_TYPE_ANY);
                } else
                        return  mu_flags_from_str (flagstr.c_str(), MU_FLAG_TYPE_ANY,
                                                   TRUE /*ignore invalid*/);
        }
}

static void
do_move (MuStore *store, DocId docid, MuMsg *msg, const std::string& maildirarg,
         MuFlags flags, bool new_name, bool no_view)
{
        bool different_mdir{};
        auto maildir{maildirarg};
        if (maildir.empty()) {
                maildir = mu_msg_get_maildir (msg);
                different_mdir = FALSE;
        } else /* are we moving to a different mdir, or is it just flags? */
                different_mdir = maildir != mu_msg_get_maildir(msg);

        GError* gerr{};
        if (!mu_msg_move_to_maildir (msg, maildir.c_str(), flags, TRUE, new_name, &gerr))
                throw Error{Error::Code::File, &gerr, "failed to move message"};

        /* after mu_msg_move_to_maildir, path will be the *new* path, and flags and maildir fields
         * will be updated as wel */
        auto rv = mu_store_update_msg (store, docid, msg, &gerr);
        if (rv == MU_STORE_INVALID_DOCID)
                throw Error{Error::Code::Store, &gerr, "failed to store updated message"};

        char *sexp = mu_msg_to_sexp (msg, docid, NULL, MU_MSG_OPTION_VERIFY);
        /* note, the :move t thing is a hint to the frontend that it
         * could remove the particular header */
        print_expr ("(:update %s :move %s :maybe-view %s)", sexp,
                    different_mdir ? "t" : "nil",
                    no_view ? "nil" : "t");
        g_free (sexp);
}

static void
move_docid (MuStore *store, DocId docid, const std::string& flagstr,
            bool new_name, bool no_view)
{
        if (docid == MU_STORE_INVALID_DOCID)
                throw Error{Error::Code::InvalidArgument, "invalid docid"};

        GError *gerr{};
        auto msg{mu_store_get_msg (store, docid, &gerr)};

        try {
                if (!msg)
                        throw Error{Error::Code::Store, &gerr, "failed to get message from store"};

                const auto flags = flagstr.empty() ? mu_msg_get_flags (msg) :
                        get_flags (mu_msg_get_path(msg), flagstr);
                if (flags == MU_FLAG_INVALID)
                        throw Error{Error::Code::InvalidArgument, "invalid flags '%s'", flagstr.c_str()};

                do_move (store, docid, msg, "", flags, new_name, no_view);

        } catch (...) {
                if (msg)
                        mu_msg_unref (msg);
                throw;
        }

        mu_msg_unref (msg);
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
static void
move_handler (Context& context, const Parameters& params)
{
        auto maildir{get_string_or(params, "maildir")};
        const auto flagstr{get_string_or(params, "flags")};
        const auto rename{get_bool_or (params, "rename")};
        const auto no_view{get_bool_or (params, "noupdate")};
        const auto docids{determine_docids (context.query, params)};

        if (docids.size() > 1) {
                if (!maildir.empty()) // ie. duplicate message-ids.
                        throw Mu::Error{Error::Code::Store,
                                        "can't move multiple messages at the same time"};
                // multi.
                for (auto&& docid: docids)
                        move_docid(context.store, docid, flagstr, rename, no_view);
                return;
        }
        auto docid{docids.at(0)};

        GError *gerr{};
        auto msg{mu_store_get_msg(context.store, docid, &gerr)};
        if (!msg)
                throw Error{Error::Code::InvalidArgument, &gerr, "could not create message"};

        /* if maildir was not specified, take the current one */
        if (maildir.empty())
                maildir = mu_msg_get_maildir (msg);

        /* determine the real target flags, which come from the flags-parameter
         * we received (ie., flagstr), if any, plus the existing message
         * flags. */
        MuFlags flags{};
        if (!flagstr.empty())
                flags = get_flags (mu_msg_get_path(msg), flagstr.c_str());
        else
                flags = mu_msg_get_flags (msg);

        if (flags == MU_FLAG_INVALID) {
                mu_msg_unref(msg);
                throw Error{Error::Code::InvalidArgument, "invalid flagse"};
        }

        try {
                do_move (context.store, docid, msg, maildir, flags, rename, no_view);
        } catch (...) {
                mu_msg_unref(msg);
                throw;
        }

        mu_msg_unref(msg);
}

static void
ping_handler (Context& context, const Parameters& params)
{
        GError *gerr{};
        const auto storecount = mu_store_count(context.store, &gerr);
        if (storecount == (unsigned)-1)
                throw Error{Error::Code::Store, &gerr, "failed to read store"};

        const auto queries  = get_string_vec (params, "queries");
        const auto qresults= [&]() -> std::string {
                if (queries.empty())
                        return {};

                std::string res{":queries ("};
                for (auto&& q: queries) {
                        const auto count{mu_query_count_run (context.query, q.c_str())};
                        const auto unreadq{format("(%s) AND flag:unread", q.c_str())};
                        const auto unread{mu_query_count_run (context.query, unreadq.c_str())};
                        res += format("(:query %s :count %zu :unread %zu)", quote(q).c_str(), count, unread);
                }
                return res + ")";
        }();

        const auto personal = [&]() ->std::string {
                auto addrs{mu_store_personal_addresses (context.store)};
                std::string res;
                if (addrs && g_strv_length(addrs) != 0) {
                        res = ":personal-addresses (";
                        for (int i = 0; addrs[i]; ++i)
                                res += quote(addrs[i]) + ' ';
                        res += ")";
                }
                g_strfreev(addrs);
                return res;
        }();

        print_expr ("(:pong \"mu\" :props ("
                    ":version \"" VERSION "\" "
                    "%s "
                    ":database-path %s "
                    ":root-maildir %s "
                    ":doccount %u "
                    "%s))",
                    personal.c_str(),
                    quote(mu_store_database_path(context.store)).c_str(),
                    quote(mu_store_root_maildir(context.store)).c_str(),
                    storecount,
                    qresults.c_str());
}

static void
quit_handler (Context& context, const Parameters& params)
{
        context.do_quit = true;
}


static void
remove_handler (Context& context, const Parameters& params)
{
        const auto docid{get_int_or(params, "docid")};
        const auto path{path_from_docid (context.store, docid)};

        if (::unlink (path.c_str()) != 0 && errno != ENOENT)
                throw Error(Error::Code::File, "could not delete %s: %s",
                                  path.c_str(), strerror (errno));

        if (!mu_store_remove_path (context.store, path.c_str()))
                throw Error(Error::Code::Store,
                            "failed to remove message @ %s (%d) from store",
                            path.c_str(), docid);

        print_expr ("(:remove %u)", docid);
}


static void
sent_handler (Context& context, const Parameters& params)
{
        GError *gerr{};
        const auto path{get_string_or(params, "path")};
        const auto docid{mu_store_add_path(context.store, path.c_str(), &gerr)};
        if (docid == MU_STORE_INVALID_DOCID)
                throw Error{Error::Code::Store, &gerr, "failed to add path"};

        print_expr ("(:sent t :path %s :docid %u)", quote(path).c_str(), docid);
}


static void
view_handler (Context& context, const Parameters& params)
{
        DocId docid{};
        const auto path{get_string_or(params, "path")};

        GError *gerr{};
        MuMsg *msg{};

        if (!path.empty())
                msg   = mu_msg_new_from_file (path.c_str(), NULL, &gerr);
        else {
                docid = determine_docids(context.query, params).at(0);
                msg   = mu_store_get_msg (context.store, docid, &gerr);
        }

        if (!msg)
                throw Error{Error::Code::Store, &gerr, "failed to find message for view"};

        auto sexp{mu_msg_to_sexp(msg, docid, {}, message_options(params))};
        mu_msg_unref(msg);

        print_expr ("(:view %s)\n", sexp);
        g_free (sexp);
}


static CommandMap
make_command_map (Context& context)
{
      CommandMap cmap;

      cmap.emplace("add",
                   CommandInfo{
                           ArgMap{ {"path",    ArgInfo{Type::String, true, "file system path to the message" }}},
                           "add a message to the store",
                           [&](const auto& params){add_handler(context, params);}});

      cmap.emplace("compose",
                   CommandInfo{
                           ArgMap{{"type", ArgInfo{Type::Symbol, true,
                                            "type of composition: reply/forward/edit/resend/new"}},
                                   {"docid", ArgInfo{Type::Integer, false,"document id of parent-message, if any"}},
                                   {"decrypt", ArgInfo{Type::Symbol, false, "whether to decrypt encrypted parts (if any)" }}},
                           "get contact information",
                           [&](const auto& params){compose_handler(context, params);}});

      cmap.emplace("contacts",
                   CommandInfo{
                           ArgMap{ {"personal", ArgInfo{Type::Symbol, false,
                                                   "only personal contacts" }},
                                   {"after", ArgInfo{Type::String, false,
                                            "only contacts seen after time_t string" }},
                                   {"tstamp",  ArgInfo{Type::String, false,
                                            "return changes since tstamp" }}},
                           "get contact information",
                           [&](const auto& params){contacts_handler(context, params);}});

      cmap.emplace("extract",
                   CommandInfo{
                           ArgMap{{"docid", ArgInfo{Type::Integer, true,  "document for the message" }},
                                   {"index", ArgInfo{Type::Integer, true,  "index for the part to operate on" }},
                                   {"action", ArgInfo{Type::Symbol, true, "what to do with the part" }},
                                   {"decrypt", ArgInfo{Type::Symbol, false,
                                            "whether to decrypt encrypted parts (if any)" }},
                                   {"path",    ArgInfo{Type::String, false, "part for saving (for action: save)" }},
                                   {"what",    ArgInfo{Type::Symbol, false, "what to do with the part (feedback)" }},
                                   {"param",   ArgInfo{Type::String, false, "parameter for 'what'" }}},
                           "extract mime-parts from a message",
                           [&](const auto& params){extract_handler(context, params);}});

      cmap.emplace("find",
                   CommandInfo{
                           ArgMap{ {"query",   ArgInfo{Type::String, true, "search expression" }},
                                   {"threads", ArgInfo{Type::Symbol, false,
                                            "whether to include threading information" }},
                                   {"sortfield",  ArgInfo{Type::Symbol, false, "the field to sort results by" }},
                                   {"descending", ArgInfo{Type::Symbol, false,
                                            "whether to sort in descending order" }},
                                   {"maxnum",  ArgInfo{Type::Integer, false,
                                            "maximum number of result (hint)" }},
                                   {"skip-dups",  ArgInfo{Type::Symbol, false,
                                            "whether to skip messages with duplicate message-ids" }},
                                   {"include-related",  ArgInfo{Type::Symbol, false,
                                            "whether to include other message related to matching ones" }}},
                           "query the database for messages",
                           [&](const auto& params){find_handler(context, params);}});

      cmap.emplace("help",
                   CommandInfo{
                           ArgMap{ {"command", ArgInfo{Type::Symbol, false,
                                                   "command to get information for" }},
                                   {"full", ArgInfo{Type::Symbol, false,
                                            "whether to include information about parameters" }}},
                           "get information about one or all commands",
                           [&](const auto& params){help_handler(context, params);}});
      cmap.emplace("index",
                   CommandInfo{
                           ArgMap{ {"my-addresses", ArgInfo{Type::List, false, "list of 'my' addresses"}},
                                   {"cleanup", ArgInfo{Type::Symbol, false,
                                                           "whether to remove stale messages from the store"}},
                                   {"lazy-check", ArgInfo{Type::Symbol, false,
                                            "whether to avoid indexing up-to-date directories"}}},
                           "scan maildir for new/updated/removed messages",
                           [&](const auto& params){index_handler(context, params);}});

      cmap.emplace("move",
                   CommandInfo{
                           ArgMap{{"docid",  ArgInfo{Type::Integer, false, "document-id"}},
                                   {"msgid",  ArgInfo{Type::String, false, "message-id"}},
                                   {"flags",   ArgInfo{Type::String, false, "new flags for the message"}},
                                   {"maildir", ArgInfo{Type::String, false, "the target maildir" }},
                                   {"rename", ArgInfo{Type::Symbol, false,  "change filename when moving" }},
                                   {"no-view", ArgInfo{Type::Symbol, false,
                                            "if set, do not hint at updating the view"}},},
                           "move messages and/or change their flags",
                           [&](const auto& params){move_handler(context, params);}});

      cmap.emplace("mkdir",
                   CommandInfo{
                           ArgMap{ {"path", ArgInfo{Type::String, true,
                                                   "location for the new maildir" }}},
                           "create a new maildir",
                          [&](const auto& params){mkdir_handler(context, params);}});
      cmap.emplace("ping",
                   CommandInfo{
                           ArgMap{ {"queries",  ArgInfo{Type::List, false,
                                                   "queries for which to get read/unread numbers"}}},
                           "ping the mu-server and get information in response",
                          [&](const auto& params){ping_handler(context, params);}});

      cmap.emplace("quit",
                   CommandInfo{{},
                           "quit the mu server",
                           [&](const auto& params){quit_handler(context, params);}});

      cmap.emplace("remove",
                   CommandInfo{
                           ArgMap{ {"docid", ArgInfo{Type::Integer, true,
                                                   "document-id for the message to remove" }}},
                           "remove a message from filesystem and database",
                          [&](const auto& params){remove_handler(context, params);}});

      cmap.emplace("sent",
                   CommandInfo{
                           ArgMap{ {"path", ArgInfo{Type::String, true,
                                            "path to the message file" }}
                           },
                           "tell mu about a message that was sent",
                          [&](const auto& params){sent_handler(context, params);}});

      cmap.emplace("view",
                   CommandInfo{
                           ArgMap{{"docid",  ArgInfo{Type::Integer, false, "document-id"}},
                                   {"msgid",  ArgInfo{Type::String, false, "message-id"}},
                                   {"path",   ArgInfo{Type::String, false, "message filesystem path"}},

                                   {"extract-images", ArgInfo{Type::Symbol, false,
                                                           "whether to extract images for this messages (if any)"}},
                                   {"decrypt", ArgInfo{Type::Symbol, false,
                                                           "whether to decrypt encrypted parts (if any)" }},
                                   {"verify", ArgInfo{Type::Symbol, false,
                                                           "whether to verify signatures (if any)" }}

                           },
                           "view a message. exactly one of docid/msgid/path must be specified",
                           [&](const auto& params){view_handler(context, params);}});
      return cmap;
}


struct  Readline {
        Readline (const std::string& histpath, size_t max_lines);
        ~Readline();
        std::string read_line(bool& do_quit);
        void save_line(const std::string& line);
        std::string histpath_;
        size_t max_lines_{0};
};

/// Wrapper around readline (if available) or nothing otherwise.
#if defined(HAVE_LIBREADLINE) && defined(HAVE_READLINE_HISTORY)
Readline::Readline (const std::string& histpath, size_t max_lines):
        histpath_{histpath}, max_lines_{max_lines}
{
        rl_bind_key('\t', rl_insert); // default (filenames) is not useful
        using_history();
        read_history (histpath_.c_str());

        if (max_lines_ > 0)
                stifle_history(max_lines_);
}

Readline::~Readline () {
        write_history(histpath_.c_str());
        if (max_lines_ > 0)
                history_truncate_file (histpath_.c_str(), max_lines_);
}

std::string
Readline::read_line(bool& do_quit)
{
        auto buf = readline(";; mu% ");
        if (!buf) {
                do_quit = true;
                return {};
        }
        std::string line{buf};
        ::free (buf);
        return line;
}

void
Readline::save_line(const std::string& line)
{
        add_history(line.c_str());
}
#else
Readline::Readline (const std::string& histpath, size_t max_lines) {}
Readline::~Readline() {}
void Readline::save_line(const std::string& line) {}

std::string
Readline::read_line(bool& do_quit)
{
        std::string line;
        std::cout << ";; mu> ";
        if (!std::getline(std::cin, line))
                do_quit = true;
        return line;
}
#endif // ! defined(HAVE_LIBREADLINE) && defined(HAVE_READLINE_HISTORY)


MuError
mu_cmd_server (MuConfig *opts, GError **err) try
{
        if (opts->commands) {
                Context ctx{};
                auto cmap = make_command_map(ctx);
                invoke(cmap, Sexp::parse("(help :full t)"));
                return MU_OK;
        }

        Context context{opts};
        context.command_map = make_command_map (context);

        const auto histpath{std::string{mu_runtime_path(MU_RUNTIME_PATH_CACHE)} + "/history"};
        Readline readline(histpath, 50);

        install_sig_handler();
        std::cout << ";; Welcome to the "  << PACKAGE_STRING << " command-server\n"
                  << ";; Use (help) to get a list of commands, (quit) to quit.\n";

        while (!MuTerminate && !context.do_quit) {

                std::string line;
                try {
                        line = readline.read_line(context.do_quit);
                        if (line.find_first_not_of(" \t") == std::string::npos)
                                continue; // skip whitespace-only lines

                        auto call{Sexp::parse(line)};
                        readline.save_line(line);

                        invoke(context.command_map, call);

                } catch (const Error& er) {
                        std::cerr << ";; error: " << er.what() << "\n";
                        print_error ((MuError)er.code(), "%s (line was:'%s')",
                                     er.what(), line.c_str());
                }
        }

        return MU_OK;

} catch (const Error& er) {
        g_set_error(err, MU_ERROR_DOMAIN, MU_ERROR, "%s", er.what());
        return MU_ERROR;
} catch (...) {
        g_set_error(err, MU_ERROR_DOMAIN, MU_ERROR, "%s", "caught exception");
        return MU_ERROR;
}
