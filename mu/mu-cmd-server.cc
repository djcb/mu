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
#include "mu-cmd.hh"

#include <iostream>
#include <string>
#include <algorithm>
#include <atomic>
#include <thread>
#include <mutex>

#include <cstring>
#include <glib.h>
#include <glib/gprintf.h>

#include "mu-msg.h"
#include "mu-runtime.h"
#include "mu-cmd.hh"
#include "mu-maildir.h"
#include "mu-query.h"
#include "index/mu-indexer.hh"
#include "mu-store.hh"
#include "mu-msg-part.h"
#include "mu-contacts.hh"

#include "utils/mu-str.h"
#include "utils/mu-utils.hh"
#include "utils/mu-command-parser.hh"
#include "utils/mu-readline.hh"

using namespace Mu;
using namespace Command;

using DocId = unsigned;

static std::mutex OutputLock;
static std::atomic<bool> MuTerminate{false};

static void
install_sig_handler (void)
{
        struct sigaction action;
        int i, sigs[] = { SIGINT, SIGHUP, SIGTERM, SIGPIPE };

        MuTerminate = false;

        action.sa_handler = [](int sig){ MuTerminate = true; };
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
        std::lock_guard<std::mutex> l {OutputLock};

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


static void
print_expr (const Sexp& sexp)
{
        print_expr ("%s", sexp.to_string().c_str());
}

static void
print_expr (Sexp::List&& list)
{
        print_expr (Sexp::make_list(std::move(list)));
}



G_GNUC_PRINTF(2,3) static MuError
print_error (MuError errcode, const char* frm, ...)
{
        char    *msg;
        va_list  ap;

        va_start (ap, frm);
        g_vasprintf (&msg, frm, ap);
        va_end (ap);

        Sexp::List err;
        err.add_prop(":error",   Sexp::make_number((int)errcode));
        err.add_prop(":message", Sexp::make_string(msg));

        print_expr(Sexp::make_list(std::move(err)));

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


/// @brief object to manage the server-context for all commands.
struct Context {
        Context(){}
        Context (const MuConfig *opts):
                store_{std::make_unique<Store>(mu_runtime_path(MU_RUNTIME_PATH_XAPIANDB),
                                               false/*writable*/)} {
                GError *gerr{};
                query = mu_query_new (reinterpret_cast<MuStore*>(store_.get()), &gerr);
                if (!query)
                        throw Error(Error::Code::Store, &gerr/*consumes*/, "failed to create query");

                g_message ("opened store @ %s; maildir @ %s; debug-mode %s",
                           store_->metadata().database_path.c_str(),
                           store_->metadata().root_maildir.c_str(),
                           opts->debug ? "yes" : "no");
        }

        ~Context() {
                if (query)
                        mu_query_destroy(query);
        }

        Context(const Context&) = delete;

        Store& store() {
                if (!store_)
                        throw Mu::Error (Error::Code::Internal, "no store");
                return *store_.get();
        }
        Indexer& indexer() { return store().indexer(); }

        std::unique_ptr<Mu::Store>   store_;

        MuQuery   *query{};
        bool      do_quit{};

        CommandMap command_map;
};


static MuMsgOptions
message_options (const Parameters& params)
{
        const auto extract_images{get_bool_or(params, ":extract-images", false)};
        const auto decrypt{get_bool_or(params, ":decrypt", false)};
        const auto verify{get_bool_or(params, ":verify", false)};

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
        auto path{get_string_or(params, ":path")};
        const auto docid{context.store().add_message(path)};

        Sexp::List expr;
        expr.add_prop(":info",  Sexp::make_symbol("add"));
        expr.add_prop(":path",  Sexp::make_string(path));
        expr.add_prop(":docid", Sexp::make_number(docid));

        print_expr (std::move(expr));

        auto msg{context.store().find_message(docid)};
        if (!msg)
                throw Error(Error::Code::Store,
                            "failed to get message at %s (docid=%u)",
                            path.c_str(), docid);

        Sexp::List update;
        update.add_prop(":update", Mu::msg_to_sexp(msg, docid, NULL,
                                                   MU_MSG_OPTION_VERIFY));

        print_expr (std::move(update));
        mu_msg_unref(msg);
}


struct PartInfo {
        Sexp::List        attseq;
        MuMsgOptions      opts;
};

static void
each_part (MuMsg *msg, MuMsgPart *part, PartInfo *pinfo)
{
        /* exclude things that don't look like proper attachments, unless they're images */
        if (!mu_msg_part_maybe_attachment(part))
                return;

        GError *gerr{};
        char *cachefile = mu_msg_part_save_temp (
                msg, (MuMsgOptions)(pinfo->opts|MU_MSG_OPTION_OVERWRITE),
                part->index, &gerr);
        if (!cachefile)
                throw Error (Error::Code::File, &gerr, "failed to save part");

        Sexp::List pi;
        pi.add_prop(":file-name", Sexp::make_string(cachefile));
        pi.add_prop(":mime-type", Sexp::make_string(format("%s/%s",
                                                            part->type,
                                                            part->subtype)));
        pinfo->attseq.add(Sexp::make_list(std::move(pi)));
        g_free (cachefile);
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
        const auto ctype{get_symbol_or(params, ":type")};

        Sexp::List comp_lst;
        comp_lst.add_prop(":compose", Sexp::make_symbol(std::string(ctype)));

        if (ctype == "reply" || ctype == "forward" || ctype == "edit" || ctype == "resend") {

                GError *gerr{};
                const unsigned docid{(unsigned)get_int_or(params, ":docid")};
                auto msg{context.store().find_message(docid)};
                if (!msg)
                        throw Error{Error::Code::Store, &gerr, "failed to get message %u", docid};

                const auto opts{message_options(params)};
                comp_lst.add_prop(":original", Mu::msg_to_sexp(msg, docid, {}, opts));

                if (ctype == "forward") {
                        PartInfo pinfo{};
                        pinfo.opts = opts;
                        mu_msg_part_foreach (msg, opts,
                                             (MuMsgPartForeachFunc)each_part, &pinfo);
                        if (!pinfo.attseq.empty())
                                comp_lst.add_prop (":include",
                                                   Sexp::make_list(std::move(pinfo.attseq)));
                }
                mu_msg_unref (msg);

        } else if (ctype != "new")
                throw Error(Error::Code::InvalidArgument, "invalid compose type '%s'",
                            ctype.c_str());

        print_expr (std::move(comp_lst));
}

static void
contacts_handler (Context& context, const Parameters& params)
{
        const auto personal  = get_bool_or(params,   ":personal");
        const auto afterstr  = get_string_or(params, ":after");
        const auto tstampstr = get_string_or(params, ":tstamp");

        const auto after{afterstr.empty() ? 0 :
                        g_ascii_strtoll(date_to_time_t_string(afterstr, true).c_str(), {}, 10)};
        const auto tstamp = g_ascii_strtoll (tstampstr.c_str(), NULL, 10);

        auto rank{0};
        Sexp::List contacts;
        context.store().contacts().for_each([&](const ContactInfo& ci) {

                rank++;

                /* since the last time we got some contacts */
                if (tstamp > ci.tstamp)
                        return;
                /* (maybe) only include 'personal' contacts */
                if (personal && !ci.personal)
                        return;
                /* only include newer-than-x contacts */
                if (after > ci.last_seen)
                        return;

                /* using name and email yields prettier results than full_address */
                auto full_email_address{std::string{ci.full_address}};
                if (!ci.name.empty()) {
                        const auto address{std::string{ci.name} + " <" + std::string{ci.email} + ">"};
                        full_email_address = address;
                }

                Sexp::List contact;
                contact.add_prop(":address", Sexp::make_string(full_email_address));
                contact.add_prop(":rank",    Sexp::make_number(rank));

                contacts.add(Sexp::make_list(std::move(contact)));
        });

        Sexp::List seq;
        seq.add_prop(":contacts", Sexp::make_list(std::move(contacts)));
        seq.add_prop(":tstamp",   Sexp::make_string(format("%" G_GINT64_FORMAT,
                                                           g_get_monotonic_time())));
        /* dump the contacts cache as a giant sexp */
        print_expr(std::move(seq));
}


static void
save_part (MuMsg *msg, unsigned docid, unsigned index,
           MuMsgOptions opts, const Parameters& params)
{
        const auto path{get_string_or(params, ":path")};
        if (path.empty())
                throw Error{Error::Code::Command, "missing path"};

        GError *gerr{};
        if (!mu_msg_part_save (msg, (MuMsgOptions)(opts | (int)MU_MSG_OPTION_OVERWRITE),
                               path.c_str(), index, &gerr))
                throw Error{Error::Code::File, &gerr, "failed to save part"};

        Sexp::List seq;
        seq.add_prop(":info", Sexp::make_symbol("save"));
        seq.add_prop(":message", Sexp::make_string(format("%s has been saved", path.c_str())));

        print_expr(std::move(seq));
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

        Sexp::List seq;
        seq.add_prop(":info",    Sexp::make_symbol("open"));
        seq.add_prop(":message", Sexp::make_string(format("%s has been opened", targetpath)));
        g_free (targetpath);

        print_expr(std::move(seq));
}

static void
temp_part (MuMsg *msg, unsigned docid, unsigned index,
           MuMsgOptions opts, const Parameters& params)
{
        const auto what{get_symbol_or(params, ":what")};
        if (what.empty())
                throw Error{Error::Code::Command, "missing 'what'"};

        const auto param{get_string_or(params, ":param")};

        GError *gerr{};
        char *path{mu_msg_part_get_cache_path (msg, opts, index, &gerr)};
        if (!path)
                throw Error{Error::Code::File, &gerr, "could not get cache path"};

        if (!mu_msg_part_save (msg, (MuMsgOptions)(opts | MU_MSG_OPTION_USE_EXISTING),
                               path, index, &gerr)) {
                g_free(path);
                throw Error{Error::Code::File, &gerr, "saving failed"};
        }

        Sexp::List lst;
        lst.add_prop(":temp",  Sexp::make_string(path));
        lst.add_prop(":what",  Sexp::make_string(what));
        lst.add_prop(":docid", Sexp::make_number(docid));

        if (!param.empty())
                lst.add_prop(":param", Sexp::make_string(param));

        g_free(path);
        print_expr(std::move(lst));
}



/* 'extract' extracts some mime part from a message */
static void
extract_handler (Context& context, const Parameters& params)
{
        const auto docid{get_int_or(params, ":docid")};
        const auto index{get_int_or(params, ":index")};
        const auto opts{message_options(params)};

        GError *gerr{};
        auto msg{context.store().find_message(docid)};
        if (!msg)
                throw Error{Error::Code::Store, "failed to get message"};

        try {
                const auto action{get_symbol_or(params, ":action")};
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
path_from_docid (const Store& store, unsigned docid)
{
        auto msg{store.find_message(docid)};
        if (!msg)
                throw Error(Error::Code::Store, "could not get message from store");

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
        auto docid{get_int_or(params, ":docid", 0)};
        const auto msgid{get_string_or(params, ":msgid")};

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
        const auto query{get_string_or(params,         ":query")};
        const auto threads{get_bool_or(params,         ":threads", false)};
        const auto sortfieldstr{get_symbol_or(params,  ":sortfield")};
        const auto descending{get_bool_or(params,      ":descending", false)};
        const auto maxnum{get_int_or(params,           ":maxnum", -1/*unlimited*/)};
        const auto skip_dups{get_bool_or(params,       ":skip-dups", false)};
        const auto include_related{get_bool_or(params, ":include-related", false)};

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
        {
                Sexp::List lst;
                lst.add_prop(":erase", Sexp::make_symbol("t"));
                print_expr(std::move(lst));
        }

        {
                const auto foundnum{print_sexps (miter, maxnum)};
                Sexp::List lst;
                lst.add_prop(":found", Sexp::make_number(foundnum));
                print_expr(std::move(lst));
        }
        //print_expr ("(:found %u)", foundnum);
        mu_msg_iter_destroy (miter);
}


static void
help_handler (Context& context, const Parameters& params)
{
        const auto command{get_symbol_or(params, ":command", "")};
        const auto terse{get_bool_or(params, ":terse", command.empty())};

        if (command.empty()) {
                std::cout << ";; Commands are s-expressions of the form\n"
                          << ";;   (<command-name> :param1 val1 :param2 val2 ...)\n"
                          << ";; For instance:\n;;  (help :command quit)\n"
                          << ";; to get detailed information about the 'quit'\n;;\n";
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
                if (terse)
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

static void
print_stats (const Indexer::Progress& stats, const std::string& state)
{
        Sexp::List lst;

        lst.add_prop(":info",       Sexp::make_symbol("index"));
        lst.add_prop(":status",     Sexp::make_symbol(std::string{state}));
        lst.add_prop(":processed",  Sexp::make_number(stats.processed));
        lst.add_prop(":updated",    Sexp::make_number(stats.updated));
        lst.add_prop(":cleaned-up", Sexp::make_number(stats.removed));

        print_expr(std::move(lst));
}

static void
index_handler (Context& context, const Parameters& params)
{
        Mu::Indexer::Config conf{};
        conf.cleanup    = get_bool_or(params, ":cleanup");
        conf.lazy_check = get_bool_or(params, ":lazy-check");

        context.indexer().stop();

        context.indexer().start(conf);
        while (context.indexer().is_running()) {
                std::this_thread::sleep_for(std::chrono::milliseconds(1000));
                print_stats (context.indexer().progress(), "running");
        }
        print_stats (context.indexer().progress(), "complete");
}

static void
mkdir_handler (Context& context, const Parameters& params)
{
        const auto path{get_string_or(params, ":path")};

        GError *gerr{};
        if (!mu_maildir_mkdir(path.c_str(), 0755, FALSE, &gerr))
                throw Error{Error::Code::File, &gerr, "failed to create maildir"};

        Sexp::List lst;
        lst.add_prop(":info",    Sexp::make_string("mkdir"));
        lst.add_prop(":message", Sexp::make_string(format("%s has been created",
                                                          path.c_str())));

        print_expr(std::move(lst));
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
do_move (Store& store, DocId docid, MuMsg *msg, const std::string& maildirarg,
         MuFlags flags, bool new_name, bool no_view)
{
        bool different_mdir{};
        auto maildir{maildirarg};
        if (maildir.empty()) {
                maildir = mu_msg_get_maildir (msg);
                different_mdir = false;
        } else /* are we moving to a different mdir, or is it just flags? */
                different_mdir = maildir != mu_msg_get_maildir(msg);

        GError* gerr{};
        if (!mu_msg_move_to_maildir (msg, maildir.c_str(), flags, TRUE, new_name, &gerr))
                throw Error{Error::Code::File, &gerr, "failed to move message"};

        /* after mu_msg_move_to_maildir, path will be the *new* path, and flags and maildir fields
         * will be updated as wel */
        if (!store.update_message (msg, docid))
                throw Error{Error::Code::Store, "failed to store updated message"};

        Sexp::List seq;
        seq.add_prop(":update", msg_to_sexp (msg, docid, NULL, MU_MSG_OPTION_VERIFY));
        /* note, the :move t thing is a hint to the frontend that it
         * could remove the particular header */
        if (different_mdir)
                seq.add_prop(":move", Sexp::make_symbol("t"));
        if (!no_view)
                seq.add_prop(":maybe-view", Sexp::make_symbol("t"));

        print_expr (std::move(seq));
}

static void
move_docid (Store& store, DocId docid, const std::string& flagstr,
            bool new_name, bool no_view)
{
        if (docid == MU_STORE_INVALID_DOCID)
                throw Error{Error::Code::InvalidArgument, "invalid docid"};

        auto msg{store.find_message(docid)};
        try {
                if (!msg)
                        throw Error{Error::Code::Store, "failed to get message from store"};

                const auto flags = flagstr.empty() ? mu_msg_get_flags (msg) :
                        get_flags (mu_msg_get_path(msg), flagstr);
                if (flags == MU_FLAG_INVALID)
                        throw Error{Error::Code::InvalidArgument, "invalid flags '%s'",
                                        flagstr.c_str()};

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
        auto maildir{get_string_or(params,       ":maildir")};
        const auto flagstr{get_string_or(params, ":flags")};
        const auto rename{get_bool_or (params,   ":rename")};
        const auto no_view{get_bool_or (params,  ":noupdate")};
        const auto docids{determine_docids (context.query, params)};

        if (docids.size() > 1) {
                if (!maildir.empty()) // ie. duplicate message-ids.
                        throw Mu::Error{Error::Code::Store,
                                        "can't move multiple messages at the same time"};
                // multi.
                for (auto&& docid: docids)
                        move_docid(context.store(), docid, flagstr, rename, no_view);
                return;
        }
        auto docid{docids.at(0)};

        GError *gerr{};
        auto msg{context.store().find_message(docid)};
        if (!msg)
                throw Error{Error::Code::InvalidArgument, &gerr,
                                "could not create message"};

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
                throw Error{Error::Code::InvalidArgument, "invalid flags"};
        }

        try {
                do_move (context.store(), docid, msg, maildir, flags,
                         rename, no_view);
        } catch (...) {
                mu_msg_unref(msg);
                throw;
        }

        mu_msg_unref(msg);
}

static void
ping_handler (Context& context, const Parameters& params)
{
        const auto storecount{context.store().size()};
        if (storecount == (unsigned)-1)
                throw Error{Error::Code::Store, "failed to read store"};

        const auto queries  = get_string_vec (params, ":queries");
        Sexp::List qresults;
        for (auto&& q: queries) {

                const auto count{mu_query_count_run (context.query, q.c_str())};
                const auto unreadq{format("flag:unread AND (%s)", q.c_str())};
                const auto unread{mu_query_count_run (context.query, unreadq.c_str())};

                Sexp::List lst;
                lst.add_prop(":query",  Sexp::make_string(q));
                lst.add_prop(":count",  Sexp::make_number(count));
                lst.add_prop(":unread", Sexp::make_number(unread));

                qresults.add(Sexp::make_list(std::move(lst)));
        }

        Sexp::List addrs;
        for (auto&& addr: context.store().metadata().personal_addresses)
                addrs.add(Sexp::make_string(addr));

        Sexp::List lst;
        lst.add_prop(":pong", Sexp::make_string("mu"));

        Sexp::List proplst;
        proplst.add_prop(":version",            Sexp::make_string(VERSION));
        proplst.add_prop(":personal-addresses", Sexp::make_list(std::move(addrs)));
        proplst.add_prop(":database-path",
                         Sexp::make_string(context.store().metadata().database_path));
        proplst.add_prop(":root-maildir",
                         Sexp::make_string(context.store().metadata().root_maildir));
        proplst.add_prop(":doccount",           Sexp::make_number(storecount));
        proplst.add_prop(":queries",            Sexp::make_list(std::move(qresults)));

        lst.add_prop(":props",   Sexp::make_list(std::move(proplst)));

        print_expr(std::move(lst));
}

static void
quit_handler (Context& context, const Parameters& params)
{
        context.do_quit = true;
}


static void
remove_handler (Context& context, const Parameters& params)
{
        const auto docid{get_int_or(params, ":docid")};
        const auto path{path_from_docid (context.store(), docid)};

        if (::unlink (path.c_str()) != 0 && errno != ENOENT)
                throw Error(Error::Code::File, "could not delete %s: %s",
                                  path.c_str(), strerror (errno));

        if (!context.store().remove_message (path))
                g_warning("failed to remove message @ %s (%d) from store",
                          path.c_str(), docid);
        // act as if it worked.

        Sexp::List lst;
        lst.add_prop(":remove", Sexp::make_number(docid));

        print_expr(std::move(lst));
}


static void
sent_handler (Context& context, const Parameters& params)
{
        const auto path{get_string_or(params, ":path")};
        const auto docid{context.store().add_message(path)};
        if (docid == MU_STORE_INVALID_DOCID)
                throw Error{Error::Code::Store, "failed to add path"};

        Sexp::List lst;
        lst.add_prop (":sent",  Sexp::make_symbol("t"));
        lst.add_prop (":path",  Sexp::make_string(path));
        lst.add_prop (":docid", Sexp::make_number(docid));

        print_expr (std::move(lst));
}

static bool
maybe_mark_as_read (Mu::Store& store, MuMsg *msg, DocId docid)
{
        if (!msg)
                throw Error{Error::Code::Store, "missing message"};
        if (docid == MU_STORE_INVALID_DOCID)
                throw Error{Error::Code::Store, "invalid docid"};

        const auto oldflags{mu_msg_get_flags (msg)};
        const auto newflags{get_flags (mu_msg_get_path(msg), "+S-u-N")};
        if (oldflags == newflags)
                return false; // nothing to do.

        GError* gerr{};
        if (!mu_msg_move_to_maildir (msg,
                                     mu_msg_get_maildir (msg),
                                     newflags,
                                     TRUE,
                                     FALSE,/*new_name,*/
                                     &gerr))
                throw Error{Error::Code::File, &gerr, "failed to move message"};

        /* after mu_msg_move_to_maildir, path will be the *new* path, and flags
         * and maildir fields will be updated as wel */
        if (!store.update_message (msg, docid))
                throw Error{Error::Code::Store, "failed to store updated message"};

        g_debug ("marked message %d as read => %s", docid, mu_msg_get_path(msg));
        return true;
}


static void
view_handler (Context& context, const Parameters& params)
{
        DocId docid{MU_STORE_INVALID_DOCID};
        const auto path{get_string_or(params,       ":path")};
        const auto mark_as_read{get_bool_or(params, ":mark-as-read")};

        GError *gerr{};
        MuMsg *msg{};

        if (!path.empty())
                msg   = mu_msg_new_from_file (path.c_str(), NULL, &gerr);
        else {
                docid = determine_docids(context.query, params).at(0);
                msg   = context.store().find_message(docid);
        }

        if (!msg)
                throw Error{Error::Code::Store, &gerr,
                                "failed to find message for view"};

        if (mark_as_read)
                maybe_mark_as_read (context.store(), msg, docid);

        Sexp::List seq;
        seq.add_prop(":view", msg_to_sexp(msg, docid, {}, message_options(params)));

        mu_msg_unref(msg);

        print_expr (std::move(seq));
}


static CommandMap
make_command_map (Context& context)
{
      CommandMap cmap;

      using Type = Sexp::Type;

      cmap.emplace("add",
                   CommandInfo{
                           ArgMap{ {":path", ArgInfo{Type::String, true, "file system path to the message" }}},
                           "add a message to the store",
                           [&](const auto& params){add_handler(context, params);}});

      cmap.emplace("compose",
                   CommandInfo{
                           ArgMap{{":type",    ArgInfo{Type::Symbol, true,
                                           "type of composition: reply/forward/edit/resend/new"}},
                                  {":docid",   ArgInfo{Type::Number, false,
                                           "document id of parent-message, if any"}},
                                  {":decrypt", ArgInfo{Type::Symbol, false,
                                           "whether to decrypt encrypted parts (if any)" }}},
                           "get contact information",
                           [&](const auto& params){compose_handler(context, params);}});

      cmap.emplace("contacts",
                   CommandInfo{
                           ArgMap{ {":personal", ArgInfo{Type::Symbol, false,
                                                   "only personal contacts" }},
                                   {":after",    ArgInfo{Type::String, false,
                                            "only contacts seen after time_t string" }},
                                   {":tstamp",   ArgInfo{Type::String, false,
                                            "return changes since tstamp" }}},
                           "get contact information",
                           [&](const auto& params){contacts_handler(context, params);}});

      cmap.emplace("extract",
                   CommandInfo{
                           ArgMap{{":docid",    ArgInfo{Type::Number, true,  "document for the message" }},
                                   {":index",   ArgInfo{Type::Number, true,
                                            "index for the part to operate on" }},
                                   {":action",  ArgInfo{Type::Symbol, true, "what to do with the part" }},
                                   {":decrypt", ArgInfo{Type::Symbol, false,
                                            "whether to decrypt encrypted parts (if any)" }},
                                   {":path",    ArgInfo{Type::String, false,
                                            "part for saving (for action: save)" }},
                                   {":what",    ArgInfo{Type::Symbol, false,
                                            "what to do with the part (feedback)" }},
                                   {":param",   ArgInfo{Type::String, false, "parameter for 'what'" }}},
                           "extract mime-parts from a message",
                           [&](const auto& params){extract_handler(context, params);}});

      cmap.emplace("find",
                   CommandInfo{
                           ArgMap{ {":query",   ArgInfo{Type::String, true, "search expression" }},
                                   {":threads", ArgInfo{Type::Symbol, false,
                                            "whether to include threading information" }},
                                   {":sortfield",  ArgInfo{Type::Symbol, false, "the field to sort results by" }},
                                   {":descending", ArgInfo{Type::Symbol, false,
                                            "whether to sort in descending order" }},
                                   {":maxnum",  ArgInfo{Type::Number, false,
                                            "maximum number of result (hint)" }},
                                   {":skip-dups",  ArgInfo{Type::Symbol, false,
                                            "whether to skip messages with duplicate message-ids" }},
                                   {":include-related",  ArgInfo{Type::Symbol, false,
                                            "whether to include other message related to matching ones" }}},
                           "query the database for messages",
                           [&](const auto& params){find_handler(context, params);}});

      cmap.emplace("help",
                   CommandInfo{
                           ArgMap{ {":command", ArgInfo{Type::Symbol, false,
                                                   "command to get information for" }},
                                   {":terse", ArgInfo{Type::Symbol, false,
                                            "only show a short description" }}},
                           "get information about one or all commands",
                           [&](const auto& params){help_handler(context, params);}});
      cmap.emplace("index",
                   CommandInfo{
                           ArgMap{ {":my-addresses", ArgInfo{Type::List, false, "list of 'my' addresses"}},
                                   {":cleanup",      ArgInfo{Type::Symbol, false,
                                                           "whether to remove stale messages from the store"}},
                                   {":lazy-check",   ArgInfo{Type::Symbol, false,
                                            "whether to avoid indexing up-to-date directories"}}},
                           "scan maildir for new/updated/removed messages",
                           [&](const auto& params){index_handler(context, params);}});

      cmap.emplace("move",
                   CommandInfo{
                           ArgMap{ {":docid",   ArgInfo{Type::Number, false, "document-id"}},
                                   {":msgid",   ArgInfo{Type::String, false, "message-id"}},
                                   {":flags",   ArgInfo{Type::String, false, "new flags for the message"}},
                                   {":maildir", ArgInfo{Type::String, false, "the target maildir" }},
                                   {":rename",  ArgInfo{Type::Symbol, false,  "change filename when moving" }},
                                   {":no-view", ArgInfo{Type::Symbol, false,
                                            "if set, do not hint at updating the view"}},},
                           "move messages and/or change their flags",

                           [&](const auto& params){move_handler(context, params);}});

      cmap.emplace("mkdir",
                   CommandInfo{
                           ArgMap{ {":path", ArgInfo{Type::String, true,
                                                   "location for the new maildir" }}},
                           "create a new maildir",
                          [&](const auto& params){mkdir_handler(context, params);}});
      cmap.emplace("ping",
                   CommandInfo{
                           ArgMap{ {":queries",    ArgInfo{Type::List, false,
                                                   "queries for which to get read/unread numbers"}},
                                   {":skip-dups",  ArgInfo{Type::Symbol, false,
                                            "whether to exclude messages with duplicate message-ids"}},},
                           "ping the mu-server and get information in response",
                          [&](const auto& params){ping_handler(context, params);}});

      cmap.emplace("quit",
                   CommandInfo{{},
                           "quit the mu server",
                           [&](const auto& params){quit_handler(context, params);}});

      cmap.emplace("remove",
                   CommandInfo{
                           ArgMap{ {":docid", ArgInfo{Type::Number, true,
                                                   "document-id for the message to remove" }}},
                           "remove a message from filesystem and database",
                          [&](const auto& params){remove_handler(context, params);}});

      cmap.emplace("sent",
                   CommandInfo{
                           ArgMap{ {":path", ArgInfo{Type::String, true,
                                            "path to the message file" }}
                           },
                           "tell mu about a message that was sent",
                          [&](const auto& params){sent_handler(context, params);}});

      cmap.emplace("view",
                   CommandInfo{
                           ArgMap{{":docid",           ArgInfo{Type::Number, false, "document-id"}},
                                   {":msgid",          ArgInfo{Type::String, false, "message-id"}},
                                   {":path",           ArgInfo{Type::String, false, "message filesystem path"}},
                                   {":mark-as-read",   ArgInfo{Type::Symbol, false,
                                            "mark message as read (if not already)"}},
                                   {":extract-images", ArgInfo{Type::Symbol, false,
                                                           "whether to extract images for this messages (if any)"}},
                                   {":decrypt",        ArgInfo{Type::Symbol, false,
                                                           "whether to decrypt encrypted parts (if any)" }},
                                   {":verify",         ArgInfo{Type::Symbol, false,
                                                           "whether to verify signatures (if any)" }}

                           },
                           "view a message. exactly one of docid/msgid/path must be specified",
                           [&](const auto& params){view_handler(context, params);}});
      return cmap;
}

MuError
mu_cmd_server (const MuConfig *opts, GError **err) try
{
        if (opts->commands) {
                Context ctx{};
                auto cmap = make_command_map(ctx);
                invoke(cmap, Sexp::make_parse("(help :full t)"));
                return MU_OK;
        }

        Context context{opts};
        context.command_map = make_command_map (context);

        if (opts->eval) { // evaluate command-line command & exit
                auto call{Sexp::Sexp::make_parse(opts->eval)};
                invoke(context.command_map, call);
                return MU_OK;
        }

        const auto histpath{std::string{mu_runtime_path(MU_RUNTIME_PATH_CACHE)} + "/history"};
        setup_readline(histpath, 50);

        install_sig_handler();
        std::cout << ";; Welcome to the "  << PACKAGE_STRING << " command-server\n"
                  << ";; Use (help) to get a list of commands, (quit) to quit.\n";

        g_debug ("starting repl");
        while (!MuTerminate && !context.do_quit) {

                std::string line;
                try {
                        line = read_line(context.do_quit);
                        if (line.find_first_not_of(" \t") == std::string::npos)
                                continue; // skip whitespace-only lines

                        auto call{Sexp::Sexp::make_parse(line)};
                        invoke(context.command_map, call);

                        save_line(line);

                } catch (const Error& er) {
                        std::cerr << ";; error: " << er.what() << "\n";
                        g_warning ("error in server: %s", er.what());
                        print_error ((MuError)er.code(), "%s (line was:'%s')",
                                     er.what(), line.c_str());
                }
        }
        shutdown_readline();

        return MU_OK;

} catch (const Error& er) {
        g_critical ("server caught exception: %s", er.what());
        g_set_error(err, MU_ERROR_DOMAIN, MU_ERROR, "%s", er.what());
        return MU_ERROR;
} catch (...) {
        g_critical ("server caught exception");
        g_set_error(err, MU_ERROR_DOMAIN, MU_ERROR, "%s", "caught exception");
        return MU_ERROR;
}
