/*
** Copyright (C) 2011-2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <string.h>
#include <ctype.h>

#include "mu-query-results.hh"
#include "utils/mu-str.h"
#include "mu-msg.hh"
#include "mu-msg-part.hh"
#include "mu-maildir.hh"

using namespace Mu;

static void
add_prop_nonempty (Sexp::List& list, const char* elm, const GSList *str_lst)
{
        Sexp::List elms;
        while (str_lst) {
                elms.add(Sexp::make_string((const char*)str_lst->data));
                str_lst = g_slist_next(str_lst);
        }

        if (!elms.empty())
                list.add_prop(elm, Sexp::make_list(std::move(elms)));
}

static void
add_prop_nonempty (Sexp::List& list, const char* name, const char *str)
{
        if (str && str[0])
                list.add_prop(name, Sexp::make_string(str));
}


static Sexp
make_contact_sexp (MuMsgContact *c)
{
        // a cons-pair...perhaps make this a plist too?

        Sexp::List contact;
        if (mu_msg_contact_name(c))
                contact.add(Sexp::make_string(Mu::remove_ctrl(mu_msg_contact_name(c))));
        else
                contact.add(Sexp::make_symbol("nil"));

        contact.add(Sexp::make_symbol("."));
        contact.add(Sexp::make_string(Mu::remove_ctrl(mu_msg_contact_email(c))));

        return Sexp::make_list(std::move(contact));
}

static void
add_list_post (Sexp::List& list, MuMsg *msg)
{
        /* some mailing lists do not set the reply-to; see pull #1278. So for
         * those cases, check the List-Post address and use that instead */

        GMatchInfo      *minfo;
        GRegex          *rx;
        const char*      list_post;

        list_post = mu_msg_get_header (msg, "List-Post");
        if (!list_post)
                return;

        rx = g_regex_new ("<?mailto:([a-z0-9!@#$%&'*+-/=?^_`{|}~]+)>?",
                          G_REGEX_CASELESS, (GRegexMatchFlags)0, NULL);
        g_return_if_fail(rx);

        if (g_regex_match (rx, list_post, (GRegexMatchFlags)0, &minfo)) {
                auto address = (char*)g_match_info_fetch (minfo, 1);
                MuMsgContact contact{NULL,  address};
                list.add_prop(":list-post",
                              Sexp::make_list(make_contact_sexp(&contact)));
                g_free(address);
        }

        g_match_info_free (minfo);
        g_regex_unref (rx);
}


struct _ContactData {
        Sexp::List from, to, cc, bcc, reply_to;
};
typedef struct _ContactData ContactData;


static gboolean
each_contact (MuMsgContact *c, ContactData *cdata)
{
        switch (mu_msg_contact_type (c)) {

        case MU_MSG_CONTACT_TYPE_FROM:
                cdata->from.add(make_contact_sexp(c));
                break;
        case MU_MSG_CONTACT_TYPE_TO:
                cdata->to.add(make_contact_sexp(c));
                break;
        case MU_MSG_CONTACT_TYPE_CC:
                cdata->cc.add(make_contact_sexp(c));
                break;
        case MU_MSG_CONTACT_TYPE_BCC:
                cdata->bcc.add(make_contact_sexp(c));
                break;
        case MU_MSG_CONTACT_TYPE_REPLY_TO:
                cdata->reply_to.add(make_contact_sexp(c));
                break;
        default:
                g_return_val_if_reached (FALSE);
                return FALSE;
        }
        return TRUE;
}

static void
add_prop_nonempty_list(Sexp::List& list, std::string&& name,
                       Sexp::List&& sexp)
{
        if (sexp.empty())
                return;

        list.add_prop(std::move(name),
                         Sexp::make_list(std::move(sexp)));
}


static void
add_contacts (Sexp::List& list, MuMsg *msg)
{
        ContactData cdata{};
        mu_msg_contact_foreach (msg, (MuMsgContactForeachFunc)each_contact,
                                &cdata);

        add_prop_nonempty_list (list, ":from",     std::move(cdata.from));
        add_prop_nonempty_list (list, ":to",       std::move(cdata.to));
        add_prop_nonempty_list (list, ":cc",       std::move(cdata.cc));
        add_prop_nonempty_list (list, ":bcc",      std::move(cdata.bcc));
        add_prop_nonempty_list (list, ":reply-to", std::move(cdata.reply_to));

        add_list_post (list, msg);
}

typedef struct {
        Sexp::List flaglist;
        MuFlags    msgflags;
} FlagData;

static void
each_flag (MuFlags flag, FlagData *fdata)
{
        if (flag & fdata->msgflags)
                fdata->flaglist.add(Sexp::make_symbol(mu_flag_name(flag)));
}

static void
add_flags (Sexp::List& list, MuMsg *msg)
{
        FlagData fdata{};
        fdata.msgflags = mu_msg_get_flags (msg);

        mu_flags_foreach ((MuFlagsForeachFunc)each_flag, &fdata);
        if (!fdata.flaglist.empty())
                list.add_prop(":flags",
                              Sexp::make_list(std::move(fdata.flaglist)));
}

static char*
get_temp_file (MuMsg *msg, MuMsgOptions opts, unsigned index)
{
        char *path;
        GError *err;

        err = NULL;
        path = mu_msg_part_get_cache_path (msg, opts, index, &err);
        if (!path)
                goto errexit;

        if (!mu_msg_part_save (msg, opts, path, index, &err))
                goto errexit;

        return path;

errexit:
        g_warning ("failed to save mime part: %s",
                   err->message ? err->message : "something went wrong");
        g_clear_error (&err);
        g_free (path);

        return NULL;
}

static gchar*
get_temp_file_maybe (MuMsg *msg, MuMsgPart *part, MuMsgOptions opts)
{
        opts = (MuMsgOptions)((int)opts | (int)MU_MSG_OPTION_USE_EXISTING);
        if  (!(opts & MU_MSG_OPTION_EXTRACT_IMAGES) ||
             g_ascii_strcasecmp (part->type, "image") != 0)
                return NULL;
        else
                return get_temp_file (msg, opts, part->index);
}

struct PartInfo {
        Sexp::List  parts;
        MuMsgOptions opts;
};

static void
sig_verdict (Sexp::List& partlist, MuMsgPart *mpart)
{
        MuMsgPartSigStatusReport *report = mpart->sig_status_report;
        if (!report)
                return;

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored   "-Wswitch-enum"
        switch (report->verdict) {
        case MU_MSG_PART_SIG_STATUS_GOOD:
                partlist.add_prop(":signature", Sexp::make_symbol("verified"));
                break;
        case MU_MSG_PART_SIG_STATUS_BAD:
                partlist.add_prop(":signature", Sexp::make_symbol("bad"));
                break;
        case MU_MSG_PART_SIG_STATUS_ERROR:
                partlist.add_prop(":signature", Sexp::make_symbol("unverified"));
                break;
        default:
                break;
        }
#pragma GCC diagnostic pop

        if (report->signers)
                partlist.add_prop(":signers", Sexp::make_string(report->signers));
}

static void
dec_verdict (Sexp::List& partlist, MuMsgPart *mpart)
{
        if (mpart->part_type & MU_MSG_PART_TYPE_DECRYPTED)
                partlist.add_prop(":decryption", Sexp::make_symbol("succeeded"));
        else if (mpart->part_type & MU_MSG_PART_TYPE_ENCRYPTED)
                partlist.add_prop(":decryption", Sexp::make_symbol("failed"));
}


static Sexp
make_part_types (MuMsgPartType ptype)
{
        struct PartTypes {
                MuMsgPartType ptype;
                const char*   name;
        } ptypes[] = {
                { MU_MSG_PART_TYPE_LEAF,       "leaf" },
                { MU_MSG_PART_TYPE_MESSAGE,    "message" },
                { MU_MSG_PART_TYPE_INLINE,     "inline" },
                { MU_MSG_PART_TYPE_ATTACHMENT, "attachment" },
                { MU_MSG_PART_TYPE_SIGNED,     "signed" },
                { MU_MSG_PART_TYPE_ENCRYPTED,  "encrypted" }
        };

        Sexp::List list;
        for (auto u = 0U; u!= G_N_ELEMENTS(ptypes); ++u)
                if (ptype & ptypes[u].ptype)
                        list.add(Sexp::make_symbol(ptypes[u].name));

        return Sexp::make_list(std::move(list));
}

static void
each_part (MuMsg *msg, MuMsgPart *part, PartInfo *pinfo)
{
        auto mimetype = format("%s/%s",
                               part->type ? part->type : "application",
                               part->subtype ? part->subtype : "octet-stream");
        auto maybe_attach = Sexp::make_symbol(mu_msg_part_maybe_attachment (part) ?
                                              "t" : "nil");
        Sexp::List  partlist;

        partlist.add_prop(":index",     Sexp::make_number(part->index));
        partlist.add_prop(":mime-type", Sexp::make_string(mimetype));
        partlist.add_prop(":size",      Sexp::make_number(part->size));

        dec_verdict (partlist, part);
        sig_verdict (partlist, part);

        if (part->part_type)
                partlist.add_prop(":type", make_part_types(part->part_type));

        char *fname = mu_msg_part_get_filename (part, TRUE);
        if (fname)
                partlist.add_prop(":name", Sexp::make_string(fname))                                ;
        g_free (fname);

        if (mu_msg_part_maybe_attachment (part))
                partlist.add_prop(":attachment", Sexp::make_symbol("t"));
        const auto cid{ mu_msg_part_get_content_id(part)};
        if (cid)
                partlist.add_prop(":cid", Sexp::make_string(cid));

        char *tempfile  = get_temp_file_maybe (msg, part, pinfo->opts);
        if (tempfile)
                partlist.add_prop (":temp", Sexp::make_string(tempfile));
        g_free (tempfile);

        pinfo->parts.add(Sexp::make_list(std::move(partlist)));
}


static void
add_parts (Sexp::List& items, MuMsg *msg, MuMsgOptions opts)
{
        PartInfo pinfo;
        pinfo.opts  = opts;

        if (mu_msg_part_foreach (msg, opts, (MuMsgPartForeachFunc)each_part, &pinfo) &&
            !pinfo.parts.empty())
                items.add_prop(":parts", Sexp::make_list(std::move(pinfo.parts)));
}


static void
add_message_file_parts (Sexp::List& items, MuMsg *msg, MuMsgOptions opts)
{
        GError *err{NULL};
        if (!mu_msg_load_msg_file (msg, &err)) {
                g_warning ("failed to load message file: %s",
                           err ? err->message : "some error occurred");
                g_clear_error (&err);
                return;
        }

        add_parts    (items, msg, opts);
        add_contacts (items, msg);

        /* add the user-agent / x-mailer */
        auto str = mu_msg_get_header (msg, "User-Agent");
        if (!str)
                str = mu_msg_get_header (msg, "X-Mailer");

        add_prop_nonempty (items, ":user-agent", str);

        add_prop_nonempty (items, ":body-txt-params",
                           mu_msg_get_body_text_content_type_parameters (msg, opts));
        add_prop_nonempty (items, ":body-txt",  mu_msg_get_body_text(msg, opts));
        add_prop_nonempty (items, ":body-html", mu_msg_get_body_html(msg, opts));
}

static void
add_date_and_size (Sexp::List& items, MuMsg *msg)
{
        auto t = mu_msg_get_date (msg);
        if (t == (time_t)-1)  /* invalid date? */
                t = 0;

        Sexp::List dlist;
        dlist.add(Sexp::make_number((unsigned)(t >> 16)));
        dlist.add(Sexp::make_number((unsigned)(t & 0xffff)));
        dlist.add(Sexp::make_number(0));

        items.add_prop(":date", Sexp::make_list(std::move(dlist)));

        auto s = mu_msg_get_size (msg);
        if (s == (size_t)-1)   /* invalid size? */
                s = 0;

        items.add_prop(":size", Sexp::make_number(s));
}


static void
add_tags (Sexp::List& items, MuMsg *msg)
{
        Sexp::List taglist;
        for (auto tags = mu_msg_get_tags(msg); tags; tags = g_slist_next(tags))
                taglist.add(Sexp::make_string((const char*)tags->data));

        if (!taglist.empty())
                items.add_prop(":tags", Sexp::make_list(std::move(taglist)));
}


Mu::Sexp::List
Mu::msg_to_sexp_list (MuMsg *msg, unsigned docid, MuMsgOptions opts)
{
        g_return_val_if_fail (msg, Sexp::List());
        g_return_val_if_fail (!((opts & MU_MSG_OPTION_HEADERS_ONLY) &&
                                (opts & MU_MSG_OPTION_EXTRACT_IMAGES)),
                              Sexp::List());
        Sexp::List items;

        if (docid != 0)
                items.add_prop(":docid", Sexp::make_number(docid));

        add_prop_nonempty (items, ":subject",      mu_msg_get_subject (msg));
        add_prop_nonempty (items, ":message-id",   mu_msg_get_msgid (msg));
        add_prop_nonempty (items, ":mailing-list", mu_msg_get_mailing_list (msg));
        add_prop_nonempty (items, ":path",         mu_msg_get_path (msg));
        add_prop_nonempty (items, ":maildir",      mu_msg_get_maildir (msg));

        items.add_prop(":priority", Sexp::make_symbol(mu_msg_prio_name(mu_msg_get_prio(msg))));

        /* in the no-headers-only case (see below) we get a more complete list of contacts, so no
         * need to get them here if that's the case */
        if (opts & MU_MSG_OPTION_HEADERS_ONLY)
                add_contacts (items, msg);

        add_prop_nonempty (items, ":references", mu_msg_get_references (msg));
        add_prop_nonempty (items, ":in-reply-to", mu_msg_get_header (msg, "In-Reply-To"));

        add_date_and_size (items, msg);
        add_flags (items, msg);
        add_tags (items, msg);

        /* headers are retrieved from the database, views from the
         * message file file attr things can only be gotten from the
         * file (ie., mu view), not from the database (mu find).  */
        if (!(opts & MU_MSG_OPTION_HEADERS_ONLY))
                add_message_file_parts (items, msg, opts);

        return items;

}

Mu::Sexp
Mu::msg_to_sexp (MuMsg *msg, unsigned docid, MuMsgOptions opts)
{
        return Sexp::make_list(msg_to_sexp_list(msg, docid, opts));
}
