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

#include "utils/mu-str.h"
#include "mu-msg.h"
#include "mu-msg-iter.h"
#include "mu-msg-part.h"
#include "mu-maildir.h"

using namespace Mu;
using namespace Sexp;

static void
add_prop_nonempty (Node::Seq& items, const char* elm, const GSList *str_lst)
{
        Node::Seq elms;
	while (str_lst) {
                elms.add((const char*)str_lst->data);
                str_lst = g_slist_next(str_lst);
        }

        if (!elms.empty())
                items.add_prop(elm, elms);
}

static void
add_prop_nonempty (Node::Seq& items, const char* elm, Node::Seq&& seq)
{
        if (!seq.empty())
                items.add_prop(elm, std::move(seq));
}

static void
add_prop_nonempty (Node::Seq& items, const char* name, const char *str)
{
        if (str && str[0])
                items.add_prop(name, str);
}

static void
add_prop_symbol (Node::Seq& items, const char* name, const char *str)
{
        items.add_prop(name, Node::make_symbol(str));
}

static Node
make_contact_node (MuMsgContact *c)
{
        // a cons-pair...perhaps make this a plist too?

        Node::Seq contact;
        if (mu_msg_contact_name(c))
                contact.add (mu_msg_contact_name(c));
        else
                contact.add (Node::make_symbol("nil"));

        contact.add(Node::make_symbol("."));
        contact.add(mu_msg_contact_email(c));

        return Node::make_list(std::move(contact));
}

static void
add_list_post (Node::Seq& items, MuMsg *msg)
{
	/* some mailing lists do not set the reply-to; see pull #1278. So for
	 * those cases, check the List-Post address and use that instead */

	GMatchInfo	*minfo;
	GRegex		*rx;
	const char*	 list_post;

	list_post = mu_msg_get_header (msg, "List-Post");
	if (!list_post)
		return;

	rx = g_regex_new ("<?mailto:([a-z0-9%+@._-]+)>?", G_REGEX_CASELESS,
                          (GRegexMatchFlags)0, NULL);
	g_return_if_fail(rx);

        Node::Seq addrs;
	if (g_regex_match (rx, list_post, (GRegexMatchFlags)0, &minfo)) {
                auto address = (char*)g_match_info_fetch (minfo, 1);
                MuMsgContact contact{NULL,  address};
                addrs.add(make_contact_node(&contact));
                items.add_prop(":list-post", std::move(addrs));
                g_free(address);
        }

	g_match_info_free (minfo);
	g_regex_unref (rx);
}


struct _ContactData {
        Node::Seq from, to, cc, bcc, reply_to;
};
typedef struct _ContactData ContactData;


static gboolean
each_contact (MuMsgContact *c, ContactData *cdata)
{
	switch (mu_msg_contact_type (c)) {

	case MU_MSG_CONTACT_TYPE_FROM:
                cdata->from.add(make_contact_node(c));
                break;
	case MU_MSG_CONTACT_TYPE_TO:
                cdata->to.add(make_contact_node(c));
		break;
	case MU_MSG_CONTACT_TYPE_CC:
                cdata->cc.add(make_contact_node(c));
		break;
	case MU_MSG_CONTACT_TYPE_BCC:
                cdata->bcc.add(make_contact_node(c));
		break;
	case MU_MSG_CONTACT_TYPE_REPLY_TO:
                cdata->reply_to.add(make_contact_node(c));
		break;
	default:
                g_return_val_if_reached (FALSE);
                return FALSE;
	}
	return TRUE;
}


static void
add_contacts (Node::Seq& items, MuMsg *msg)
{
	ContactData cdata{};
	mu_msg_contact_foreach (msg, (MuMsgContactForeachFunc)each_contact,
				&cdata);

        add_prop_nonempty(items, ":from",     std::move(cdata.from));
        add_prop_nonempty(items, ":to",       std::move(cdata.to));
        add_prop_nonempty(items, ":cc",       std::move(cdata.cc));
        add_prop_nonempty(items, ":bcc",      std::move(cdata.bcc));
        add_prop_nonempty(items, ":reply-to", std::move(cdata.reply_to));

        add_list_post (items, msg);
}

typedef struct {
        Node::Seq flagseq;
	MuFlags   msgflags;
} FlagData;

static void
each_flag (MuFlags flag, FlagData *fdata)
{
	if (flag & fdata->msgflags)
                fdata->flagseq.add(Node::make_symbol(mu_flag_name(flag)));
}

static void
add_flags (Node::Seq& items, MuMsg *msg)
{
	FlagData fdata{};
	fdata.msgflags = mu_msg_get_flags (msg);

	mu_flags_foreach ((MuFlagsForeachFunc)each_flag, &fdata);
        add_prop_nonempty(items, ":flags", std::move(fdata.flagseq));
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
	Node::Seq    parts;
	MuMsgOptions opts;
};

static void
sig_verdict (Node::Seq& partseq, MuMsgPart *mpart)
{
	MuMsgPartSigStatusReport *report = mpart->sig_status_report;
	if (!report)
		return;

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored   "-Wswitch-enum"
	switch (report->verdict) {
	case MU_MSG_PART_SIG_STATUS_GOOD:
                add_prop_symbol (partseq, ":signature", "verified");
		break;
	case MU_MSG_PART_SIG_STATUS_BAD:
                add_prop_symbol (partseq, ":signature", "bad");
		break;
	case MU_MSG_PART_SIG_STATUS_ERROR:
                add_prop_symbol (partseq, ":signature", "unverified");
		break;
	default:
		break;
	}
#pragma GCC diagnostic pop

        add_prop_nonempty (partseq, ":signers", report->signers);
}

static void
dec_verdict (Node::Seq& partseq, MuMsgPart *mpart)
{
	if (mpart->part_type & MU_MSG_PART_TYPE_DECRYPTED)
                partseq.add_prop(":decryption", Node::make_symbol("succeeded"));
        else if (mpart->part_type & MU_MSG_PART_TYPE_ENCRYPTED)
                partseq.add_prop(":decryption", Node::make_symbol("failed"));
}


static Node::Seq
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

        Node::Seq seq;
	for (auto u = 0U; u!= G_N_ELEMENTS(ptypes); ++u)
		if (ptype & ptypes[u].ptype)
                        seq.add(Node::make_symbol(ptypes[u].name));

        return seq;
}


static void
each_part (MuMsg *msg, MuMsgPart *part, PartInfo *pinfo)
{
        auto mimetype = format("%s/%s",
                               part->type ? part->type : "application",
                               part->subtype ? part->subtype : "octet-stream");
        auto maybe_attach = Node::make_symbol(mu_msg_part_maybe_attachment (part) ?
                                              "t" : "nil");

        Node::Seq  partseq;

        partseq.add_prop(":index",     part->index);
        partseq.add_prop(":mime-type", mimetype);
        partseq.add_prop(":size",      part->size);

        dec_verdict (partseq, part);
        sig_verdict (partseq, part);

        add_prop_nonempty(partseq, ":type", make_part_types(part->part_type));

        char *fname = mu_msg_part_get_filename (part, TRUE);
        add_prop_nonempty(partseq, ":name", fname);
        g_free (fname);

        if (mu_msg_part_maybe_attachment (part))
                add_prop_symbol (partseq, ":attachment", "t");

        add_prop_nonempty (partseq, ":cid", mu_msg_part_get_content_id(part));

        char *tempfile  = get_temp_file_maybe (msg, part, pinfo->opts);
        add_prop_nonempty (partseq, ":temp", tempfile);
        g_free (tempfile);

        pinfo->parts.add(Node::make_list(std::move(partseq)));
}


static void
add_parts (Node::Seq& items, MuMsg *msg, MuMsgOptions opts)
{
	PartInfo pinfo;
	pinfo.opts  = opts;

	if (mu_msg_part_foreach (msg, opts, (MuMsgPartForeachFunc)each_part, &pinfo))
                add_prop_nonempty (items, ":parts", std::move(pinfo.parts));
}

static void
add_thread_info (Node::Seq& items, const MuMsgIterThreadInfo *ti)
{
        Node::Seq info;

        info.add_prop(":path", ti->threadpath);
        info.add_prop(":level", ti->level);

        if (ti->prop & MU_MSG_ITER_THREAD_PROP_FIRST_CHILD)
                add_prop_symbol(info, ":first-child", "t");
        if (ti->prop & MU_MSG_ITER_THREAD_PROP_LAST_CHILD)
                add_prop_symbol(info, ":last-child", "t");
        if (ti->prop & MU_MSG_ITER_THREAD_PROP_EMPTY_PARENT)
                add_prop_symbol(info, ":empty-parent", "t");
        if (ti->prop & MU_MSG_ITER_THREAD_PROP_DUP)
                add_prop_symbol(info, ":duplicate", "t");
        if (ti->prop & MU_MSG_ITER_THREAD_PROP_HAS_CHILD)
                add_prop_symbol(info, ":has-child", "t");

        items.add_prop(":thread", std::move(info));
}


static void
add_message_file_parts (Node::Seq& items, MuMsg *msg, MuMsgOptions opts)
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
add_date_and_size (Node::Seq& items, MuMsg *msg)
{
        auto t = mu_msg_get_date (msg);
	if (t == (time_t)-1)  /* invalid date? */
		t = 0;

        Node::Seq dseq;
        dseq.add((unsigned)(t >> 16));
        dseq.add((unsigned)(t && 0xffff));
        dseq.add(0);

        items.add_prop(":date", std::move(dseq));

        auto s = mu_msg_get_size (msg);
	if (s == (size_t)-1)   /* invalid size? */
		s = 0;

        items.add_prop(":size", s);
}


static void
add_tags (Node::Seq& items, MuMsg *msg)
{
        Node::Seq tagseq;
        for (auto tags = mu_msg_get_tags(msg); tags; tags = g_slist_next(tags))
                tagseq.add((const char*)tags->data);

        add_prop_nonempty (items, ":tags", std::move(tagseq));
}


Mu::Sexp::Node
Mu::msg_to_sexp (MuMsg *msg, unsigned docid,
                 const struct _MuMsgIterThreadInfo *ti,
                 MuMsgOptions opts)
{
        g_return_val_if_fail (msg, Sexp::Node::make("error"));
	g_return_val_if_fail (!((opts & MU_MSG_OPTION_HEADERS_ONLY) &&
	 			(opts & MU_MSG_OPTION_EXTRACT_IMAGES)),
                              Sexp::Node::make("error"));
        Node::Seq items;

        if (docid != 0)
                items.add_prop(":docid", docid);

        if (ti)
                add_thread_info (items, ti);

	add_prop_nonempty (items, ":subject",      mu_msg_get_subject (msg));
	add_prop_nonempty (items, ":message-id",   mu_msg_get_msgid (msg));
	add_prop_nonempty (items, ":mailing-list", mu_msg_get_mailing_list (msg));
	add_prop_nonempty (items, ":path",	   mu_msg_get_path (msg));
	add_prop_nonempty (items, ":maildir",      mu_msg_get_maildir (msg));

        items.add_prop(":priority", Node::make_symbol(mu_msg_prio_name(mu_msg_get_prio(msg))));

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

        return Node::make_list(std::move(items));
}


char*
mu_msg_to_sexp (MuMsg *msg, unsigned docid, const MuMsgIterThreadInfo *ti,
		MuMsgOptions opts)
{
        return g_strdup (Mu::msg_to_sexp (msg, docid, ti, opts)
                         .to_string().c_str());
}
