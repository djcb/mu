/*
** Copyright (C) 2018 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <json-glib/json-glib.h>

#include "mu-msg.h"
#include "mu-msg-iter.h"
#include "mu-msg-part.h"
#include "mu-maildir.h"

static void
add_list_member (JsonBuilder *bob, const char* elm, const GSList *lst)
{
	const GSList *cur;

	if (!lst)
		return; /* empty list, don't include */

	bob = json_builder_set_member_name (bob, elm);
	bob = json_builder_begin_array (bob);

	for (cur = lst; cur; cur = g_slist_next(cur))
		bob = json_builder_add_string_value (bob, (const char*)cur->data);

	bob = json_builder_end_array (bob);
}

static void
add_string_member (JsonBuilder *bob, const char* elm, const char *str)
{
	if (!str)
		return; /* don't include */

	bob = json_builder_set_member_name (bob, elm);
	bob = json_builder_add_string_value (bob, str);
}

static void
add_int_member (JsonBuilder *bob, const char* elm, gint64 num)
{
	bob = json_builder_set_member_name (bob, elm);
	bob = json_builder_add_int_value (bob, num);
}

static void
add_bool_member (JsonBuilder *bob, const char* elm, gboolean b)
{
	bob = json_builder_set_member_name (bob, elm);
	bob = json_builder_add_boolean_value (bob, b);
}


static void
consume_array_member (JsonBuilder *bob, const char* elm, JsonArray *arr)
{
	JsonNode *node;

	if (!arr)
		return; /* nothing to do */

	node = json_node_new (JSON_NODE_ARRAY);
	json_node_init_array (node, arr);
	json_array_unref (arr);

	bob = json_builder_set_member_name (bob, elm);
	bob = json_builder_add_value (bob, node); /* consumes */
}



typedef struct {
	JsonArray *from, *to, *cc, *bcc, *reply_to;
} ContactData;

static void
add_contact (JsonArray **arr, MuMsgContact *c)
{
	JsonObject *cell;

	if (!*arr)
		*arr = json_array_new ();

	cell = json_object_new ();
	if (c->name)
		json_object_set_string_member (cell, "name", c->name);
	if (c->email)
		json_object_set_string_member (cell, "email", c->email);

	json_array_add_object_element (*arr, cell); /* consumes */
}

static gboolean
each_contact (MuMsgContact *c, ContactData *cdata)
{
	switch (mu_msg_contact_type (c)) {

	case MU_MSG_CONTACT_TYPE_FROM:
		add_contact(&cdata->from, c);
		break;
	case MU_MSG_CONTACT_TYPE_TO:
		add_contact(&cdata->to ,c);
		break;
	case MU_MSG_CONTACT_TYPE_CC:
		add_contact(&cdata->cc, c);
		break;
	case MU_MSG_CONTACT_TYPE_BCC:
		add_contact(&cdata->bcc, c);
		break;
	case MU_MSG_CONTACT_TYPE_REPLY_TO:
		add_contact(&cdata->reply_to, c);
		break;
	default: g_return_val_if_reached (FALSE);
	}

	return TRUE;
}


static void
maybe_append_list_post_as_reply_to (JsonBuilder *bob, MuMsg *msg)
{
	/* some mailing lists do not set the reply-to; see pull #1278. So for
	 * those cases, check the List-Post address and use that instead */

	GMatchInfo	*minfo;
	GRegex		*rx;
	const char*	 list_post;

	list_post = mu_msg_get_header (msg, "List-Post");
	if (!list_post)
		return;

	rx = g_regex_new ("^(<?mailto:)?([a-z0-9%+@.-]+)>?", G_REGEX_CASELESS, 0, NULL);
	g_return_if_fail(rx);

	if (g_regex_match (rx, list_post, 0, &minfo)) {
		char	*addr;
		addr = g_match_info_fetch (minfo, 2);

		bob = json_builder_set_member_name (bob, "reply-to");
		bob = json_builder_begin_array(bob);
		bob = json_builder_begin_object(bob);
		add_string_member(bob, "email", addr);
		g_free (addr);

		bob = json_builder_end_object(bob);
		bob = json_builder_end_array(bob);
	}

	g_match_info_free (minfo);
	g_regex_unref (rx);
}


static void
add_contacts (JsonBuilder *bob, MuMsg *msg)
{
	ContactData cdata;
	memset (&cdata, 0, sizeof(cdata));

	mu_msg_contact_foreach (msg,
				(MuMsgContactForeachFunc)each_contact,
				&cdata);

	consume_array_member (bob, "to" ,      cdata.to);
	consume_array_member (bob, "from" ,    cdata.from);
	consume_array_member (bob, "cc" ,      cdata.cc);
	consume_array_member (bob, "bcc" ,     cdata.bcc);
	consume_array_member (bob, "reply-to", cdata.reply_to);

	if (!cdata.reply_to)
		maybe_append_list_post_as_reply_to (bob, msg);
}

struct _FlagData {
	JsonBuilder		*bob;
	MuFlags			 msgflags;
};
typedef struct _FlagData	 FlagData;

static void
each_flag (MuFlags flag, FlagData *fdata)
{
	if (!(flag & fdata->msgflags))
		return;

	json_builder_add_string_value (fdata->bob,
				       mu_flag_name(flag));
}

static void
add_flags (JsonBuilder *bob, MuMsg *msg)
{
	FlagData fdata;

	fdata.msgflags = mu_msg_get_flags (msg);
	fdata.bob      = bob;

	bob = json_builder_set_member_name (bob, "flags");

	bob = json_builder_begin_array (bob);
	mu_flags_foreach ((MuFlagsForeachFunc)each_flag, &fdata);
	bob = json_builder_end_array (bob);

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
	opts |= MU_MSG_OPTION_USE_EXISTING;

	if  (!(opts & MU_MSG_OPTION_EXTRACT_IMAGES) ||
	     g_ascii_strcasecmp (part->type, "image") != 0)
		return NULL;

	return get_temp_file (msg, opts, part->index);
}


struct _PartInfo {
	JsonBuilder		*bob;
	MuMsgOptions		 opts;
};
typedef struct _PartInfo	 PartInfo;


static void
add_part_crypto (JsonBuilder *bob, MuMsgPart *mpart, PartInfo *pinfo)
{
	const char			*verdict;
	MuMsgPartSigStatusReport	*report;


	add_string_member (bob, "decryption",
			   pinfo->opts & MU_MSG_PART_TYPE_DECRYPTED ? "ok" :
			   pinfo->opts & MU_MSG_PART_TYPE_ENCRYPTED ? "failed" :
			   NULL);

	report = mpart->sig_status_report;
	if (!report)
		return;

	switch (report->verdict) {
	case MU_MSG_PART_SIG_STATUS_GOOD:  verdict = "verified"; break;
	case MU_MSG_PART_SIG_STATUS_BAD:   verdict = "bad"; break;
	case MU_MSG_PART_SIG_STATUS_ERROR: verdict = "unverified"; break;
	default: verdict = NULL;
 	}

	add_string_member (bob, "signature", verdict);
	add_string_member (bob, "signers", report->signers);
}

static void
add_part_type (JsonBuilder *bob, MuMsgPartType ptype)
{
	unsigned u;
	struct PartTypes {
		MuMsgPartType ptype;
		const char* name;
	} ptypes[] = {
		{ MU_MSG_PART_TYPE_LEAF,       "leaf" },
		{ MU_MSG_PART_TYPE_MESSAGE,    "message" },
		{ MU_MSG_PART_TYPE_INLINE,     "inline" },
		{ MU_MSG_PART_TYPE_ATTACHMENT, "attachment" },
		{ MU_MSG_PART_TYPE_SIGNED,     "signed" },
		{ MU_MSG_PART_TYPE_ENCRYPTED,  "encrypted" }
	};

	bob =  json_builder_set_member_name (bob, "type");
	bob = json_builder_begin_array(bob);

	for (u = 0; u!= G_N_ELEMENTS(ptypes); ++u)
		if (ptype & ptypes[u].ptype)
			json_builder_add_string_value (bob, ptypes[u].name);

	bob = json_builder_end_array(bob);
}


static void
each_part (MuMsg *msg, MuMsgPart *part, PartInfo *pinfo)
{
	char		*name, *tmpfile;

	pinfo->bob = json_builder_begin_object(pinfo->bob);

	name	 = mu_msg_part_get_filename (part, TRUE);
	tmpfile  = get_temp_file_maybe (msg, part, pinfo->opts);

	add_int_member    (pinfo->bob, "index", part->index);
	add_string_member (pinfo->bob, "name", name);

	if (part->type && part->subtype) {
		char *mime_type =
			g_strdup_printf ("%s/%s", part->type, part->subtype);
		add_string_member (pinfo->bob, "mime-type", mime_type);
		g_free(mime_type);
	}

	add_string_member (pinfo->bob, "temp", tmpfile);
	add_part_type (pinfo->bob, part->part_type);

	if (mu_msg_part_maybe_attachment (part))
		add_bool_member (pinfo->bob, "attachment", TRUE);

	add_string_member (pinfo->bob, "cid", mu_msg_part_get_content_id(part));
	add_int_member (pinfo->bob, "size", part->size);

	add_part_crypto (pinfo->bob, part, pinfo);

	g_free (name);
	g_free (tmpfile);

	pinfo->bob = json_builder_end_object(pinfo->bob);
}


static void
add_parts (JsonBuilder *bob, MuMsg *msg, MuMsgOptions opts)
{
	PartInfo pinfo;

	pinfo.opts = opts;
	bob	   = json_builder_set_member_name (bob, "parts");
	bob	   = json_builder_begin_array (bob);

	mu_msg_part_foreach (msg, opts, (MuMsgPartForeachFunc)each_part, &pinfo);

	bob = json_builder_end_array (bob);
}

static void
add_thread_info (JsonBuilder *bob, const MuMsgIterThreadInfo *ti)
{
	bob = json_builder_set_member_name (bob, "thread");
	bob = json_builder_begin_object(bob);

	add_string_member (bob, "path",  ti->threadpath);
	add_int_member    (bob, "level", ti->level);

	bob = json_builder_set_member_name (bob, "flags");
	bob = json_builder_begin_array (bob);

	if (ti->prop & MU_MSG_ITER_THREAD_PROP_FIRST_CHILD)
		bob = json_builder_add_string_value (bob, "first-child");
	if (ti->prop & MU_MSG_ITER_THREAD_PROP_LAST_CHILD)
		bob = json_builder_add_string_value (bob, "last-child");
	if (ti->prop & MU_MSG_ITER_THREAD_PROP_EMPTY_PARENT)
		bob = json_builder_add_string_value (bob, "empty-parent");
	if (ti->prop & MU_MSG_ITER_THREAD_PROP_DUP)
		bob = json_builder_add_string_value (bob, "duplicate");
	if (ti->prop & MU_MSG_ITER_THREAD_PROP_HAS_CHILD)
		bob = json_builder_add_string_value (bob, "has-child");

	bob = json_builder_end_array (bob);
	bob = json_builder_end_object(bob);
}

static void
add_body_txt_params (JsonBuilder *bob, MuMsg *msg, MuMsgOptions opts)
{
	const GSList    *params;

	params = mu_msg_get_body_text_content_type_parameters (msg, opts);
	if (!params)
		return;

	bob = json_builder_set_member_name (bob, "body-txt-params");
	bob = json_builder_begin_array (bob);

        while (params) {
		const char *key, *val;

		key = (const char *)params->data;
		params = g_slist_next(params);
		if (!params)
			break;
		val = (const char *)params->data;

                if (key && val) {
			bob = json_builder_begin_object(bob);
			add_string_member(bob, key, val);
			bob = json_builder_end_object(bob);
                }

		params = g_slist_next(params);
        }

        bob = json_builder_end_array(bob);
}

static void /* ie., parts that require opening the message file */
add_file_parts (JsonBuilder *bob, MuMsg *msg, MuMsgOptions opts)
{
	const char	*str;
	GError		*err;

	err = NULL;

	if (!mu_msg_load_msg_file (msg, &err)) {
		g_warning ("failed to load message file: %s",
			   err ? err->message : "some error occurred");
		g_clear_error (&err);
		return;
	}

	add_parts (bob, msg, opts);
	add_contacts (bob, msg);

	/* add the user-agent / x-mailer */
	str = mu_msg_get_header (msg, "User-Agent");
	if (!str)
		str = mu_msg_get_header (msg, "X-Mailer");
	add_string_member (bob, "user-agent", str);
	add_body_txt_params (bob, msg, opts);
	add_string_member (bob, "body-txt", mu_msg_get_body_text(msg, opts));
	add_string_member (bob, "body-html", mu_msg_get_body_html(msg, opts));
}

struct _JsonNode*
mu_msg_to_json (MuMsg *msg, unsigned docid, const MuMsgIterThreadInfo *ti,
		MuMsgOptions opts)
{
	JsonNode	*node;
	JsonBuilder	*bob;

	time_t	t;
	size_t	s;

	g_return_val_if_fail (msg, NULL);
	g_return_val_if_fail (!((opts & MU_MSG_OPTION_HEADERS_ONLY) &&
				(opts & MU_MSG_OPTION_EXTRACT_IMAGES)),NULL);
	bob = json_builder_new ();
	bob = json_builder_begin_object (bob);

	if (ti)
		add_thread_info (bob, ti);

	add_string_member (bob, "subject", mu_msg_get_subject (msg));

	/* in the no-headers-only case (see below) we get a more complete list
	 * of contacts, so no need to get them here if that's the case */
	if (opts & MU_MSG_OPTION_HEADERS_ONLY)
		add_contacts (bob, msg);

	t = mu_msg_get_date (msg);
	if (t != (time_t)-1)
		add_int_member (bob, "date", t);

	s = mu_msg_get_size (msg);
	if (s != (size_t)-1)
		add_int_member (bob, "size", s);

	add_string_member (bob, "message-id", mu_msg_get_msgid (msg));
	add_string_member (bob, "mailing-list", mu_msg_get_mailing_list (msg));
	add_string_member (bob, "path", mu_msg_get_path (msg));
	add_string_member (bob, "maildir", mu_msg_get_maildir (msg));
	add_string_member (bob, "priority", mu_msg_prio_name(mu_msg_get_prio(msg)));

	add_flags (bob, msg);

	add_list_member (bob, "tags", mu_msg_get_tags(msg));
	add_list_member (bob, "references", mu_msg_get_references (msg));
	add_string_member (bob, "in-reply-to",
			   mu_msg_get_header (msg, "In-Reply-To"));

	/* headers are retrieved from the database, views from the
	 * message file file attr things can only be gotten from the
	 * file (ie., mu view), not from the database (mu find).  */
	if (!(opts & MU_MSG_OPTION_HEADERS_ONLY))
		add_file_parts (bob, msg, opts);

	bob  = json_builder_end_object (bob);
	node = json_builder_get_root (bob);

	g_clear_object (&bob);

	return node;
}
