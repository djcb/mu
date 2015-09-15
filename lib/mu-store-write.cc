/* -*-mode: c++; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8-*- */
/*
** Copyright (C) 2008-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <cstdio>
#include <xapian.h>
#include <cstring>
#include <stdexcept>

#include "mu-store.h"
#include "mu-store-priv.hh" /* _MuStore */

#include "mu-msg.h"
#include "mu-msg-part.h"
#include "mu-store.h"
#include "mu-util.h"
#include "mu-str.h"
#include "mu-date.h"
#include "mu-flags.h"
#include "mu-contacts.h"

void
_MuStore::begin_transaction ()
{
	try {
		db_writable()->begin_transaction();
		in_transaction (true);
	} MU_XAPIAN_CATCH_BLOCK;
}


void
_MuStore::commit_transaction () {
	try {
		in_transaction (false);
		db_writable()->commit_transaction();
	} MU_XAPIAN_CATCH_BLOCK;
}

void
_MuStore::rollback_transaction () {
	try {
		in_transaction (false);
		db_writable()->cancel_transaction();
	} MU_XAPIAN_CATCH_BLOCK;
}


/* we cache these prefix strings, so we don't have to allocate them all
 * the time; this should save 10-20 string allocs per message */
G_GNUC_CONST static const std::string&
prefix (MuMsgFieldId mfid)
{
	static std::string fields[MU_MSG_FIELD_ID_NUM];
	static bool initialized = false;

	if (G_UNLIKELY(!initialized)) {
		for (int i = 0; i != MU_MSG_FIELD_ID_NUM; ++i)
			fields[i] = std::string (1, mu_msg_field_xapian_prefix
						 ((MuMsgFieldId)i));
		initialized = true;
	}

	return fields[mfid];
}


static void
add_synonym_for_flag (MuFlags flag, Xapian::WritableDatabase *db)
{
	static const std::string pfx(prefix(MU_MSG_FIELD_ID_FLAGS));

	db->clear_synonyms (pfx + mu_flag_name (flag));
	db->add_synonym (pfx + mu_flag_name (flag), pfx +
			 (std::string(1, (char)(tolower(mu_flag_char(flag))))));
}


static void
add_synonym_for_prio (MuMsgPrio prio, Xapian::WritableDatabase *db)
{
	static const std::string pfx (prefix(MU_MSG_FIELD_ID_PRIO));

	std::string s1 (pfx + mu_msg_prio_name (prio));
	std::string s2 (pfx + (std::string(1, mu_msg_prio_char (prio))));

	db->clear_synonyms (s1);
	db->clear_synonyms (s2);

	db->add_synonym (s1, s2);
}


static void
add_synonyms (MuStore *store)
{
	mu_flags_foreach ((MuFlagsForeachFunc)add_synonym_for_flag,
			  store->db_writable());
	mu_msg_prio_foreach ((MuMsgPrioForeachFunc)add_synonym_for_prio,
			     store->db_writable());
}


MuStore*
mu_store_new_writable (const char* xpath, const char *contacts_cache,
		       gboolean rebuild, GError **err)
{
	g_return_val_if_fail (xpath, NULL);

	try {
		try {
			MuStore *store;
			store = new _MuStore (xpath, contacts_cache,
					      rebuild ? true : false);
			add_synonyms (store);
			return store;

		} MU_STORE_CATCH_BLOCK_RETURN(err,NULL);

	} MU_XAPIAN_CATCH_BLOCK_G_ERROR_RETURN (err, MU_ERROR_XAPIAN, NULL);
}



void
mu_store_set_batch_size (MuStore *store, guint batchsize)
{
	g_return_if_fail (store);
	store->set_batch_size (batchsize);
}


gboolean
mu_store_set_metadata (MuStore *store, const char *key, const char *val,
		       GError **err)
{
	g_return_val_if_fail (store, FALSE);
	g_return_val_if_fail (key, FALSE);
	g_return_val_if_fail (val, FALSE);

	try {
		try {
			store->db_writable()->set_metadata (key, val);
			return TRUE;

		} MU_STORE_CATCH_BLOCK_RETURN(err, FALSE);

	} MU_XAPIAN_CATCH_BLOCK_G_ERROR_RETURN(err, MU_ERROR_XAPIAN, FALSE);

}


gboolean
mu_store_clear (MuStore *store, GError **err)
{
	g_return_val_if_fail (store, FALSE);

	try {
		try {
			store->clear();
			return TRUE;

		} MU_STORE_CATCH_BLOCK_RETURN(err, FALSE);

	} MU_XAPIAN_CATCH_BLOCK_RETURN(FALSE);
}


void
mu_store_flush (MuStore *store)
{
	g_return_if_fail (store);

	try {
		if (store->in_transaction())
			store->commit_transaction ();
		store->db_writable()->commit ();

	} MU_XAPIAN_CATCH_BLOCK;
}


static void
add_terms_values_date (Xapian::Document& doc, MuMsg *msg, MuMsgFieldId mfid)
{
	time_t t;
	const char *datestr;

	t = (time_t)mu_msg_get_field_numeric (msg, mfid);

	datestr = mu_date_time_t_to_str_s (t, FALSE /*UTC*/);
	doc.add_value ((Xapian::valueno)mfid, datestr);
}

G_GNUC_CONST
static const std::string&
flag_val (char flagchar)
{
	static const std::string
		pfx (prefix(MU_MSG_FIELD_ID_FLAGS)),
		draftstr   (pfx + (char)tolower(mu_flag_char(MU_FLAG_DRAFT))),
		flaggedstr (pfx + (char)tolower(mu_flag_char(MU_FLAG_FLAGGED))),
		passedstr  (pfx + (char)tolower(mu_flag_char(MU_FLAG_PASSED))),
		repliedstr (pfx + (char)tolower(mu_flag_char(MU_FLAG_REPLIED))),
		seenstr	   (pfx + (char)tolower(mu_flag_char(MU_FLAG_SEEN))),
		trashedstr (pfx + (char)tolower(mu_flag_char(MU_FLAG_TRASHED))),
		newstr	   (pfx + (char)tolower(mu_flag_char(MU_FLAG_NEW))),
		signedstr  (pfx + (char)tolower(mu_flag_char(MU_FLAG_SIGNED))),
		cryptstr   (pfx + (char)tolower(mu_flag_char(MU_FLAG_ENCRYPTED))),
		attachstr  (pfx + (char)tolower(mu_flag_char(MU_FLAG_HAS_ATTACH))),
		unreadstr  (pfx + (char)tolower(mu_flag_char(MU_FLAG_UNREAD))),
		liststr    (pfx + (char)tolower(mu_flag_char(MU_FLAG_LIST)));

	switch (flagchar) {

	case 'D': return draftstr;
	case 'F': return flaggedstr;
	case 'P': return passedstr;
	case 'R': return repliedstr;
	case 'S': return seenstr;
	case 'T': return trashedstr;

	case 'N': return newstr;

	case 'z': return signedstr;
	case 'x': return cryptstr;
	case 'a': return attachstr;
	case 'l': return liststr;

	case 'u': return unreadstr;

	default:
		g_return_val_if_reached (flaggedstr);
		return flaggedstr;
	}
}




/* pre-calculate; optimization */
G_GNUC_CONST static const std::string&
prio_val (MuMsgPrio prio)
{
	static const std::string pfx (prefix(MU_MSG_FIELD_ID_PRIO));

	static const std::string
		low (pfx + std::string(1, mu_msg_prio_char(MU_MSG_PRIO_LOW))),
		norm (pfx + std::string(1, mu_msg_prio_char(MU_MSG_PRIO_NORMAL))),
		high (pfx + std::string(1, mu_msg_prio_char(MU_MSG_PRIO_HIGH)));

	switch (prio) {
	case MU_MSG_PRIO_LOW:    return low;
	case MU_MSG_PRIO_NORMAL: return norm;
	case MU_MSG_PRIO_HIGH:   return high;
	default:
		g_return_val_if_reached (norm);
		return norm;
	}
}



static void
add_terms_values_number (Xapian::Document& doc, MuMsg *msg, MuMsgFieldId mfid)
{
	gint64 num = mu_msg_get_field_numeric (msg, mfid);

	const std::string numstr (Xapian::sortable_serialise((double)num));
	doc.add_value ((Xapian::valueno)mfid, numstr);

	if (mfid == MU_MSG_FIELD_ID_FLAGS) {
		const char *cur = mu_flags_to_str_s
			((MuFlags)num,(MuFlagType)MU_FLAG_TYPE_ANY);
		g_return_if_fail (cur);
		while (*cur) {
			doc.add_term (flag_val(*cur));
			++cur;
		}

	} else if (mfid == MU_MSG_FIELD_ID_PRIO)
		doc.add_term (prio_val((MuMsgPrio)num));
}

static void
add_terms_values_msgid (Xapian::Document& doc, MuMsg *msg)
{
	char *str;
	const char *orig;

	if (!(orig = mu_msg_get_field_string (
		      msg, MU_MSG_FIELD_ID_MSGID)))
		return; /* nothing to do */

	str = mu_str_process_msgid (orig, FALSE);

	doc.add_value ((Xapian::valueno)MU_MSG_FIELD_ID_MSGID, orig);
	doc.add_term (prefix(MU_MSG_FIELD_ID_MSGID) +
		      std::string(str, 0, _MuStore::MAX_TERM_LENGTH));

	g_free (str);
}



/* for string and string-list */
static void
add_terms_values_str (Xapian::Document& doc, const char *val, MuMsgFieldId mfid)
{
	char *str;

	if (mu_msg_field_preprocess (mfid))
		str = mu_str_process_term (val);
	else
		str = g_strdup (val);

	if (mu_msg_field_xapian_index (mfid)) {
		Xapian::TermGenerator termgen;
		termgen.set_document (doc);
		termgen.index_text_without_positions (str, 1, prefix(mfid));
		if (g_strcmp0 (val, str) != 0)
			termgen.index_text_without_positions (
				val, 1, prefix(mfid));
	}

	if (mu_msg_field_xapian_term(mfid))
		doc.add_term (prefix(mfid) +
			      std::string(str, 0, _MuStore::MAX_TERM_LENGTH));

	g_free (str);
}


static void
add_terms_values_string (Xapian::Document& doc, MuMsg *msg, MuMsgFieldId mfid)
{
	const char *orig;

	if (!(orig = mu_msg_get_field_string (msg, mfid)))
		return; /* nothing to do */

	/* the value is what we display in search results; the
	 * unchanged original */
	if (mu_msg_field_xapian_value(mfid))
		doc.add_value ((Xapian::valueno)mfid, orig);

	add_terms_values_str (doc, orig, mfid);
}



static void
add_terms_values_string_list (Xapian::Document& doc, MuMsg *msg,
			      MuMsgFieldId mfid)
{
	const GSList *lst;

	lst = mu_msg_get_field_string_list (msg, mfid);
	if (!lst)
		return;

	if (mu_msg_field_xapian_value (mfid)) {
		gchar *str;
		str = mu_str_from_list (lst, ',');
		if (str)
			doc.add_value ((Xapian::valueno)mfid, str);
		g_free (str);
	}

	if (mu_msg_field_xapian_term (mfid)) {
		for (; lst; lst = g_slist_next ((GSList*)lst))
			add_terms_values_str (doc, (const gchar*)lst->data,
					      mfid);
	}
}


struct PartData {
	PartData (Xapian::Document& doc, MuMsgFieldId mfid):
		_doc (doc), _mfid(mfid) {}
	Xapian::Document _doc;
	MuMsgFieldId _mfid;
};

/* index non-body text parts */
static void
maybe_index_text_part (MuMsg *msg, MuMsgPart *part, PartData *pdata)
{
	char *txt, *str;
	Xapian::TermGenerator termgen;

	/* only deal with attachments/messages; inlines are indexed as
	 * body parts */
	if (!(part->part_type & MU_MSG_PART_TYPE_ATTACHMENT) &&
	    !(part->part_type & MU_MSG_PART_TYPE_MESSAGE))
		return;

	txt = mu_msg_part_get_text (msg, part, MU_MSG_OPTION_NONE);
	if (!txt)
		return;

	termgen.set_document(pdata->_doc);

	str = mu_str_process_text (txt);

	termgen.index_text_without_positions
		(str, 1, prefix(MU_MSG_FIELD_ID_EMBEDDED_TEXT));

	g_free (txt);
	g_free (str);
}


static void
each_part (MuMsg *msg, MuMsgPart *part, PartData *pdata)
{
	char *fname;
	static const std::string
		file (prefix(MU_MSG_FIELD_ID_FILE)),
		mime (prefix(MU_MSG_FIELD_ID_MIME));

	/* save the mime type of any part */
	if (part->type) {
		/* note, we use '_' instead of '/' to separate
		 * type/subtype -- Xapian doesn't treat '/' as
		 * desired, so we use '_' and pre-process queries; see
		 * mu_query_preprocess */
		char ctype[MuStore::MAX_TERM_LENGTH + 1];
		snprintf (ctype, sizeof(ctype), "%s_%s",
			  part->type, part->subtype);

		pdata->_doc.add_term
			(mime + std::string(ctype, 0, MuStore::MAX_TERM_LENGTH));
	}

	if ((fname = mu_msg_part_get_filename (part, FALSE))) {
		char *str;
		str = mu_str_process_term (fname);
		g_free (fname);
		pdata->_doc.add_term
			(file + std::string(str, 0, MuStore::MAX_TERM_LENGTH));
		g_free (str);
	}

	maybe_index_text_part (msg, part, pdata);
}


static void
add_terms_values_attach (Xapian::Document& doc, MuMsg *msg,
			 MuMsgFieldId mfid)
{
	PartData pdata (doc, mfid);
	mu_msg_part_foreach (msg, MU_MSG_OPTION_RECURSE_RFC822,
			     (MuMsgPartForeachFunc)each_part, &pdata);
}


static void
add_terms_values_body (Xapian::Document& doc, MuMsg *msg,
		       MuMsgFieldId mfid)
{
	const char *str;
	char *flat;

	if (mu_msg_get_flags(msg) & MU_FLAG_ENCRYPTED)
		return; /* ignore encrypted bodies */

	str = mu_msg_get_body_text (msg, MU_MSG_OPTION_NONE);
	if (!str) /* FIXME: html->txt fallback needed */
		str = mu_msg_get_body_html (msg, MU_MSG_OPTION_NONE);
	if (!str)
		return; /* no body... */

	Xapian::TermGenerator termgen;
	termgen.set_document(doc);

	flat = mu_str_process_text (str);

	// g_print ("\n--\n%s\n--\n", flat);
	termgen.index_text_without_positions (flat, 1, prefix(mfid));
	g_free (flat);
}

struct _MsgDoc {
	Xapian::Document	*_doc;
	MuMsg			*_msg;
	MuStore                 *_store;

	/* callback data, to determine whether this message is 'personal' */
	gboolean                _personal;
	GSList                 *_my_addresses;
};
typedef struct _MsgDoc		 MsgDoc;


static void
add_terms_values_default (MuMsgFieldId mfid, MsgDoc *msgdoc)
{
	if (mu_msg_field_is_numeric (mfid))
		add_terms_values_number
			(*msgdoc->_doc, msgdoc->_msg, mfid);
	else if (mu_msg_field_is_string (mfid))
		add_terms_values_string
			(*msgdoc->_doc, msgdoc->_msg, mfid);
	else if (mu_msg_field_is_string_list(mfid))
		add_terms_values_string_list
			(*msgdoc->_doc, msgdoc->_msg, mfid);
	else
		g_return_if_reached ();

}

static void
add_terms_values (MuMsgFieldId mfid, MsgDoc* msgdoc)
{
	/* note: contact-stuff (To/Cc/From) will handled in
	 * each_contact_info, not here */
	if (!mu_msg_field_xapian_index(mfid) &&
	    !mu_msg_field_xapian_term(mfid) &&
	    !mu_msg_field_xapian_value(mfid))
		return;

	// if (mu_msg_field_xapian_contact (mfid))
	// 	return; /* handled in new_doc_from_message */

	switch (mfid) {
	case MU_MSG_FIELD_ID_DATE:
		add_terms_values_date (*msgdoc->_doc, msgdoc->_msg, mfid);
		break;
	case MU_MSG_FIELD_ID_BODY_TEXT:
		add_terms_values_body (*msgdoc->_doc, msgdoc->_msg, mfid);
		break;
	/* note: add_terms_values_attach handles _FILE, _MIME and
	 * _ATTACH_TEXT msgfields */
	case MU_MSG_FIELD_ID_FILE:
		add_terms_values_attach (*msgdoc->_doc, msgdoc->_msg, mfid);
		break;
	case MU_MSG_FIELD_ID_MIME:
	case MU_MSG_FIELD_ID_EMBEDDED_TEXT:
		break;

	case MU_MSG_FIELD_ID_MSGID:
		add_terms_values_msgid (*msgdoc->_doc, msgdoc->_msg);
		break;

	case MU_MSG_FIELD_ID_THREAD_ID:
	case MU_MSG_FIELD_ID_UID:
		break; /* already taken care of elsewhere */
	default:
		return add_terms_values_default (mfid, msgdoc);
	}
}


static const std::string&
xapian_pfx (MuMsgContact *contact)
{
	static const std::string empty;

	/* use ptr to string to prevent copy... */
	switch (contact->type) {
	case MU_MSG_CONTACT_TYPE_TO:
		return prefix(MU_MSG_FIELD_ID_TO);
	case MU_MSG_CONTACT_TYPE_FROM:
		return prefix(MU_MSG_FIELD_ID_FROM);
	case MU_MSG_CONTACT_TYPE_CC:
		return prefix(MU_MSG_FIELD_ID_CC);
	case MU_MSG_CONTACT_TYPE_BCC:
		return prefix(MU_MSG_FIELD_ID_BCC);
	default:
		g_warning ("unsupported contact type %u",
			   (unsigned)contact->type);
		return empty;
	}
}


static void
add_address_subfields (Xapian::Document& doc, const char *addr,
		       const std::string& pfx)
{
	const char *at, *domain_part;
	char *name_part, *f1, *f2;

	/* add "foo" and "bar.com" as terms as well for
	 * "foo@bar.com" */
	if (G_UNLIKELY(!(at = (g_strstr_len (addr, -1, "@")))))
		return;

	name_part   = g_strndup(addr, at - addr); // foo
	domain_part = at + 1;

	f1 = mu_str_process_term (name_part);
	f2 = mu_str_process_term (domain_part);

	g_free (name_part);

	doc.add_term (pfx + std::string(f1, 0, _MuStore::MAX_TERM_LENGTH));
	doc.add_term (pfx + std::string(f2, 0, _MuStore::MAX_TERM_LENGTH));

	g_free (f1);
	g_free (f2);
}

static void
each_contact_info (MuMsgContact *contact, MsgDoc *msgdoc)
{
	/* for now, don't store reply-to addresses */
	if (mu_msg_contact_type (contact) == MU_MSG_CONTACT_TYPE_REPLY_TO)
		return;

	const std::string pfx (xapian_pfx(contact));
	if (pfx.empty())
		return; /* unsupported contact type */

	if (!mu_str_is_empty(contact->name)) {
		Xapian::TermGenerator termgen;
		termgen.set_document (*msgdoc->_doc);
		char *flat = mu_str_process_text (contact->name);
		termgen.index_text_without_positions (flat, 1, pfx);
		g_free (flat);
	}

	if (!mu_str_is_empty(contact->address)) {
		char *flat;
		flat = mu_str_process_term (contact->address);
		msgdoc->_doc->add_term
			(std::string (pfx + flat, 0, MuStore::MAX_TERM_LENGTH));
		g_free (flat);
		add_address_subfields (*msgdoc->_doc, contact->address, pfx);

		/* store it also in our contacts cache */
		if (msgdoc->_store->contacts())
			mu_contacts_add (msgdoc->_store->contacts(),
					 contact->address, contact->name,
					 msgdoc->_personal,
					 mu_msg_get_date(msgdoc->_msg));
	}
}


static void
each_contact_check_if_personal (MuMsgContact *contact, MsgDoc *msgdoc)
{
	GSList *cur;

	if (msgdoc->_personal || !contact->address)
		return;

	for (cur = msgdoc->_my_addresses; cur; cur = g_slist_next (cur)) {
		if (g_ascii_strcasecmp (contact->address,
					(const char*)cur->data) == 0)
			msgdoc->_personal = TRUE;
	}
}

Xapian::Document
new_doc_from_message (MuStore *store, MuMsg *msg)
{
	Xapian::Document doc;
	MsgDoc docinfo = {&doc, msg, store, 0, FALSE};

	mu_msg_field_foreach ((MuMsgFieldForeachFunc)add_terms_values, &docinfo);

	/* determine whether this is 'personal' email, ie. one of my
	 * e-mail addresses is explicitly mentioned -- it's not a
	 * mailing list message. Callback will update docinfo->_personal */
	if (store->my_addresses()) {
		docinfo._my_addresses = store->my_addresses();
		mu_msg_contact_foreach
			(msg,
			 (MuMsgContactForeachFunc)each_contact_check_if_personal,
			 &docinfo);
	}

	/* also store the contact-info as separate terms, and add it
	 * to the cache */
	mu_msg_contact_foreach (msg, (MuMsgContactForeachFunc)each_contact_info,
				&docinfo);

	// g_printerr ("\n--%s\n--\n", doc.serialise().c_str());

	return doc;
}

static void
update_threading_info (Xapian::WritableDatabase* db,
		       MuMsg *msg, Xapian::Document& doc)
{
	const GSList *refs;

	// refs contains a list of parent messages, with the oldest
	// one first until the last one, which is the direct parent of
	// the current message. of course, it may be empty.
	//
	// NOTE: there may be cases where the the list is truncated;
	// we happily ignore that case.
	refs  = mu_msg_get_references (msg);

	std::string thread_id;
	if (refs)
		thread_id = mu_util_get_hash ((const char*)refs->data);
	else
		thread_id = mu_util_get_hash (mu_msg_get_msgid (msg));

	doc.add_term (prefix(MU_MSG_FIELD_ID_THREAD_ID) + thread_id);
	doc.add_value((Xapian::valueno)MU_MSG_FIELD_ID_THREAD_ID, thread_id);
}


unsigned
add_or_update_msg (MuStore *store, unsigned docid, MuMsg *msg, GError **err)
{
	g_return_val_if_fail (store, MU_STORE_INVALID_DOCID);
	g_return_val_if_fail (msg, MU_STORE_INVALID_DOCID);

	try {
		Xapian::docid id;
		Xapian::Document doc (new_doc_from_message(store, msg));
		const std::string term (store->get_uid_term (mu_msg_get_path(msg)));

		if (!store->in_transaction())
			store->begin_transaction();

		doc.add_term (term);

		// update the threading info if this message has a message id
		if (mu_msg_get_msgid (msg))
			update_threading_info (store->db_writable(), msg, doc);

		if (docid == 0)
			id = store->db_writable()->replace_document (term, doc);
		else {
			store->db_writable()->replace_document (docid, doc);
			id = docid;
		}

		if (store->inc_processed() % store->batch_size() == 0)
			store->commit_transaction();

		return id;

	} MU_XAPIAN_CATCH_BLOCK_G_ERROR (err, MU_ERROR_XAPIAN_STORE_FAILED);

	if (store->in_transaction())
		store->rollback_transaction();

	return MU_STORE_INVALID_DOCID;
}

unsigned
mu_store_add_msg (MuStore *store, MuMsg *msg, GError **err)
{
	g_return_val_if_fail (store, MU_STORE_INVALID_DOCID);
	g_return_val_if_fail (msg, MU_STORE_INVALID_DOCID);

	return add_or_update_msg (store, 0, msg, err);
}

unsigned
mu_store_update_msg (MuStore *store, unsigned docid, MuMsg *msg, GError **err)
{
	g_return_val_if_fail (store, MU_STORE_INVALID_DOCID);
	g_return_val_if_fail (msg, MU_STORE_INVALID_DOCID);
	g_return_val_if_fail (docid != 0, MU_STORE_INVALID_DOCID);

	return add_or_update_msg (store, docid, msg, err);
}

unsigned
mu_store_add_path (MuStore *store, const char *path, const char *maildir,
		   GError **err)
{
	MuMsg *msg;
	unsigned docid;

	g_return_val_if_fail (store, FALSE);
	g_return_val_if_fail (path, FALSE);

	msg = mu_msg_new_from_file (path, maildir, err);
	if (!msg)
		return MU_STORE_INVALID_DOCID;

	docid = add_or_update_msg (store, 0, msg, err);
	mu_msg_unref (msg);

	return docid;
}


XapianWritableDatabase*
mu_store_get_writable_database (MuStore *store)
{
	g_return_val_if_fail (store, NULL);

	return (XapianWritableDatabase*)store->db_writable();
}


gboolean
mu_store_remove_path (MuStore *store, const char *msgpath)
{
	g_return_val_if_fail (store, FALSE);
	g_return_val_if_fail (msgpath, FALSE);

	try {
		const std::string term
			(store->get_uid_term(msgpath));

		store->db_writable()->delete_document (term);
		store->inc_processed();

		return TRUE;

	} MU_XAPIAN_CATCH_BLOCK_RETURN (FALSE);
}


gboolean
mu_store_set_timestamp (MuStore *store, const char* msgpath,
			time_t stamp, GError **err)
{
	char buf[21];

	g_return_val_if_fail (store, FALSE);
	g_return_val_if_fail (msgpath, FALSE);

	sprintf (buf, "%" G_GUINT64_FORMAT, (guint64)stamp);
	return mu_store_set_metadata (store, msgpath, buf, err);
}
