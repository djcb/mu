/*
** Copyright (C) 2008-2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include <cstdio>
#include <xapian.h>
#include <cstring>

#include "mu-msg.h"
#include "mu-msg-contact.h"
#include "mu-store.h"
#include "mu-util.h"

/* number of new messages after which we commit to the database */
#define MU_STORE_TRX_SIZE 6666

/* http://article.gmane.org/gmane.comp.search.xapian.general/3656 */
#define MU_STORE_MAX_TERM_LENGTH 240

struct _MuStore {
	Xapian::WritableDatabase *_db;

	/* transaction handling */
	bool   _in_transaction;
	int    _processed;
	size_t _trx_size;
};

MuStore*
mu_store_new  (const char* xpath)
{
	MuStore *store (0);
	
	g_return_val_if_fail (xpath, NULL);
	
	try {
		store = g_new0(MuStore,1);
		store->_db = new Xapian::WritableDatabase 
			(xpath, Xapian::DB_CREATE_OR_OPEN);
		
		/* keep count of processed docs */
		store->_trx_size = MU_STORE_TRX_SIZE; 
		store->_in_transaction = false;
		store->_processed = 0;
		
		MU_WRITE_LOG ("%s: opened %s", __FUNCTION__, xpath);
		return store;

	} MU_XAPIAN_CATCH_BLOCK;		

	try { delete store->_db; } MU_XAPIAN_CATCH_BLOCK;
	
	g_free (store);
	return NULL;
}



char*
mu_store_version (MuStore *store)
{
	g_return_val_if_fail (store, NULL);

	try {
		const std::string version (
			store->_db->get_metadata (MU_XAPIAN_VERSION_KEY));
		
		return version.empty() ? NULL : g_strdup (version.c_str());
		
	} MU_XAPIAN_CATCH_BLOCK;

	return NULL;
}

gboolean
mu_store_set_version (MuStore *store, const char* version)
{
	g_return_val_if_fail (store, FALSE);
	g_return_val_if_fail (version, FALSE);
	
	try {
		store->_db->set_metadata (MU_XAPIAN_VERSION_KEY, version);
		return TRUE;
		
	} MU_XAPIAN_CATCH_BLOCK;

	return FALSE;
}


static void
begin_trx_if (MuStore *store, gboolean cond)
{
	if (cond) {
		g_debug ("beginning Xapian transaction");
		store->_db->begin_transaction();
		store->_in_transaction = true;
	}
}

static void
commit_trx_if (MuStore *store, gboolean cond)
{
	if (cond) {
		g_debug ("comitting Xapian transaction");
		store->_in_transaction = false;
		store->_db->commit_transaction();
	}
}

static void
rollback_trx_if (MuStore *store, gboolean cond)
{
	if (cond) {
		g_debug ("rolling back Xapian transaction");
		store->_in_transaction = false;
		store->_db->cancel_transaction();
	}
}


void
mu_store_destroy (MuStore *store)
{
	if (!store)
		return;

	try {
		mu_store_flush (store);
		
		MU_WRITE_LOG ("closing xapian database with %d documents",
			      (int)store->_db->get_doccount());

		delete store->_db;
		g_free (store);

	} MU_XAPIAN_CATCH_BLOCK;
}

void
mu_store_flush (MuStore *store)
{
	g_return_if_fail (store);
	
	try {
		commit_trx_if (store, store->_in_transaction);
		store->_db->flush ();

	} MU_XAPIAN_CATCH_BLOCK;
}


static void
add_terms_values_number (Xapian::Document& doc, MuMsg *msg, 
			 const MuMsgField* field)
{
	const std::string pfx (mu_msg_field_xapian_prefix(field), 1);
	gint64 num = mu_msg_get_field_numeric (msg, field);
	const std::string numstr (Xapian::sortable_serialise((double)num));
	
	doc.add_value ((Xapian::valueno)mu_msg_field_id(field), numstr);
	doc.add_term  (pfx + numstr);
}

static void
add_terms_values_string (Xapian::Document& doc, MuMsg *msg,
			 const MuMsgField* field)
{
	const char* str;
	
	str = mu_msg_get_field_string (msg, field);
	if (!str)
		return;

	const std::string value  (str);
	const std::string prefix (mu_msg_field_xapian_prefix(field));
	
	if (mu_msg_field_xapian_index (field)) {
		Xapian::TermGenerator termgen;
		termgen.set_document (doc);
		termgen.index_text_without_positions (str, 1, prefix);
	}

	if (mu_msg_field_xapian_term(field))
		/* terms can be up to MU_STORE_MAX_TERM_LENGTH (240)
		 * long; this is a Xapian limit */
		doc.add_term (std::string (prefix + value, 0,
		 			   MU_STORE_MAX_TERM_LENGTH));

	if (mu_msg_field_xapian_value(field)) 			 
		doc.add_value ((Xapian::valueno)mu_msg_field_id (field),
			       value);
}

static void
add_terms_values_body (Xapian::Document& doc, MuMsg *msg,
		       const MuMsgField* field)
{
	const char *str;
	
	if (mu_msg_get_flags(msg) & MU_MSG_FLAG_ENCRYPTED)
		return; /* don't store encrypted bodies */

	str = mu_msg_get_body_text (msg);
	if (!str) /* FIXME: html->txt fallback needed */
		str = mu_msg_get_body_html (msg);
	
	if (!str)  
		return; /* no body... */

	Xapian::TermGenerator termgen;
	termgen.set_document(doc);
	termgen.index_text(str, 1, mu_msg_field_xapian_prefix(field));
}

struct _MsgDoc {
	Xapian::Document	*_doc;
	MuMsg			*_msg;
};
typedef struct _MsgDoc		 MsgDoc;

static void
add_terms_values (const MuMsgField* field, MsgDoc* msgdoc)
{
	MuMsgFieldType type;

	/* note: contact-stuff (To/Cc/From) will handled in
	 * add_contact_info, not here */
	if (!mu_msg_field_xapian_index(field) &&
	    !mu_msg_field_xapian_term(field) &&
	    !mu_msg_field_xapian_value(field))
		return;
	
	type = mu_msg_field_type (field);

	if (type == MU_MSG_FIELD_TYPE_STRING) {		
		if (mu_msg_field_id (field) == MU_MSG_FIELD_ID_BODY_TEXT) 
			add_terms_values_body (*msgdoc->_doc, msgdoc->_msg, 
 					       field);
		else
			add_terms_values_string (*msgdoc->_doc, msgdoc->_msg,
						 field);
		return;
	}

	if (type == MU_MSG_FIELD_TYPE_BYTESIZE ||
	    type == MU_MSG_FIELD_TYPE_TIME_T ||
	    type == MU_MSG_FIELD_TYPE_INT) {
		add_terms_values_number (*msgdoc->_doc, msgdoc->_msg, field);
		return;
	}
	
	g_return_if_reached ();
}


static void
each_contact_info (MuMsgContact *contact, MsgDoc *data)
{
	std::string pfx;
	
	static const MuMsgField *to_field =
		mu_msg_field_from_id (MU_MSG_FIELD_ID_TO);
	static const MuMsgField *from_field =
		mu_msg_field_from_id (MU_MSG_FIELD_ID_FROM);
	static const MuMsgField *cc_field =
		mu_msg_field_from_id (MU_MSG_FIELD_ID_CC);

	static const std::string to_pfx (mu_msg_field_xapian_prefix(to_field));
	static const std::string from_pfx (mu_msg_field_xapian_prefix(from_field));
	static const std::string cc_pfx (mu_msg_field_xapian_prefix(cc_field));
	
	switch (contact->type) {
	case MU_MSG_CONTACT_TYPE_TO: pfx   = to_pfx; break;
	case MU_MSG_CONTACT_TYPE_FROM: pfx = from_pfx; break;
	case MU_MSG_CONTACT_TYPE_CC: pfx   = cc_pfx; break;
	default: return;		/* other types (like bcc) are ignored */
	}

	// g_print ("[%s %s]\n", pfx.c_str(), contact->address);
	
	if (contact->name && strlen(contact->name) > 0) {
		Xapian::TermGenerator termgen;
		termgen.set_document (*data->_doc);
		termgen.index_text_without_positions (contact->name, 1, pfx);
	}

	if (contact->address && strlen (contact->address)) 
		data->_doc->add_term (std::string (pfx + contact->address, 0,
						   MU_STORE_MAX_TERM_LENGTH));	
}



/* get a unique id for this message */
static std::string
get_message_uid (const char* path)
{
	static const MuMsgField* pathfield =
		mu_msg_field_from_id(MU_MSG_FIELD_ID_PATH);
	static const std::string pathprefix 
		(mu_msg_field_xapian_prefix(pathfield));

	return pathprefix + path;
}

static const std::string
get_message_uid (MuMsg *msg)
{
	return get_message_uid (mu_msg_get_path(msg));
}



MuResult
mu_store_store (MuStore *store, MuMsg *msg)
{	
	g_return_val_if_fail (store, MU_ERROR);
	g_return_val_if_fail (msg, MU_ERROR);

	try {
		Xapian::Document newdoc;
		Xapian::docid id;
		MsgDoc msgdoc = { &newdoc, msg };
		const std::string uid(get_message_uid(msg));

		begin_trx_if (store, !store->_in_transaction);
		/* we must add a unique term, so we can replace
		 * matching documents */
		newdoc.add_term (uid);
		mu_msg_field_foreach ((MuMsgFieldForEachFunc)add_terms_values,
				      &msgdoc);
		/* also store the contact-info as separate terms */
		mu_msg_contact_foreach (msg,
					(MuMsgContactForeachFunc)each_contact_info,
					&msgdoc);
			
		/* we replace all existing documents for this file */
		id = store->_db->replace_document (uid, newdoc);
		++store->_processed;
		commit_trx_if (store,
			       store->_processed % store->_trx_size == 0);

		return MU_OK;

	} MU_XAPIAN_CATCH_BLOCK;

	rollback_trx_if (store, store->_in_transaction);

	return MU_ERROR;
}


MuResult
mu_store_remove (MuStore *store, const char* msgpath)
{
	g_return_val_if_fail (store, MU_ERROR);
	g_return_val_if_fail (msgpath, MU_ERROR);

	try {
		const std::string uid (get_message_uid (msgpath));

		begin_trx_if (store, !store->_in_transaction);
		
		store->_db->delete_document (uid);
		g_debug ("deleting %s", msgpath);
		
		++store->_processed;

		/* do we need to commit now? */
		bool commit_now = store->_processed % store->_trx_size == 0;
		commit_trx_if (store, commit_now);

		return MU_OK; 
		
	} MU_XAPIAN_CATCH_BLOCK_RETURN (MU_ERROR);
	
}

gboolean
mu_store_contains_message (MuStore *store, const char* path)
{
	g_return_val_if_fail (store, NULL);
	g_return_val_if_fail (path, NULL);
	
	try {
		const std::string uid (get_message_uid(path));
		return store->_db->term_exists (uid) ? TRUE: FALSE;
		
	} MU_XAPIAN_CATCH_BLOCK_RETURN (FALSE);
}



time_t
mu_store_get_timestamp (MuStore *store, const char* msgpath)
{
	g_return_val_if_fail (store, 0);
	g_return_val_if_fail (msgpath, 0);
	
	try {
		const std::string stamp (store->_db->get_metadata (msgpath));
		if (stamp.empty())
			return 0;
	
		return (time_t) g_ascii_strtoull (stamp.c_str(), NULL, 10);
		
	} MU_XAPIAN_CATCH_BLOCK_RETURN (0);

	return 0;
}


void
mu_store_set_timestamp (MuStore *store, const char* msgpath, 
			time_t stamp)
{
	g_return_if_fail (store);
	g_return_if_fail (msgpath);

	try {
		char buf[24];
		sprintf (buf, "%" G_GUINT64_FORMAT, (guint64)stamp);
		store->_db->set_metadata (msgpath, buf);
				
	} MU_XAPIAN_CATCH_BLOCK;
}


MuResult
mu_store_foreach (MuStore *self, 
		  MuStoreForeachFunc func, void *user_data)  
{
	g_return_val_if_fail (self, MU_ERROR);
	g_return_val_if_fail (func, MU_ERROR);
	
	try {
		Xapian::Enquire enq (*self->_db);
		enq.set_query  (Xapian::Query::MatchAll);
		enq.set_cutoff (0,0);
		
		Xapian::MSet matches
			(enq.get_mset (0, self->_db->get_doccount()));
		if (matches.empty())
			return MU_OK; /* database is empty */
		
		for (Xapian::MSet::iterator iter = matches.begin();
		     iter != matches.end(); ++iter) {
			Xapian::Document doc (iter.get_document());
			const std::string path(
				doc.get_value(MU_MSG_FIELD_ID_PATH));

			MuResult res = func (path.c_str(), user_data);
			if (res != MU_OK)
				return res;
		}
		
	} MU_XAPIAN_CATCH_BLOCK_RETURN (MU_ERROR);

	return MU_OK;
}


