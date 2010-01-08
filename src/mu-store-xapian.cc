/*
** Copyright (C) 2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <xapian.h>
#include <string.h>
#include <stdio.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include "mu-msg.h"
#include "mu-msg-gmime.h"
#include "mu-store-xapian.h"
#include "mu-util.h"

/* number of new messages after which we commit to the database */
#define MU_STORE_XAPIAN_TRX_SIZE 2000

struct _MuStoreXapian {
	Xapian::WritableDatabase *_db;

	/* transaction handling */
	bool   _in_transaction;
	int    _processed;
	size_t _trx_size;
};

MuStoreXapian*
mu_store_xapian_new  (const char* xpath)
{
	MuStoreXapian *store;
	
	g_return_val_if_fail (xpath, NULL);
	
	try {
		store = g_new0(MuStoreXapian,1);
		store->_db = new Xapian::WritableDatabase
			(xpath, Xapian::DB_CREATE_OR_OPEN);
		
		/* keep count of processed docs */
		store->_trx_size = MU_STORE_XAPIAN_TRX_SIZE; 
		store->_in_transaction = false;
		store->_processed = 0;
		
		MU_WRITE_LOG ("%s: opened %s", __FUNCTION__, xpath);
		return store;

	} MU_XAPIAN_CATCH_BLOCK;		

	try {
		delete store->_db;
	} MU_XAPIAN_CATCH_BLOCK;
	
	g_free (store);
	return NULL;
}


static void
_begin_transaction_if (MuStoreXapian *store, gboolean cond)
{
	if (cond) {
		g_debug ("beginning Xapian transaction");
		store->_db->begin_transaction();
		store->_in_transaction = true;
	}
}

static void
_commit_transaction_if (MuStoreXapian *store, gboolean cond)
{
	if (cond) {
		g_debug ("comitting Xapian transaction");
		store->_in_transaction = false;
		store->_db->commit_transaction();
	}
}

static void
_rollback_transaction_if (MuStoreXapian *store, gboolean cond)
{
	if (cond) {
		g_debug ("rolling back Xapian transaction");
		store->_in_transaction = false;
		store->_db->cancel_transaction();
	}
}


void
mu_store_xapian_destroy (MuStoreXapian *store)
{
	if (!store)
		return;

	try {
		mu_store_xapian_flush (store);
		
		MU_WRITE_LOG ("closing xapian database with %d documents",
			(int)store->_db->get_doccount());

		delete store->_db;
		g_free (store);

	} MU_XAPIAN_CATCH_BLOCK;
}

void
mu_store_xapian_flush (MuStoreXapian *store)
{
	g_return_if_fail (store);
	
	try {
		_commit_transaction_if (store, store->_in_transaction);
		store->_db->flush ();

	} MU_XAPIAN_CATCH_BLOCK;
	
}


static void
add_terms_values_number (Xapian::Document& doc, MuMsgGMime *msg, 
			 const MuMsgField* field)
{
	const std::string pfx (mu_msg_field_xapian_prefix(field), 1);
	gint64 num = mu_msg_gmime_get_field_numeric (msg, field);
	const std::string numstr (Xapian::sortable_serialise((double)num));
	
	doc.add_value ((Xapian::valueno)mu_msg_field_id(field), numstr);	
	doc.add_term  (pfx + numstr);
}


static void
add_terms_values_string (Xapian::Document& doc, MuMsgGMime *msg,
			 const MuMsgField* field)
{
	const char* str;

	str = mu_msg_gmime_get_field_string (msg, field);
	if (!str)
		return;
	
	const std::string value (str);
	const std::string prefix (mu_msg_field_xapian_prefix(field));
	
	if (mu_msg_field_is_xapian_indexable (field)) {
		Xapian::TermGenerator termgen;
		termgen.set_document (doc);
		termgen.index_text_without_positions (value, 1, prefix);
	} else
		doc.add_term(prefix + value);
	
	doc.add_value ((Xapian::valueno)mu_msg_field_id(field),
		       value);
}

static void
add_terms_values_body (Xapian::Document& doc, MuMsgGMime *msg,
		       const MuMsgField* field)
{
	const char *str;
	
	if (mu_msg_gmime_get_flags(msg) & MU_MSG_FLAG_ENCRYPTED)
		return; /* don't store encrypted bodies */

	str = mu_msg_gmime_get_body_text(msg);
	if (!str) /* FIXME: html->html fallback */
		str = mu_msg_gmime_get_body_html (msg);
	
	if (!str)  
		return; /* no body... */

	Xapian::TermGenerator termgen;
	termgen.set_document(doc);
	termgen.index_text(str, 1, mu_msg_field_xapian_prefix(field));
}

struct _MsgDoc {
	Xapian::Document *_doc;
	MuMsgGMime       *_msg;
};
typedef struct _MsgDoc MsgDoc;

static void
add_terms_values (const MuMsgField* field, MsgDoc* msgdoc)
{
	MuMsgFieldType type;
	
	if (!mu_msg_field_is_xapian_enabled(field))
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

/* get a unique id for this message */
static std::string
_get_message_uid (const char* path)
{
	static const MuMsgField* pathfield =
		mu_msg_field_from_id(MU_MSG_FIELD_ID_PATH);
	static const std::string pathprefix 
		(mu_msg_field_xapian_prefix(pathfield));

	return pathprefix + path;
}

static std::string
_get_message_uid (MuMsgGMime *msg)
{
	return _get_message_uid (mu_msg_gmime_get_path(msg));
}



MuResult
mu_store_xapian_store (MuStoreXapian *store, MuMsgGMime *msg)
{	
	g_return_val_if_fail (store, MU_ERROR);
	g_return_val_if_fail (msg, MU_ERROR);

	try {
		Xapian::Document newdoc;
		Xapian::docid id;
		MsgDoc msgdoc = { &newdoc, msg };
		const std::string uid(_get_message_uid(msg));

		_begin_transaction_if (store, !store->_in_transaction);
		/* we must add a unique term, so we can replace matching
		 * documents */
		newdoc.add_term (uid);
		mu_msg_field_foreach ((MuMsgFieldForEachFunc)add_terms_values,
				      &msgdoc);
			
		/* we replace all existing documents for this file */
		id = store->_db->replace_document (uid, newdoc);
		++store->_processed;
		_commit_transaction_if (store,
					store->_processed % store->_trx_size == 0);

		return MU_OK;

	} MU_XAPIAN_CATCH_BLOCK;

	_rollback_transaction_if (store, store->_in_transaction);

	return MU_ERROR;
}


MuResult
mu_store_xapian_remove (MuStoreXapian *store, const char* msgpath)
{
	g_return_val_if_fail (store, MU_ERROR);
	g_return_val_if_fail (msgpath, MU_ERROR);

	try {
		const std::string uid (_get_message_uid (msgpath));

		_begin_transaction_if (store, !store->_in_transaction);
		
		store->_db->delete_document (uid);
		g_debug ("deleting %s", msgpath);
		
		++store->_processed;
		_commit_transaction_if (store,
					store->_processed % store->_trx_size == 0);

		return MU_OK; 
		
	} MU_XAPIAN_CATCH_BLOCK_RETURN (MU_ERROR);
	
}

time_t
mu_store_xapian_get_timestamp (MuStoreXapian *store, const char* msgpath)
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
mu_store_xapian_set_timestamp (MuStoreXapian *store, const char* msgpath, 
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
mu_store_xapian_foreach (MuStoreXapian *self, 
			 MuStoreXapianForeachFunc func, void *user_data)  
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


