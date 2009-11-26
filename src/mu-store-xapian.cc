/*
** Copyright (C) 2008 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

struct _MuStoreXapian {
	Xapian::WritableDatabase *_db;

	/* transaction handling */
	bool   _in_transaction;
	int    _processed;
	size_t _transaction_size;
};

MuStoreXapian*
mu_store_xapian_new  (const char* path)
{
	MuStoreXapian *store;
	
	g_return_val_if_fail (path, NULL);
	
	try {
		store = g_new0(MuStoreXapian,1);
		store->_db = new Xapian::WritableDatabase
			(path,Xapian::DB_CREATE_OR_OPEN);

		/* keep count of processed docs */
		store->_transaction_size = 1000; /* default */
		store->_in_transaction = false;
		store->_processed = 0;

		g_message ("%s: opened %s", __FUNCTION__, path);
		return store;

	} catch (const Xapian::Error &err) {

		delete store->_db;
		g_free (store);
		g_warning ("%s: caught xapian exception '%s' (%s)",
			   __FUNCTION__, err.get_msg().c_str(),
			   err.get_error_string());
		return NULL;

	} catch (...) {
		delete store->_db;
		g_free (store);
		g_warning ("%s: caught exception", __FUNCTION__);
		return NULL;
	}
}



void
mu_store_xapian_destroy (MuStoreXapian *store)
{
	if (!store)
		return;

	try { 	
		if (store->_in_transaction) {
			store->_in_transaction = false;
			store->_db->commit_transaction();
		}
		store->_db->flush();

		g_message ("closing xapian database with %d documents",
			   (int)store->_db->get_doccount());

		delete store->_db;
		g_free (store);

	} catch (const Xapian::Error &err) {
		g_free (store);
		g_warning ("%s: caught xapian exception '%s' (%s)", 
			   __FUNCTION__, err.get_msg().c_str(), 
			   err.get_error_string());
	} catch (...) {
		g_free (store);
		g_warning ("%s: caught exception", __FUNCTION__);
	}
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
	const char* str = mu_msg_gmime_get_field_string (msg, field);
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
	
	doc.add_value ((Xapian::valueno)mu_msg_field_id(field), value);
}

static void
add_terms_values_body (Xapian::Document& doc, MuMsgGMime *msg,
		       const MuMsgField* field)
{	
	if (mu_msg_gmime_get_flags(msg) & MU_MSG_FLAG_ENCRYPTED)
		return; /* don't store encrypted bodies */

	const char *str = mu_msg_gmime_get_body_text(msg);
	if (!str) /* FIXME: html->html fallback */
		str = mu_msg_gmime_get_body_html(msg); 
	if (!str)
		return; /* no body... */
		
	Xapian::TermGenerator termgen;
	termgen.index_text(str, 1, mu_msg_field_xapian_prefix(field));
	termgen.set_document(doc);
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


MuResult
mu_store_xapian_store (MuStoreXapian *store, MuMsgGMime *msg)
{
	static const MuMsgField* pathfield =
		mu_msg_field_from_id(MU_MSG_FIELD_ID_PATH);
	static const std::string pathprefix 
		(mu_msg_field_xapian_prefix(pathfield));
		
	g_return_val_if_fail (store, MU_ERROR);
	g_return_val_if_fail (msg, MU_ERROR);

	try {
		const char* body;
		Xapian::Document newdoc;
		Xapian::docid id;
		gboolean commit_now;
		MsgDoc msgdoc = { &newdoc, msg };

		// start transaction if needed
		if (!store->_in_transaction) {
			store->_db->begin_transaction();
			store->_in_transaction = true;
		}
		
		mu_msg_field_foreach ((MuMsgFieldForEachFunc)add_terms_values,
				      &msgdoc);
			
		/* we replace all existing documents for this file */
		const std::string pathterm (pathprefix +
					    mu_msg_gmime_get_path(msg));
		id = store->_db->replace_document (pathterm, newdoc);
		
		commit_now = ++store->_processed % store->_transaction_size == 0;
		if (commit_now) {
			store->_in_transaction = false;
			store->_db->commit_transaction();
		}

		return MU_OK;

	} catch (const Xapian::Error &err) {
		g_warning ("%s: caught xapian exception '%s' (%s)", 
			   __FUNCTION__, err.get_msg().c_str(), 
			   err.get_error_string());
	} catch (...) {
		g_warning ("%s: caught exception", __FUNCTION__);
	}
	
	if (store->_in_transaction) {
		store->_in_transaction = false;
		store->_db->cancel_transaction();
	}

	return MU_ERROR;
}


MuResult
mu_store_xapian_cleanup (MuStoreXapian *store, const char* msgpath)
{
	g_return_val_if_fail (store, MU_ERROR);
	g_return_val_if_fail (msgpath, MU_ERROR);

	try {
		return MU_OK; /* FIXME: TODO: */
		
	} catch (const Xapian::Error &err) {
		g_warning ("%s: caught xapian exception '%s' (%s)", 
			   __FUNCTION__, err.get_msg().c_str(), 
			   err.get_error_string());
		
		return MU_ERROR;
	} catch (...) {
		g_warning ("%s: caught exception", __FUNCTION__);		
		return MU_ERROR;
	}
	
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
		
	} catch (const Xapian::Error &err) {
		g_warning ("%s: caught xapian exception '%s' (%s)", 
			   __FUNCTION__, err.get_msg().c_str(), 
			   err.get_error_string());
		
		return MU_ERROR;
	} catch (...) {
		g_warning ("%s: caught exception", __FUNCTION__);		
		return MU_ERROR;
	}

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
				
	} catch (const Xapian::Error &err) {
		g_warning ("%s: caught xapian exception '%s' (%s)", 
			   __FUNCTION__, err.get_msg().c_str(), 
			   err.get_error_string());
	} catch (...) {
		g_warning ("%s: caught exception", __FUNCTION__);		
	}
}
