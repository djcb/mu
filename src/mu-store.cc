/* -*-mode: c++; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8-*- */
/*
** Copyright (C) 2008-2011 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-msg.h"
#include "mu-msg-part.h"
#include "mu-store.h"
#include "mu-util.h"
#include "mu-str.h"
#include "mu-date.h"
#include "mu-flags.h"
#include "mu-contacts.h"

/* by default, use transactions of 30000 messages */
#define MU_STORE_DEFAULT_TRX_SIZE 30000

/* http://article.gmane.org/gmane.comp.search.xapian.general/3656 */
#define MU_STORE_MAX_TERM_LENGTH 240

static void add_synonyms (MuStore *store);
static gboolean  check_version (MuStore *store);

struct _MuStore {
	_MuStore (const char *xpath, const char *contacts_cache) :
		_db (xpath, Xapian::DB_CREATE_OR_OPEN), _in_transaction(0),
		_processed (0), _trx_size(MU_STORE_DEFAULT_TRX_SIZE), _contacts (0),
		_version (0) {
	
		if (!check_version (this)) 
			throw std::runtime_error
				("xapian db version check failed");
		
		if (contacts_cache) {
			_contacts = mu_contacts_new (contacts_cache);
			if (!_contacts) /* don't bail-out for this */
				throw std::runtime_error
					("failed to init contacts cache");
		}
		
		add_synonyms (this);
		MU_WRITE_LOG ("%s: opened %s (batch size: %u)",
			      __FUNCTION__, xpath, _trx_size);
	}

	~_MuStore () {
		try {
			g_free (_version);
			mu_contacts_destroy (_contacts);
			mu_store_flush (this);
			
			MU_WRITE_LOG ("closing xapian database with %d documents",
				      (int)_db.get_doccount());

		} MU_XAPIAN_CATCH_BLOCK;
	}
	
	Xapian::WritableDatabase _db;
	
	/* transaction handling */
	bool   _in_transaction;
	int    _processed;
	size_t _trx_size;
	guint  _batchsize;  /* batch size of a xapian transaction */
	
	/* contacts object to cache all the contact information */
	MuContacts *_contacts;
	
	char *_version; 
};


/* we cache these prefix strings, so we don't have to allocate the all
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
	const std::string pfx(prefix(MU_MSG_FIELD_ID_FLAGS));

	db->clear_synonyms (pfx + mu_flag_name (flag));
	db->add_synonym (pfx + mu_flag_name (flag), pfx +
			 (std::string(1, tolower(mu_flag_char (flag)))));
}


static void
add_synonym_for_prio (MuMsgPrio prio, Xapian::WritableDatabase *db)
{
	const std::string pfx (prefix(MU_MSG_FIELD_ID_PRIO));
	
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
			      &store->_db);
	mu_msg_prio_foreach ((MuMsgPrioForeachFunc)add_synonym_for_prio,
			     &store->_db);
}

static gboolean
check_version (MuStore *store)
{
	/* FIXME clear up versioning semantics */
	const gchar *version;
	
	version = mu_store_version (store); 

	/* no version yet? it must be a new db then; we'll set the version */
	if (!version)  {
		if (!mu_store_set_metadata (store, MU_STORE_VERSION_KEY,
					    MU_XAPIAN_DB_VERSION)) {
			g_warning ("failed to set database version");
			return FALSE;
		}
		return TRUE; /* ok, done. */
	}
	
	/* we have a version, but is it the right one? */
	if (std::strcmp (version, MU_XAPIAN_DB_VERSION) != 0) {
		g_warning ("expected db version %s, but got %s",
			   MU_XAPIAN_DB_VERSION,
			   version ? version : "<none>" );
		return FALSE;
	}
	
	return TRUE;
}

MuStore*
mu_store_new (const char* xpath,
	      const char *contacts_cache,
	      GError **err)
{
	g_return_val_if_fail (xpath, NULL);

	try {
		return new _MuStore (xpath, contacts_cache);		

	} MU_XAPIAN_CATCH_BLOCK_G_ERROR(err,MU_ERROR_XAPIAN);		

	return NULL;
}


void
mu_store_destroy (MuStore *store)
{
	try { delete store; } MU_XAPIAN_CATCH_BLOCK;
}


void
mu_store_set_batch_size (MuStore *store, guint batchsize)
{
	g_return_if_fail (store);

	store->_trx_size = batchsize ? batchsize : MU_STORE_DEFAULT_TRX_SIZE;
}



unsigned
mu_store_count (MuStore *store)
{
	g_return_val_if_fail (store, 0);

	try {
		return store->_db.get_doccount();
		
	} MU_XAPIAN_CATCH_BLOCK;

	return 0;
}


const char*
mu_store_version (MuStore *store)
{
	g_return_val_if_fail (store, NULL);
	
	g_free (store->_version);
	return store->_version =
		mu_store_get_metadata (store, MU_STORE_VERSION_KEY);
}

gboolean
mu_store_set_metadata (MuStore *store, const char *key, const char *val)
{
	g_return_val_if_fail (store, FALSE);
	g_return_val_if_fail (key, FALSE);
	g_return_val_if_fail (val, FALSE);
		
	try {
		store->_db.set_metadata (key, val);
		return TRUE;
		
	} MU_XAPIAN_CATCH_BLOCK;

	return FALSE;
}


char*
mu_store_get_metadata (MuStore *store, const char *key)
{
	g_return_val_if_fail (store, NULL);
	g_return_val_if_fail (key, NULL);
		
	try {
		const std::string val (store->_db.get_metadata (key));
		return val.empty() ? NULL : g_strdup (val.c_str());

	} MU_XAPIAN_CATCH_BLOCK;

	return NULL;
}



static void
begin_trx_if (MuStore *store, gboolean cond)
{
	if (cond) {
		g_debug ("beginning Xapian transaction");
		store->_db.begin_transaction();
		store->_in_transaction = true;
	}
}

static void
commit_trx_if (MuStore *store, gboolean cond)
{
	if (cond) {
		g_debug ("comitting Xapian transaction");
		store->_in_transaction = false;
		store->_db.commit_transaction();
	}
}

static void
rollback_trx_if (MuStore *store, gboolean cond)
{
	if (cond) {
		g_debug ("rolling back Xapian transaction");
		store->_in_transaction = false;
		store->_db.cancel_transaction();
	}
}

void
mu_store_flush (MuStore *store)
{
	g_return_if_fail (store);
	
	try {
		commit_trx_if (store, store->_in_transaction);
		store->_db.flush (); /* => commit, post X 1.1.x */

	} MU_XAPIAN_CATCH_BLOCK;
}


static void
add_terms_values_date (Xapian::Document& doc, MuMsg *msg, MuMsgFieldId mfid)
{
	time_t t;
	const char *datestr;
	
	t = (time_t)mu_msg_get_field_numeric (msg, mfid);
	if (t != 0) { 
		datestr = mu_date_time_t_to_str_s (t, FALSE /*UTC*/);
		doc.add_value ((Xapian::valueno)mfid, datestr);
	}
}

/* TODO: we could pre-calculate the add_term values for FLAGS */


/* pre-calculate; optimization */
G_GNUC_CONST static const std::string&
prio_val (MuMsgPrio prio)
{
	static const std::string pfx (prefix(MU_MSG_FIELD_ID_PRIO));	

	static const std::string lowstr
		(pfx + std::string(1, mu_msg_prio_char(MU_MSG_PRIO_LOW)));
	static const std::string normalstr
		(pfx + std::string(1, mu_msg_prio_char(MU_MSG_PRIO_NORMAL)));
	static const std::string highstr
		(pfx + std::string(1, mu_msg_prio_char(MU_MSG_PRIO_HIGH)));


	switch (prio) {
	case MU_MSG_PRIO_LOW:    return lowstr;
	case MU_MSG_PRIO_NORMAL: return normalstr;
	case MU_MSG_PRIO_HIGH:   return highstr;
	default:
		g_return_val_if_reached (normalstr);
		return normalstr;
	}
}



static void
add_terms_values_number (Xapian::Document& doc, MuMsg *msg, MuMsgFieldId mfid)
{
	gint64 num = mu_msg_get_field_numeric (msg, mfid);

	const std::string numstr (Xapian::sortable_serialise((double)num));
	doc.add_value ((Xapian::valueno)mfid, numstr);
	
	if (mfid == MU_MSG_FIELD_ID_FLAGS) {
		for (const char *cur =
			     mu_flags_to_str_s ((MuFlags)num,
						(MuFlagType)MU_FLAG_TYPE_ANY); 
		     cur && *cur; ++cur)
			doc.add_term  (prefix(mfid) + (char)tolower (*cur));

	} else if (mfid == MU_MSG_FIELD_ID_PRIO)
		doc.add_term (prio_val((MuMsgPrio)num));
}


/* for string and string-list */
static void
add_terms_values_str (Xapian::Document& doc, char *val,
		      MuMsgFieldId mfid)
{
	/* the value is what we'll display; the unchanged original */
	if (mu_msg_field_xapian_value(mfid)) 			 
		doc.add_value ((Xapian::valueno)mfid, val);
	
	/* now, let's create some search terms... */
	if (mu_msg_field_normalize (mfid))
		mu_str_normalize_in_place (val, TRUE);
	if (mu_msg_field_xapian_escape (mfid))
		mu_str_ascii_xapian_escape_in_place (val);
	
	if (mu_msg_field_xapian_index (mfid)) {
		Xapian::TermGenerator termgen;
		termgen.set_document (doc);
		termgen.index_text_without_positions (val, 1, prefix(mfid));
	}
	
	if (mu_msg_field_xapian_term(mfid))
		doc.add_term (prefix(mfid) +
			      std::string(val, 0, MU_STORE_MAX_TERM_LENGTH));
}
	

static void
add_terms_values_string (Xapian::Document& doc, MuMsg *msg,
			 MuMsgFieldId mfid)
{
	const char *orig;
	char *val;
	size_t len;
	
	if (!(orig = mu_msg_get_field_string (msg, mfid)))
		return; /* nothing to do */

	/* try stack-allocation, it's much faster*/
	len = strlen (orig);
	val = (char*)(G_LIKELY(len < 1024)?g_alloca(len+1):g_malloc(len+1));
	strcpy (val, orig);

	add_terms_values_str (doc, val, mfid);

	if (!(G_LIKELY(len < 1024)))
		g_free (val);
}



static void
add_terms_values_string_list  (Xapian::Document& doc, MuMsg *msg,
			       MuMsgFieldId mfid)
{
	const GSList *lst;

	lst = mu_msg_get_field_string_list (msg, mfid);
	
	if (lst && mu_msg_field_xapian_value (mfid)) {
		gchar *str;
		str = mu_str_from_list (lst, ',');
		if (str)
			doc.add_value ((Xapian::valueno)mfid, str);
		g_free (str);
	}

	if (lst && mu_msg_field_xapian_term (mfid)) {
		while (lst) {
			size_t len;
			char *val;
			/* try stack-allocation, it's much faster*/
			len = strlen ((char*)lst->data);
			if (G_LIKELY(len < 1024)) 
				val =  (char*)g_alloca(len+1);
			else
				val  = (char*)g_malloc(len+1);
			strcpy (val, (char*)lst->data);
			
			add_terms_values_str (doc, val, mfid);
			
			if (!(G_LIKELY(len < 1024)))
				g_free (val);

			lst = g_slist_next ((GSList*)lst);
		}
	}
}


struct PartData {
	PartData (Xapian::Document& doc, MuMsgFieldId mfid):
		_doc (doc), _mfid(mfid) {}
	Xapian::Document _doc;
	MuMsgFieldId _mfid;
};

static void
each_part (MuMsg *msg, MuMsgPart *part, PartData *pdata)
{
	if (mu_msg_part_looks_like_attachment (part, TRUE) &&
	    (part->file_name)) {

		char val[MU_STORE_MAX_TERM_LENGTH + 1];
		strncpy (val, part->file_name, sizeof(val));

		/* now, let's create a terms... */
		mu_str_normalize_in_place (val, TRUE);
		mu_str_ascii_xapian_escape_in_place (val);
		
		pdata->_doc.add_term (prefix(pdata->_mfid) +
			      std::string(val, 0, MU_STORE_MAX_TERM_LENGTH));
	}
}


static void
add_terms_values_attach (Xapian::Document& doc, MuMsg *msg,
		       MuMsgFieldId mfid)
{
	PartData pdata (doc, mfid);	
	mu_msg_part_foreach (msg, (MuMsgPartForeachFunc)each_part, &pdata);
}	


static void
add_terms_values_body (Xapian::Document& doc, MuMsg *msg,
		       MuMsgFieldId mfid)
{
	const char *str;
	char *norm;
	
	if (mu_msg_get_flags(msg) & MU_FLAG_ENCRYPTED)
		return; /* ignore encrypted bodies */

	str = mu_msg_get_body_text (msg);
	if (!str) /* FIXME: html->txt fallback needed */
		str = mu_msg_get_body_html (msg);
	
	if (!str)  
		return; /* no body... */
	
	Xapian::TermGenerator termgen;
	termgen.set_document(doc);
	
	norm = mu_str_normalize (str, TRUE);
	termgen.index_text_without_positions
		(norm, 1, prefix(mfid));

	g_free (norm);
}

struct _MsgDoc {
	Xapian::Document	*_doc;
	MuMsg			*_msg;
	MuStore                 *_store;
};
typedef struct _MsgDoc		 MsgDoc;

static void
add_terms_values (MuMsgFieldId mfid, MsgDoc* msgdoc)
{
	/* note: contact-stuff (To/Cc/From) will handled in
	 * add_contact_info, not here */
	if (!mu_msg_field_xapian_index(mfid) &&
	    !mu_msg_field_xapian_term(mfid) &&
	    !mu_msg_field_xapian_value(mfid))
		return;

	switch (mfid) {
	case MU_MSG_FIELD_ID_DATE:
		add_terms_values_date (*msgdoc->_doc, msgdoc->_msg, mfid);
		break;
	case MU_MSG_FIELD_ID_BODY_TEXT:
		add_terms_values_body (*msgdoc->_doc, msgdoc->_msg, mfid);
		break;
	case MU_MSG_FIELD_ID_ATTACH:
		add_terms_values_attach (*msgdoc->_doc, msgdoc->_msg, mfid);
		break;
	default:	
		if (mu_msg_field_is_numeric (mfid)) 
			add_terms_values_number (*msgdoc->_doc, msgdoc->_msg,
						 mfid);
		else if (mu_msg_field_is_string (mfid))
			add_terms_values_string (*msgdoc->_doc,
						 msgdoc->_msg,
						 mfid);
		else if (mu_msg_field_is_string_list(mfid))
			add_terms_values_string_list (*msgdoc->_doc,
						      msgdoc->_msg,
						      mfid);
		else
			g_return_if_reached ();
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
each_contact_info (MuMsgContact *contact, MsgDoc *msgdoc)
{
	const std::string pfx (xapian_pfx(contact));
	if (pfx.empty())
		return; /* unsupported contact type */
	
	if (!mu_str_is_empty(contact->name)) {
		Xapian::TermGenerator termgen;
		termgen.set_document (*msgdoc->_doc);
		char *norm = mu_str_normalize (contact->name, TRUE);
		termgen.index_text_without_positions (norm, 1, pfx);
		g_free (norm);
	}

	/* don't normalize e-mail address, but do lowercase it */
	if (!mu_str_is_empty(contact->address)) {
		char *escaped = mu_str_ascii_xapian_escape (contact->address);
		msgdoc->_doc->add_term
			(std::string  (pfx + escaped, 0,
				       MU_STORE_MAX_TERM_LENGTH));
		g_free (escaped);
		
		/* store it also in our contacts cache */
		if (msgdoc->_store->_contacts)
			mu_contacts_add (msgdoc->_store->_contacts,
					 contact->address, contact->name, 
					 mu_msg_get_date(msgdoc->_msg));
	}
}

/* get a unique id for this message; note, this function returns a
 * static buffer -- not reentrant */
static const char*
get_message_uid (const char* path)
{
	char pfx = 0;
	static char buf[PATH_MAX + 10];
	
	if (G_UNLIKELY(!pfx)) {
		pfx = mu_msg_field_xapian_prefix(MU_MSG_FIELD_ID_PATH);
		buf[0]=pfx;
	}

	std::strcpy (buf + 1, path);

	return buf;
}

/* get a unique id for this message; note, this function returns a
 * static buffer -- not reentrant */
static const char*
get_message_uid (MuMsg *msg)
{
 	return get_message_uid (mu_msg_get_path(msg));
}

gboolean
mu_store_store_msg (MuStore *store, MuMsg *msg, gboolean replace)
{	
	g_return_val_if_fail (store, FALSE);
	g_return_val_if_fail (msg, FALSE);

	try {
		Xapian::Document newdoc;
		Xapian::docid id;
		MsgDoc msgdoc = { &newdoc, msg, store };
		const std::string uid(get_message_uid(msg));
		
		begin_trx_if (store, !store->_in_transaction);
		/* we must add a unique term, so we can replace
		 * matching documents */
		newdoc.add_term (uid);
		mu_msg_field_foreach
			((MuMsgFieldForEachFunc)add_terms_values, &msgdoc);
		/* also store the contact-info as separate terms */
		mu_msg_contact_foreach
			(msg,
			 (MuMsgContactForeachFunc)each_contact_info,
			 &msgdoc);
		
		/* add_document is slightly
		   faster, we can use it when
		 * we know the document does not exist yet, eg., in
		 * case of a rebuild */
		if (replace) /* we replace all existing documents for this file */
			id = store->_db.replace_document (uid, newdoc);
		else
			id = store->_db.add_document (newdoc);
		
		++store->_processed;
		commit_trx_if (store,
			       store->_processed % store->_trx_size == 0);

		return TRUE;

	} MU_XAPIAN_CATCH_BLOCK;

	rollback_trx_if (store, store->_in_transaction);

	return FALSE;
}


gboolean
mu_store_store_path (MuStore *store, const char *path)
{
	MuMsg *msg;
	GError *err;
	gboolean rv;

	g_return_val_if_fail (store, FALSE);
	g_return_val_if_fail (path, FALSE);
		
	err = NULL;
	msg = mu_msg_new_from_file (path, NULL, &err);

	if (!msg) {
		if (err) {
			g_warning ("failed to create message %s to store: %s",
				   path, err->message);
			g_error_free (err);
		} else
			g_warning ("failed to create message %s to store", path);

		return FALSE;	
	}

	rv = mu_store_store_msg (store, msg, TRUE);
	if (!rv)
		g_warning ("failed to store %s", path);

	mu_msg_unref (msg);
		
	return rv;
}


gboolean
mu_store_remove_path (MuStore *store, const char *msgpath)
{
	g_return_val_if_fail (store, FALSE);
	g_return_val_if_fail (msgpath, FALSE);

	try {
		const std::string uid (get_message_uid (msgpath));

		begin_trx_if (store, !store->_in_transaction);
		
		store->_db.delete_document (uid);
		++store->_processed;

		/* do we need to commit now? */
		bool commit_now = store->_processed % store->_trx_size == 0;
		commit_trx_if (store, commit_now);

		return TRUE; 
		
	} MU_XAPIAN_CATCH_BLOCK_RETURN (FALSE);	
}

gboolean
mu_store_contains_message (MuStore *store, const char* path)
{
	g_return_val_if_fail (store, FALSE);
	g_return_val_if_fail (path, FALSE);
	
	try {
		const std::string uid (get_message_uid(path));
		return store->_db.term_exists (uid) ? TRUE: FALSE;
		
	} MU_XAPIAN_CATCH_BLOCK_RETURN (FALSE);
}



time_t
mu_store_get_timestamp (MuStore *store, const char* msgpath)
{
	char *stampstr;
	time_t rv;
	
	g_return_val_if_fail (store, 0);
	g_return_val_if_fail (msgpath, 0);

	stampstr = mu_store_get_metadata (store, msgpath);
	if (!stampstr)
		return (time_t)0;

	rv = (time_t) g_ascii_strtoull (stampstr, NULL, 10);
	g_free (stampstr);

	return rv;
}

gboolean
mu_store_set_timestamp (MuStore *store, const char* msgpath, 
			time_t stamp)
{
	char buf[21];
	
	g_return_val_if_fail (store, FALSE);
	g_return_val_if_fail (msgpath, FALSE);

	sprintf (buf, "%" G_GUINT64_FORMAT, (guint64)stamp);
	return mu_store_set_metadata (store, msgpath, buf);
}


MuError
mu_store_foreach (MuStore *self, 
		  MuStoreForeachFunc func, void *user_data)  
{
	g_return_val_if_fail (self, MU_ERROR);
	g_return_val_if_fail (func, MU_ERROR);
	
	try {
		Xapian::Enquire enq (self->_db);

		enq.set_query  (Xapian::Query::MatchAll);
		enq.set_cutoff (0,0);
		
		Xapian::MSet matches (enq.get_mset (0, self->_db.get_doccount()));
		if (matches.empty())
			return MU_OK; /* database is empty */
		
		for (Xapian::MSet::iterator iter = matches.begin();
		     iter != matches.end(); ++iter) {
			Xapian::Document doc (iter.get_document());
			const std::string path(doc.get_value(MU_MSG_FIELD_ID_PATH));
			MuError res = func (path.c_str(), user_data);
			if (res != MU_OK)
				return res;
		}
		
	} MU_XAPIAN_CATCH_BLOCK_RETURN (MU_ERROR);

	return MU_OK;
}
