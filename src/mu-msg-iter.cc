/* -*- mode: c++; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-
** 
** Copyright (C) 2008-2011 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 3 of the License, or
** (at your option) any later version.
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

#include <stdlib.h>
#include <iostream>
#include <string.h>
#include <errno.h>
#include <algorithm>
#include <xapian.h>
#include <string>

#include "mu-util.h"
#include "mu-msg.h"
#include "mu-msg-iter.h"
#include "mu-msg-threader.h"


/* just a guess... */
#define MAX_FETCH_SIZE 10000

class ThreadKeyMaker: public Xapian::KeyMaker {
public:
	ThreadKeyMaker (GHashTable *threadinfo): _threadinfo(threadinfo) {}
	virtual std::string operator()(const Xapian::Document &doc) const  {
		const char *key;
		key = (const char*)g_hash_table_lookup
			(_threadinfo,
			 GUINT_TO_POINTER(doc.get_docid()));		
		return std::string (key ? key : "");
	}
private:
	GHashTable *_threadinfo;	
};


struct _MuMsgIter {
	_MuMsgIter (Xapian::Enquire &enq, size_t maxnum):
		_enq(enq), _msg(0), _threadhash (0) {
		
		_matches = _enq.get_mset (0, maxnum);

		if (!_matches.empty()) { 
			_matches.fetch();
			_threadhash = mu_msg_threader_calculate (this);
			ThreadKeyMaker keymaker(_threadhash);
			enq.set_sort_by_key (&keymaker, false);
			_matches = _enq.get_mset (0, maxnum);
		}

		_cursor	 = _matches.begin();

		/* this seems to make search slightly faster, some
		 * non-scientific testing suggests. 5-10% or so */ 
		if (_matches.size() <= MAX_FETCH_SIZE)
			_matches.fetch ();
	}
	
	~_MuMsgIter () {
		if (_msg)
			mu_msg_unref (_msg);

		g_hash_table_destroy (_threadhash);
	}
	
	const Xapian::Enquire		_enq;
	Xapian::MSet			_matches;
	Xapian::MSet::const_iterator	_cursor;
		
	MuMsg		*_msg;
	GHashTable      *_threadhash;
};



MuMsgIter*
mu_msg_iter_new (XapianEnquire *enq, size_t maxnum)
{
	g_return_val_if_fail (enq, NULL);
	
	try {
		return new MuMsgIter ((Xapian::Enquire&)*enq, maxnum);
		
	} MU_XAPIAN_CATCH_BLOCK_RETURN(NULL);
}


void
mu_msg_iter_destroy (MuMsgIter *iter)
{
	try { delete iter; } MU_XAPIAN_CATCH_BLOCK;
}
	
MuMsg*
mu_msg_iter_get_msg (MuMsgIter *iter, GError **err)
{
	Xapian::Document *docp;
	
	g_return_val_if_fail (iter, NULL);
	g_return_val_if_fail (!mu_msg_iter_is_done(iter), NULL);
	
	/* get a new MuMsg based on the current doc */
	if (iter->_msg) {
		mu_msg_unref (iter->_msg);
		iter->_msg = NULL;
	}

	docp = new Xapian::Document(iter->_cursor.get_document());
	iter->_msg = mu_msg_new_from_doc ((XapianDocument*)docp, err);
	if (!iter->_msg) {
		g_warning ("%s: failed to create MuMsg",__FUNCTION__);
		return NULL;
	}

	return iter->_msg;
}

gboolean
mu_msg_iter_reset (MuMsgIter *iter)
{
	g_return_val_if_fail (iter, FALSE);

	try {
		iter->_cursor = iter->_matches.begin();
		
	} MU_XAPIAN_CATCH_BLOCK_RETURN (FALSE);

	return TRUE;
}



gboolean
mu_msg_iter_next (MuMsgIter *iter)
{
	g_return_val_if_fail (iter, FALSE);
	
	if (mu_msg_iter_is_done(iter))
		return FALSE;
	
	try {
		++iter->_cursor;
		return iter->_cursor == iter->_matches.end() ? FALSE:TRUE;
		/* try to get a new MuMsg based on the current doc */
		//return update_msg (iter);
		
	} MU_XAPIAN_CATCH_BLOCK_RETURN(FALSE);
}


gboolean
mu_msg_iter_is_done (MuMsgIter *iter)
{
	g_return_val_if_fail (iter, TRUE);

	try {
		return iter->_cursor == iter->_matches.end() ? TRUE : FALSE;

	} MU_XAPIAN_CATCH_BLOCK_RETURN (TRUE);
}


static const gchar*
get_field (MuMsgIter *iter, MuMsgFieldId mfid)
{
	return mu_msg_get_field_string (iter->_msg, mfid);
}

const gchar*
mu_msg_iter_get_field (MuMsgIter *iter, MuMsgFieldId mfid)
{
	g_return_val_if_fail (!mu_msg_iter_is_done(iter), NULL);
	g_return_val_if_fail (mu_msg_field_id_is_valid(mfid), NULL);
	
	return get_field (iter, mfid);
}

static gint64
get_field_numeric (MuMsgIter *iter, MuMsgFieldId mfid)
{
	return mu_msg_get_field_numeric (iter->_msg, mfid);
}


gint64
mu_msg_iter_get_field_numeric (MuMsgIter *iter, MuMsgFieldId mfid)
{
	g_return_val_if_fail (!mu_msg_iter_is_done(iter), -1);	
	g_return_val_if_fail (mu_msg_field_is_numeric(mfid), -1);
	
	return get_field_numeric (iter, mfid);
}

/* hmmm.... is it impossible to get a 0 docid, or just very improbable? */
unsigned int
mu_msg_iter_get_docid (MuMsgIter *iter)
{
	g_return_val_if_fail (!mu_msg_iter_is_done(iter),
			      (unsigned int)-1);	
	try {
		return iter->_cursor.get_document().get_docid();

	} MU_XAPIAN_CATCH_BLOCK_RETURN (0);
}


const char*
mu_msg_iter_get_thread_path (MuMsgIter *iter)
{
	g_return_val_if_fail (!mu_msg_iter_is_done(iter), NULL);
	g_return_val_if_fail (iter->_threadhash, NULL);

	try {
		unsigned int docid;
		docid = mu_msg_iter_get_docid (iter);
		
		return  (const char*)g_hash_table_lookup
			(iter->_threadhash, GUINT_TO_POINTER(docid));
		
	} MU_XAPIAN_CATCH_BLOCK_RETURN (NULL);
}


