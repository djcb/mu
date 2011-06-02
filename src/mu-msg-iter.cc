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
#include <xapian.h>

#include "mu-util.h"
#include "mu-msg.h"
#include "mu-msg-iter.h"

static gboolean update_msg (MuMsgIter *iter);

struct _MuMsgIter {

		_MuMsgIter (const Xapian::Enquire &enq, size_t maxnum):
			_enq(enq), _maxnum(maxnum), _offset(0), _msg(0) {
			
			_matches = _enq.get_mset (0, _maxnum);
			_cursor	 = _matches.begin();

			if (!_matches.empty())
				update_msg (this);
		}

		~_MuMsgIter () {
			if (_msg)
				mu_msg_unref (_msg);
		}
	
		const Xapian::Enquire          _enq;
		Xapian::MSet                   _matches;
		Xapian::MSet::const_iterator   _cursor;
		size_t                         _maxnum, _offset;

		Xapian::Document _doc;
		MuMsg *_msg;
};



MuMsgIter*
mu_msg_iter_new (XapianEnquire *enq, size_t maxnum)
{
	g_return_val_if_fail (enq, NULL);
	
	try {
		return new MuMsgIter ((const Xapian::Enquire&)*enq, maxnum);
		
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
	g_return_val_if_fail (iter, NULL);
	g_return_val_if_fail (!mu_msg_iter_is_done(iter), NULL);
	g_return_val_if_fail (iter->_msg, NULL);
	
	return iter->_msg;
}


static gboolean
message_is_readable (MuMsgIter *iter)
{
	Xapian::Document doc (iter->_cursor.get_document());
	const std::string path(doc.get_value(MU_MSG_FIELD_ID_PATH));
	
	if (access (path.c_str(), R_OK) != 0) {
		g_debug ("cannot read %s: %s", path.c_str(),
			 strerror(errno));
		return FALSE;
	}

	return TRUE;
}

static gboolean
update_msg (MuMsgIter *iter)
{
	GError *err;
	
	/* get a new MuMsg based on the current doc */
	if (iter->_msg) 
		mu_msg_unref (iter->_msg);
	
	iter->_doc = iter->_cursor.get_document();
	err = NULL;
	iter->_msg = mu_msg_new_from_doc ((XapianDocument*)&iter->_doc, &err);
	if (!iter->_msg) {
		g_warning ("%s: failed to create MuMsg: %s",
			   __FUNCTION__, err->message ? err->message : "?");
		g_error_free (err);
		return FALSE;
	}

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
		++iter->_offset;

		if (mu_msg_iter_is_done(iter))
			return FALSE; /* no more matches */
		
		/* the message may not be readable / existing, e.g.,
		 * because of the database not being fully up to
		 * date. in that case, we ignore the message. it
		 * might be nice to auto-delete these messages from
		 * the db, but that might screw up the search;
		 * also, we only have read-only access to the db
		 * here */
		/* TODO: only mark it as such, let clients handle
		 * it */
		if (!message_is_readable (iter))
			return mu_msg_iter_next (iter);
		
		/* try to get a new MuMsg based on the current doc */
		return update_msg (iter);
		
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


unsigned int
mu_msg_iter_get_index (MuMsgIter *iter)
{
	g_return_val_if_fail (iter, (unsigned int)-1);

	return iter->_offset;
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


