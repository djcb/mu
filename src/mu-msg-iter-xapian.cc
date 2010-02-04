/*
** Copyright (C) 2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include "xapian.h"

#include "mu-util.h"
#include "mu-msg-iter-xapian.h"

struct _MuMsgIterXapian {
	Xapian::Enquire		       *_enq;
	Xapian::MSet                   _matches;
	Xapian::MSet::const_iterator   _cursor;
	size_t                         _batchsize;
	size_t		               _offset;
	char*                          _str[MU_MSG_FIELD_ID_NUM];
	bool                           _is_null; 
};



MuMsgIterXapian*
mu_msg_iter_xapian_new (const Xapian::Enquire& enq, size_t batchsize)
{
	MuMsgIterXapian *iter;

	try {
		iter = new MuMsgIterXapian;
		memset (iter->_str, 0, sizeof(iter->_str));

		iter->_enq       = new Xapian::Enquire(enq);
		iter->_matches   = iter->_enq->get_mset (0, batchsize);
		if (!iter->_matches.empty()) {
			iter->_cursor    = iter->_matches.begin();
			iter->_is_null   = false;
		} else
			iter->_is_null = true;
		
		iter->_batchsize = batchsize; 
		iter->_offset    = 0;
		
		return iter;

	} MU_XAPIAN_CATCH_BLOCK_RETURN(NULL);
}

void
mu_msg_iter_xapian_destroy (MuMsgIterXapian *iter)
{
	if (iter) {
		for (int i = 0; i != MU_MSG_FIELD_ID_NUM; ++i) 
			g_free (iter->_str[i]); 

		try {
			delete iter->_enq;
			delete iter;
			
		} MU_XAPIAN_CATCH_BLOCK;
	}
}

static gboolean
message_is_readable (MuMsgIterXapian *iter)
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
		
static MuMsgIterXapian*
get_next_batch (MuMsgIterXapian *iter)
{	
	iter->_matches = iter->_enq->get_mset (iter->_offset,
					       iter->_batchsize);
	if (iter->_matches.empty()) {
		iter->_cursor = iter->_matches.end();
		iter->_is_null = true;
	} else {
		iter->_cursor = iter->_matches.begin();
		iter->_is_null = false;
	}
		
	return iter;
}

gboolean
mu_msg_iter_xapian_next (MuMsgIterXapian *iter)
{
	g_return_val_if_fail (iter, FALSE);

	try {
		++iter->_offset;
		if (++iter->_cursor == iter->_matches.end())
			iter = get_next_batch (iter);
		if (iter->_cursor == iter->_matches.end())
			return FALSE; /* no more matches */
			
		/* the message may not be readable / existing, e.g.,
		    * because of the database not being fully up to
		    * date. in that case, we ignore the message. it
		    * might be nice to auto-delete these messages from
		    * the db, but that would might screw up the
		    * search; also, we only have read-only access to
		    * the db here */
		if (!message_is_readable (iter))
			return mu_msg_iter_xapian_next (iter);
		
		for (int i = 0; i != MU_MSG_FIELD_ID_NUM; ++i) {
			g_free (iter->_str[i]); 
			iter->_str[i] = NULL;
		}

		return TRUE;
		
	} MU_XAPIAN_CATCH_BLOCK_RETURN(FALSE);
}


gboolean
mu_msg_iter_xapian_is_null (MuMsgIterXapian *iter)
{
	g_return_val_if_fail (iter, TRUE);

	return iter->_is_null;
}


const gchar*
mu_msg_iter_xapian_get_field (MuMsgIterXapian *iter, const MuMsgField *field)
{
	g_return_val_if_fail (iter, NULL);
	g_return_val_if_fail (!mu_msg_iter_xapian_is_null(iter), NULL);
	g_return_val_if_fail (field, NULL);
	
	try {
		MuMsgFieldId id;
		
		id = mu_msg_field_id (field);
		if (!iter->_str[id]) { 	/* cache the value */
			Xapian::Document doc (iter->_cursor.get_document());
			iter->_str[id] = g_strdup (doc.get_value(id).c_str());
		}
		
		return iter->_str[id];
						 
	} MU_XAPIAN_CATCH_BLOCK_RETURN(NULL);
}


gint64
mu_msg_iter_xapian_get_field_numeric (MuMsgIterXapian *iter,
				      const MuMsgField *field)
{
	g_return_val_if_fail (mu_msg_field_is_numeric(field), -1);

	try {
		return Xapian::sortable_unserialise(
			mu_msg_iter_xapian_get_field(iter, field));
	} MU_XAPIAN_CATCH_BLOCK_RETURN(-1);
}



static const gchar*
get_field (MuMsgIterXapian *iter, MuMsgFieldId id)
{
	return mu_msg_iter_xapian_get_field(iter, mu_msg_field_from_id (id));
}

static long
get_field_number (MuMsgIterXapian *iter, MuMsgFieldId id)
{
	const char* str = get_field (iter, id);
	return str ? atol (str) : 0;
}



/* hmmm.... is it impossible to get a 0 docid, or just very improbable? */
unsigned int
mu_msg_iter_xapian_get_docid (MuMsgIterXapian *iter)
{
	g_return_val_if_fail (iter, 0);

	try {
		return iter->_cursor.get_document().get_docid();

	} MU_XAPIAN_CATCH_BLOCK_RETURN (0);
}


const char*
mu_msg_iter_xapian_get_path (MuMsgIterXapian *iter)
{
	return get_field (iter, MU_MSG_FIELD_ID_PATH);
}


const char*
mu_msg_iter_xapian_get_from (MuMsgIterXapian *iter)
{
	return get_field (iter, MU_MSG_FIELD_ID_FROM);
}

const char*
mu_msg_iter_xapian_get_to (MuMsgIterXapian *iter)
{
	return get_field (iter, MU_MSG_FIELD_ID_TO);
}


const char*
mu_msg_iter_xapian_get_cc (MuMsgIterXapian *iter)
{
	return get_field (iter, MU_MSG_FIELD_ID_CC);
}


const char*
mu_msg_iter_xapian_get_subject (MuMsgIterXapian *iter)
{
	return get_field (iter, MU_MSG_FIELD_ID_SUBJECT);
}


size_t
mu_msg_iter_xapian_get_size (MuMsgIterXapian *iter)
{
	return (size_t) get_field_number (iter, MU_MSG_FIELD_ID_SIZE);
} 


time_t
mu_msg_iter_xapian_get_date (MuMsgIterXapian *iter)
{
	return (size_t) get_field_number (iter, MU_MSG_FIELD_ID_DATE);
} 


MuMsgFlags
mu_msg_iter_xapian_get_flags (MuMsgIterXapian *iter)
{
	return (MuMsgFlags) get_field_number (iter, MU_MSG_FIELD_ID_FLAGS);
} 

MuMsgPriority
mu_msg_iter_xapian_get_priority (MuMsgIterXapian *iter)
{
	return (MuMsgPriority) get_field_number (iter,
						 MU_MSG_FIELD_ID_PRIORITY);
} 
