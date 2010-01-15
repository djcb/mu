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
#include "mu-msg-xapian.h"

struct _MuMsgXapian {
	Xapian::Enquire		       *_enq;
	Xapian::MSet                   _matches;
	Xapian::MSet::const_iterator   _cursor;
	size_t                         _batchsize;
	size_t		               _offset;
	char*                          _str[MU_MSG_FIELD_ID_NUM];
};


/* FIXME: maybe use get_doccount() on the database object instead
 * of specifying the batch size? */
MuMsgXapian *
mu_msg_xapian_new (const Xapian::Enquire& enq, size_t batchsize)
{
	MuMsgXapian *msg;

	try {
		msg = new MuMsgXapian;
		memset (msg->_str, 0, sizeof(msg->_str));

		msg->_enq       = new Xapian::Enquire(enq);
		msg->_matches   =  msg->_enq->get_mset (0, batchsize);
		if (!msg->_matches.empty()) 
			msg->_cursor    = msg->_matches.begin();
		
		msg->_batchsize = batchsize; 
		msg->_offset    = 0;
		
		return msg;

	} MU_XAPIAN_CATCH_BLOCK_RETURN(NULL);
}

void
mu_msg_xapian_destroy (MuMsgXapian *msg)
{
	if (msg) {
		for (int i = 0; i != MU_MSG_FIELD_ID_NUM; ++i) 
			g_free (msg->_str[i]); 

		try {
			delete msg->_enq;
			delete msg;
			
		} MU_XAPIAN_CATCH_BLOCK;
	}
}

static gboolean
message_is_readable (MuMsgXapian *msg)
{
	Xapian::Document doc (msg->_cursor.get_document());
	const std::string path(doc.get_value(MU_MSG_FIELD_ID_PATH));
	
	if (access (path.c_str(), R_OK) != 0) {
		g_debug ("cannot read %s: %s", path.c_str(),
			 strerror(errno));
		return FALSE;
	}

	return TRUE;
}
		


gboolean
mu_msg_xapian_next (MuMsgXapian *msg)
{
	g_return_val_if_fail (msg, FALSE);
	g_return_val_if_fail (!mu_msg_xapian_is_done(msg), FALSE);

	try {
		if (++msg->_cursor == msg->_matches.end()) 
			return FALSE; /* no more matches */
		
		/* the message may not be readable / existing, e.g., because
		 * of the database not being fully up to date. in that case,
		 * we ignore the message. it might be nice to auto-delete
		 * these messages from the db, but that would might screw
		 * up the search; also, we only have read-only access to the
		 * db here */
		if (!message_is_readable (msg))
			return mu_msg_xapian_next (msg);
		
		for (int i = 0; i != MU_MSG_FIELD_ID_NUM; ++i) {
			g_free (msg->_str[i]); 
			msg->_str[i] = NULL;
		}

		return TRUE;
		
	} MU_XAPIAN_CATCH_BLOCK_RETURN(FALSE);
}


gboolean
mu_msg_xapian_is_done (MuMsgXapian *msg)
{
	g_return_val_if_fail (msg, TRUE);

	if (msg->_matches.empty())
		return TRUE;

	if (msg->_cursor == msg->_matches.end())
		return TRUE;

	return FALSE;
}


const gchar*
mu_msg_xapian_get_field (MuMsgXapian *row, const MuMsgField *field)
{
	g_return_val_if_fail (row, NULL);
	g_return_val_if_fail (!mu_msg_xapian_is_done(row), NULL);
	g_return_val_if_fail (field, NULL);
	
	try {
		MuMsgFieldId id;
		
		id = mu_msg_field_id (field);
		if (!row->_str[id]) { 	/* cache the value */
			Xapian::Document doc (row->_cursor.get_document());
			row->_str[id] = g_strdup (doc.get_value(id).c_str());
		}
		
		return row->_str[id];
						 
	} MU_XAPIAN_CATCH_BLOCK_RETURN(NULL);
}


gint64
mu_msg_xapian_get_field_numeric (MuMsgXapian *row, const MuMsgField *field)
{
	g_return_val_if_fail (mu_msg_field_is_numeric(field), -1);

	try {
		return Xapian::sortable_unserialise(
			mu_msg_xapian_get_field(row, field));
	} MU_XAPIAN_CATCH_BLOCK_RETURN(-1);
}



static const gchar*
get_field (MuMsgXapian *row, MuMsgFieldId id)
{
	return mu_msg_xapian_get_field(row, mu_msg_field_from_id (id));
}

static long
get_field_number (MuMsgXapian *row, MuMsgFieldId id)
{
	const char* str = get_field (row, id);
	return str ? atol (str) : 0;
}



/* hmmm.... is it impossible to get a 0 docid, or just very improbable? */
unsigned int
mu_msg_xapian_get_docid (MuMsgXapian *row)
{
	g_return_val_if_fail (row, 0);

	try {
		return row->_cursor.get_document().get_docid();

	} MU_XAPIAN_CATCH_BLOCK_RETURN (0);
}


const char*
mu_msg_xapian_get_path (MuMsgXapian *row)
{
	return get_field (row, MU_MSG_FIELD_ID_PATH);
}


const char*
mu_msg_xapian_get_from (MuMsgXapian *row)
{
	return get_field (row, MU_MSG_FIELD_ID_FROM);
}

const char*
mu_msg_xapian_get_to (MuMsgXapian *row)
{
	return get_field (row, MU_MSG_FIELD_ID_TO);
}


const char*
mu_msg_xapian_get_cc (MuMsgXapian *row)
{
	return get_field (row, MU_MSG_FIELD_ID_CC);
}


const char*
mu_msg_xapian_get_subject (MuMsgXapian *row)
{
	return get_field (row, MU_MSG_FIELD_ID_SUBJECT);
}


size_t
mu_msg_xapian_get_size (MuMsgXapian *row)
{
	return (size_t) get_field_number (row, MU_MSG_FIELD_ID_SIZE);
} 


time_t
mu_msg_xapian_get_date (MuMsgXapian *row)
{
	return (size_t) get_field_number (row, MU_MSG_FIELD_ID_DATE);
} 


MuMsgFlags
mu_msg_xapian_get_flags (MuMsgXapian *row)
{
	return (MuMsgFlags) get_field_number (row, MU_MSG_FIELD_ID_FLAGS);
} 

MuMsgPriority
mu_msg_xapian_get_priority (MuMsgXapian *row)
{
	return (MuMsgPriority) get_field_number (row, MU_MSG_FIELD_ID_PRIORITY);
} 
