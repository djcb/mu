/*
** Copyright (C) 2008 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "xapian.h"
#include "mu-msg-xapian.h"

struct _MuMsgXapian {

	Xapian::Enquire		       *_enq;
	Xapian::MSet                   _matches;
	Xapian::MSet::const_iterator   _cursor;
	size_t                         _batchsize;
	size_t		               _offset;
	char*                          _str[MU_MSG_FIELD_ID_NUM];
};

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

	} catch (const Xapian::Error &err) {
                g_warning ("%s: caught xapian exception '%s' (%s)",
                           __FUNCTION__, err.get_msg().c_str(),
                           err.get_error_string());
        } catch (...) {
                g_warning ("%s: caught exception", __FUNCTION__);
        }

	return msg;
}

void
mu_msg_xapian_destroy (MuMsgXapian *msg)
{
	if (msg) {
		for (int i = 0; i != MU_MSG_FIELD_ID_NUM; ++i) 
			g_free (msg->_str[i]); 

		delete msg->_enq;
		delete msg;
	}
}

gboolean
mu_msg_xapian_next (MuMsgXapian *msg)
{
	g_return_val_if_fail (msg, FALSE);
	g_return_val_if_fail (!mu_msg_xapian_is_done(msg), FALSE);
	
	if (++msg->_cursor == msg->_matches.end()) 
		return FALSE; /* no more matches */
	
	for (int i = 0; i != MU_MSG_FIELD_ID_NUM; ++i) {
		g_free (msg->_str[i]); 
		msg->_str[i] = NULL;
	}
	
	return TRUE; 
}


gboolean
mu_msg_xapian_is_done (MuMsgXapian *msg)
{
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
						 
	} catch (const Xapian::Error &err) {
                g_warning ("%s: caught xapian exception '%s' (%s)",
                           __FUNCTION__, err.get_msg().c_str(),
                           err.get_error_string());
        } catch (...) {
                g_warning ("%s: caught exception", __FUNCTION__);
        }

	return NULL;
}

gint64
mu_msg_xapian_get_field_numeric (MuMsgXapian *row, const MuMsgField *field)
{
	g_return_val_if_fail (mu_msg_field_is_numeric(field), -1);
	
	return Xapian::sortable_unserialise(
		mu_msg_xapian_get_field(row, field));
}



static const gchar*
get_field (MuMsgXapian *row, MuMsgFieldId id)
{
	return mu_msg_xapian_get_field(row, mu_msg_field_from_id (id));
}



unsigned int
mu_msg_xapian_get_id (MuMsgXapian *row)
{
	g_return_val_if_fail (row, NULL);

	try {
		return row->_cursor.get_document().get_docid();

	} catch (const Xapian::Error &err) {
                g_warning ("%s: caught xapian exception '%s' (%s)",
                           __FUNCTION__, err.get_msg().c_str(),
                           err.get_error_string());
        } catch (...) {
                g_warning ("%s: caught exception", __FUNCTION__);
        }

	return 0;
}


const char*
mu_msg_xapian_get_path(MuMsgXapian *row)
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
	return atoi(get_field (row, MU_MSG_FIELD_ID_SIZE));
} 


time_t
mu_msg_xapian_get_date (MuMsgXapian *row)
{
	return atoi(get_field (row, MU_MSG_FIELD_ID_DATE));
} 


MuMsgFlags
mu_msg_xapian_get_flags (MuMsgXapian *row)
{
	return (MuMsgFlags)atoi(get_field 
				(row, MU_MSG_FIELD_ID_FLAGS));

} 

MuMsgPriority
mu_msg_xapian_get_priority (MuMsgXapian *row)
{
	return (MuMsgPriority)atoi(get_field 
				   (row, MU_MSG_FIELD_ID_PRIORITY));
} 
