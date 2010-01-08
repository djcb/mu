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

#ifndef __MU_MSG_XAPIAN_H__
#define __MU_MSG_XAPIAN_H__

#include "mu-msg.h"

G_BEGIN_DECLS

struct _MuMsgXapian;
typedef struct _MuMsgXapian MuMsgXapian;

/** 
 * get the next next message (which you got from
 * e.g. mu_query_xapian_run)
 * 
 * @param msg a valid MuMsgXapian message
 * 
 * @return TRUE if it succeeded, FALSE otherwise (e.g., because there
 * are no more messages in the query result)
 */
gboolean         mu_msg_xapian_next              (MuMsgXapian *msg);

/** 
 * are there any message left? together with mu_msg_xapian_next, this
 * function can be used to iterate over query results.
 * 
 * @param msg a valid MuMsgXapian message
 * 
 * @return TRUE if there are messages left, FALSE otherwise
 */
gboolean         mu_msg_xapian_is_done           (MuMsgXapian *msg);

/** 
 * destroy the sequence of messages
 * 
 * @param msg a valid MuMsgXapian message or NULL
 */
void		 mu_msg_xapian_destroy           (MuMsgXapian *msg);

/** 
 * get the document id for the current message
 * 
 * @param row a message
 * 
 * @return the docid or 0 in case of error
 */
unsigned int     mu_msg_xapian_get_docid         (MuMsgXapian *row);


/** 
 * get the directory path of the message
 * 
 * @param row a message
 * 
 * @return the path, or NULL in case of error
 */
const char*      mu_msg_xapian_get_path          (MuMsgXapian *row);


/** 
 * get the size of the message
 * 
 * @param row a message
 * 
 * @return the size, or 0 in case of error
 */
size_t           mu_msg_xapian_get_size          (MuMsgXapian *row);  

/** 
 * get the timestamp (ctime) of the message file
 * 
 * @param row a message
 * 
 * @return the size, or 0 in case of error
 */
time_t           mu_msg_xapian_get_timestamp     (MuMsgXapian *row);  

/** 
 * get the sent time of the message
 * 
 * @param row a message
 * 
 * @return the time, or 0 in case of error
 */
time_t           mu_msg_xapian_get_date          (MuMsgXapian *row);  

/** 
 * get the message sender(s) of the message
 * 
 * @param row a message
 * 
 * @return the time, or 0 in case of error
 */
const char*      mu_msg_xapian_get_from          (MuMsgXapian *row);

/** 
 * get the message recipient (To:) of the message
 * 
 * @param row a message
 * 
 * @return the To-recipient(s), or NULL in case of error
 */
const char*      mu_msg_xapian_get_to            (MuMsgXapian *row);


/** 
 * get the message recipient (Cc:) of the message
 * 
 * @param row a message
 * 
 * @return the Cc-recipient(s), or NULL in case of error
 */
const char*      mu_msg_xapian_get_cc            (MuMsgXapian *row);

/** 
 * get the subject of the message
 * 
 * @param row a message
 * 
 * @return the subject, or NULL in case of error
 */
const char*      mu_msg_xapian_get_subject       (MuMsgXapian *row);

/** 
 * get the message flags 
 * 
 * @param row a message
 * 
 * @return the message flags, or MU_MSG_FLAG_UNKNOWN
 */
MuMsgFlags       mu_msg_xapian_get_flags         (MuMsgXapian *row);


/** 
 * get the message priority 
 * 
 * @param row a message
 * 
 * @return the message priority, or MU_MSG_PRIORITY_NONE
 */
MuMsgPriority    mu_msg_xapian_get_priority      (MuMsgXapian *row);


/** 
 * get some message field
 * 
 * @param row a message
 * @param field the string field to retrieve
 * 
 * @return the field value, or NULL
 */
const gchar*     mu_msg_xapian_get_field         (MuMsgXapian *row, 
						  const MuMsgField *field);

/** 
 * get some numeric message field
 * 
 * @param row a message
 * @param field the numeric field to retrieve
 * 
 * @return the field value, or -1 in case of error
 */
gint64           mu_msg_xapian_get_field_numeric     (MuMsgXapian *row, 
						      const MuMsgField *field);
G_END_DECLS

#endif /*__MU_MSG_XAPIAN_H__*/
