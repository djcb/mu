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

#ifndef __MU_MSG_ITER_XAPIAN_H__
#define __MU_MSG_ITER_XAPIAN_H__

#include "mu-msg.h"
#include "mu-msg-gmime.h"

G_BEGIN_DECLS

struct _MuMsgIterXapian;
typedef struct _MuMsgIterXapian MuMsgIterXapian;


/**
 * get the next next message (which you got from
 * e.g. mu_query_xapian_run)
 * 
 * @param iter a valid MuMsgIterXapian iterator
 * 
 * @return TRUE if it succeeded, FALSE otherwise (e.g., because there
 * are no more messages in the query result)
 */
gboolean         mu_msg_iter_xapian_next              (MuMsgIterXapian *iter);


/**
 * does the iter point to a real message?
 * 
 * @param iter a valid MuMsgIterXapian iterator
 * 
 * @return TRUE if the iterator points to a message, FALSE other
 */
gboolean         mu_msg_iter_xapian_is_null (MuMsgIterXapian *iter);


/**
 * destroy the sequence of messages; ie. /all/ of them
 * 
 * @param msg a valid MuMsgIterXapian message or NULL
 */
void		 mu_msg_iter_xapian_destroy           (MuMsgIterXapian *iter);


/**
 * get the corresponding GMime message for this iter; this requires
 * the corresponding message file to be present at the expected place
 * 
 * @param iter a valid MuMsgIterXapian instance
 * 
 * @return a MuMsgGMime instance, or NULL in case of error. Use
 * mu_msg_gmime_destroy when the instance is no longer needed
 */
MuMsgGMime* mu_msg_iter_xapian_get_msg_gmime (MuMsgIterXapian *iter);

/**
 * get the document id for the current message
 * 
 * @param iter a valid MuMsgIterXapian iterator
 * 
 * @return the docid or 0 in case of error
 */
unsigned int     mu_msg_iter_xapian_get_docid         (MuMsgIterXapian *iter);


/**
 * get the directory path of the message
 * 
 * @param iter a valid MuMsgIterXapian iterator
 * 
 * @return the path, or NULL in case of error
 */
const char*      mu_msg_iter_xapian_get_path          (MuMsgIterXapian *iter);


/**
 * get the size of the message
 * 
 * @param iter a valid MuMsgIterXapian iterator
 * 
 * @return the size, or 0 in case of error
 */
size_t           mu_msg_iter_xapian_get_size          (MuMsgIterXapian *iter);  

/**
 * get the timestamp (ctime) of the message file
 * 
 * @param iter a valid MuMsgIterXapian iterator
 * 
 * @return the size, or 0 in case of error
 */
time_t           mu_msg_iter_xapian_get_timestamp     (MuMsgIterXapian *iter);  

/**
 * get the sent time of the message
 * 
 * @param iter a valid MuMsgIterXapian iterator
 * 
 * @return the time, or 0 in case of error
 */
time_t           mu_msg_iter_xapian_get_date          (MuMsgIterXapian *iter);  

/**
 * get the message sender(s) of the message
 * 
 * @param iter a valid MuMsgIterXapian iterator
 * 
 * @return the time, or 0 in case of error
 */
const char*      mu_msg_iter_xapian_get_from          (MuMsgIterXapian *iter);

/**
 * get the message recipient (To:) of the message
 * 
 * @param iter a valid MuMsgIterXapian iterator
 * 
 * @return the To-recipient(s), or NULL in case of error
 */
const char*      mu_msg_iter_xapian_get_to            (MuMsgIterXapian *iter);


/**
 * get the message recipient (Cc:) of the message
 * 
 * @param iter a valid MuMsgIterXapian iterator
 * 
 * @return the Cc-recipient(s), or NULL in case of error
 */
const char*      mu_msg_iter_xapian_get_cc            (MuMsgIterXapian *iter);

/**
 * get the subject of the message
 * 
 * @param iter a valid MuMsgIterXapian iterator
 * 
 * @return the subject, or NULL in case of error
 */
const char*      mu_msg_iter_xapian_get_subject       (MuMsgIterXapian *iter);

/**
 * get the message flags 
 * 
 * @param iter a valid MuMsgIterXapian iterator
 * 
 * @return the message flags, or MU_MSG_FLAG_UNKNOWN
 */
MuMsgFlags       mu_msg_iter_xapian_get_flags         (MuMsgIterXapian *iter);


/**
 * get the message priority 
 * 
 * @param iter a valid MuMsgIterXapian iterator
 * 
 * @return the message priority, or MU_MSG_PRIORITY_NONE
 */
MuMsgPriority    mu_msg_iter_xapian_get_priority      (MuMsgIterXapian *iter);


/**
 * get some message field
 * 
 * @param iter a valid MuMsgIterXapian iterator
 * @param field the string field to retrieve
 * 
 * @return the field value, or NULL
 */
const gchar*     mu_msg_iter_xapian_get_field         (MuMsgIterXapian *iter, 
						  const MuMsgField *field);

/**
 * get some numeric message field
 * 
 * @param iter a valid MuMsgIterXapian iterator
 * @param field the numeric field to retrieve
 * 
 * @return the field value, or -1 in case of error
 */
gint64           mu_msg_iter_xapian_get_field_numeric     (MuMsgIterXapian *iter, 
						      const MuMsgField *field);
G_END_DECLS

#endif /*__MU_MSG_ITER_XAPIAN_H__*/
