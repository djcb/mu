/*
** Copyright (C) 2008-2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef __MU_MSG_ITER_H__
#define __MU_MSG_ITER_H__

#include <glib.h>
#include <mu-msg.h>

G_BEGIN_DECLS

struct _MuMsgIter;
typedef struct _MuMsgIter MuMsgIter;

/**
 * get the next message (which you got from
 * e.g. mu_query_run)
 * 
 * @param iter a valid MuMsgIter iterator
 * 
 * @return TRUE if it succeeded, FALSE otherwise (e.g., because there
 * are no more messages in the query result)
 */
gboolean         mu_msg_iter_next              (MuMsgIter *iter);


/**
 * does this iterator point past the end of the list?
 * 
 * @param iter a valid MuMsgIter iterator
 * 
 * @return TRUE if the iter points past end of the list, FALSE
 * otherwise
 */
gboolean         mu_msg_iter_is_done (MuMsgIter *iter);


/**
 * destroy the sequence of messages; ie. /all/ of them
 * 
 * @param msg a valid MuMsgIter message or NULL
 */
void		 mu_msg_iter_destroy           (MuMsgIter *iter);


/**
 * get the corresponding MuMsg for this iter; this requires the
 * corresponding message file to be present at the expected place in
 * the maildir in the file system. Note, it's faster to use the
 * database fields (the various mu_msg_iter_get_... functions), so
 * MuMsg should use only when information is needed that is not
 * provided from the iter).
 * 
 * @param iter a valid MuMsgIter instance 
 * @param err which receives error info or NULL. err is only filled
 * when the function returns NULL
 * 
 * @return a MuMsgGMime instance, or NULL in case of error. Use
 * mu_msg_gmime_destroy when the instance is no longer needed
 */
MuMsg* mu_msg_iter_get_msg (MuMsgIter *iter, GError **err)
        G_GNUC_MALLOC G_GNUC_WARN_UNUSED_RESULT;
   
/**
 * get the document id for the current message
 * 
 * @param iter a valid MuMsgIter iterator
 * 
 * @return the docid or 0 in case of error
 */
unsigned int     mu_msg_iter_get_docid         (MuMsgIter *iter);


/**
 * get the full path of the message file
 * 
 * @param iter a valid MuMsgIter iterator
 * 
 * @return the path, or NULL in case of error
 */
const char*      mu_msg_iter_get_path          (MuMsgIter *iter);


/**
 * get the maildir of the message - e.g., a message file 
 * /home/user/Maildir/foo/bar/cur/abc123 would have maildir
 *   "/foo/bar"
 * 
 * @param iter a valid MuMsgIter iterator
 * 
 * @return the path, or NULL in case of error
 */
const char*      mu_msg_iter_get_maildir (MuMsgIter *iter);


/**
 * get the size of the message
 * 
 * @param iter a valid MuMsgIter iterator
 * 
 * @return the size, or 0 in case of error
 */
size_t           mu_msg_iter_get_size          (MuMsgIter *iter);  

/**
 * get the timestamp (ctime) of the message file
 * 
 * @param iter a valid MuMsgIter iterator
 * 
 * @return the size, or 0 in case of error
 */
time_t           mu_msg_iter_get_timestamp     (MuMsgIter *iter);  

/**
 * get the sent time of the message
 * 
 * @param iter a valid MuMsgIter iterator
 * 
 * @return the time, or 0 in case of error
 */
time_t           mu_msg_iter_get_date          (MuMsgIter *iter);  

/**
 * get the message sender(s) of the message
 * 
 * @param iter a valid MuMsgIter iterator
 * 
 * @return the time, or 0 in case of error
 */
const char*      mu_msg_iter_get_from          (MuMsgIter *iter);

/**
 * get the message recipient (To:) of the message
 * 
 * @param iter a valid MuMsgIter iterator
 * 
 * @return the To-recipient(s), or NULL in case of error
 */
const char*      mu_msg_iter_get_to            (MuMsgIter *iter);


/**
 * get the message recipient (Cc:) of the message
 * 
 * @param iter a valid MuMsgIter iterator
 * 
 * @return the Cc-recipient(s), or NULL in case of error
 */
const char*      mu_msg_iter_get_cc            (MuMsgIter *iter);

/**
 * get the subject of the message
 * 
 * @param iter a valid MuMsgIter iterator
 * 
 * @return the subject, or NULL in case of error
 */
const char*      mu_msg_iter_get_subject       (MuMsgIter *iter);

/**
 * get the message flags 
 * 
 * @param iter a valid MuMsgIter iterator
 * 
 * @return the message flags, or MU_MSG_FLAG_UNKNOWN
 */
MuMsgFlags       mu_msg_iter_get_flags         (MuMsgIter *iter);


/**
 * get the message priority 
 * 
 * @param iter a valid MuMsgIter iterator
 * 
 * @return the message priority, or MU_MSG_PRIO_NONE
 */
MuMsgPrio    mu_msg_iter_get_prio      (MuMsgIter *iter);


/**
 * get some message field
 * 
 * @param iter a valid MuMsgIter iterator
 * @param field the string field to retrieve
 * 
 * @return the field value, or NULL
 */
const gchar*     mu_msg_iter_get_field         (MuMsgIter *iter, 
						MuMsgFieldId mfid);

/**
 * get some numeric message field
 * 
 * @param iter a valid MuMsgIter iterator
 * @param field the numeric field to retrieve
 * 
 * @return the field value, or -1 in case of error
 */
gint64           mu_msg_iter_get_field_numeric     (MuMsgIter *iter, 
						    MuMsgFieldId mfid);
G_END_DECLS

#endif /*__MU_MSG_ITER_H__*/
