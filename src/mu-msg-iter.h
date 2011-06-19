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


/**
 * MuMsgIter is a structure to iterate over the results of a
 * query. You can iterate only in one-direction, and you can do it
 * only once.
 * 
 */

struct _MuMsgIter;
typedef struct _MuMsgIter MuMsgIter;


/**
 * create a new MuMsgIter -- basically, an iterator over the search
 * results
 * 
 * @param enq a Xapian::Enquire* cast to XapianEnquire* (because this
 * is C, not C++),providing access to search results
 * @param batchsize how many results to retrieve at once
 * @param threads whether to calculate threads
 * 
 * @return a new MuMsgIter, or NULL in case of error
 */
MuMsgIter *mu_msg_iter_new (XapianEnquire *enq,
			    size_t batchsize, gboolean threads) G_GNUC_WARN_UNUSED_RESULT;

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
 * reset the iterator to the beginning
 * 
 * @param iter a valid MuMsgIter iterator
 * 
 * @return TRUE if it succeeded, FALSE otherwise
 */
gboolean mu_msg_iter_reset (MuMsgIter *iter);

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
 * get the corresponding MuMsg for this iter; this instance is owned
 * by MuMsgIter, and becomes invalid after either mu_msg_iter_destroy
 * or mu_msg_iter_next. _do not_ unref it.
 * 
 * @param iter a valid MuMsgIter instance 
 * @param err which receives error info or NULL. err is only filled
 * when the function returns NULL
 * 
 * @return a MuMsg instance, or NULL in case of error
 */
MuMsg* mu_msg_iter_get_msg (MuMsgIter *iter, GError **err)
          G_GNUC_MALLOC G_GNUC_WARN_UNUSED_RESULT;



/**
 * get the document id for the current message
 * 
 * @param iter a valid MuMsgIter iterator
 * 
 * @return the docid or (unsigned int)-1 in case of error
 */
unsigned int     mu_msg_iter_get_docid         (MuMsgIter *iter);


/**
 * calculate the message threads
 * 
 * @param iter a valid MuMsgIter iterator 
 * 
 * @return TRUE if it worked, FALSE otherwsie.
 */
gboolean mu_msg_iter_calculate_threads (MuMsgIter *iter);


/**
 * get a sortable string describing the path of a thread
 * 
 * @param iter a valid MuMsgIter iterator 
 * 
 * @return a thread path
 */
const char* mu_msg_iter_get_thread_path (MuMsgIter *iter);

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
