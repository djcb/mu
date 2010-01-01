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

#ifndef __MU_MSG_READ_H__
#define __MU_MSG_READ_H__

#include "mu-msg.h"

G_BEGIN_DECLS

struct _MuMsgGMime;
typedef struct _MuMsgGMime MuMsgGMime;

/** 
 * initialize the message parsing system; this function must be called
 * before doing any message parsing (ie., any of the other
 * mu_msg_gmime functions). when done with the message parsing system,
 * call mu_msg_gmime_uninit. Note: calling this function on an already
 * initialized system has no effect
 */
void     mu_msg_gmime_init            (void);

/** 
 * uninitialize the messge parsing system that has previously been
 * initialized with mu_msg_init. not calling mu_msg_uninit after
 * mu_msg_init has been called will lead to memory leakage. Note:
 * calling mu_msg_uninit on an uninitialized system has no
 * effect
 */
void     mu_msg_gmime_uninit          (void);


/** 
 * create a new MuMsgGMime* instance which parses a message and provides
 * read access to its properties; call mu_msg_destroy when done with
 * done with it.
 *
 * @param path full path to an email message file
 * 
 * @return a new MuMsgGMime instance or NULL in case of error
 */
MuMsgGMime*   mu_msg_gmime_new		   (const char* filepath);


/** 
 * destroy a MuMsgGMime* instance; call this function when done with
 * a MuMsgGMime
 * 
 * @param msg a MuMsgGMime* instance or NULL
 */
void     mu_msg_gmime_destroy         (MuMsgGMime *msg);



/** 
 * get the plain text body of this message
 *
 * @param a valid MuMsgGMime* instance
 * 
 * @return the plain text body or NULL in case of error or if there is no
 * such body. the returned string should *not* be modified or freed.
 * The returned data is in UTF8 or NULL.
 */
const char*     mu_msg_gmime_get_body_text       (MuMsgGMime *msg);


/** 
 * get the html body of this message
 *
 * @param a valid MuMsgGMime* instance
 * 
 * @return the html body or NULL in case of error or if there is no
 * such body. the returned string should *not* be modified or freed.
 */
const char*     mu_msg_gmime_get_body_html       (MuMsgGMime *msg);


/** 
 * get the sender (From:) of this message
 *
 * @param a valid MuMsgGMime* instance
 * 
 * @return the sender of this Message or NULL in case of error or if there 
 * is no sender. the returned string should *not* be modified or freed.
 */
const char*     mu_msg_gmime_get_from	   (MuMsgGMime *msg);


/** 
 * get the recipients (To:) of this message
 *
 * @param a valid MuMsgGMime* instance
 * 
 * @return the sender of this Message or NULL in case of error or if there 
 * are no recipients. the returned string should *not* be modified or freed.
 */
const char*     mu_msg_gmime_get_to	   (MuMsgGMime *msg);


/** 
 * get the carbon-copy recipients (Cc:) of this message
 *
 * @param a valid MuMsgGMime* instance
 * 
 * @return the Cc: recipients of this Message or NULL in case of error or if 
 * there are no such recipients. the returned string should *not* be modified 
 * or freed.
 */
const char*     mu_msg_gmime_get_cc	           (MuMsgGMime *msg);

/** 
 * get the file system path of this message
 *
 * @param a valid MuMsgGMime* instance
 * 
 * @return the path of this Message or NULL in case of error. 
 * the returned string should *not* be modified or freed.
 */
const char*     mu_msg_gmime_get_path            (MuMsgGMime *msg);

/** 
 * get the subject of this message
 *
 * @param a valid MuMsgGMime* instance
 * 
 * @return the subject of this Message or NULL in case of error or if there 
 * is no subject. the returned string should *not* be modified or freed.
 */
const char*     mu_msg_gmime_get_subject         (MuMsgGMime *msg);

/** 
 * get the Message-Id of this message
 *
 * @param a valid MuMsgGMime* instance
 * 
 * @return the Message-Id of this Message or NULL in case of error or if there 
 * is none. the returned string should *not* be modified or freed.
 */
const char*     mu_msg_gmime_get_msgid           (MuMsgGMime *msg);


/** 
 * get any arbitrary header from this message
 *
 * @param a valid MuMsgGMime* instance
 * @header the header requested
 * 
 * @return the header requested or NULL in case of error or if there 
 * is no such header. the returned string should *not* be modified or freed.
 */
const char*     mu_msg_gmime_get_header          (MuMsgGMime *msg, 
					      const char* header);

/** 
 * get the message date/time (the Date: field) as time_t, using UTC
 *
 * @param a valid MuMsgGMime* instance
 * 
 * @return message date/time or 0 in case of error or if there 
 * is no such header.
 */
time_t          mu_msg_gmime_get_date            (MuMsgGMime *msg);

/** 
 * get the flags for this message
 *
 * @param msg valid MuMsgGMime* instance
 * 
 * @return the fileflags as logically OR'd #Mu MsgFlags or 0 if
 * there are none.
 */
MuMsgFlags     mu_msg_gmime_get_flags      (MuMsgGMime *msg);


/** 
 * get the file size in bytes of this message
 *
 * @param a valid MuMsgGMime* instance
 * 
 * @return the filesize 
 */
size_t          mu_msg_gmime_get_size       (MuMsgGMime *msg);


/** 
 * get some field value as string
 * 
 * @param msg a valid MuMsgGmime instance
 * @param field the field to retrieve; it must be a string-typed field
 * 
 * @return a string that should not be freed
 */
const char*  mu_msg_gmime_get_field_string  (MuMsgGMime *msg, 
					     const MuMsgField* field);

/** 
 * get some field value as string
 * 
 * @param msg a valid MuMsgGmime instance
 * @param field the field to retrieve; it must be a numeric field
 * 
 * @return a string that should not be freed
 */
gint64      mu_msg_gmime_get_field_numeric (MuMsgGMime *msg, 
					    const MuMsgField* field);

/** 
 * get the message priority for this message 
 * (MU_MSG_PRIORITY_LOW, MU_MSG_PRIORITY_NORMAL or MU_MSG_PRIORITY_HIGH)
 * the X-Priority, X-MSMailPriority, Importance and Precedence header are
 * checked, in that order. 
 * if no explicit priority is set, MU_MSG_PRIORITY_NORMAL is assumed
 *
 * @param a valid MuMsgGMime* instance
 * 
 * @return the message priority (!= 0) or 0 in case of error
 */
MuMsgPriority   mu_msg_gmime_get_priority        (MuMsgGMime *msg);

/** 
 * get the timestamp (mtime) for the file containing this message 
 *
 * @param a valid MuMsgGMime* instance
 * 
 * @return the timestamp or 0 in case of error
 */
time_t          mu_msg_gmime_get_timestamp       (MuMsgGMime *msg);


typedef int (*MuMsgGMimeContactsCallback) (MuMsgContact*, void *ptr);


/** 
 * call a function for each of the contacts in a message 
 *
 * @param msg a valid MuMsgGMime* instance
 * @param cb a callback function to call for each contact; when
 * the callback returns non-0, the function stops, and this last
 * callback return value is returned
 * @param ptr a user-provide pointer that will be passed to the callback
 * 
 * @return 0 if the callback was called for each recipient, -1 if there
 * was an error and any other != 0 number the callback returned
 */
int mu_msg_gmime_get_contacts_foreach (MuMsgGMime *msg,
				       MuMsgGMimeContactsCallback cb,
				       void *ptr);
G_END_DECLS

#endif /*__MU_MSG_H__*/
