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

#ifndef __MU_MSG_H__
#define __MU_MSG_H__

#include "mu-msg-flags.h"
#include "mu-msg-fields.h"
#include "mu-msg-status.h"
#include "mu-msg-prio.h"

G_BEGIN_DECLS

struct _MuMsg;
typedef struct _MuMsg MuMsg;

/**
 * create a new MuMsg* instance which parses a message and provides
 * read access to its properties; call mu_msg_destroy when done with it.
 *
 * @param path full path to an email message file
 *
 * @param mdir the maildir for this message; ie, if the path is
 * ~/Maildir/foo/bar/cur/msg, the maildir would be foo/bar; you can
 * pass NULL for this parameter, in which case some maildir-specific
 * information is not available.
 * 
 * @return a new MuMsg instance or NULL in case of error
 */
MuMsg*   mu_msg_new		   (const char* filepath,
				    const char *maildir);


/**
 * destroy a MuMsg* instance; call this function when done with
 * a MuMsg
 * 
 * @param msg a MuMsg* instance or NULL
 */
void     mu_msg_destroy         (MuMsg *msg);


/**
 * get the plain text body of this message
 *
 * @param msg a valid MuMsg* instance
 * 
 * @return the plain text body or NULL in case of error or if there is no
 * such body. the returned string should *not* be modified or freed.
 * The returned data is in UTF8 or NULL.
 */
const char*     mu_msg_get_body_text       (MuMsg *msg);


/**
 * get the html body of this message
 *
 * @param msg a valid MuMsg* instance
 * 
 * @return the html body or NULL in case of error or if there is no
 * such body. the returned string should *not* be modified or freed.
 */
const char*     mu_msg_get_body_html       (MuMsg *msg);


/**
 * get a summary of the body of this message; a summary takes up to
 * the first @max_lines from the message, and replaces newlines
 * etc. with single whitespace characters. This only works if there's
 * a text-body for the message
 *
 * @param msg a valid MuMsg* instance
 * @param max_lines the max number of lines to summarize
 * 
 * @return the summary of the message or NULL in case of error or if
 * there is no such body. the returned string should *not* be modified
 * or freed. When called multiple times, the function will always
 * return the same summary, regardless of a different max_lines value.
 */
const char*     mu_msg_get_summary (MuMsg *msg, size_t max_lines);

/**
 * save a specific attachment to some targetdir 
 * 
 * @param msg a valid MuMsg instance
 * @param wanted_idx index of the attachment you want to save
 * @param targetdir filesystem directory to save the attachment
 * @param overwrite existing files?
 * 
 * @return TRUE if saving succeeded, FALSE otherwise
 */
gboolean
mu_msg_mime_part_save (MuMsg *msg, unsigned wanted_idx,
		       const char *targetdir, gboolean overwrite);

/**
 * get the sender (From:) of this message
 *
 * @param msg a valid MuMsg* instance
 * 
 * @return the sender of this Message or NULL in case of error or if there 
 * is no sender. the returned string should *not* be modified or freed.
 */
const char*     mu_msg_get_from	   (MuMsg *msg);


/**
 * get the recipients (To:) of this message
 *
 * @param msg a valid MuMsg* instance
 * 
 * @return the sender of this Message or NULL in case of error or if there 
 * are no recipients. the returned string should *not* be modified or freed.
 */
const char*     mu_msg_get_to	   (MuMsg *msg);


/**
 * get the carbon-copy recipients (Cc:) of this message
 *
 * @param msg a valid MuMsg* instance
 * 
 * @return the Cc: recipients of this Message or NULL in case of error or if 
 * there are no such recipients. the returned string should *not* be modified 
 * or freed.
 */
const char*     mu_msg_get_cc	     (MuMsg *msg);

/**
 * get the file system path of this message
 *
 * @param msg a valid MuMsg* instance
 * 
 * @return the path of this Message or NULL in case of error. 
 * the returned string should *not* be modified or freed.
 */
const char*     mu_msg_get_path            (MuMsg *msg);


/**
 * get the maildir this message lives in; ie, if the path is
 * ~/Maildir/foo/bar/cur/msg, the maildir would be foo/bar
 *
 * @param msg a valid MuMsg* instance
 * 
 * @return the maildir requested or NULL in case of error. The returned
 * string should *not* be modified or freed.
 */
const char*    mu_msg_get_maildir        (MuMsg *msg);


/**
 * get the subject of this message
 *
 * @param msg a valid MuMsg* instance
 * 
 * @return the subject of this Message or NULL in case of error or if there 
 * is no subject. the returned string should *not* be modified or freed.
 */
const char*     mu_msg_get_subject         (MuMsg *msg);

/**
 * get the Message-Id of this message
 *
 * @param msg a valid MuMsg* instance
 * 
 * @return the Message-Id of this Message (without the enclosing <>)
 * or NULL in case of error or if there is none. the returned string
 * should *not* be modified or freed.
 */
const char*     mu_msg_get_msgid           (MuMsg *msg);


/**
 * get any arbitrary header from this message
 *
 * @param msg a valid MuMsg* instance
 * @header the header requested
 * 
 * @return the header requested or NULL in case of error or if there 
 * is no such header. the returned string should *not* be modified or freed.
 */
const char*     mu_msg_get_header          (MuMsg *msg, 
						  const char* header);

/**
 * get the message date/time (the Date: field) as time_t, using UTC
 *
 * @param msg a valid MuMsg* instance
 * 
 * @return message date/time or 0 in case of error or if there 
 * is no such header.
 */
time_t          mu_msg_get_date            (MuMsg *msg);

/**
 * get the flags for this message
 *
 * @param msg valid MuMsg* instance
 * 
 * @return the fileflags as logically OR'd #Mu MsgFlags or 0 if
 * there are none.
 */
MuMsgFlags     mu_msg_get_flags      (MuMsg *msg);


/**
 * get the file size in bytes of this message
 *
 * @param msg a valid MuMsg* instance
 * 
 * @return the filesize 
 */
size_t          mu_msg_get_size       (MuMsg *msg);


/**
 * get some field value as string
 * 
 * @param msg a valid MuMsg instance
 * @param field the field to retrieve; it must be a string-typed field
 * 
 * @return a string that should not be freed
 */
const char*  mu_msg_get_field_string  (MuMsg *msg, 
				       const MuMsgField* field);

/**
 * get some field value as string
 * 
 * @param msg a valid MuMsg instance
 * @param field the field to retrieve; it must be a numeric field
 * 
 * @return a string that should not be freed
 */
gint64      mu_msg_get_field_numeric (MuMsg *msg, 
				      const MuMsgField* field);

/**
 * get the message priority for this message (MU_MSG_PRIO_LOW,
 * MU_MSG_PRIO_NORMAL or MU_MSG_PRIO_HIGH) the X-Priority,
 * X-MSMailPriority, Importance and Precedence header are checked, in
 * that order.  if no known or explicit priority is set,
 * MU_MSG_PRIO_NORMAL is assumed
 *
 * @param msg a valid MuMsg* instance
 * 
 * @return the message priority (!= 0) or 0 in case of error
 */
MuMsgPrio   mu_msg_get_prio        (MuMsg *msg);

/**
 * get the timestamp (mtime) for the file containing this message 
 *
 * @param msg a valid MuMsg* instance
 * 
 * @return the timestamp or 0 in case of error
 */
time_t          mu_msg_get_timestamp       (MuMsg *msg);




G_END_DECLS

#endif /*__MU_MSG_H__*/
