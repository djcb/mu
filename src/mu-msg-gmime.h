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

#ifndef __MU_MSG_GMIME_H__
#define __MU_MSG_GMIME_H__

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
 * read access to its properties; call mu_msg_destroy when done with it.
 *
 * @param path full path to an email message file
 *
 * @param mdir the maildir for this message; ie, if the path is
 * ~/Maildir/foo/bar/cur/msg, the maildir would be foo/bar; you can
 * pass NULL for this parameter, in which case some maildir-specific
 * information is not available.
 * 
 * @return a new MuMsgGMime instance or NULL in case of error
 */
MuMsgGMime*   mu_msg_gmime_new		   (const char* filepath,
					    const char *maildir);


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
 * @param msg a valid MuMsgGMime* instance
 * 
 * @return the plain text body or NULL in case of error or if there is no
 * such body. the returned string should *not* be modified or freed.
 * The returned data is in UTF8 or NULL.
 */
const char*     mu_msg_gmime_get_body_text       (MuMsgGMime *msg);


/**
 * get the html body of this message
 *
 * @param msg a valid MuMsgGMime* instance
 * 
 * @return the html body or NULL in case of error or if there is no
 * such body. the returned string should *not* be modified or freed.
 */
const char*     mu_msg_gmime_get_body_html       (MuMsgGMime *msg);


/**
 * get a summary of the body of this message; a summary takes up to
 * the first @max_lines from the message, and replaces newlines
 * etc. with single whitespace characters. This only works if there's
 * a text-body for the message
 *
 * @param msg a valid MuMsgGMime* instance
 * @param max_lines the max number of lines to summarize
 * 
 * @return the summary of the message or NULL in case of error or if
 * there is no such body. the returned string should *not* be modified
 * or freed. When called multiple times, the function will always
 * return the same summary, regardless of a different max_lines value.
 */
const char*     mu_msg_gmime_get_summary (MuMsgGMime *msg, size_t max_lines);

struct _MuMsgGMimeAttach {
	unsigned    index;     /* index of the attachment (names may not be unique) */
	const char* name;      /* name of the attachment */
	size_t      size;      /* size in bytes, or 0 if not available */
	const char* mime_type; /* the mime type */
};
typedef struct _MuMsgGMimeAttach MuMsgGMimeAttach;

typedef gboolean (*MuMsgGMimeAttachForeachFunc) (MuMsgGMimeAttach *att, gpointer data);

/**
 * call a user function for each attachment found in the message. the
 * function will be calle d for each attachment and as long a the
 * user-function returns TRUE
 * 
 * @param msg a valid MuMsgGMime
 * @param the function to call (callback)
 * @param user_data a user pointer which will be passed to the callback function
 * 
 * @return a list of attachment 
 */
void 	mu_msg_gmime_attach_foreach (MuMsgGMime* msg,
				     MuMsgGMimeAttachForeachFunc func,
				     gpointer user_data);

/**
 * save a specific attachment to some targetdir 
 * 
 * @param msg a valid MuMsgGMime instance
 * @param index index of the attachment you want to save
 * @param targetdir filesystem directory to save the attachment
 * 
 * @return TRUE if saving succeeded, FALSE otherwise
 */
gboolean	mu_msg_gmime_save_attachment (MuMsgGMime *msg, unsigned num,
					      const char *targetdir);

/**
 * get the sender (From:) of this message
 *
 * @param msg a valid MuMsgGMime* instance
 * 
 * @return the sender of this Message or NULL in case of error or if there 
 * is no sender. the returned string should *not* be modified or freed.
 */
const char*     mu_msg_gmime_get_from	   (MuMsgGMime *msg);


/**
 * get the recipients (To:) of this message
 *
 * @param msg a valid MuMsgGMime* instance
 * 
 * @return the sender of this Message or NULL in case of error or if there 
 * are no recipients. the returned string should *not* be modified or freed.
 */
const char*     mu_msg_gmime_get_to	   (MuMsgGMime *msg);


/**
 * get the carbon-copy recipients (Cc:) of this message
 *
 * @param msg a valid MuMsgGMime* instance
 * 
 * @return the Cc: recipients of this Message or NULL in case of error or if 
 * there are no such recipients. the returned string should *not* be modified 
 * or freed.
 */
const char*     mu_msg_gmime_get_cc	     (MuMsgGMime *msg);

/**
 * get the file system path of this message
 *
 * @param msg a valid MuMsgGMime* instance
 * 
 * @return the path of this Message or NULL in case of error. 
 * the returned string should *not* be modified or freed.
 */
const char*     mu_msg_gmime_get_path            (MuMsgGMime *msg);


/**
 * get the maildir this message lives in; ie, if the path is
 * ~/Maildir/foo/bar/cur/msg, the maildir would be foo/bar
 *
 * @param msg a valid MuMsgGMime* instance
 * 
 * @return the maildir requested or NULL in case of error. The returned
 * string should *not* be modified or freed.
 */
const char*    mu_msg_gmime_get_maildir        (MuMsgGMime *msg);


/**
 * get the subject of this message
 *
 * @param msg a valid MuMsgGMime* instance
 * 
 * @return the subject of this Message or NULL in case of error or if there 
 * is no subject. the returned string should *not* be modified or freed.
 */
const char*     mu_msg_gmime_get_subject         (MuMsgGMime *msg);

/**
 * get the Message-Id of this message
 *
 * @param msg a valid MuMsgGMime* instance
 * 
 * @return the Message-Id of this Message or NULL in case of error or if there 
 * is none. the returned string should *not* be modified or freed.
 */
const char*     mu_msg_gmime_get_msgid           (MuMsgGMime *msg);


/**
 * get any arbitrary header from this message
 *
 * @param msg a valid MuMsgGMime* instance
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
 * @param msg a valid MuMsgGMime* instance
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
 * @param msg a valid MuMsgGMime* instance
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
 * @param msg a valid MuMsgGMime* instance
 * 
 * @return the message priority (!= 0) or 0 in case of error
 */
MuMsgPriority   mu_msg_gmime_get_priority        (MuMsgGMime *msg);

/**
 * get the timestamp (mtime) for the file containing this message 
 *
 * @param msg a valid MuMsgGMime* instance
 * 
 * @return the timestamp or 0 in case of error
 */
time_t          mu_msg_gmime_get_timestamp       (MuMsgGMime *msg);

typedef gboolean  (*MuMsgGMimeContactsForeachFunc) (MuMsgContact* contact,
						    gpointer user_data);

/**
 * call a function for each of the contacts in a message 
 *
 * @param msg a valid MuMsgGMime* instance
 * @param func a callback function to call for each contact; when
 * the callback does not return TRUE, it won't be called again
 * @param user_data a user-provide pointer that will be passed to the callback
 * 
 */
void mu_msg_gmime_contacts_foreach (MuMsgGMime *msg,
				    MuMsgGMimeContactsForeachFunc func,
				    gpointer user_data);
G_END_DECLS

#endif /*__MU_MSG_GIME_H__*/
