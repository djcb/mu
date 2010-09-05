/* 
** Copyright (C) 2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
**
** This program is free software; you can redistribute it and/or modify it
** under the terms of the GNU General Public License as published by the
** Free Software Foundation; either version 3, or (at your option) any
** later version.
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

#ifndef __MU_MSG_FLAGS_H__
#define __MU_MSG_FLAGS_H__

#include <glib.h>

G_BEGIN_DECLS

enum _MuMsgFlags {	
	MU_MSG_FLAG_UNKNOWN     = 0,
	MU_MSG_FLAG_NONE        = 1 << 0,
	
	/* these we get from the file */
	MU_MSG_FLAG_NEW		= 1 << 1,
	MU_MSG_FLAG_SEEN	= 1 << 2,
	MU_MSG_FLAG_UNREAD	= 1 << 3,
	MU_MSG_FLAG_REPLIED	= 1 << 4,
	MU_MSG_FLAG_FLAGGED	= 1 << 5,
	MU_MSG_FLAG_TRASHED	= 1 << 6,
	MU_MSG_FLAG_DRAFT	= 1 << 7,
	MU_MSG_FLAG_PASSED	= 1 << 8,

	/* these we get from the contents */
	MU_MSG_FLAG_SIGNED      = 1 << 10,
	MU_MSG_FLAG_ENCRYPTED   = 1 << 11,
	MU_MSG_FLAG_HAS_ATTACH  = 1 << 12

	/* any new fields go here  */
	/* so the existing numbers stay valid note that we're also */
	/* using these numbers in the database, so they should not change */
};
typedef enum _MuMsgFlags MuMsgFlags;

/** 
 * convert the char-per-flag description into a MuMsgFlags value; the characters
 * D=draft,F=flagged,N=new,P=passed,R=replied,S=seen,T=trashed 
 * a=has-attachment,s=signed, x=encrypted
 * if any other characters are seen, MU_MSG_FLAG_UNKNOWN is returned.
 * 
 * @param str a string
 * 
 * @return a MuMSgFlags value, or MU_MSG_FLAG_UNKNOWN in case of error
 */
MuMsgFlags mu_msg_flags_from_str  (const char* str) G_GNUC_PURE;

/** 
 * convert the char-per-flag description into a MuMsgFlags value
 * 
 * @param c a character
 * 
 * @return a MuMSgFlags value, or MU_MSG_FLAG_UNKNOWN in case of error
 */
MuMsgFlags  mu_msg_flags_from_char (char c) G_GNUC_CONST;

/** 
 * get a string for a given set of flags, OR'ed in 
 * @param flags; one character per flag:
 * D=draft,F=flagged,N=new,P=passed,R=replied,S=seen,T=trashed 
 * a=has-attachment,s=signed, x=encrypted
 * 
 * mu_msg_flags_to_str_s returns a ptr to a static buffer, ie.,
 * ie, is not re-entrant. copy the string if needed.
 *
 * @param flags file flags
 * 
 * @return a string representation of the flags
 */
const char* mu_msg_flags_to_str_s  (MuMsgFlags flags) G_GNUC_CONST;

/** 
 * get the Maildir flags from a mailfile. The flags are as specified
 * in http://cr.yp.to/proto/maildir.html, plus MU_MSG_FLAG_NEW for
 * new messages, ie the ones that live in new/. The flags are
 * logically OR'ed. Note that the file does not have to exist; the
 * flags are based on the name only. 
 *
 * @param pathname of a mailfile; it does not have to refer to an
 * actual message
 * 
 * @return the flags, or MU_MSG_FILE_FLAG_UNKNOWN in case of error
 */
MuMsgFlags mu_msg_flags_from_file (const char* pathname) G_GNUC_PURE;

/** 
 * is the message flag a file flag? ie. encoded in the filename
 *
 * @param flag the flag to check; note, this should be a single flag
 * not some flags OR'ed together
 * 
 * @return TRUE if it is a file flag, FALSE otherwise
 */
gboolean   mu_msg_flags_is_file_flag (MuMsgFlags flag) G_GNUC_CONST;

G_END_DECLS

#endif /*__MU_MSG_FLAGS_H__*/


