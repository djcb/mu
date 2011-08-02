/* 
** Copyright (C) 2008-2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

/* the MU_MSG_FLAG_-flags are retrieved from the filename of
 * the message, as per the maildir spec:
 * http://cr.yp.to/proto/maildir.html not all (possibly not any) mail
 * clients respect this fully -- therefore, we map these flags to
 * MU_MSG_FLAG values.
 *
 * the other flags are determined from the contents of the
 * message. Note that these flags are far from orthogonal,
 * unfortunately.
 *
 */
enum _MuMsgFlags {
	MU_MSG_FLAG_NONE        = 0,
	
	/* unlike the other MAILDIR flags, the NEW flag is not
	 * determined by the file name but by the dir: if the message
	 * is in the 'new' directory, it's considered... new */
	MU_MSG_FLAG_NEW         = 1 << 0,
	
	/* "P"->resent,forwarded,bounced message */
	MU_MSG_FLAG_PASSED	= 1 << 1,
	/* "R"->replied message */
	MU_MSG_FLAG_REPLIED	= 1 << 2,
	/* "S"->seen message */
	MU_MSG_FLAG_SEEN	= 1 << 3,
	/* "T"->trashed message */	
	MU_MSG_FLAG_TRASHED	= 1 << 4,
	/* "D"->draft message */
	MU_MSG_FLAG_DRAFT	= 1 << 5,
	/* "F"->flagged message */
	MU_MSG_FLAG_FLAGGED	= 1 << 6,

	/* "U"->unread message; it's a pseudo/convenience flag that
	 * means (NEW or not SEEN) */
	MU_MSG_FLAG_UNREAD      = 1 << 7,
	
	/* these we get from the contents */
	
	/* "Z"->signed message */
	MU_MSG_FLAG_SIGNED      = 1 << 8,
	/* "X"->encrypted message */
	MU_MSG_FLAG_ENCRYPTED   = 1 << 9,
	/* "A"->message has attachment */
	MU_MSG_FLAG_HAS_ATTACH  = 1 << 10
};
typedef enum _MuMsgFlags MuMsgFlags;


/**
 * convert the char-per-flag description into a MuMsgFlags value; the
 * characters
 * D=draft,F=flagged,N=new,P=passed,R=replied,S=seen,T=trashed
 * a=has-attachment,s=signed, x=encrypted if any other characters are
 * seen, MU_MSG_FLAG_NONE is returned.
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
 * convert the char-per-flag description into a MuMsgFlags value. The
 * difference is that with 'mu_msg_flags_from_char' is that this
 * function only cares about the chars that are part of the
 * Maildir-standard (ie., in files), and that is ignores unknown chars
 * (returns 0), rather than printing warnings
 * 
 * @param c a character
 * 
 * @return a MuMSgFlags value, or 0 if the char is not known as a
 * maildir char
 */
MuMsgFlags mu_msg_flag_from_file_char (char k) G_GNUC_CONST;



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
const char* mu_msg_flags_str_s  (MuMsgFlags flags) G_GNUC_CONST;


/**
 * get the character for some msg flag, or 0 in case it's not found
 * 
 * @param flags on specific flag (not OR'ed)
 * 
 * @return the char for this flag, or 0 if not found
 */
char mu_msg_flag_char  (MuMsgFlags flag) G_GNUC_CONST;



/**
 * get a string describing this flag
 * 
 * @param flag a single flag (not OR'ed)
 * 
 * @return a string describing this flag, e.g.,
 * MU_MSG_FLAG_ATTACH=>"attach"
 */
const char* mu_msg_flag_name (MuMsgFlags flag) G_GNUC_CONST;


typedef void (*MuMsgFlagsForeachFunc)
	(MuMsgFlags flag, gpointer user_data);

/**
 * call a function for each available message flag
 * 
 * @param func a function to call (callback)
 * @param user_data  user pointer passed to the callback
 */
void mu_msg_flags_foreach (MuMsgFlagsForeachFunc func, gpointer user_data);


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


