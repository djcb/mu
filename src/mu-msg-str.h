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

#ifndef __MU_MSG_STR_H__
#define __MU_MSG_STR_H__

#include <time.h>
#include <sys/types.h>

#include "mu-msg.h"
#include "mu-msg-flags.h"

G_BEGIN_DECLS

/**
 * get a string for a given time_t
 * 
 * mu_msg_str_date_s returns a ptr to a static buffer,
 * while mu_msg_str_date returns dynamically allocated
 * memory that must be freed after use.
 *
 * @param frm the format of the string (in strftime(3) format) 
 * @param t the time as time_t
 * 
 * @return a string representation of the time; see above for what to
 * do with it. Lenght is max. 128 bytes, inc. the ending \0.  if the
 * format is too long, the value will be truncated. in practice this
 * should not happen.
 */
const char* mu_msg_str_date_s (const char* frm, time_t t) G_GNUC_CONST;
char*       mu_msg_str_date   (const char* frm, time_t t) G_GNUC_WARN_UNUSED_RESULT;


/**
 * get a display string for a given time_t; if the given is less than
 * 24h from the current time, we display the time, otherwise the date,
 * using the preferred date/time for the current locale
 * 
 * mu_msg_str_display_date_s returns a ptr to a static buffer,
 *
 * @param t the time as time_t
 * 
 * @return a string representation of the time/date
 */
const char* mu_msg_str_display_date_s (time_t t);


/**
 * create a 'display contact' from an email header To/Cc/Bcc/From-type address
 * ie., turn
 *     "Foo Bar" <foo@bar.com>
 * into
 *      Foo Bar
 * Note that this is based on some simple heuristics. Max length is 255 bytes.
 *
 *   mu_msg_str_display_contact_s returns a statically allocated
 *   buffer (ie, non-reentrant), while mu_msg_str_display_contact
 *   returns a newly allocated string that you must free with g_free
 *   when done with it.
 * 
 * @param str a 'contact str' (ie., what is in the To/Cc/Bcc/From fields), or NULL
 * 
 * @return a newly allocated string with a display contact
 */
const char* mu_msg_str_display_contact_s (const char *str);
char *mu_msg_str_display_contact (const char *str);


/**
 * get a display size for a given size_t; uses M for sizes >
 * 1000*1000, k for smaller sizes. Note: this function use the
 * 10-based SI units, _not_ the powers-of-2 based ones.
 * 
 * mu_msg_str_size_s returns a ptr to a static buffer,
 * while mu_msg_str_size returns dynamically allocated
 * memory that must be freed after use.
 *
 * @param t the size as an size_t
 * 
 * @return a string representation of the size; see above
 * for what to do with it
 */
const char* mu_msg_str_size_s  (size_t s) G_GNUC_CONST;
char*       mu_msg_str_size    (size_t s) G_GNUC_WARN_UNUSED_RESULT;

/**
 * get a display string for a given set of flags, OR'ed in 
 * @param flags; one character per flag:
 * D=draft,F=flagged,N=new,P=passed,R=replied,S=seen,T=trashed 
 * a=has-attachment,s=signed, x=encrypted
 * 
 * mu_msg_str_file_flags_s  returns a ptr to a static buffer,
 * while mu_msg_str_file_flags returns dynamically allocated
 * memory that must be freed after use.
 *
 * @param flags file flags
 * 
 * @return a string representation of the flags; see above
 * for what to do with it
 */
const char* mu_msg_str_flags_s  (MuMsgFlags flags) G_GNUC_CONST;
char*       mu_msg_str_flags    (MuMsgFlags flags) G_GNUC_WARN_UNUSED_RESULT;


/**
 * get a display string for a message priority; either
 * high, low or normal
 *
 * @param flags file flags
 * 
 * @return a string representation of the priority; see above
 * for what to do with it, or NULL in case of error
 */
const char* mu_msg_str_prio  (MuMsgPrio prio) G_GNUC_CONST;


/**
 * get a 'summary' of the string, ie. the first /n/ lines of the
 * strings, with all newlines removed, replaced by single spaces
 * 
 * @param str the source string
 * @param max_lines the maximum number of lines to include in the summary
 * 
 * @return a newly allocated string with the summary. use g_free to free it.
 */
char* mu_msg_str_summarize (const char* str,
			    size_t max_lines) G_GNUC_WARN_UNUSED_RESULT;



/**
 * normalize a string (ie., collapse accented characters etc.), and
 * optionally, downcase it
 *
 * 
 * @param str a valid utf8 string or NULL
 * @param downcase if TRUE, convert the string to lowercase
 * 
 * @return the normalize string, or NULL in case of error or str was NULL
 */
char* mu_msg_str_normalize (const char *str, gboolean downcase);


/**
 * normalize a string (ie., collapse accented characters etc.), and
 * optionally, downcase it. this happen by changing the string; if
 * that is not desired, use mu_msg_str_normalize
 * 
 * @param str a valid utf8 string or NULL
 * @param downcase if TRUE, convert the string to lowercase
 * 
 * @return the normalize string, or NULL in case of error or str was NULL
 */
char* mu_msg_str_normalize_in_place (char *str, gboolean downcase);


G_END_DECLS

#endif /*__MU_MSG_STR_H__*/
