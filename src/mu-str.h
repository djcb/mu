/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

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

#ifndef __MU_STR_H__
#define __MU_STR_H__

#include <time.h>
#include <sys/types.h>

#include <mu-msg.h>
#include <mu-msg-flags.h>

G_BEGIN_DECLS

/**
 * get a string for a given time_t
 * 
 * mu_str_date_s returns a ptr to a static buffer,
 * while mu_str_date returns dynamically allocated
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
const char* mu_str_date_s (const char* frm, time_t t) G_GNUC_CONST;
char*       mu_str_date   (const char* frm, time_t t) G_GNUC_WARN_UNUSED_RESULT;


/**
 * get a display string for a given time_t; if the given is less than
 * 24h from the current time, we display the time, otherwise the date,
 * using the preferred date/time for the current locale
 * 
 * mu_str_display_date_s returns a ptr to a static buffer,
 *
 * @param t the time as time_t
 * 
 * @return a string representation of the time/date
 */
const char* mu_str_display_date_s (time_t t);


/**
 * create a 'display contact' from an email header To/Cc/Bcc/From-type address
 * ie., turn
 *     "Foo Bar" <foo@bar.com>
 * into
 *      Foo Bar
 * Note that this is based on some simple heuristics. Max length is 255 bytes.
 *
 *   mu_str_display_contact_s returns a statically allocated
 *   buffer (ie, non-reentrant), while mu_str_display_contact
 *   returns a newly allocated string that you must free with g_free
 *   when done with it.
 * 
 * @param str a 'contact str' (ie., what is in the To/Cc/Bcc/From fields), or NULL
 * 
 * @return a newly allocated string with a display contact
 */
const char* mu_str_display_contact_s (const char *str);
char *mu_str_display_contact (const char *str);


/**
 * get a display size for a given size_t; uses M for sizes >
 * 1000*1000, k for smaller sizes. Note: this function use the
 * 10-based SI units, _not_ the powers-of-2 based ones.
 * 
 * mu_str_size_s returns a ptr to a static buffer,
 * while mu_str_size returns dynamically allocated
 * memory that must be freed after use.
 *
 * @param t the size as an size_t
 * 
 * @return a string representation of the size; see above
 * for what to do with it
 */
const char* mu_str_size_s  (size_t s) G_GNUC_CONST;
char*       mu_str_size    (size_t s) G_GNUC_WARN_UNUSED_RESULT;

/**
 * get a display string for a given set of flags, OR'ed in 
 * @param flags; one character per flag:
 * D=draft,F=flagged,N=new,P=passed,R=replied,S=seen,T=trashed 
 * a=has-attachment,s=signed, x=encrypted
 * 
 * mu_str_file_flags_s  returns a ptr to a static buffer,
 * while mu_str_file_flags returns dynamically allocated
 * memory that must be freed after use.
 *
 * @param flags file flags
 * 
 * @return a string representation of the flags; see above
 * for what to do with it
 */
const char* mu_str_flags_s  (MuMsgFlags flags) G_GNUC_CONST;
char*       mu_str_flags    (MuMsgFlags flags)
    G_GNUC_MALLOC G_GNUC_WARN_UNUSED_RESULT;

/**
 * get a 'summary' of the string, ie. the first /n/ lines of the
 * strings, with all newlines removed, replaced by single spaces
 * 
 * @param str the source string
 * @param max_lines the maximum number of lines to include in the summary
 * 
 * @return a newly allocated string with the summary. use g_free to free it.
 */
char* mu_str_summarize (const char* str, size_t max_lines)
    G_GNUC_MALLOC G_GNUC_WARN_UNUSED_RESULT;

/**
 * normalize a string (ie., collapse accented characters etc.), and
 * optionally, downcase it. Works for accented chars in Unicode Blocks
 * 'Latin-1 Supplement' and 'Latin Extended-A'
 *
 * @param str a valid utf8 string or NULL
 * @param downcase if TRUE, convert the string to lowercase 
 * 
 * @return the normalize string, or NULL in case of error or str was NULL
 */
char* mu_str_normalize (const char *str, gboolean downcase)
    G_GNUC_MALLOC G_GNUC_WARN_UNUSED_RESULT;



/**
 * normalize a string (ie., collapse accented characters etc.), and
 * optionally, downcase it. this happen by changing the string; if
 * that is not desired, use mu_str_normalize. Works for accented chars
 * in Unicode Blocks 'Latin-1 Supplement' and 'Latin Extended-A'
 * 
 * @param str a valid utf8 string or NULL
 * @param downcase if TRUE, convert the string to lowercase
 *  
 * @return the normalized string, or NULL in case of error or str was
 * NULL
 */
char* mu_str_normalize_in_place (char *str, gboolean downcase);

/**
 * escape the string for use with xapian matching. in practice, if the
 * string contains an '@', replace '@', single-'.' with '_'. Also,
 * replace ':' with '_', if it's not following a xapian-prefix (such
 * as 'subject:', 't:' etc, as defined in mu-msg-fields.[ch]).
 * changing is done in-place (by changing the argument string). in
 * any case, the string will be downcased.
 *
 * works for ascii strings, like e-mail addresses and message-id.
 * 
 * @param query a query string
 * 
 * @return the escaped string or NULL in case of error
 */
char* mu_str_ascii_xapian_escape_in_place (char *query);

/**
 * escape the string for use with xapian matching. in practice, if the
 * string contains an '@', replace '@', single-'.' with '_'. Also,
 * replace ':' with '_', if it's not following a xapian-prefix (such
 * as 'subject:', 't:' etc, as defined in mu-msg-fields.[ch]).
 *
 * works for ascii strings, like e-mail addresses and message-id.
 * 
 * @param query a query string
 * 
 * @return the escaped string (free with g_free) or NULL in case of error
 */
char* mu_str_ascii_xapian_escape (const char *query)
        G_GNUC_WARN_UNUSED_RESULT;

/**
 * 
 * parse strings like 1h, 3w, 2m to mean '1 hour before now', '3 weeks
 * before now' and '2 * 30 days before now'
 * 
 * the format is <n>(h|d|w|m|y), where <n> is an integer > 0, and
 * h=hour, d=day, w=week, m=30 days, year=365 days. function returns
 * *now* minus this value as time_t (UTC)
 *
 * if the number cannot be parsed, return (time_t)-1
 * 
 * @param str a str
 * 
 * @return the time_t of the point in time indicated by 'now' minus
 * the value, or (time_t)-1 otherwise
 */
time_t mu_str_date_parse_hdwmy (const char* str);



/**
 * parse a byte size; a size is a number, with optionally a
 * unit. Units recognized are K (1000) and M (1000*1000). Only the
 * first letter is checked and the function is not case-sensitive, so
 * 1000Kb, 3M will work equally well.  Note, for kB, MB etc., we then
 * follow the SI standards, not 2^10 etc.
 *
 * practical sizes for email messages are in terms of Mb; even in
 * extreme cases it should be under 100 Mb. Function return
 * GUINT64_MAX if there a parsing error
 * 
 * @param str a string with a size, such a "100", "100Kb", "1Mb"
 * 
 * @return the corresponding time_t value (as a guint64)
 */
guint64 mu_str_size_parse_kmg (const char* str);

/**
 * create a full path from a path + a filename. function is _not_
 * reentrant.
 * 
 * @param path a path (!= NULL)
 * @param name a name (may be NULL)
 * 
 * @return the path as a statically allocated buffer. don't free.
 */
const char* mu_str_fullpath_s (const char* path, const char* name);


/**
 * escape a string like a string literal in C; ie. replace \ with \\,
 * and " with \"
 * 
 * @param str a non-NULL str
 * 
 * @return the escaped string, newly allocated (free with g_free)
 */
char* mu_str_escape_c_literal (const gchar* str)
        G_GNUC_WARN_UNUSED_RESULT;


/**
 * macro to check whether the string is empty, ie. if it's NULL or
 * it's length is 0
 * 
 * @param S a string
 * 
 * @return TRUE if the string is empty, FALSE otherwise
 */
#define mu_str_is_empty(S) ((!(S)||!(*S))?TRUE:FALSE)



/**
 * guess some nick name for the given name; if we can determine an
 * first name, last name, the nick will be first name + the first char
 * of the last name. otherwise, it's just the first name. clearly,
 * this is just a rough guess for setting an initial value for nicks.
 * 
 * @param name a name
 * 
 * @return the guessed nick, as a newly allocated string (free with g_free)
 */
gchar* mu_str_guess_nick (const char* name)
        G_GNUC_WARN_UNUSED_RESULT;


/**
 * guess the first name for the given name; clearly,
 * this is just a rough guess for setting an initial value.
 * 
 * @param name a name
 * 
 * @return the first name, as a newly allocated string (free with
 * g_free)
 */
gchar* mu_str_guess_first_name (const char* name)
        G_GNUC_WARN_UNUSED_RESULT;



/**
 * guess the last name for the given name; clearly,
 * this is just a rough guess for setting an initial value.
 * 
 * @param name a name
 * 
 * @return the last name, as a newly allocated string (free with
 * g_free)
 */
gchar* mu_str_guess_last_name (const char* name)
        G_GNUC_WARN_UNUSED_RESULT;

G_END_DECLS

#endif /*__MU_STR_H__*/
