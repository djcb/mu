/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

/*
** Copyright (C) 2008-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <mu-flags.h>

G_BEGIN_DECLS

/**
 * @addtogroup MuStr
 * Various string utilities
 * @{
 */

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
 * @param str a 'contact str' (ie., what is in the To/Cc/Bcc/From
 * fields), or NULL
 *
 * @return a newly allocated string with a display contact
 */
const char* mu_str_display_contact_s (const char *str) G_GNUC_CONST;
char *mu_str_display_contact (const char *str) G_GNUC_WARN_UNUSED_RESULT;


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
 * Replace all occurences of substr in str with repl
 *
 * @param str a string
 * @param substr some string to replace
 * @param repl a replacement string
 *
 * @return a newly allocated string with the substr replaced by repl; free with g_free
 */
char *mu_str_replace (const char *str, const char *substr, const char *repl);


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
const char* mu_str_flags_s  (MuFlags flags) G_GNUC_CONST;
char*       mu_str_flags    (MuFlags flags)
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
 * Process some text (e.g. message bodies) -- flatten (remove accents
 * etc.), and remove some punctuation.
 *
 * @param text some text
 *
 * @return the processed text, free with g_free
 */
char* mu_str_process_text (const char *text)
	 G_GNUC_MALLOC G_GNUC_WARN_UNUSED_RESULT;

/**
 * Process some term (e.g., an e-mail address, subject field):
 * remove accents, replace some punctuation by _
 *
 * @param term some term
 *
 * @return the processed text, free with g_free
 */
char* mu_str_process_term (const char *term)
	 G_GNUC_MALLOC G_GNUC_WARN_UNUSED_RESULT;


/**
 * Process some query term (e.g., an e-mail address, subject field):
 * remove accents, replace some punctuation by _, but leave some query
 * metachars alone.
 *
 * @param qterm some query term
 *
 * @return the processed text, free with g_free
 */
char* mu_str_process_query_term (const char *qterm)
	 G_GNUC_MALLOC G_GNUC_WARN_UNUSED_RESULT;


/**
 * Handle the message-id in a special way
 *
 * @param str the message-id str
 * @param query is this a query?
 *
 * @return the massaged message-id
 */
char* mu_str_process_msgid (const char *str, gboolean query);



/**
 * Fixup values for some fields in the DWIM manner:
 * - if term is date:YYYYMMDD, replace it with the range
 *   date:YYYYMMDD..YYYYMMDD.
 *
 * @param query a query string
 *
 * @return the fixup'd string that must be g_free()d
 * after use or NULL in case of error.
 */
gchar* mu_str_xapian_fixup_terms (const gchar *term);

/**
 * parse a byte size; a size is a number, with optionally a
 * unit. Units recognized are b/B (bytes) k/K (1000) and m/M
 * (1000*1000). Only the first letter is checked and the function is
 * not case-sensitive, so 1000Kb, 3M will work equally well.  Note,
 * for kB, MB etc., we then follow the SI standards, not 2^10 etc. The
 * 'b' may be omitted.
 *
 * practical sizes for email messages are in terms of Mb; even in
 * extreme cases it should be under 100 Mb. Function return
 * GUINT64_MAX if there a parsing error
 *
 * @param str a string with a size, such a "100", "100Kb", "1Mb"
 *
 * @return the corresponding size in bytes, or -1 in case of error
 */
gint64 mu_str_size_parse_bkm (const char* str);

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
 * @param in_quotes whether the result should be enclosed in ""
 *
 * @return the escaped string, newly allocated (free with g_free)
 */
char* mu_str_escape_c_literal (const gchar* str, gboolean in_quotes)
        G_GNUC_WARN_UNUSED_RESULT;



/**
 * turn a string into plain ascii by replacing each non-ascii
 * character with a dot ('.'). Replacement is done in-place.
 *
 * @param buf a buffer to asciify
 *
 * @return the buf ptr (as to allow for function composition)
 */
char* mu_str_asciify_in_place (char *buf);


/**
 * turn string in buf into valid utf8. If this string is not valid
 * utf8 already, the function massages the offending characters.
 *
 * @param buf a buffer to utf8ify
 *
 * @return a newly allocated utf8 string
 */
char* mu_str_utf8ify (const char *buf);


/**
 * convert a string in a certain charset into utf8
 *
 * @param buffer a buffer to convert
 * @param charset source character set.
 *
 * @return a UTF8 string (which you need to g_free when done with it),
 * or NULL in case of error
 */
gchar* mu_str_convert_to_utf8 (const char* buffer, const char *charset);



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
 * convert a GSList of strings to a #sepa-separated list
 *
 * @param lst a GSList
 * @param the separator character
 *
 * @return a newly allocated string
 */
char* mu_str_from_list (const GSList *lst, char sepa);


/**
 * convert a #sepa-separated list of strings in to a GSList
 *
 * @param str a #sepa-separated list of strings
 * @param the separator character
 * @param remove leading/trailing whitespace from the string
 *
 * @return a newly allocated GSList (free with mu_str_free_list)
 */
GSList* mu_str_to_list (const char *str, char sepa, gboolean strip);


/**
 * convert a string (with possible escaping) to a list. list items are
 * separated by one or more spaces. list items can be quoted (using
 * '"').
 *
 * @param str a string
 *
 * @return a list of elements or NULL in case of error, free with
 * mu_str_free_list
 */
GSList* mu_str_esc_to_list (const char *str);



/**
 * Parse a list of <key>:<value> arguments, where <value> supports
 * quoting and escaping.
 *
 * @param args a list of arguments
 * @param err receives error information
 *
 * @return a hash table with key->value, or NULL in case of
 * error. Free with g_hash_table_destroy.
 */
GHashTable* mu_str_parse_arglist (const char *args, GError **err)
G_GNUC_WARN_UNUSED_RESULT;


/**
 * free a GSList consisting of allocated strings
 *
 * @param lst a GSList
 */
void mu_str_free_list (GSList *lst);



/**
 * strip the subject of Re:, Fwd: etc.
 *
 * @param str a subject string
 *
 * @return a new string -- this is pointing somewhere inside the @str;
 * no copy is made, don't free
 */
const gchar* mu_str_subject_normalize (const gchar* str);


/**
 * take a list of strings, and return the concatenation of their
 * quoted forms
 *
 * @param params NULL-terminated array of strings
 *
 * @return the quoted concatenation of the strings
 */
gchar* mu_str_quoted_from_strv (const gchar **params);




/**
 * Remove control characters from a string
 *
 * @param str a string
 *
 * @return the str with control characters removed
 */
char* mu_str_remove_ctrl_in_place (char *str);


/** @} */

G_END_DECLS

#endif /*__MU_STR_H__*/
