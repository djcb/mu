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

#if HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/


#include <glib.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>

#include "mu-util.h" /* PATH_MAX */
#include "mu-str.h"
#include "mu-msg-fields.h"


const char*
mu_str_size_s  (size_t s)
{
	static char buf[32];

#ifdef HAVE_GLIB216
	char *tmp;

	tmp = g_format_size_for_display ((goffset)s);
	strncpy (buf, tmp, sizeof(buf));
	buf[sizeof(buf) -1] = '\0'; /* just in case */
	g_free (tmp);

#else
	if (s >= 1000 * 1000)
		g_snprintf(buf, sizeof(buf), "%.1f MB",
			   (double)s/(1000*1000));
	else
		g_snprintf(buf, sizeof(buf), "%.1f kB", (double)s/(1000));
#endif /*HAVE_GLIB216*/


	return buf;
}

char*
mu_str_size (size_t s)
{
	return g_strdup (mu_str_size_s(s));
}

const char*
mu_str_flags_s  (MuFlags flags)
{
	return mu_flags_to_str_s (flags, MU_FLAG_TYPE_ANY);
}

char*
mu_str_flags  (MuFlags flags)
{
	return g_strdup (mu_str_flags_s(flags));
}

char*
mu_str_summarize (const char* str, size_t max_lines)
{
	char *summary;
	size_t nl_seen;
	unsigned i,j;
	gboolean last_was_blank;

	g_return_val_if_fail (str, NULL);
	g_return_val_if_fail (max_lines > 0, NULL);

	/* len for summary <= original len */
	summary = g_new (gchar, strlen(str) + 1);

	/* copy the string up to max_lines lines, replace CR/LF/tab with
	 * single space */
	for (i = j = 0, nl_seen = 0, last_was_blank = TRUE;
	     nl_seen < max_lines && str[i] != '\0'; ++i) {

		if (str[i] == '\n' || str[i] == '\r' ||
		    str[i] == '\t' || str[i] == ' ' ) {

			if (str[i] == '\n')
				++nl_seen;

			/* no double-blanks or blank at end of str */
			if (!last_was_blank && str[i+1] != '\0')
				summary[j++] = ' ';

			last_was_blank = TRUE;
		} else {

			summary[j++] = str[i];
			last_was_blank = FALSE;
		}
	}

	summary[j] = '\0';
	return summary;
}


static void
cleanup_contact (char *contact)
{
	char *c, *c2;

	/* replace "'<> with space */
	for (c2 = contact; *c2; ++c2)
		if (*c2 == '"' || *c2 == '\'' || *c2 == '<' || *c2 == '>')
			*c2 = ' ';

	/* remove everything between '()' if it's after the 5th pos;
	 * good to cleanup corporate contact address spam... */
	c = g_strstr_len (contact, -1, "(");
	if (c && c - contact > 5)
		*c = '\0';

	g_strstrip (contact);
}


/* this is still somewhat simplistic... */
const char*
mu_str_display_contact_s (const char *str)
{
	static gchar contact[255];
	gchar *c, *c2;

	str = str ? str : "";
	g_strlcpy (contact, str, sizeof(contact));

	/* we check for '<', so we can strip out the address stuff in
	 * e.g. 'Hello World <hello@world.xx>, but only if there is
	 * something alphanumeric before the <
	 */
	c = g_strstr_len (contact, -1, "<");
	if (c != NULL) {
		for (c2 = contact; c2 < c && !(isalnum(*c2)); ++c2);
		if (c2 != c) /* apparently, there was something,
			      * so we can remove the <... part*/
			*c = '\0';
	}

	cleanup_contact (contact);

	return contact;
}

char*
mu_str_display_contact (const char *str)
{
	g_return_val_if_fail (str, NULL);

	return g_strdup (mu_str_display_contact_s (str));
}


gint64
mu_str_size_parse_bkm (const char* str)
{
	gint64 num;

	g_return_val_if_fail (str, -1);

	if (!isdigit(str[0]))
		return -1;

	num = atoi(str);
	for (++str; isdigit(*str); ++str);

	switch (tolower(*str)) {
	case '\0':
	case 'b' : return num;                      /* bytes */
	case 'k':  return num * 1000;               /* kilobyte */
	case 'm':  return num * 1000 * 1000;        /* megabyte */
	default:
		return -1;
	}
}


char*
mu_str_replace (const char *str, const char *substr, const char *repl)
{
	GString		*gstr;
	const char	*cur;

	g_return_val_if_fail (str, NULL);
	g_return_val_if_fail (substr, NULL);
	g_return_val_if_fail (repl, NULL);

	gstr = g_string_sized_new (2 * strlen (str));

	for (cur = str; *cur; ++cur) {
		if (g_str_has_prefix (cur, substr)) {
			g_string_append (gstr, repl);
			cur += strlen (substr) - 1;
		} else
			g_string_append_c (gstr, *cur);
	}

	return g_string_free (gstr, FALSE);
}






char*
mu_str_from_list (const GSList *lst, char sepa)
{
	const GSList *cur;
	char *str;

	g_return_val_if_fail (sepa, NULL);

	for (cur = lst, str = NULL; cur; cur = g_slist_next(cur)) {

		char *tmp;
		/* two extra dummy '\0' so -Wstack-protector won't complain */
		char sep[4] = { '\0', '\0', '\0', '\0' };
		sep[0] = cur->next ? sepa : '\0';

		tmp = g_strdup_printf ("%s%s%s",
				       str ? str : "",
				       (gchar*)cur->data,
				       sep);
		g_free (str);
		str = tmp;
	}

	return str;
}

GSList*
mu_str_to_list (const char *str, char sepa, gboolean strip)
{
	GSList *lst;
	gchar **strs, **cur;
	/* two extra dummy '\0' so -Wstack-protector won't complain */
	char sep[4] = { '\0', '\0', '\0', '\0' };

	g_return_val_if_fail (sepa, NULL);

	if (!str)
		return NULL;

	sep[0] = sepa;
	strs = g_strsplit (str, sep, -1);

	for (cur = strs, lst = NULL; cur && *cur; ++cur) {
		char *elm;
		elm = g_strdup(*cur);
		if (strip)
			elm = g_strstrip (elm);

		lst = g_slist_prepend (lst, elm);
	}

	lst = g_slist_reverse (lst);
	g_strfreev (strs);

	return lst;
}

GSList*
mu_str_esc_to_list (const char *strings)
{
	GSList *lst;
	GString *part;
	unsigned u;
	gboolean quoted, escaped;

	g_return_val_if_fail (strings, NULL);

	part = g_string_new (NULL);

	for (u = 0, lst = NULL, quoted = FALSE, escaped = FALSE;
	     u != strlen (strings); ++u) {

		char kar;
		kar = strings[u];

		if (kar == '\\') {
			if (escaped)
				g_string_append_c (part, '\\');
			escaped = !escaped;
			continue;
		}

		if (quoted && kar != '"') {
			g_string_append_c (part, kar);
			continue;
		}

		switch (kar) {
		case '"':
			if (!escaped)
				quoted = !quoted;
			else
				g_string_append_c (part, kar);
			continue;
		case ' ':
 			if (part->len > 0) {
				lst = g_slist_prepend
					(lst, g_string_free (part, FALSE));
				part = g_string_new (NULL);
			}
			continue;
		default:
			g_string_append_c (part, kar);
		}
	}

	if (part->len)
		lst = g_slist_prepend (lst, g_string_free (part, FALSE));

	return g_slist_reverse (lst);
}


void
mu_str_free_list (GSList *lst)
{
	g_slist_foreach (lst, (GFunc)g_free, NULL);
	g_slist_free (lst);
}


/* this function is critical for sorting performance; therefore, no
 * regexps, but just some good old c pointer magic */
const gchar*
mu_str_subject_normalize (const gchar* str)
{
	const char* cur;

	g_return_val_if_fail (str, NULL);

	cur = str;
	while (isspace(*cur)) ++cur; /* skip space */

	/* starts with Re:? */
	if (tolower(cur[0]) == 'r' && tolower(cur[1]) == 'e')
		cur += 2;
	/* starts with Fwd:? */
	else if (tolower(cur[0]) == 'f' && tolower(cur[1]) == 'w' &&
		 tolower(cur[2]) == 'd')
		cur += 3;
	else /* nope, different string */
		return str;

	/* we're now past either 'Re' or 'Fwd'. Maybe there's a [<num>] now?
	 * ie., the Re[3]: foo case */
	if (cur[0] == '[') { /* handle the Re[3]: case */
		if (isdigit(cur[1])) {
			do { ++cur; } while (isdigit(*cur));
			if ( cur[0] != ']') {
				return str; /* nope: no ending ']' */
			} else /* skip ']' and space */
				do { ++cur; } while (isspace(*cur));
		} else /* nope: no number after '[' */
			return str;
	}

	/* now, cur points past either 're' or 'fwd', possibly with
	 * [<num>]; check if it's really a prefix -- after re or fwd
	 * there should either a ':' and possibly some space */
	if (cur[0] == ':') {
		do { ++cur; } while (isspace(*cur));
		/* note: there may still be another prefix, such as
		 * Re[2]: Fwd: foo */
		return mu_str_subject_normalize (cur);
	} else
		return str; /* nope, it was not a prefix */
}


struct _CheckPrefix {
	const char *str;
	gboolean   match;
	gboolean   range_field;
};
typedef struct _CheckPrefix	 CheckPrefix;



static void
each_check_prefix (MuMsgFieldId mfid, CheckPrefix *cpfx)
{
	const char *pfx;
	char pfx_short[3] = { 'X', ':', '\0'};
	char k;

	if (!cpfx || cpfx->match)
		return;

	k = pfx_short[0] = mu_msg_field_shortcut (mfid);
	if (k && g_str_has_prefix (cpfx->str, pfx_short)) {
		cpfx->match = TRUE;
		cpfx->range_field = mu_msg_field_is_range_field (mfid);
	}

	pfx = mu_msg_field_name (mfid);
	if (pfx && g_str_has_prefix (cpfx->str, pfx) &&
	    cpfx->str[strlen(pfx)] == ':') {
		cpfx->match = TRUE;
		cpfx->range_field = mu_msg_field_is_range_field (mfid);
	}
}

/* check if it looks like either i:<msgid> or msgid:<msgid> */
static gboolean
is_msgid_field (const char *str)
{
	const char *name;

	if (!str || strlen(str) < 3)
		return FALSE;

	if (str[0] == mu_msg_field_shortcut (MU_MSG_FIELD_ID_MSGID) &&
	    str[1] == ':')
		return TRUE;

	name = mu_msg_field_name (MU_MSG_FIELD_ID_MSGID);
	if (g_str_has_prefix (str, name) && str[strlen(name)] == ':')
		return TRUE;

	return FALSE;
}

/* message-ids need a bit more massaging -- we replace all
 * non-alphanum with '_'. Note, this function assumes we're looking at
 * a msg-id field, ie. i:<msgid> or msgid:<msgid> */
char*
mu_str_process_msgid (const char *str, gboolean query)
{
	char *s, *c;

	g_return_val_if_fail (str, NULL);
	g_return_val_if_fail (!query || strchr(str, ':'), NULL);

	if (!str)
		return NULL;

	s = g_strdup (str);

	if (query)
		c = strchr (s, ':') + 1;
	else
		c = s;

	for (; *c; ++c)
		*c = isalnum (*c) ? tolower (*c) : '_';

	return s;
}



static void
check_for_field (const char *str, gboolean *is_field,
		 gboolean *is_range_field)
{
	CheckPrefix pfx;

	pfx.str   = str;

	/* skip any non-alphanum starts in cpfx->str; this is to
	 * handle the case where we have e.g. "(maildir:/abc)"
	 */
	while (pfx.str && *pfx.str && !isalnum(*pfx.str))
		++pfx.str;

	pfx.match = pfx.range_field = FALSE;

	mu_msg_field_foreach ((MuMsgFieldForeachFunc)each_check_prefix,
			      &pfx);
	/* also check special prefixes... */
	if (!pfx.match)
		pfx.match =
			g_str_has_prefix
			(str, MU_MSG_FIELD_PSEUDO_CONTACT ":") ||
			g_str_has_prefix
			(str, MU_MSG_FIELD_PSEUDO_RECIP ":");

	*is_field	= pfx.match;
	*is_range_field = pfx.range_field;
}


static gboolean
handle_esc_maybe (GString *gstr, char **cur, gunichar uc,
		  gboolean query_esc, gboolean range_field)
{
	char kar;

	kar = *cur[0];

	if (query_esc) {
		switch (kar) {
		case ':':
		case '(':
		case ')':
		case '*':
		case '&':
		case '"':
			g_string_append_c (gstr, kar);
			return TRUE;
		case '.':
			if (!range_field)
				break;

			if ((*cur)[1] == '.' && (*cur)[2] != '.') {
				g_string_append (gstr, "..");
				*cur = g_utf8_next_char (*cur);
				return TRUE;
			}
		default: break;
		}
	}

	if (g_unichar_ispunct(uc) || isblank(kar)) {
		g_string_append_c (gstr, '_');
		return TRUE;
	}

	return FALSE;
}


static char*
process_str (const char *str, gboolean xapian_esc, gboolean query_esc)
{
	GString *gstr;
	char *norm, *cur;
	gboolean is_field, is_range_field;

	norm = g_utf8_normalize (str, -1, G_NORMALIZE_ALL);
	if (G_UNLIKELY(!norm)) {  /* not valid utf8? */
		char *u8;
		u8 = mu_str_utf8ify (str);
		norm = g_utf8_normalize (u8, -1, G_NORMALIZE_ALL);
		g_free (u8);
	}

 	if (!norm)
		return NULL;

	/* msg-id needs some special care in queries */
	if (query_esc && is_msgid_field (str))
		return mu_str_process_msgid (str, TRUE);

	check_for_field (str, &is_field, &is_range_field);
	gstr = g_string_sized_new (strlen (norm));

	for (cur = norm; cur && *cur; cur = g_utf8_next_char (cur)) {

		gunichar uc;
		uc = g_utf8_get_char (cur);
		if (xapian_esc)
			if (handle_esc_maybe (gstr, &cur, uc, query_esc,
					      is_range_field))
				continue;

		if (g_unichar_ismark(uc))
			continue;

		if (!is_range_field)
			uc = g_unichar_tolower (uc);
		
		g_string_append_unichar (gstr, uc);
	}

	g_free (norm);
	return g_string_free (gstr, FALSE);
}


char*
mu_str_process_text (const char *str)
{
	g_return_val_if_fail (str, NULL);

	return process_str (str, FALSE, FALSE);

}


char*
mu_str_process_term (const char *str)
{
	g_return_val_if_fail (str, NULL);

	return process_str (str, TRUE, FALSE);

}


char*
mu_str_process_query_term (const char *str)
{
	g_return_val_if_fail (str, NULL);

	return process_str (str, TRUE, TRUE);

}



/*
 * Split simple search term into prefix, expression and suffix.
 * Meant to handle cases like "(maildir:/abc)", prefix and
 * suffix are the non-alphanumeric stuff at the beginning
 * and the end of string.
 *
 * Values of *pfx, *cond and *sfx will be allocated from heap
 * and must be g_free()d.
 *
 * Returns TRUE if all went fine and FALSE if some error was
 * occured.
 */
static gboolean
split_term (const gchar *term,
	    const gchar **pfx, const gchar **cond, const gchar **sfx)
{
	size_t l;
	const gchar *start, *tail;
	const gchar *p, *c, *s;

	g_return_val_if_fail (term, FALSE);
	g_return_val_if_fail (pfx, FALSE);
	g_return_val_if_fail (cond, FALSE);
	g_return_val_if_fail (sfx, FALSE);

	l = strlen (term);
	if (l == 0) {
		p = g_strdup ("");
		c = g_strdup ("");
		s = g_strdup ("");
		goto _done;
	}

	/*
	 * Invariants:
	 * - start will point to the first symbol after leading
	 *   non-alphanumerics (can be alphanumeric or '\0');
	 * - tail will point to the beginning of trailing
	 *   non-alphanumerics or '\0'.
	 * So:
	 * - len (prefix) = start - term;
	 * - len (cond) = tail - start;
	 * - len (suffix) = term + len (term) - tail.
	 */
	for (start = term; *start && !isalnum (*start); start++);
	for (tail = term + l; tail > start && !isalnum (*(tail-1)); tail--);

	p = g_strndup (term, start - term);
	c = g_strndup (start, tail - start);
	s = g_strndup (tail, term + l - tail);

_done:
	if (!p || !c || !s) {
		g_free ((gchar *)p);
		g_free ((gchar *)c);
		g_free ((gchar *)s);
		return FALSE;
	} else {
		*pfx = p;
		*cond = c;
		*sfx = s;
		return TRUE;
	}
	/* NOTREACHED */
}


/*
 * Fixup handlers.
 *
 * Every fixup handler will take three string arguments,
 * prefix, condition and suffix (as split by split_term).
 *
 * It will either return NULL that means "no fixup was done"
 * or the pointer to the newly-allocated string with the
 * new contents.
 */
typedef gchar *
  (*fixup_handler_t)(const gchar *pfx, const gchar *cond, const gchar *sfx);

static gchar*
fixup_date(const gchar *pfx, const gchar *cond, const gchar *sfx)
{
	const gchar *p;

	p = cond + sizeof ("date:") - 1;

	if (strstr (p, ".."))
		return NULL;
	return g_strdup_printf ("%s%s..%s%s", pfx, cond, p, sfx);
}


/*
 * Looks up fixup handler for the given condition.
 *
 * Returns fixup handler if we can and NULL if there is
 * no fixup for this condition.
 */
static fixup_handler_t
find_fixup (const gchar *cond)
{
	size_t n;
	/* NULL-terminated list of term names for fixups. */
	static struct {
		const char *name;
		size_t len;
		fixup_handler_t handler;
	} fixups[] = {
	  {"date:", sizeof("date:") - 1, fixup_date},
	  {NULL, 0, NULL}
	};

	g_return_val_if_fail (cond, NULL);

	for (n = 0; fixups[n].name; n++) {
		if (!strncasecmp (cond, fixups[n].name, fixups[n].len))
			break;
	}

	return fixups[n].handler;
}


gchar*
mu_str_xapian_fixup_terms (const gchar *term)
{
	gboolean is_field, is_range_field;
	const gchar *cond, *pfx, *sfx;
	gchar *retval;
	fixup_handler_t fixup;

	g_return_val_if_fail (term, NULL);

	if (strlen (term) == 0)
		return g_strdup (term);

	check_for_field (term, &is_field, &is_range_field);
	if (!is_field || !is_range_field)
		return g_strdup (term);

	if (!split_term (term, &pfx, &cond, &sfx))
		return g_strdup (term);

	retval = NULL;
	fixup = find_fixup (cond);
	if (fixup)
		retval = fixup (pfx, cond, sfx);
	if (!retval)
		retval = g_strdup (term);

	/* At this point retval should contain the result */
	g_free ((gchar *)pfx);
	g_free ((gchar *)sfx);
	g_free ((gchar *)cond);

	return retval;
}

/* note: this function is *not* re-entrant, it returns a static buffer */
const char*
mu_str_fullpath_s (const char* path, const char* name)
{
	static char buf[PATH_MAX + 1];

	g_return_val_if_fail (path, NULL);

	snprintf (buf, sizeof(buf), "%s%c%s", path, G_DIR_SEPARATOR,
		  name ? name : "");

	return buf;
}


char*
mu_str_escape_c_literal (const gchar* str, gboolean in_quotes)
{
	const char* cur;
	GString *tmp;

	g_return_val_if_fail (str, NULL);

	tmp = g_string_sized_new (2 * strlen(str));

	if (in_quotes)
		g_string_append_c (tmp, '"');

	for (cur = str; *cur; ++cur)
		switch (*cur) {
		case '\\': tmp = g_string_append   (tmp, "\\\\"); break;
		case '"':  tmp = g_string_append   (tmp, "\\\""); break;
		default:   tmp = g_string_append_c (tmp, *cur);
		}

	if (in_quotes)
		g_string_append_c (tmp, '"');

	return g_string_free (tmp, FALSE);
}



/* turn \0-terminated buf into ascii (which is a utf8 subset); convert
 *   any non-ascii into '.'
 */
char*
mu_str_asciify_in_place (char *buf)
{
	char *c;

	g_return_val_if_fail (buf, NULL);

	for (c = buf; c && *c; ++c) {
		if ((!isprint(*c) && !isspace (*c)) || !isascii(*c))
			*c = '.';
	}

	return buf;
}

char*
mu_str_utf8ify (const char *buf)
{
	char *utf8;

	g_return_val_if_fail (buf, NULL);

	utf8 = g_strdup (buf);

	if (!g_utf8_validate (buf, -1, NULL))
		mu_str_asciify_in_place (utf8);

	return utf8;
}



gchar*
mu_str_convert_to_utf8 (const char* buffer, const char *charset)
{
	GError *err;
	gchar * utf8;

	g_return_val_if_fail (buffer, NULL);
	g_return_val_if_fail (charset, NULL );

	err = NULL;
	utf8 = g_convert_with_fallback (buffer, -1, "UTF-8",
					charset, NULL,
					NULL, NULL, &err);
	if (!utf8) /* maybe the charset lied; try 8859-15 */
		utf8 = g_convert_with_fallback (buffer, -1, "UTF-8",
						"ISO8859-15", NULL,
						NULL, NULL, &err);
	/* final attempt, maybe it was utf-8 already */
	if (!utf8 && g_utf8_validate (buffer, -1, NULL))
		utf8 = g_strdup (buffer);

	if (!utf8) {
		g_warning ("%s: conversion failed from %s: %s",
			 __func__, charset, err ? err->message : "");
	}

	g_clear_error (&err);

	return utf8;
}


gchar*
mu_str_quoted_from_strv (const gchar **params)
{
	GString *str;
	int i;

	g_return_val_if_fail (params, NULL);

	if (!params[0])
		return g_strdup ("");

	str = g_string_sized_new (64); /* just a guess */

	for (i = 0; params[i]; ++i) {

		if (i > 0)
			g_string_append_c (str, ' ');

		g_string_append_c (str, '"');
		g_string_append (str, params[i]);
		g_string_append_c (str, '"');
	}

	return g_string_free (str, FALSE);
}


static char*
read_key (const char *str, const char **val, GError **err)
{
	const char *cur;
	GString *gstr;

	cur = str;

	gstr = g_string_sized_new (strlen(cur));
	while (*cur && *cur != ':') {
		g_string_append_c (gstr, *cur);
		++cur;
	}

	if (*cur != ':' || gstr->len == 0) {
		g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR,
			     "expected: '<alphanum>+:' (%s)",
			     str);
		g_string_free (gstr, TRUE);
		*val = NULL;
		return NULL;
	} else {
		*val = cur + 1;
		return g_string_free (gstr, FALSE);
	}
}


static char*
read_val (const char *str, const char **endval, GError **err)
{
	const char *cur;
	gboolean quoted;
	GString *gstr;

	gstr = g_string_sized_new (strlen(str));

	for (quoted = FALSE, cur = str; *cur; ++cur) {

		if (*cur == '\\') {
			if (cur[1] != '"' && cur[1] != '\\') {
				g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR,
					     "invalid escaping");
				goto errexit;
			} else {
				++cur;
				g_string_append_c (gstr, *cur);
				continue;
			}
		} else if (*cur == '"') {
			quoted = !quoted;
			continue;
		} else if (isblank(*cur) && !quoted)
			break;
		else
			g_string_append_c (gstr, *cur);
	}

	if (quoted) {
		g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR,
			     "error in quoting");
		goto errexit;
	}

	*endval = cur;
	return g_string_free (gstr, FALSE);

errexit:
	g_string_free (gstr, TRUE);
	return NULL;
}


GHashTable*
mu_str_parse_arglist (const char *args, GError **err)
{
	GHashTable *hash;
	const char *cur;

	g_return_val_if_fail (args, NULL);

	hash = g_hash_table_new_full (
		g_str_hash,
		g_str_equal,
		(GDestroyNotify)g_free,
		(GDestroyNotify)g_free);

	cur = args;
	while ((isblank(*cur)))
		++cur;

	do {
		char *key, *val;
		const char *valstart, *valend;

		key = read_key (cur, &valstart, err);
		if (!key)
			goto errexit;

		val = read_val (valstart, &valend, err);
		if (!val)
			goto errexit;

		/* g_print ("%s->%s\n", key, val); */
		g_hash_table_insert (hash, key, val);

		cur = valend;
		while ((isblank(*cur)))
			++cur;
	} while (*cur);

	return hash;

errexit:
	g_hash_table_destroy (hash);
	return NULL;
}



char *
mu_str_remove_ctrl_in_place (char *str)
{
	char *cur;

	g_return_val_if_fail (str, NULL);

	for (cur = str; *cur; ++cur) {

		GString *gstr;

		if (!iscntrl(*cur))
			continue;

		if (isspace(*cur)) {
			/* squash special white space into a simple space */
			*cur = ' ';
		} else {
			/* remove other control characters */
			gstr = g_string_sized_new (strlen (str));
			for (cur = str; *cur; ++cur)
				if (!iscntrl (*cur))
					g_string_append_c (gstr, *cur);
			memcpy (str, gstr->str, gstr->len); /* fits */
			g_string_free (gstr, TRUE);
			break;
		}
	}

	return str;
}
