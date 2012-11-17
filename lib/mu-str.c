/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/
/*
** Copyright (C) 2008-2012 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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


static gchar*
eat_esc_string (char **strlst, GError **err)
{
	char *str;
	gboolean quoted;
	GString *gstr;

	str  = g_strchug (*strlst);
	gstr = g_string_sized_new (strlen(str));

	for (quoted = FALSE; *str; ++str) {

		if (*str == '"') {
			quoted = !quoted;
			continue;
		} else if (*str == '\\') {
			if (str[1] != ' ' && str[1] != '"' && str[1] != '\\')
				goto err; /* invalid escaping */
			g_string_append_c (gstr, str[1]);
			++str;
			continue;
		} else if (*str == ' ' && !quoted) {
			++str;
			goto leave;
		} else
			g_string_append_c (gstr, *str);
	}
leave:
	*strlst = str;
	return g_string_free (gstr, FALSE);
err:
	g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_IN_PARAMETERS,
		     "error parsing string '%s'", g_strchug(*strlst));
	*strlst = NULL;
	return g_string_free (gstr, TRUE);
}


GSList*
mu_str_esc_to_list (const char *strings, GError **err)
{
	GSList *lst;
	char *mystrings, *freeme;
	const char* cur;

	g_return_val_if_fail (strings, NULL);

	for (cur = strings; *cur && (*cur == ' ' || *cur == '\t'); ++cur);
	freeme = mystrings = g_strdup (cur);

	lst = NULL;
	do {
		gchar *str;
		str = eat_esc_string (&mystrings, err);
		if (str)
			lst = g_slist_prepend (lst, str);
		else {
			g_free (freeme);
			mu_str_free_list (lst);
			return NULL;
		}

	} while (mystrings && *mystrings);

	g_free (freeme);
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

	pfx.match =  pfx.range_field = FALSE;

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

/*
 * Xapian treats various characters such as '@', '-', ':' and '.'
 * specially; function below is an ugly hack to make it DWIM in most
 * cases...
 *
 * function expects search terms (not complete queries)
 * */
char*
mu_str_xapian_escape_in_place_try (char *term, gboolean esc_space, GStringChunk *strchunk)
{
	unsigned char *cur;
	const char escchar = '_';
	gboolean is_field, is_range_field;
	unsigned colon;

	g_return_val_if_fail (term, NULL);

	check_for_field (term, &is_field, &is_range_field);

	for (colon = 0, cur = (unsigned char*)term; *cur; ++cur) {

		switch (*cur) {

		case '.': /* escape '..' if it's not a range field*/
			if (is_range_field && cur[1] == '.')
				cur += 1;
			else
				*cur = escchar;
			break;
		case ':':
			/* if there's a registered xapian prefix
			 * before the *first* ':', don't touch
			 * it. Otherwise replace ':' with '_'... ugh
			 * yuck ugly...
			 */
			if (colon != 0 || !is_field)
				*cur = escchar;
			++colon;
			break;
		case '(':
		case ')':
		case '\'':
		case '*':   /* wildcard */
			break;
		default:
			/* escape all other special stuff */
			if (*cur < 0x80 && !isalnum (*cur))
				*cur = escchar;
		}
	}

	/* downcase try to remove accents etc. */
	return mu_str_normalize_in_place_try (term, TRUE, strchunk);
}

char*
mu_str_xapian_escape (const char *query, gboolean esc_space, GStringChunk *strchunk)
{
	char *mystr;

	g_return_val_if_fail (query, NULL);

	if (strchunk)
		mystr = g_string_chunk_insert (strchunk, query);
	else
		mystr = g_strdup (query);

	return mu_str_xapian_escape_in_place_try (mystr, esc_space, strchunk);
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

	for (c = buf; c && *c; ++c)
		if (!isascii(*c))
			c[0] = '.';

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
			 __FUNCTION__, charset, err ? err->message : "");
		g_clear_error (&err);
	}

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
