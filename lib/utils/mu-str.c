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

const char*
mu_str_size_s  (size_t s)
{
	static char	 buf[32];
	char		*tmp;

	tmp = g_format_size_for_display ((goffset)s);
	strncpy (buf, tmp, sizeof(buf));
	buf[sizeof(buf) -1] = '\0'; /* just in case */
	g_free (tmp);

	return buf;
}

char*
mu_str_size (size_t s)
{
	return g_strdup (mu_str_size_s(s));
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


/* note: this function is *not* re-entrant, it returns a static buffer */
const char*
mu_str_fullpath_s (const char* path, const char* name)
{
	static char buf[PATH_MAX + 1];

	g_return_val_if_fail (path, NULL);

	g_snprintf (buf, sizeof(buf), "%s%c%s", path, G_DIR_SEPARATOR,
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


char*
mu_str_remove_ctrl_in_place (char *str)
{
	char *orig, *cur;

	g_return_val_if_fail (str, NULL);

	orig = str;

	for (cur = orig; *cur; ++cur) {
		if (isspace(*cur)) {
			/* squash special white space into a simple space */
			*orig++ = ' ';
		} else if (iscntrl(*cur)) {
			/* eat it */
		} else
			*orig++ = *cur;
	}

	*orig = '\0';  /* ensure the updated string has a NULL */

	return str;
}
