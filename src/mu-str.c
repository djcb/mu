/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

/* 
** Copyright (C) 2008-2011 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

/* hopefully, this should get us a sane PATH_MAX */
#include <limits.h>
/* not all systems provide PATH_MAX in limits.h */
#ifndef PATH_MAX
#include <sys/param.h>
#ifndef PATH_MAX
#define PATH_MAX MAXPATHLEN
#endif /*!PATH_MAX*/
#endif /*PATH_MAX*/

#include "mu-str.h"
#include "mu-msg-flags.h"
#include "mu-msg-fields.h"

const char* 
mu_str_date_s (const char* frm, time_t t)
{
	struct tm *tmbuf;
	static char buf[128];
	static int is_utf8 = -1;
	
	if (G_UNLIKELY(is_utf8 == -1))
		is_utf8 = mu_util_locale_is_utf8 () ? 1 : 0; 
	
	g_return_val_if_fail (frm, NULL);
	
	tmbuf = localtime(&t);
	
	strftime (buf, sizeof(buf), frm, tmbuf);

	if (!is_utf8) {
		/* charset is _not_ utf8, so we need to convert it, so
		 * the date could contain locale-specific characters*/
		gchar *conv;
		GError *err;
		err = NULL;
		conv = g_locale_to_utf8 (buf, -1, NULL, NULL, &err);
		if (err) {
			g_warning ("conversion failed: %s", err->message);
			g_error_free (err);
			strcpy (buf, "<error>");
		} else
			strncpy (buf, conv, sizeof(buf));
		
		g_free (conv);
	}
	
	return buf;
}

char* 
mu_str_date (const char *frm, time_t t)
{
	return g_strdup (mu_str_date_s(frm, t));
}



const char* 
mu_str_display_date_s (time_t t)
{
	time_t now;
	static const time_t SECS_IN_DAY = 24 * 60 * 60;
	
	now = time (NULL);

	if (ABS(now - t) > SECS_IN_DAY)
		return mu_str_date_s ("%x", t);
	else
		return mu_str_date_s ("%X", t);
}

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
mu_str_flags_s  (MuMsgFlags flags)
{
	return mu_msg_flags_str_s (flags);
}

char*
mu_str_flags  (MuMsgFlags flags)
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


struct _CheckPrefix {
	const char		*pfx;
	guint			 len;
	gboolean		 match;
};
typedef struct _CheckPrefix	 CheckPrefix;

static void
each_check_prefix (MuMsgFieldId mfid, CheckPrefix *cpfx)
{
	const char *field_name;
	char field_shortcut;

	if (!cpfx || cpfx->match)
		return;
	
	field_shortcut = mu_msg_field_shortcut (mfid);
	if (field_shortcut == cpfx->pfx[0] && cpfx->pfx[1] == ':') {
		cpfx->match = TRUE;
		return;
	}

	field_name = mu_msg_field_name (mfid);
	if (field_name &&
	    strncmp (cpfx->pfx, field_name, cpfx->len) == 0) {
		cpfx->match = TRUE;
		return;
	}
}

/* 'colon' points at a position inside q pointing at a ':'
 * character. function determines whether the prefix is a registered
 * prefix (like 'subject' or 'from' or 's') */
static gboolean
is_xapian_prefix (const char *q, const char *colon)
{
	const char *cur;
	
	if (colon == q)
		return FALSE; /* : at beginning, not a prefix */
	
	/* track back from colon until a boundary or beginning of the
	 * str */
	for (cur = colon - 1; cur >= q; --cur) {

		if (cur == q || !isalpha (*(cur-1))) {

			CheckPrefix cpfx;
			memset (&cpfx, 0, sizeof(CheckPrefix));

			cpfx.pfx   = cur;
			cpfx.len   = (colon - cur);
			cpfx.match = FALSE;
			
			mu_msg_field_foreach ((MuMsgFieldForEachFunc)
					      each_check_prefix,
					      &cpfx);
			
			return (cpfx.match);
		}
	}
	
	return FALSE;
}

time_t
mu_str_date_parse_hdwmy (const char *nptr)
{
	long int num;
	char *endptr;
	time_t now, delta;
	time_t never = (time_t)-1;
       
	g_return_val_if_fail (nptr, never);
       
	num = strtol  (nptr, &endptr, 10);
	if (num <= 0 || num > 9999)  
		return never;

	if (endptr == NULL || *endptr == '\0')
		return never;
       
	switch (endptr[0]) {
	case 'h': /* hour */    
		delta = num * 60 * 60; break;
	case 'd': /* day */
		delta = num * 24 * 60 * 60; break;
	case 'w': /* week */
		delta = num * 7 * 24 * 60 * 60; break;
	case 'm': /* month */
		delta = num * 30 * 24 * 60 * 60; break;
	case 'y': /* year */
		delta = num * 365 * 24 * 60 * 60; break; 
	default:
		return never;
	}

	now = time(NULL);
	return delta <= now ? now - delta : never;  
}

guint64
mu_str_size_parse_kmg (const char* str)
{
	gint64 num;
	char *end;
	
	g_return_val_if_fail (str, G_MAXUINT64);
	
	num = strtol (str, &end, 10);
	if (num < 0)  
		return G_MAXUINT64;
	
	if (!end || end[1] != '\0')
		return G_MAXUINT64;
	
	switch (tolower(end[0])) {
	case 'b': return num;                      /* bytes */	
	case 'k': return num * 1000;               /* kilobyte */
	case 'm': return num * 1000 * 1000;        /* megabyte */
	/* case 'g': return num * 1000 * 1000 * 1000; /\* gigabyte *\/ */
	default:
		return G_MAXUINT64;
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
		char sep[2] = { '\0', '\0' };
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
mu_str_to_list (const char *str, char sepa)
{
	GSList *lst;
	gchar **strs, **cur;
	char sep[] = { '\0', '\0' };
	
	g_return_val_if_fail (sepa, NULL);
	
	if (!str)
		return NULL;

	sep[0] = sepa;
	strs = g_strsplit (str, sep, -1);

	for (cur = strs, lst = NULL; cur && *cur; ++cur)
		lst = g_slist_prepend (lst, g_strdup(*cur));
		
	lst = g_slist_reverse (lst);
	g_strfreev (strs);

	return lst;
}

void
mu_str_free_list (GSList *lst)
{
	g_slist_foreach (lst, (GFunc)g_free, NULL);
	g_slist_free (lst);	
}

const gchar*
mu_str_subject_normalize (const gchar* str)
{
	gchar *last_colon;
	g_return_val_if_fail (str, NULL);

	/* FIXME: improve this */
	last_colon = g_strrstr (str, ":");
	if (!last_colon)
		return str;
	else {
		gchar *str;
		str = last_colon + 1;
		while (*str == ' ')
			++str;
		return str;
	}
}



/*
 * Xapian treats various characters such as '@', '-', ':' and '.'
 * specially; function below is an ugly hack to make it DWIM in most
 * cases...*/
char*
mu_str_ascii_xapian_escape_in_place (char *query)
{
	gchar *cur;
	gboolean replace_dot;
		
	g_return_val_if_fail (query, NULL);

	/* only replace the '.' if the string looks like an e-mail
	 * address or msg-id */
	replace_dot = (g_strstr_len(query, -1, "@") != NULL);
	
	for (cur = query; *cur; ++cur) {

		*cur = tolower(*cur);

		switch (*cur) {
		case '@':
		case '-':
			*cur = '_'; break;
		case '.': {
			/* don't replace a final cur */
			if (cur[1]== ' ' || cur[1]=='\t' || cur[1]== '.')  
				++cur;
			else if (cur[1] == '\0')
				break;
			else
				*cur = '_';
			break;
		}
		case ':':
			/* if there's a registered xapian prefix before the
			 * ':', don't touch it. Otherwise replace ':' with
			 * a space'... ugh yuck ugly...
			 */			 
			if (!is_xapian_prefix (query, cur))
				*cur = '_';
			break;
		}
	}
	
	return query;
}

char*
mu_str_ascii_xapian_escape (const char *query)
{
	g_return_val_if_fail (query, NULL);

	return mu_str_ascii_xapian_escape_in_place (g_strdup(query));
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
mu_str_escape_c_literal (const gchar* str)
{
	const char* cur;
	GString *tmp;
	
	g_return_val_if_fail (str, NULL);
	
	tmp = g_string_sized_new (2 * strlen(str));
	for (cur = str; *cur; ++cur)
		switch (*cur) {
		case '\\': tmp = g_string_append   (tmp, "\\\\"); break;
		case '"':  tmp = g_string_append   (tmp, "\\\""); break;
		default:   tmp = g_string_append_c (tmp, *cur);
		}

	return g_string_free (tmp, FALSE);
}


gchar*
mu_str_guess_last_name (const char *name)
{
	const gchar *lastsp;

	if (!name)
		return g_strdup ("");
	
	lastsp = g_strrstr (name, " ");
	
	return g_strdup (lastsp ? lastsp + 1 : "");
}


gchar*
mu_str_guess_first_name (const char *name)
{
	const gchar *lastsp;

	if (!name)
		return g_strdup ("");
	
	lastsp = g_strrstr (name, " ");

	if (lastsp)
		return g_strndup (name, lastsp - name);
	else
		return g_strdup (name);
}

static gchar*
cleanup_str (const char* str)
{
	gchar *s;
	const gchar *cur;
	unsigned i;

	if (mu_str_is_empty(str))
		return g_strdup ("");
	
	s = g_new0 (char, strlen(str) + 1);
	
	for (cur = str, i = 0; *cur; ++cur) {
		if (ispunct(*cur) || isspace(*cur))
			continue;
		else
			s[i++] = *cur;
	}

	return s;
}


gchar*
mu_str_guess_nick (const char* name)
{
	gchar *fname, *lname, *nick;
	gchar initial[7];
	
	fname	  = mu_str_guess_first_name (name);
	lname	  = mu_str_guess_last_name (name);
	
	/* if there's no last name, use first name as the nick */
	if (mu_str_is_empty(fname) || mu_str_is_empty(lname)) {
		g_free (lname);
		nick = fname;
		goto leave;
	}
	
	memset (initial, 0, sizeof(initial));
	/* couldn't we get an initial for the last name? */
	if (g_unichar_to_utf8 (g_utf8_get_char (lname), initial) == 0) {
		g_free (lname);
		nick = fname;
		goto leave;
	}

	nick = g_strdup_printf ("%s%s", fname, initial);
	g_free (fname);
	g_free (lname);
	
leave:
	{
		gchar *tmp;
		tmp = cleanup_str (nick);
		g_free (nick);
		nick = tmp;
	}
	
	return nick;
}
