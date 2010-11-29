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


#if HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/


#include <glib.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#include "mu-str.h"
#include "mu-msg-flags.h"
#include "mu-msg-fields.h"

const char* 
mu_str_date_s (const char* frm, time_t t)
{
	struct tm *tmbuf;
	static char buf[128];

	g_return_val_if_fail (frm, NULL);
	
	tmbuf = localtime(&t);
	
	strftime (buf, sizeof(buf), frm, tmbuf);

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


const char*
mu_str_display_contact_s (const char *str)
{
	static gchar contact[255];
	gchar *c, *c2;
	
	if (!str)
		str = "";
	
	g_strlcpy (contact, str, sizeof(contact));

	/* strip the address, if any */
	c = g_strstr_len (contact, -1, "<");
	if (c != NULL)
		*c = '\0';

	/* replace " with space */
	for (c2 = contact; *c2; ++c2)
		if (*c2 == '"')
			*c2 = ' ';

	g_strstrip (contact);

	return contact;
}

char*
mu_str_display_contact (const char *str)
{
	g_return_val_if_fail (str, NULL);

	return g_strdup (mu_str_display_contact_s (str));
}



time_t
mu_date_parse_hdwmy (const char* str)
{
	long int num;
	char *end;
	time_t now, delta;
	time_t never = (time_t)-1;
	
	g_return_val_if_fail (str, never);
	
	num = strtol  (str, &end, 10);
	if (num <= 0 || num > 9999)  
		return never;

	if (!end || end[1] != '\0')
		return never;
	
	switch (end[0]) {
	case 'h': /* hour */	
		delta = num * 24 * 60; break;
	case 'd': /* day */
		delta = num * 24 * 60 * 60; break;
	case 'w': /* week */
		delta = num * 7 * 24 * 60 * 60; break;
	case 'm':
		delta = num * 30 * 24 * 60 * 60; break;
	case 'y':
		delta = num * 365 * 24 * 60 * 60; break; 
	default:
		return never;
	}

	now = time(NULL);
	return delta <= now ? now - delta : never;  
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

/* colon is a position inside q pointing at a ':' character. function
 * determines whether the prefix is a registered prefix (like
 * 'subject' or 'from' or 's') */
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
		if (*cur == '@') 
			*cur = '_';

		else if (replace_dot && *cur == '.') {
			if (cur[1] == '.')  /* don't replace '..' */
				cur += 2;
			else
				*cur = '_';
		} else if (*cur == ':') {
			/* if there's a registered xapian prefix before the
			 * ':', don't touch it. Otherwise replace ':' with
			  * a space'... ugly...
			  */			 
			if (!is_xapian_prefix (query, cur))
				*cur = '_';
		} else
			*cur = tolower(*cur);
	}
	
	return query;
}

