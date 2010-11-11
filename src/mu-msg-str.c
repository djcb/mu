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

#include <glib.h>
#include <string.h>
#include <ctype.h>

#include "mu-msg-str.h"
#include "mu-msg-flags.h"



char*
mu_msg_str_normalize (const char *str, gboolean downcase)
{
	const guchar *cur;
	gchar *output;
	size_t len;
	int i;
	
	g_return_val_if_fail (str, NULL);

	len = strlen (str);
	output = g_new0 (char, 2 * len);
	
	for (i = 0, cur = (const guchar*)str; *cur; ++cur) {
		if (*cur != 0xc3) { /* != latin-1 supplement? */
			output[i++] = *cur;
			continue; 
		}
		++cur;

		switch (*cur) {

		case 0x80:
		case 0x81:
		case 0x82:
		case 0x83:
		case 0x84:
		case 0x85: output[i++] = 'A'; break;

		case 0x86: output[i++] = 'A'; output[i++] = 'e'; break;
		case 0x87: output[i++] = 'C'; break;
			
		case 0x88:
		case 0x89:
		case 0x8a:
		case 0x8b: output[i++] = 'E'; break;

		case 0x8c:
		case 0x8d:
		case 0x8e:
		case 0x8f: output[i++] = 'I'; break;

		case 0x90: output[i++] = 'D'; break;
		case 0x91: output[i++] = 'N'; break;
			
		case 0x92:
		case 0x93:
		case 0x94:
		case 0x95:
		case 0x96: output[i++] = 'O'; break;

		case 0x99:
		case 0x9a:
		case 0x9b:
		case 0x9c: output[i++] = 'U'; break;
			
		case 0x9d: output[i++] = 'Y'; break;
		case 0x9e: output[i++] = 'T'; output[i++] = 'h'; break;
		case 0x9f: output[i++] = 's'; output[i++] = 's'; break;
			
		case 0xa0:
		case 0xa1:
		case 0xa2:
		case 0xa3:
		case 0xa4:
		case 0xa5: output[i++] = 'a'; break;

		case 0xa6: output[i++] = 'a'; output[i++] = 'e'; break;
		case 0xa7: output[i++] = 'c'; break;
			
		case 0xa8:
		case 0xa9:
		case 0xaa:
		case 0xab: output[i++] = 'e'; break;

		case 0xac:
		case 0xad:
		case 0xae:
		case 0xaf: output[i++] = 'i'; break;

		case 0xb0: output[i++] = 'd'; break;
		case 0xb1: output[i++] = 'n'; break;
			
		case 0xb2:
		case 0xb3:
		case 0xb4:
		case 0xb5:
		case 0xb6: output[i++] = 'o'; break;

		case 0xb9:
		case 0xba:
		case 0xbb:
		case 0xbc: output[i++] = 'u'; break;

		case 0xbd: output[i++] = 'y'; break;
		case 0xbe: output[i++] = 't'; output[i++] = 'h'; break;
		case 0xbf: output[i++] = 'y'; break;

		default:
			output[i++] = *cur;
		}
	}

	output [i] = '\0';

	/* for utf8, this should not interfere with anything it shouldn't... */
	if (downcase) {
		gchar *c;
		for (c = output; *c; ++c)
			*c = tolower (*c);
	}
	
	return output;
}


const char* 
mu_msg_str_date_s (const char* frm, time_t t)
{
	struct tm *tmbuf;
	static char buf[128];

	g_return_val_if_fail (frm, NULL);
	
	tmbuf = localtime(&t);
	
	strftime (buf, sizeof(buf), frm, tmbuf);

	return buf;
}

char* 
mu_msg_str_date (const char *frm, time_t t)
{
	return g_strdup (mu_msg_str_date_s(frm, t));
}



const char* 
mu_msg_str_display_date_s (time_t t)
{
	time_t now;
	static const guint SECS_IN_DAY = 24 * 60 * 60;
	
	now = time (NULL);

	if (ABS(now - t) > SECS_IN_DAY)
		return mu_msg_str_date_s ("%x", t);
	else
		return mu_msg_str_date_s ("%X", t);
}





const char*
mu_msg_str_size_s  (size_t s)
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
		g_snprintf(buf, sizeof(buf), "%.1f MB", (double)s/(1000*1000));
	else
		g_snprintf(buf, sizeof(buf), "%.1f kB", (double)s/(1000));
#endif /*HAVE_GLIB216*/

	
	return buf;
}

char* 
mu_msg_str_size (size_t s)
{
	return g_strdup (mu_msg_str_size_s(s));
}

const char*
mu_msg_str_flags_s  (MuMsgFlags flags)
{
	return mu_msg_flags_to_str_s (flags);
}

char*
mu_msg_str_flags  (MuMsgFlags flags)
{
	return g_strdup (mu_msg_str_flags_s(flags));
}

const char* 
mu_msg_str_prio  (MuMsgPrio prio)
{
	switch (prio) {

	case MU_MSG_PRIO_LOW:
		return "low";
		
	case MU_MSG_PRIO_NONE:
	case MU_MSG_PRIO_NORMAL:
		return "normal";

	case MU_MSG_PRIO_HIGH:
		return "high";

	default:
		g_warning ("%s: invalid priority %d", __FUNCTION__, prio);
		return NULL;
	}
}


char*
mu_msg_str_summarize (const char* str, size_t max_lines)
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
mu_msg_str_display_contact_s (const char *str)
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
mu_msg_str_display_contact (const char *str)
{
	g_return_val_if_fail (str, NULL);

	return g_strdup (mu_msg_str_display_contact_s (str));
}
