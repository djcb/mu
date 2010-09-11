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

#include "mu-msg-str.h"
#include "mu-msg-flags.h"


const char* 
mu_msg_str_date_s (time_t t)
{
	struct tm *tmbuf;
	static char buf[64];

	tmbuf = localtime(&t);
	
	strftime (buf, sizeof(buf), "%c", tmbuf);

	return buf;
}

char* 
mu_msg_str_date (time_t t)
{
	return g_strdup (mu_msg_str_date_s(t));
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

