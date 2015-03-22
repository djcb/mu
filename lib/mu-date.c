/*
** Copyright (C) 2012  <djcb@djcbsoftware.nl>
**
** This program is free software; you can redistribute it and/or modify it
** under the terms of the GNU General Public License as published by the
** Free Software Foundation; either version 3, or (at your option) any
** later version.
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

#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#include "mu-util.h"
#include "mu-date.h"
#include "mu-str.h"

const char*
mu_date_str_s (const char* frm, time_t t)
{
	struct tm	*tmbuf;
	static char	 buf[128];
	static int	 is_utf8 = -1;
	size_t		 len;

	if (G_UNLIKELY(is_utf8 == -1))
		is_utf8 = mu_util_locale_is_utf8 () ? 1 : 0;

	g_return_val_if_fail (frm, NULL);

	tmbuf = localtime(&t);
	len = strftime (buf, sizeof(buf) - 1, frm, tmbuf);
	if (len == 0)
		return ""; /* not necessarily an error... */

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
mu_date_str (const char *frm, time_t t)
{
	return g_strdup (mu_date_str_s(frm, t));
}


const char*
mu_date_display_s (time_t t)
{
	time_t now;
	static const time_t SECS_IN_DAY = 24 * 60 * 60;

	now = time (NULL);

	if (ABS(now - t) > SECS_IN_DAY)
		return mu_date_str_s ("%x", t);
	else
		return mu_date_str_s ("%X", t);
}



time_t
mu_date_parse_hdwmy (const char *nptr)
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
	case 'H':
		delta = num * 60 * 60; break;
	case 'd': /* day */
	case 'D':
		delta = num * 24 * 60 * 60; break;
	case 'w': /* week */
	case 'W':
		delta = num * 7 * 24 * 60 * 60; break;
	case 'm': /* month */
	case 'M':
		delta = num * 30 * 24 * 60 * 60; break;
	case 'y': /* year */
	case 'Y':
		delta = num * 365 * 24 * 60 * 60; break;
	default:
		return never;
	}

	now = time(NULL);
	return delta <= now ? now - delta : never;
}


/* clear a date of anything non-numberic; static string, non-reentrant */
static char*
clear_date_s (const char *date)
{
	static char cleandate [14 + 1];
	unsigned u1, u2;

	for (u1 = u2 = 0; date[u1] != '\0'; ++u1)
		if (isdigit(date[u1]))
			cleandate[u2++] = date[u1];

	cleandate[u2] = '\0';

	return cleandate;
}



const char*
mu_date_complete_s (const char *date, gboolean is_begin)
{
	static char fulldate[14 + 1];
	static const char* full_begin = "00000101000000";
	static const char* full_end   = "99991231235959";

	char *cleardate;

	g_return_val_if_fail (date, NULL);

	cleardate = clear_date_s (date);

	strncpy (fulldate, is_begin ? full_begin : full_end,
		sizeof(fulldate));
	memcpy (fulldate, cleardate, strlen(cleardate));

	return fulldate;
}


char*
mu_date_complete (const char *date, gboolean is_begin)
{
	const char *s;

	g_return_val_if_fail (date, NULL);

	s = mu_date_complete_s (date, is_begin);
	return s ? g_strdup (s) : NULL;
}


const char*
mu_date_interpret_s (const char *datespec, gboolean is_begin)
{
	static char fulldate[14 + 1];
	time_t now, t;

	g_return_val_if_fail (datespec, NULL);

	if (mu_str_is_empty (datespec) && is_begin)
		return "000000000000"; /* beginning of time*/

	now = time(NULL);
	if (strcmp (datespec, "today") == 0) {
		strftime(fulldate, sizeof(fulldate),
			 is_begin ? "%Y%m%d000000" : "%Y%m%d235959",
			 localtime(&now));
		return fulldate;
	}

	if (mu_str_is_empty (datespec) || strcmp (datespec, "now") == 0) {
		strftime(fulldate, sizeof(fulldate), "%Y%m%d%H%M%S",
			 localtime(&now));
		return fulldate;
	}

	t = mu_date_parse_hdwmy (datespec);
	if (t != (time_t)-1) {
		strftime(fulldate, sizeof(fulldate), "%Y%m%d%H%M%S",
			 localtime(&t));
		return fulldate;
	}

	return datespec; /* nothing changed */
}


char*
mu_date_interpret (const char *datespec, gboolean is_begin)
{
	char *s;

	g_return_val_if_fail (datespec, NULL);

	s = mu_date_interpret (datespec, is_begin);
	return s ? g_strdup(s) : NULL;
}


time_t
mu_date_str_to_time_t (const char* date, gboolean local)
{
	struct tm	tm;
	char		mydate[14 + 1]; /* YYYYMMDDHHMMSS */
	time_t		t;

	memset (&tm, 0, sizeof(struct tm));
	strncpy (mydate, date, 15);
	mydate[sizeof(mydate)-1] = '\0';

	g_return_val_if_fail (date, (time_t)-1);

	tm.tm_sec   = atoi (mydate + 12);     mydate[12] = '\0';
	tm.tm_min   = atoi (mydate + 10);     mydate[10] = '\0';
	tm.tm_hour  = atoi (mydate +  8);     mydate[8]  = '\0';
	tm.tm_mday  = atoi (mydate +  6);     mydate[6]  = '\0';
	tm.tm_mon   = atoi (mydate +  4) - 1; mydate[4]  = '\0';
	tm.tm_year  = atoi (mydate) - 1900;
	tm.tm_isdst = -1;
	/* let timegm/mktime figure out the dst */

	if (local)
		t = mktime (&tm);
	else
		t = timegm (&tm); /* GNU/BSD specific */

	return t;
}

const char*
mu_date_time_t_to_str_s (time_t t, gboolean local)
{
	/* static char datestr[14 + 1]; /\* YYYYMMDDHHMMSS *\/ */
	static char		 datestr[14+1];	/* YYYYMMDDHHMMSS */
	static const char	*frm = "%Y%m%d%H%M%S";
	size_t len;

	len = strftime (datestr, sizeof(datestr), frm,
			local ? localtime (&t) : gmtime(&t));
	if (len == 0) {
		g_warning ("bug: error converting time");
		return "00000000000000";
	}

	return datestr;
}


char*
mu_date_time_t_to_str (time_t t, gboolean local)
{
	const char* str;

	str = mu_date_time_t_to_str_s (t, local);

	return str ? g_strdup(str): NULL;
}
