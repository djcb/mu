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
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wformat-nonliteral"
	len = strftime (buf, sizeof(buf) - 1, frm, tmbuf);
#pragma GCC diagnostic pop

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
		} else {
			strncpy (buf, conv, sizeof(buf)-1);
			buf[sizeof(buf)-1] = '\0';
		}

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
