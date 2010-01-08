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
#include "mu-msg-str.h"
#include "mu-msg-flags.h"

const char* 
mu_msg_str_date_s (time_t t)
{
	struct tm *tmbuf;
	static char buf[64];

	tmbuf = localtime(&t);
	
	strftime (buf, 64, "%c", tmbuf);

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
	/* note: we we use the powers-of-10, not powers-of-2 */

	static char buf[32];
	
	if (s >= 1000 * 1000)
		g_snprintf(buf, 32, "%.1fM", (double)s/(1000*1000));
	else
		g_snprintf(buf, 32, "%.1fk", (double)s/(1000));
	
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
mu_msg_str_prio  (MuMsgPriority prio)
{
	switch (prio) {

	case MU_MSG_PRIORITY_LOW:
		return "low";
		
	case MU_MSG_PRIORITY_NONE:
	case MU_MSG_PRIORITY_NORMAL:
		return "normal";

	case MU_MSG_PRIORITY_HIGH:
		return "high";

	default:
		g_warning ("%s: invalid priority %d", __FUNCTION__, prio);
		return "<error>";
	}
}


