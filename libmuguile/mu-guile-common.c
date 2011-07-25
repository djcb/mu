/*
** Copyright (C) 2011 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#if HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include "mu-guile-common.h"
#include "mu-guile-store.h"
#include "mu-guile-msg.h"
#include "mu-guile-log.h"

void
mu_guile_error (const char *func_name, int status,
		      const char *fmt, SCM args)
{
	scm_error_scm (scm_from_locale_symbol ("MuError"),
		       scm_from_utf8_string (func_name ? func_name : "<nameless>"),
		       scm_from_utf8_string (fmt), args,
		       scm_list_1 (scm_from_int (status)));
}



void
mu_guile_g_error (const char *func_name, GError *err)
{	
	scm_error_scm (scm_from_locale_symbol ("MuError"),
		       scm_from_utf8_string (func_name),
		       scm_from_utf8_string (err->message),
		       SCM_UNDEFINED, SCM_UNDEFINED);
}



void
mu_guile_init (void)
{
	scm_with_guile (&mu_guile_msg_init, NULL);
	scm_with_guile (&mu_guile_store_init, NULL);
	scm_with_guile (&mu_guile_log_init, NULL);
}


/*
 * backward compat for pre-2.x guile - note, this will fail miserably
 * if you don't use a UTF8 locale 
 */ 
#if HAVE_PRE2_GUILE

SCM
scm_from_utf8_string (const char* str)
{
	return scm_from_locale_string (str);
}

char*
scm_to_utf8_string (SCM scm)
{
	return scm_to_locale_string (scm);
}

#endif /*HAVE_PRE2_GUILE*/
