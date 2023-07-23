/*
** Copyright (C) 2011-2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef __MU_GUILE_H__
#define __MU_GUILE_H__

#include <glib.h>
#include <mu-query.hh>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wredundant-decls"
#include <libguile.h>
#pragma GCC diagnostic pop

/**
 * get the singleton Store instance
 */
Mu::Store& mu_guile_store();

/**
 * whether mu-guile is initialized
 *
 * @return TRUE if MuGuile is Initialized, FALSE otherwise
 */
gboolean mu_guile_initialized();

/**
 * raise a guile error (based on a GError)
 *
 * @param func_name function name
 * @param err the error
 *
 * @return SCM_UNSPECIFIED
 */
SCM mu_guile_g_error(const char* func_name, GError* err);

/**
 * raise a guile error
 *
 * @param func_name function
 * @param status err code
 * @param fmt format string for error msg
 * @param args params for format string
 *
 * @return SCM_UNSPECIFIED
 */
SCM mu_guile_error(const char* func_name, int status, const char* fmt, SCM args);

/**
 * convert a string into an SCM -- . It assumes str is in UTF8 encoding, and
 * replace characters with '?' if needed.
 *
 * @param str a string
 *
 * @return a guile string or #f for empty
 */
SCM mu_guile_scm_from_string(const std::string& str);

/**
 * Initialize this mu guile module.
 *
 * @param data
 *
 * @return
 */
extern "C" {
void* mu_guile_init(void* data);
}
#endif /*__MU_GUILE_H__*/
