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

#ifndef __MU_GUILE_UTIL_H__
#define __MU_GUILE_UTIL_H__

#include <libguile.h>
#include <glib.h>

G_BEGIN_DECLS


/**
 * start a guile shell with the mu modules loaded. function does not return
 *
 * @param argcp pointer to argc
 * @param argvp pointer to argv
 *
 * @return FALSE in case of error, otherwise, the function will not return
 */
gboolean mu_guile_util_run (int *argcp, char **argvp[]);


/**
 * output an error
 *
 * @param func_name
 * @param status
 * @param fmt
 * @param args
 */
SCM mu_guile_util_error (const char *func_name, int status,
			  const char *fmt, SCM args);

/**
 * display a GError as a Guile error
 *
 * @param func_name function name
 * @param err Gerror
 */
SCM mu_guile_util_g_error (const char *func_name, GError *err);

G_END_DECLS

#endif /*__MU_GUILE_UTIL_H__*/
