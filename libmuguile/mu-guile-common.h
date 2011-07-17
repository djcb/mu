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

#ifndef __MU_GUILE_UTILS_H__
#define __MU_GUILE_UTILS_H__

#include <libguile.h>

#if HAVE_CONFIG_H
#include <config.h>
#endif /*HAVE_CONFIG_H*/

#include <glib.h>

G_BEGIN_DECLS

/** 
 * 
 * 
 * @param func_name 
 * @param status 
 * @param fmt 
 * @param args 
 */
void mu_guile_error (const char *func_name, int status,
		     const char *fmt, SCM args);


/** 
 * display a GError as a Guile error
 * 
 * @param func_name function name
 * @param err Gerror
 */
void mu_guile_g_error (const char *func_name, GError *err);

/* compatibility functions for old guile */
#if HAVE_PRE2_GUILE
SCM   scm_from_utf8_string (const char* str);
char* scm_to_utf8_string (SCM scm);
#endif /*HAVE_PRE2_GUILE*/

G_END_DECLS

#endif /*__MU_GUILE_UTILS_H__*/

