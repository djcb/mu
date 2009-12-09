/*
** Copyright (C) 2008 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef __MU_UTIL_H__
#define __MU_UTIL_H__

#include <glib.h>

G_BEGIN_DECLS

/** 
 * get the expanded path; ie. perform shell expansion on the path
 *
 * @param path path to expand
 * 
 * @return the expanded path as a newly allocated string, or NULL in
 * case of error
 */
char*       mu_util_dir_expand (const char* path);



/** 
 * guess the maildir; first try MAILDIR, then try ~/Maildir
 * if both fail, return NULL
 * 
 * @return full path of the guessed Maildir, or NULL; must be freed (gfree)
 */
char*       mu_util_guess_maildir (void);

/** 
 * take a char*[] and turn it into a GSList
 * 
 * @param argc numbers of strings
 * @param argv array of strings
 * 
 * @return a newly allocated GSList of the arguments; or NULL in case
 * of error. use mu_exprs_helpers_strlist_free when done with the list
 */
GSList   *mu_util_strlist_from_args    (int argc, char *argv[]);



/** 
 * free a list of strings, as produced by mu_expr_helpers_strlist_from_args or
 * mu_expr_helpers_strlist_from_str
 *
 * @param lst a list or NULL
 */
void      mu_util_strlist_free         (GSList *lst);


G_END_DECLS

#endif /*__MU_UTIL_H__*/
