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

#ifndef __MU_QUERY_XAPIAN_H__
#define __MU_QUERY_XAPIAN_H__

#include <glib.h>
#include "mu-msg-xapian.h"

G_BEGIN_DECLS


/*
 * MuQueryXapian
 */

struct _MuQueryXapian;
typedef struct _MuQueryXapian MuQueryXapian;

/** 
 * create a new MuQueryXapian instance. 
 * 
 * @param path path to the xapian db to search
 * @param err receives error information (if there is any)
 *
 * @return a new MuQueryXapian instance, or NULL in case of error.
 * when the instance is no longer needed, use mu_query_xapian_destroy
 * to free it
 */
MuQueryXapian  *mu_query_xapian_new      (const char* path);

/** 
 * destroy the MuQueryXapian instance
 * 
 * @param self a MuQueryXapian instance, or NULL
 */
void            mu_query_xapian_destroy  (MuQueryXapian *self);


/** 
 * run a Xapian query; for the syntax, please refer to the mu-find
 * manpage, or http://xapian.org/docs/queryparser.html
 * 
 * @param self a valid MuQueryXapian instance
 * @param expr the search expression
 *
 * @return a MuMsgXapian instance you can iterate over, or NULL in
 * case of error
 */
MuMsgXapian*     mu_query_xapian_run      (MuQueryXapian *self, 
					   const char* expr,
					   const MuMsgField* sortfield,
					   gboolean ascending);

/** 
 * create a xapian query from list of expressions; for the syntax,
 * please refer to the mu-find manpage, or
 * http://xapian.org/docs/queryparser.html
 *
 * @param string array of search expressions
 * @param connect_or if TRUE, combine the expressions with OR, otherwise use AND
 * 
 * @return a string with the combined xapian expression or NULL in
 * case of error; free with g_free when it's no longer needed
 */
char*  mu_query_xapian_combine  (gchar **params, gboolean connect_or);

/** 
 * get a string representation of the Xapian search query
 * 
 * @param self a MuQueryXapian instance 
 * @param searchexpr a xapian search expression
 * 
 * @return the string representation of the xapian query, or NULL in case of
 * error; free the returned value with g_free
 */
char* mu_query_xapian_as_string  (MuQueryXapian *self, const char* searchexpr);


G_END_DECLS

#endif /*__MU_QUERY_XAPIAN_H__*/
