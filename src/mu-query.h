/*
** Copyright (C) 2008-2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef __MU_QUERY_H__
#define __MU_QUERY_H__

#include <glib.h>
#include <mu-store.h>
#include <mu-msg-iter.h>
#include <mu-util.h>

G_BEGIN_DECLS

struct _MuQuery;
typedef struct _MuQuery MuQuery;

/**
 * create a new MuQuery instance.
 *
 * @param store a MuStore object
 * @param err receives error information (if there is any); if
 * function returns non-NULL, err will _not_be set. err can be NULL
 * possble errors (err->code) are MU_ERROR_XAPIAN_DIR and
 * MU_ERROR_XAPIAN_NOT_UPTODATE
 *
 * @return a new MuQuery instance, or NULL in case of error.
 * when the instance is no longer needed, use mu_query_destroy
 * to free it
 */
MuQuery  *mu_query_new  (MuStore *store, GError **err)
      G_GNUC_MALLOC G_GNUC_WARN_UNUSED_RESULT;

/**
 * destroy the MuQuery instance
 *
 * @param self a MuQuery instance, or NULL
 */
void mu_query_destroy  (MuQuery *self);


/**
 * get a version string for the database
 *
 * @param store a valid MuQuery
 *
 * @return the version string (free with g_free), or NULL in case of error
 */
char* mu_query_version (MuQuery *store)
    G_GNUC_MALLOC G_GNUC_WARN_UNUSED_RESULT;

/**
 * run a Xapian query; for the syntax, please refer to the mu-find
 * manpage, or http://xapian.org/docs/queryparser.html
 *
 * @param self a valid MuQuery instance
 * @param expr the search expression or "" to match all messages
 * @param threads calculate message-threads
 * @param sortfield the field id to sort by or MU_MSG_FIELD_ID_NONE if
 * sorting is not desired
 * @param ascending if TRUE sort in ascending (A-Z) order, otherwise,
 * sort in descending (Z-A) order
 * @param err receives error information (if there is any); if
 * function returns non-NULL, err will _not_be set. err can be NULL
 * possible error (err->code) is MU_ERROR_QUERY,
 *
 * @return a MuMsgIter instance you can iterate over, or NULL in
 * case of error
 */
MuMsgIter* mu_query_run (MuQuery *self, const char* expr, gboolean threads,
			 MuMsgFieldId sortfieldid, gboolean ascending,
			 GError **err)
    G_GNUC_MALLOC G_GNUC_WARN_UNUSED_RESULT;

/**
 * get a string representation of the Xapian search query
 *
 * @param self a MuQuery instance
 * @param searchexpr a xapian search expression
 * @param err receives error information (if there is any); if
 * function returns non-NULL, err will _not_be set. err can be NULL
 *
 * @return the string representation of the xapian query, or NULL in case of
 * error; free the returned value with g_free
 */
char* mu_query_as_string (MuQuery *self, const char* searchexpr, GError **err)
    G_GNUC_MALLOC G_GNUC_WARN_UNUSED_RESULT;

/**
 * pre-process the query; this function is useful mainly for debugging mu
 *
 * @param query a query string
 *
 * @return a pre-processed query, free it with g_free
 */
char* mu_query_preprocess (const char *query)
        G_GNUC_MALLOC G_GNUC_WARN_UNUSED_RESULT;

G_END_DECLS

#endif /*__MU_QUERY_H__*/
