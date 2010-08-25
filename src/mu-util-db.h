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


#ifndef __MU_UTIL_DB_H__
#define __MU_UTIL_DB_H__

#include <glib.h>

G_BEGIN_DECLS

/**
 * get the version of the xapian database (ie., the version of the
 * 'schema' we are using). If this version != MU_XAPIAN_DB_VERSION,
 * it's means we need to a full reindex.
 * 
 * @param xpath path to the xapian database
 * 
 * @return the version of the database as a newly allocated string
 * (free with g_free); if there is no version yet, it will return NULL
 */
gchar* mu_util_db_version (const gchar *xpath) G_GNUC_WARN_UNUSED_RESULT;


/**
 * check whether the database is empty (contains 0 documents); in
 * addition, a non-existing database is considered 'empty' too
 * 
 * @param xpath path to the xapian database
 * 
 * @return TRUE if the database is empty, FALSE otherwise
 */
gboolean mu_util_db_is_empty (const gchar *xpath);

/**
 * check if the 'schema' of the current database is up-to-date
 * 
 * @param xpath path to the xapian database
 * 
 * @return TRUE if it's up-to-date, FALSE otherwise
 */
gboolean mu_util_db_version_up_to_date (const gchar *xpath);

/**
 * clear the database, ie., remove all of the contents. This is a
 * destructive operation, but the database can be restored be doing a
 * full scan of the maildirs.
 * 
 * @param xpath path to the database
 * 
 * @return TRUE if the clearing succeeded, FALSE otherwise.
 */
gboolean mu_util_clear_database (const gchar *xpath);


G_END_DECLS

#endif /*__MU_UTIL_DB_H__*/
