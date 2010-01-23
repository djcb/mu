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


#ifndef __MU_UTIL_XAPIAN_H__
#define __MU_UTIL_XAPIAN_H__

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
char* mu_util_xapian_db_version (const gchar *xpath);


/** 
 * check if the 'schema' of the current database is up-to-date
 * 
 * @param xpath path to the xapian database
 * 
 * @return TRUE if it's up-to-date, FALSE otherwise
 */
gboolean mu_util_xapian_db_version_up_to_date (const gchar *xpath);

G_END_DECLS

#endif /*__MU_UTIL_XAPIAN_H__*/
