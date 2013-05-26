/*
** Copyright (C) 2008-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef __MU_BOOKMARKS_H__
#define __MU_BOOKMARKS_H__

#include <glib.h>

G_BEGIN_DECLS
/**
 * @addtogroup MuBookmarks
 * Functions for dealing with bookmarks
 * @{
 */

struct _MuBookmarks;
/*! \struct MuBookmarks
 * \brief Opaque structure representing a sequence of bookmarks
 */
typedef struct _MuBookmarks MuBookmarks;


/**
 * create a new bookmarks object. when it's no longer needed, use
 * mu_bookmarks_destroy
 *
 * @param bmpath path to the bookmarks file
 *
 * @return a new BookMarks object, or NULL in case of error
 */
MuBookmarks *mu_bookmarks_new (const gchar *bmpath)
    G_GNUC_MALLOC G_GNUC_WARN_UNUSED_RESULT;

/**
 * destroy a bookmarks object
 *
 * @param bm a bookmarks object, or NULL
 */
void         mu_bookmarks_destroy (MuBookmarks *bm);


/**
 * get the value for some bookmark
 *
 * @param bm a valid bookmarks object
 * @param name name of the bookmark to retrieve
 *
 * @return the value of the bookmark or NULL in case in error, e.g. if
 * the bookmark was not found
 */
const gchar* mu_bookmarks_lookup (MuBookmarks *bm, const gchar *name);

typedef void (*MuBookmarksForeachFunc) (const gchar *key, const gchar *val,
					gpointer user_data);

/**
 * call a function for each bookmark
 *
 * @param bm a valid bookmarks object
 * @param func a callback function to be called for each bookmarks
 * @param user_data a user pointer passed to the callback
 */
void         mu_bookmarks_foreach (MuBookmarks *bm, MuBookmarksForeachFunc func,
				   gpointer user_data);

/** @} */

G_END_DECLS

#endif /*__MU_BOOKMARKS_H__*/
