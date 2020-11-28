/*
** Copyright (C) 2011-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef __MU_WIDGET_UTIL_H__
#define __MU_WIDGET_UTIL_H__

#include <gdk-pixbuf/gdk-pixbuf.h>

G_BEGIN_DECLS
/**
 * get a pixbuf (icon) for a certain content-type (ie., 'image/jpeg')
 *
 * @param ctype the content-type (MIME-type)
 * @param size the size of the icon
 *
 * @return a new GdkPixbuf, or NULL in case of error. Use
 * g_object_unref when the pixbuf is no longer needed.
 */
GdkPixbuf* mu_widget_util_get_icon_pixbuf_for_content_type (const char *ctype,
							    size_t size)
      G_GNUC_WARN_UNUSED_RESULT;


G_END_DECLS

#endif /*__MU_WIDGET_UTIL_H__*/
