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


#include <gtk/gtk.h>
#include <gio/gio.h>
#include "mu-widget-util.h"

static const char*
second_guess_content_type (const char *ctype)
{
	int i;
	struct {
		const char *orig, *subst;
	} substtable [] = {
		{"text", "text/plain"},
		{"image/pjpeg", "image/jpeg" }
	};

	for (i = 0; i != G_N_ELEMENTS(substtable); ++i)
		if (g_str_has_prefix (ctype, substtable[i].orig))
			return substtable[i].subst;

	return "application/octet-stream";
}

static GdkPixbuf*
get_icon_pixbuf_for_content_type (const char *ctype, size_t size)
{
	GIcon *icon;
	GdkPixbuf *pixbuf;
	GError *err;

	icon = g_content_type_get_icon (ctype);
	pixbuf = NULL;
	err = NULL;

	/* based on a snippet from http://www.gtkforums.com/about4721.html */
	if (G_IS_THEMED_ICON(icon)) {
		gchar const * const *names;
		names = g_themed_icon_get_names (G_THEMED_ICON(icon));
		pixbuf = gtk_icon_theme_load_icon (gtk_icon_theme_get_default(),
						   *names, size, 0, &err);
	} else if (G_IS_FILE_ICON(icon)) {
		GFile *icon_file;
		gchar *path;
		icon_file = g_file_icon_get_file (G_FILE_ICON(icon));
		path = g_file_get_path (icon_file);
		pixbuf = gdk_pixbuf_new_from_file_at_size (path, size, size, &err);
		g_free (path);
		g_object_unref(icon_file);
	}
	g_object_unref(icon);

	if (err) {
		g_warning ("error: %s\n", err->message);
		g_clear_error (&err);
	}

	return pixbuf;
}


GdkPixbuf*
mu_widget_util_get_icon_pixbuf_for_content_type (const char *ctype, size_t size)
{
	GdkPixbuf *pixbuf;

	g_return_val_if_fail (ctype, NULL);
	g_return_val_if_fail (size > 0, NULL);

	pixbuf = get_icon_pixbuf_for_content_type (ctype, size);
	if (!pixbuf)
		pixbuf = get_icon_pixbuf_for_content_type
			(second_guess_content_type (ctype), size);

	return pixbuf;
}
