/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/
/*
** Copyright (C) 2010-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <glib.h>
#include "mu-bookmarks.h"

#define MU_BOOKMARK_GROUP "mu"

struct _MuBookmarks {
	char		*_bmpath;
	GHashTable	*_hash;
};


static void
fill_hash (GHashTable *hash, GKeyFile *kfile)
{
	gchar **keys, **cur;

	keys = g_key_file_get_keys (kfile, MU_BOOKMARK_GROUP, NULL, NULL);
	if (!keys)
		return;

	for (cur = keys; *cur; ++cur) {
		gchar *val;
		val = g_key_file_get_string (kfile, MU_BOOKMARK_GROUP,
					     *cur, NULL);
		if (val)
			g_hash_table_insert (hash, *cur, val);
	}

	/* don't use g_strfreev, because we put them in the hash table;
	 * only free the gchar** itself */
	g_free (keys);
}

static GHashTable*
create_hash_from_key_file (const gchar *bmpath)
{
	GKeyFile *kfile;
	GHashTable *hash;

	kfile = g_key_file_new ();

	if (!g_key_file_load_from_file (kfile, bmpath, G_KEY_FILE_NONE, NULL)) {
		g_key_file_free (kfile);
		return NULL;
	}

	hash = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_free);
	fill_hash (hash, kfile);

	g_key_file_free (kfile);

	return hash;
}



MuBookmarks*
mu_bookmarks_new (const gchar *bmpath)
{
	MuBookmarks *bookmarks;
	GHashTable *hash;

	g_return_val_if_fail (bmpath, NULL);

	hash = create_hash_from_key_file (bmpath);
	if (!hash)
		return NULL;

	bookmarks	   = g_new (MuBookmarks, 1);

	bookmarks->_bmpath = g_strdup (bmpath);
	bookmarks->_hash   = hash;

	return bookmarks;
}



void
mu_bookmarks_destroy (MuBookmarks *bm)
{
	if (!bm)
		return;

	g_free (bm->_bmpath);
	g_hash_table_destroy (bm->_hash);
	g_free (bm);
}

const gchar*
mu_bookmarks_lookup (MuBookmarks *bm, const gchar *name)
{
	g_return_val_if_fail (bm, NULL);
	g_return_val_if_fail (name, NULL);

	return g_hash_table_lookup (bm->_hash, name);
}

struct _BMData {
	MuBookmarksForeachFunc _func;
	gpointer _user_data;
};
typedef struct _BMData BMData;


static void
each_bookmark (const gchar* key, const gchar *val, BMData *bmdata)
{
	bmdata->_func (key, val, bmdata->_user_data);
}


void
mu_bookmarks_foreach (MuBookmarks *bm, MuBookmarksForeachFunc func,
		      gpointer user_data)
{
	BMData bmdata;

	g_return_if_fail (bm);
	g_return_if_fail (func);

	bmdata._func	  = func;
	bmdata._user_data = user_data;

	g_hash_table_foreach (bm->_hash, (GHFunc)each_bookmark, &bmdata);
}
