/*
** Copyright (C) 2010-2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mug-shortcuts.h"
#include "mu-bookmarks.hh"

/* include other impl specific header files */

/* 'private'/'protected' functions */
static void mug_shortcuts_class_init (MugShortcutsClass * klass);
static void mug_shortcuts_init (MugShortcuts * obj);
static void mug_shortcuts_finalize (GObject * obj);

#define MUG_SHORTCUT_BOOKMARK "bookmark"

/* list my signals  */
enum {
	SHORTCUT_CLICKED,
	/* MY_SIGNAL_1, */
	/* MY_SIGNAL_2, */
	LAST_SIGNAL
};

struct _MugShortcutsPrivate {
	GtkWidget *_bbox;

};
#define MUG_SHORTCUTS_GET_PRIVATE(o)      (G_TYPE_INSTANCE_GET_PRIVATE((o), \
                                           MUG_TYPE_SHORTCUTS, \
                                           MugShortcutsPrivate))
/* globals */

static guint signals[LAST_SIGNAL] = { 0 };


static GtkBoxClass *parent_class = NULL;
G_DEFINE_TYPE (MugShortcuts, mug_shortcuts, GTK_TYPE_BOX);

static void
mug_shortcuts_class_init (MugShortcutsClass * klass)
{
	GObjectClass *gobject_class;
	gobject_class = (GObjectClass *) klass;

	parent_class = (GtkBoxClass*)g_type_class_peek_parent (klass);
	gobject_class->finalize = mug_shortcuts_finalize;

	g_type_class_add_private (gobject_class, sizeof (MugShortcutsPrivate));

	/* signal definitions go here, e.g.: */
	signals[SHORTCUT_CLICKED] =
	    g_signal_new ("clicked",
			  G_TYPE_FROM_CLASS (gobject_class),
			  G_SIGNAL_RUN_FIRST,
			  G_STRUCT_OFFSET (MugShortcutsClass, clicked),
			  NULL, NULL,
			  g_cclosure_marshal_VOID__STRING,
			  G_TYPE_NONE, 1, G_TYPE_STRING);

/* 	signals[MY_SIGNAL_2] = */
/* 		g_signal_new ("my_signal_2",....); */
/* 	etc. */
}

static void
mug_shortcuts_init (MugShortcuts * obj)
{
	obj->_priv = MUG_SHORTCUTS_GET_PRIVATE (obj);
	obj->_priv->_bbox = gtk_button_box_new (GTK_ORIENTATION_VERTICAL);

	gtk_button_box_set_layout (GTK_BUTTON_BOX (obj->_priv->_bbox),
				   GTK_BUTTONBOX_START);
	gtk_box_pack_start (GTK_BOX (obj), obj->_priv->_bbox, TRUE, TRUE, 0);
}

static void
mug_shortcuts_finalize (GObject * obj)
{
/* 	free/unref instance resources here */
	G_OBJECT_CLASS (parent_class)->finalize (obj);
}

static void
on_button_clicked (GtkWidget * button, MugShortcuts * self)
{
	g_signal_emit (G_OBJECT (self),
		       signals[SHORTCUT_CLICKED], 0,
		       (const gchar *)g_object_get_data (G_OBJECT (button),
							 MUG_SHORTCUT_BOOKMARK));
}

static void
each_bookmark (const char *key, const char *val, MugShortcuts * self)
{
	GtkWidget *button;

	button = gtk_button_new_with_label (key);
	g_object_set_data_full (G_OBJECT (button), MUG_SHORTCUT_BOOKMARK,
				g_strdup (val), g_free);
	g_signal_connect (G_OBJECT (button), "clicked",
			  G_CALLBACK (on_button_clicked), self);

	gtk_container_add (GTK_CONTAINER (self->_priv->_bbox), button);
}

static gboolean
init_shortcuts (MugShortcuts * self, const char *bmpath)
{
	MuBookmarks *bookmarks;

	bookmarks = mu_bookmarks_new (bmpath);
	if (!bookmarks)
		return TRUE;

	mu_bookmarks_foreach (bookmarks, (MuBookmarksForeachFunc) each_bookmark,
			      self);

	mu_bookmarks_destroy (bookmarks);
	return TRUE;
}

GtkWidget *
mug_shortcuts_new (const char *bmpath)
{
	MugShortcuts *self;

	self = MUG_SHORTCUTS (g_object_new (MUG_TYPE_SHORTCUTS, NULL));
	if (!init_shortcuts (self, bmpath)) {
		g_object_unref (self);
		return NULL;
	}

	return GTK_WIDGET (self);
}

/* following: other function implementations */
/* such as mug_shortcuts_do_something, or mug_shortcuts_has_foo */
