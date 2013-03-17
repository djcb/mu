/*
** Copyright (C) 2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef __MUG_SHORTCUTS_H__
#define __MUG_SHORTCUTS_H__

#if HAVE_CONFIG_H
#include <config.h>
#endif /*HAVE_CONFIG_H*/


#include <gtk/gtk.h>
/* other include files */

G_BEGIN_DECLS
/* convenience macros */
#define MUG_TYPE_SHORTCUTS             (mug_shortcuts_get_type())
#define MUG_SHORTCUTS(obj)             (G_TYPE_CHECK_INSTANCE_CAST((obj),MUG_TYPE_SHORTCUTS,MugShortcuts))
#define MUG_SHORTCUTS_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST((klass),MUG_TYPE_SHORTCUTS,MugShortcutsClass))
#define MUG_IS_SHORTCUTS(obj)          (G_TYPE_CHECK_INSTANCE_TYPE((obj),MUG_TYPE_SHORTCUTS))
#define MUG_IS_SHORTCUTS_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE((klass),MUG_TYPE_SHORTCUTS))
#define MUG_SHORTCUTS_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS((obj),MUG_TYPE_SHORTCUTS,MugShortcutsClass))
typedef struct _MugShortcuts MugShortcuts;
typedef struct _MugShortcutsClass MugShortcutsClass;
typedef struct _MugShortcutsPrivate MugShortcutsPrivate;

struct _MugShortcuts {
	GtkBox parent;
	/* private */
	MugShortcutsPrivate *_priv;
};

struct _MugShortcutsClass {
	GtkBoxClass parent_class;
	void (*clicked) (MugShortcuts * obj, const char *query);
};

/* member functions */
GType mug_shortcuts_get_type (void) G_GNUC_CONST;

/* parameter-less _new function (constructor) */
/* if this is a kind of GtkWidget, it should probably return at GtkWidget* */
GtkWidget *
mug_shortcuts_new (const char *bmpath);

/* fill in other public functions, e.g.: */
/* 	void       mug_shortcuts_do_something (MugShortcuts *self, const gchar* param); */
/* 	gboolean   mug_shortcuts_has_foo      (MugShortcuts *self, gint value); */

G_END_DECLS

#endif	/* __MUG_SHORTCUTS_H__ */
