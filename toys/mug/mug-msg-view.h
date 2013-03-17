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

#ifndef __MUG_MSG_VIEW_H__
#define __MUG_MSG_VIEW_H__

#include <gtk/gtk.h>
/* other include files */

G_BEGIN_DECLS
/* convenience macros */
#define MUG_TYPE_MSG_VIEW             (mug_msg_view_get_type())
#define MUG_MSG_VIEW(obj)             (G_TYPE_CHECK_INSTANCE_CAST((obj),MUG_TYPE_MSG_VIEW,MugMsgView))
#define MUG_MSG_VIEW_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST((klass),MUG_TYPE_MSG_VIEW,MugMsgViewClass))
#define MUG_IS_MSG_VIEW(obj)          (G_TYPE_CHECK_INSTANCE_TYPE((obj),MUG_TYPE_MSG_VIEW))
#define MUG_IS_MSG_VIEW_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE((klass),MUG_TYPE_MSG_VIEW))
#define MUG_MSG_VIEW_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS((obj),MUG_TYPE_MSG_VIEW,MugMsgViewClass))
typedef struct _MugMsgView MugMsgView;
typedef struct _MugMsgViewClass MugMsgViewClass;

struct _MugMsgView {
	GtkBox parent;
};

struct _MugMsgViewClass {
	GtkBoxClass parent_class;
	/* insert signal callback declarations, e.g. */
	/* void (* my_event) (MugMsg* obj); */
};

/* member functions */
GType mug_msg_view_get_type (void) G_GNUC_CONST;

/* parameter-less _new function (constructor) */
/* if this is a kind of GtkWidget, it should probably return at GtkWidget* */
GtkWidget* mug_msg_view_new (void);
gboolean mug_msg_view_set_msg (MugMsgView * self, const char *msgpath);
void mug_msg_view_set_note (MugMsgView * self, const char* html);

G_END_DECLS

#endif	/* __MUG_MSG_VIEW_H__ */
