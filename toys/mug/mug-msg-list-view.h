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

#ifndef __MUG_MSG_LIST_VIEW_H__
#define __MUG_MSG_LIST_VIEW_H__

#include <gtk/gtk.h>
#include <utils/mu-util.h>

G_BEGIN_DECLS
/* convenience macros */
#define MUG_TYPE_MSG_LIST_VIEW             (mug_msg_list_view_get_type())
#define MUG_MSG_LIST_VIEW(obj)             (G_TYPE_CHECK_INSTANCE_CAST((obj),MUG_TYPE_MSG_LIST_VIEW,MugMsgListView))
#define MUG_MSG_LIST_VIEW_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST((klass),MUG_TYPE_MSG_LIST_VIEW,MugMsgListViewClass))
#define MUG_IS_MSG_LIST_VIEW(obj)          (G_TYPE_CHECK_INSTANCE_TYPE((obj),MUG_TYPE_MSG_LIST_VIEW))
#define MUG_IS_MSG_LIST_VIEW_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE((klass),MUG_TYPE_MSG_LIST_VIEW))
#define MUG_MSG_LIST_VIEW_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS((obj),MUG_TYPE_MSG_LIST_VIEW,MugMsgListViewClass))
typedef struct _MugMsgListView MugMsgListView;
typedef struct _MugMsgListViewClass MugMsgListViewClass;

struct _MugMsgListView {
	GtkTreeView parent;
	/* insert public members, if any */
};

enum _MugError {
	MUG_ERROR_XAPIAN_NOT_UPTODATE,
	MUG_ERROR_XAPIAN_DIR,
	MUG_ERROR_QUERY,
	MUG_ERROR_OTHER
};
typedef enum _MugError MugError;

struct _MugMsgListViewClass {
	GtkTreeViewClass parent_class;
	/* insert signal callback declarations, e.g. */
	void (*msg_selected) (MugMsgListView * obj, const char *msgpath);
	void (*error_occured) (MugMsgListView * obj, MugError err);
};

/* member functions */
GType
mug_msg_list_view_get_type (void)
    G_GNUC_CONST;

/* parameter-less _new function (constructor) */
/* if this is a kind of GtkWidget, it should probably return at GtkWidget* */
GtkWidget *
mug_msg_list_view_new (const char *xpath);

int
mug_msg_list_view_query (MugMsgListView * self, const char *query);

void
mug_msg_list_view_move_first (MugMsgListView * self);

gboolean
mug_msg_list_view_move_prev (MugMsgListView * self);
gboolean
mug_msg_list_view_move_next (MugMsgListView * self);

const gchar *
mug_msg_list_view_get_query (MugMsgListView * self);

G_END_DECLS
#endif				/* __MUG_MSG_LIST_VIEW_H__ */
