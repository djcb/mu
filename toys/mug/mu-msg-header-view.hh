/*
** Copyright (C) 2011-2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef __MU_MSG_HEADER_VIEW_H__
#define __MU_MSG_HEADER_VIEW_H__

#include <gtk/gtk.h>
#include <mu-msg.hh>

G_BEGIN_DECLS

/* convenience macros */
#define MU_TYPE_MSG_HEADER_VIEW             (mu_msg_header_view_get_type())
#define MU_MSG_HEADER_VIEW(obj)             (G_TYPE_CHECK_INSTANCE_CAST((obj),MU_TYPE_MSG_HEADER_VIEW,MuMsgHeaderView))
#define MU_MSG_HEADER_VIEW_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST((klass),MU_TYPE_MSG_HEADER_VIEW,MuMsgHeaderViewClass))
#define MU_IS_MSG_HEADER_VIEW(obj)          (G_TYPE_CHECK_INSTANCE_TYPE((obj),MU_TYPE_MSG_HEADER_VIEW))
#define MU_IS_MSG_HEADER_VIEW_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE((klass),MU_TYPE_MSG_HEADER_VIEW))
#define MU_MSG_HEADER_VIEW_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS((obj),MU_TYPE_MSG_HEADER_VIEW,MuMsgHeaderViewClass))

typedef struct _MuMsgHeaderView      MuMsgHeaderView;
typedef struct _MuMsgHeaderViewClass MuMsgHeaderViewClass;
typedef struct _MuMsgHeaderViewPrivate         MuMsgHeaderViewPrivate;

struct _MuMsgHeaderView {
	GtkBox parent;
	/* insert public members, if any */
	/* private */
	MuMsgHeaderViewPrivate *_priv;
};

struct _MuMsgHeaderViewClass {
	GtkBoxClass parent_class;
	/* insert signal callback declarations, e.g. */
	/* void (* my_event) (MuMsgHeaderView* obj); */
};

/* member functions */
GType        mu_msg_header_view_get_type    (void) G_GNUC_CONST;

/* parameter-less _new function (constructor) */
/* if this is a kind of GtkWidget, it should probably return at GtkWidget* */
GtkWidget*   mu_msg_header_view_new         (void);

struct MuMsg;
void mu_msg_header_view_set_message (MuMsgHeaderView *self, Mu::MuMsg *msg);


G_END_DECLS

#endif /* __MU_MSG_HEADER_VIEW_H__ */
