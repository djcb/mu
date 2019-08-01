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

#ifndef __MU_MSG_BODY_VIEW_H__
#define __MU_MSG_BODY_VIEW_H__

#include <webkit2/webkit2.h>
#include <mu-msg.h>

G_BEGIN_DECLS

/* convenience macros */
#define MU_TYPE_MSG_BODY_VIEW             (mu_msg_body_view_get_type())
#define MU_MSG_BODY_VIEW(obj)             (G_TYPE_CHECK_INSTANCE_CAST((obj),MU_TYPE_MSG_BODY_VIEW,MuMsgBodyView))
#define MU_MSG_BODY_VIEW_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST((klass),MU_TYPE_MSG_BODY_VIEW,MuMsgBodyViewClass))
#define MU_IS_MSG_BODY_VIEW(obj)          (G_TYPE_CHECK_INSTANCE_TYPE((obj),MU_TYPE_MSG_BODY_VIEW))
#define MU_IS_MSG_BODY_VIEW_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE((klass),MU_TYPE_MSG_BODY_VIEW))
#define MU_MSG_BODY_VIEW_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS((obj),MU_TYPE_MSG_BODY_VIEW,MuMsgBodyViewClass))

typedef struct _MuMsgBodyView      MuMsgBodyView;
typedef struct _MuMsgBodyViewClass MuMsgBodyViewClass;
typedef struct _MuMsgBodyViewPrivate         MuMsgBodyViewPrivate;


struct _MuMsgBodyView {
	 WebKitWebView parent;
	/* insert public members, if any */

	/* private */
	MuMsgBodyViewPrivate *_priv;
};

struct _MuMsgBodyViewClass {
	WebKitWebViewClass parent_class;

	/* supported actions: "reindex", "view-source" */
	void (* action_requested) (MuMsgBodyView* self, const char* action);
};

/* member functions */
GType        mu_msg_body_view_get_type    (void) G_GNUC_CONST;

/* parameter-less _new function (constructor) */
/* if this is a kind of GtkWidget, it should probably return at GtkWidget* */
GtkWidget*    mu_msg_body_view_new         (void);

void mu_msg_body_view_set_message (MuMsgBodyView *self, MuMsg *msg);
void mu_msg_body_view_set_note (MuMsgBodyView *self, const gchar *html);
void mu_msg_body_view_set_message_source (MuMsgBodyView *self, MuMsg *msg);

G_END_DECLS

#endif /* __MU_MSG_BODY_VIEW_H__ */
