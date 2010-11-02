/*
** Copyright (C) 2008-2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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


#include "mug-msg-view.h"
#include "mu-msg.h"

/* 'private'/'protected' functions */
static void mug_msg_view_class_init (MugMsgViewClass *klass);
static void mug_msg_view_init       (MugMsgView *obj);
static void mug_msg_view_finalize   (GObject *obj);

/* list my signals  */
enum {
	/* MY_SIGNAL_1, */
	/* MY_SIGNAL_2, */
	LAST_SIGNAL
};

typedef struct _MugMsgViewPrivate MugMsgViewPrivate;
struct _MugMsgViewPrivate {
	GtkWidget *_view;
};
#define MUG_MSG_VIEW_GET_PRIVATE(o)      (G_TYPE_INSTANCE_GET_PRIVATE((o), \
						MUG_TYPE_MSG_VIEW, MugMsgViewPrivate))
/* globals */
static GtkVBoxClass *parent_class = NULL;

/* uncomment the following if you have defined any signals */
/* static guint signals[LAST_SIGNAL] = {0}; */

GType
mug_msg_view_get_type (void)
{
	static GType my_type = 0;
	if (!my_type) {
		static const GTypeInfo my_info = {
			sizeof(MugMsgViewClass),
			NULL,		/* base init */
			NULL,		/* base finalize */
			(GClassInitFunc) mug_msg_view_class_init,
			NULL,		/* class finalize */
			NULL,		/* class data */
			sizeof(MugMsgView),
			0,		/* n_preallocs, ignored since 2.10 */
			(GInstanceInitFunc) mug_msg_view_init,
			NULL
		};
		my_type = g_type_register_static (GTK_TYPE_VBOX,
		                                  "MugMsgView",
		                                  &my_info, 0);
	}
	return my_type;
}

static void
mug_msg_view_class_init (MugMsgViewClass *klass)
{
	GObjectClass *gobject_class;
	gobject_class = (GObjectClass*) klass;

	parent_class            = g_type_class_peek_parent (klass);
	gobject_class->finalize = mug_msg_view_finalize;

	g_type_class_add_private (gobject_class, sizeof(MugMsgViewPrivate));

	/* signal definitions go here, e.g.: */
/* 	signals[MY_SIGNAL_1] = */
/* 		g_signal_new ("my_signal_1",....); */
/* 	signals[MY_SIGNAL_2] = */
/* 		g_signal_new ("my_signal_2",....); */
/* 	etc. */
}

static void
mug_msg_view_init (MugMsgView *obj)
{
	MugMsgViewPrivate *priv;

	priv = MUG_MSG_VIEW_GET_PRIVATE(obj);

	priv->_view = gtk_text_view_new ();
	gtk_text_view_set_editable (GTK_TEXT_VIEW(priv->_view), FALSE);
	gtk_text_view_set_left_margin (GTK_TEXT_VIEW(priv->_view), 10);	
	gtk_text_view_set_right_margin (GTK_TEXT_VIEW(priv->_view), 10);
	
	gtk_box_pack_start (GTK_BOX(obj), priv->_view, TRUE, TRUE, 0);
}

static void
mug_msg_view_finalize (GObject *obj)
{
/* 	free/unref instance resources here */
	G_OBJECT_CLASS(parent_class)->finalize (obj);
}

GtkWidget*
mug_msg_view_new (void)
{
	return GTK_WIDGET(g_object_new(MUG_TYPE_MSG_VIEW, NULL));
}
 

gboolean
mug_msg_view_set_msg (MugMsgView *self, const char* msgpath)
{
	MugMsgViewPrivate *priv;
	MuMsg* msg;
	const char *txt;
	GtkTextBuffer *buf;
	
	g_return_val_if_fail (MUG_IS_MSG_VIEW(self), FALSE);
	g_return_val_if_fail (msgpath, FALSE);

	priv = MUG_MSG_VIEW_GET_PRIVATE(self);

	msg = mu_msg_new (msgpath, NULL);
	if (!msg)
		return FALSE;

	txt = mu_msg_get_body_text (msg);
	buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW(priv->_view));

	gtk_text_buffer_set_text (buf, txt ? txt : "", -1);
	
	return TRUE;
}

