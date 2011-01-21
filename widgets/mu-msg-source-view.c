/*
** Copyright (C) 2011 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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


#include "mu-msg-source-view.h"
/* include other impl specific header files */

/* 'private'/'protected' functions */
static void mu_msg_source_view_class_init (MuMsgSourceViewClass *klass);
static void mu_msg_source_view_init       (MuMsgSourceView *obj);
static void mu_msg_source_view_finalize   (GObject *obj);

/* list my signals  */
enum {
	ACTION_REQUESTED,
	/* MY_SIGNAL_2, */
	LAST_SIGNAL
};

struct _MuMsgSourceViewPrivate {
	GtkWidget *_view;
};
#define MU_MSG_SOURCE_VIEW_GET_PRIVATE(o)      (G_TYPE_INSTANCE_GET_PRIVATE((o), \
                                                MU_TYPE_MSG_SOURCE_VIEW, \
                                                MuMsgSourceViewPrivate))
/* globals */
static GtkVBoxClass *parent_class = NULL;


static guint signals[LAST_SIGNAL] = {0};

G_DEFINE_TYPE (MuMsgSourceView, mu_msg_source_view, GTK_TYPE_VBOX);

static void
mu_msg_source_view_class_init (MuMsgSourceViewClass *klass)
{
	GObjectClass *gobject_class;
	gobject_class = (GObjectClass*) klass;

	parent_class            = g_type_class_peek_parent (klass);
	gobject_class->finalize = mu_msg_source_view_finalize;

	g_type_class_add_private (gobject_class, sizeof(MuMsgSourceViewPrivate));

	signals[ACTION_REQUESTED] =
		g_signal_new ("action-requested",
			      G_TYPE_FROM_CLASS (gobject_class),
			      G_SIGNAL_RUN_FIRST,
			      G_STRUCT_OFFSET (MuMsgSourceViewClass,
					       action_requested),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__STRING,
			      G_TYPE_NONE, 1, G_TYPE_STRING);
}



static void
on_menu_item_activate (GtkMenuItem *item, MuMsgSourceView *self)
{
	g_signal_emit (G_OBJECT(self),
		       signals[ACTION_REQUESTED], 0,
		       g_object_get_data (G_OBJECT(item), "action"));	
}

static void
popup_menu (MuMsgSourceView *self, guint button, guint32 activate_time)
{
	GtkWidget *menu;
	int i;
	struct {
		const char* title;
		const char* action;
	} actions[] = {
		{ "View message...", "view-message" }
	};

	menu = gtk_menu_new ();
	
	for (i = 0; i != G_N_ELEMENTS(actions); ++i) {
		GtkWidget *item;
		item = gtk_menu_item_new_with_label(actions[i].title);
		g_object_set_data (G_OBJECT(item), "action", (gpointer)actions[i].action);
		g_signal_connect (item, "activate", G_CALLBACK(on_menu_item_activate),
				  self);
		gtk_menu_attach (GTK_MENU(menu), item, 0, 1, i, i+1);
		gtk_widget_show (item);
	}
	gtk_menu_popup (GTK_MENU(menu), NULL, NULL, NULL, NULL, 0, 0);
}


static gboolean
on_button_press_event (GtkWidget *w, GdkEventButton *event,
		       MuMsgSourceView *self)
{
	/* ignore all but the first (typically, left) mouse button */
	switch (event->button) {
	case 1: return FALSE; /* propagate, let widget handle it */
	case 3: popup_menu (self, event->button, event->time);
	default: return TRUE; /* ignore */
	}
	
	return (event->button > 1) ? TRUE : FALSE;	
}



static void
mu_msg_source_view_init (MuMsgSourceView *self)
{
	self->_priv = MU_MSG_SOURCE_VIEW_GET_PRIVATE(self);

	self->_priv->_view = gtk_text_view_new ();

	/* handle right-button clicks */
	g_signal_connect (self->_priv->_view, "button-press-event",
			  G_CALLBACK(on_button_press_event), self);
	
	gtk_box_pack_start (GTK_BOX(self), self->_priv->_view,
			    TRUE, TRUE, 0);
}

static void
mu_msg_source_view_finalize (GObject *obj)
{
/* 	free/unref instance resources here */
	G_OBJECT_CLASS(parent_class)->finalize (obj);
}

GtkWidget*
mu_msg_source_view_new (void)
{
	return GTK_WIDGET(g_object_new(MU_TYPE_MSG_SOURCE_VIEW, NULL));
}


void
mu_msg_source_view_set_message (MuMsgSourceView *self, MuMsg *msg)
{
	GtkTextBuffer *buf;
	const gchar *path;
	gchar *data;

	g_return_if_fail (MU_IS_MSG_SOURCE_VIEW(self));
	g_return_if_fail (msg);
	
	buf = gtk_text_view_get_buffer
		(GTK_TEXT_VIEW(self->_priv->_view));
	path = msg ? mu_msg_get_path (msg) : NULL;
	
	if (path && g_file_get_contents (path, &data, NULL, NULL)) {
		gtk_text_buffer_set_text (buf, data, -1);
		g_free (data);
	} else
		gtk_text_buffer_set_text (buf, "", 0);
}

