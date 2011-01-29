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

#include "mu-msg-view.h"

#include "mu-msg-normal-view.h"
#include "mu-msg-body-view.h"
#include "mu-msg-source-view.h"

#include <webkit/webkitwebview.h>

#include <mu-msg.h>
#include <mu-msg-part.h>

/* 'private'/'protected' functions */
static void mu_msg_view_class_init (MuMsgViewClass *klass);
static void mu_msg_view_init       (MuMsgView *obj);
static void mu_msg_view_finalize   (GObject *obj);

/* list my signals  */
enum {
	/* MY_SIGNAL_1, */
	/* MY_SIGNAL_2, */
	LAST_SIGNAL
};

struct _MuMsgViewPrivate {

	/* 'normal' view */
	GtkWidget *_normal_view, *_source_view, *_internal_view;
	
	/* TRUE if we're in view-source mode, FALSE otherwise */
	gboolean _view_source;

	/* the message */
	MuMsg *_msg;
};
#define MU_MSG_VIEW_GET_PRIVATE(o)      (G_TYPE_INSTANCE_GET_PRIVATE((o), \
                                         MU_TYPE_MSG_VIEW, \
                                         MuMsgViewPrivate))
/* globals */
static GtkVBoxClass *parent_class = NULL;

/* uncomment the following if you have defined any signals */
/* static guint signals[LAST_SIGNAL] = {0}; */

G_DEFINE_TYPE (MuMsgView, mu_msg_view, GTK_TYPE_VBOX);

static void
mu_msg_view_class_init (MuMsgViewClass *klass)
{
	GObjectClass *gobject_class;
	gobject_class = (GObjectClass*) klass;

	parent_class            = g_type_class_peek_parent (klass);
	gobject_class->finalize = mu_msg_view_finalize;

	g_type_class_add_private (gobject_class, sizeof(MuMsgViewPrivate));

	/* signal definitions go here, e.g.: */
/* 	signals[MY_SIGNAL_1] = */
/* 		g_signal_new ("my_signal_1",....); */
/* 	signals[MY_SIGNAL_2] = */
/* 		g_signal_new ("my_signal_2",....); */
/* 	etc. */
}

static void
each_child_remove (GtkWidget *child, MuMsgView *self)
{
	/* g_object_ref (child); /\* save the children *\/ */	
	gtk_container_remove (GTK_CONTAINER(self), child);
}
	

static void
remove_widgets (MuMsgView *self)
{
	/* FIXME: keep the widgets around for re-use */

	/* remove the old children */
	gtk_container_foreach (GTK_CONTAINER(self),
			       (GtkCallback)each_child_remove,
			       self);
	
	self->_priv->_normal_view   = NULL;
	self->_priv->_source_view   = NULL;
	self->_priv->_internal_view = NULL;

}

static void
on_action_requested (GtkWidget *w, const char* action,
				 MuMsgView *self)
{
	if (g_strcmp0 (action, "view-source") == 0) {

		self->_priv->_view_source = TRUE;
		if (self->_priv->_msg)
			mu_msg_ref (self->_priv->_msg);
		mu_msg_view_set_message (self, self->_priv->_msg);

	} else if (g_strcmp0 (action, "view-message") == 0) {

		self->_priv->_view_source = FALSE;
		if (self->_priv->_msg)
			mu_msg_ref (self->_priv->_msg);	
		mu_msg_view_set_message (self, self->_priv->_msg);

	} else if (g_strcmp0 (action, "reindex") == 0) {
		g_warning ("reindex");
	} else {
		g_warning ("unknown action '%s'", action);
	}	
}



static GtkWidget*
get_source_view (MuMsgView *self, MuMsg *msg)
{
	if (!self->_priv->_source_view) {
		self->_priv->_source_view = mu_msg_source_view_new ();
		g_signal_connect (self->_priv->_source_view,
				  "action-requested",
				  G_CALLBACK(on_action_requested),
				  self);
	}
	
	mu_msg_source_view_set_message
		(MU_MSG_SOURCE_VIEW (self->_priv->_source_view), msg);

	gtk_widget_show_all (self->_priv->_source_view);
	
	return self->_priv->_source_view;
}

static GtkWidget*
get_normal_view (MuMsgView *self, MuMsg *msg)
{
	GtkWidget *scrolledwin;
	
	if (self->_priv->_normal_view) {
		mu_msg_normal_view_set_message
			(MU_MSG_NORMAL_VIEW(self->_priv->_normal_view), msg);
		return gtk_widget_get_parent (self->_priv->_normal_view);
	}
		
	self->_priv->_normal_view = mu_msg_normal_view_new ();
	
	mu_msg_normal_view_set_message
		(MU_MSG_NORMAL_VIEW(self->_priv->_normal_view), msg);
	
	g_signal_connect (self->_priv->_normal_view,
			  "action-requested",
			  G_CALLBACK(on_action_requested),
			  self);		

	scrolledwin = gtk_scrolled_window_new (NULL, NULL);
	gtk_scrolled_window_set_policy (
		GTK_SCROLLED_WINDOW(scrolledwin),
		GTK_POLICY_AUTOMATIC,GTK_POLICY_AUTOMATIC);
	
	gtk_scrolled_window_add_with_viewport
		(GTK_SCROLLED_WINDOW(scrolledwin),
		 self->_priv->_normal_view);
	
	gtk_widget_show (self->_priv->_normal_view);
	gtk_widget_show (scrolledwin);
	
	return scrolledwin;
}


static GtkWidget*
get_internal_view (MuMsgView *self, const char *html)
{
	if (!self->_priv->_internal_view)  {
		self->_priv->_internal_view = mu_msg_body_view_new();
		g_signal_connect (self->_priv->_internal_view,
				  "action-requested",
				  G_CALLBACK(on_action_requested),
				  self);
	}
	mu_msg_body_view_set_note (MU_MSG_BODY_VIEW(self->_priv->_internal_view),
				   html);
				   	
	gtk_widget_show (self->_priv->_internal_view);
	
	return self->_priv->_internal_view;
}



static void
mu_msg_view_init (MuMsgView *self)
{
 	self->_priv = MU_MSG_VIEW_GET_PRIVATE(self);
	
	self->_priv->_normal_view   = NULL;
	self->_priv->_source_view   = NULL;
	self->_priv->_internal_view = NULL;
	
	self->_priv->_view_source = FALSE;
}

static void
mu_msg_view_finalize (GObject *obj)
{
	MuMsgViewPrivate *priv;

	priv = MU_MSG_VIEW_GET_PRIVATE(obj);
	
	if (priv->_msg)
		mu_msg_unref (priv->_msg);
	
/* 	free/unref instance resources here */
	G_OBJECT_CLASS(parent_class)->finalize (obj);
}

GtkWidget*
mu_msg_view_new (void)
{
	return GTK_WIDGET(g_object_new(MU_TYPE_MSG_VIEW, NULL));
}



void
mu_msg_view_set_message (MuMsgView *self, MuMsg *msg)
{	
	g_return_if_fail (MU_IS_MSG_VIEW(self));
	
	if (self->_priv->_msg) 
		mu_msg_unref (self->_priv->_msg);

	self->_priv->_msg = msg ? mu_msg_ref (msg) : NULL;

	remove_widgets (self);

	if (!msg)
		return;
	
	if (!self->_priv->_view_source)
		gtk_box_pack_start (GTK_BOX(self),
				    get_normal_view (self, msg),
				    TRUE, TRUE, 0);
	else
		gtk_box_pack_start (GTK_BOX(self),
				    get_source_view (self, msg),
				    TRUE, TRUE, 0);
}


void
mu_msg_view_set_note (MuMsgView *self, const gchar* html)
{

	g_return_if_fail (MU_IS_MSG_VIEW(self));

	remove_widgets (self);
	
	gtk_box_pack_start (GTK_BOX(self),
			    get_internal_view (self, html),
			    TRUE, TRUE, 0);
}
