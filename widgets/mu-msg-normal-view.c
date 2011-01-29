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


#include "mu-msg-normal-view.h"

#include "mu-msg-body-view.h"
#include "mu-msg-attach-view.h"
#include "mu-msg-header-view.h"
#include "mu-msg-part.h"


/* 'private'/'protected' functions */
static void mu_msg_normal_view_class_init (MuMsgNormalViewClass *klass);
static void mu_msg_normal_view_init       (MuMsgNormalView *obj);
static void mu_msg_normal_view_finalize   (GObject *obj);

/* list my signals  */
enum {
	ACTION_REQUESTED,
	/* MY_SIGNAL_2, */
	LAST_SIGNAL
};

struct _MuMsgNormalViewPrivate {

	GtkWidget *_headers;
	GtkWidget *_body;
	GtkWidget *_attach, *_attacharea;
};
#define MU_MSG_NORMAL_VIEW_GET_PRIVATE(o)      (G_TYPE_INSTANCE_GET_PRIVATE((o), \
                                                MU_TYPE_MSG_NORMAL_VIEW, \
                                                MuMsgNormalViewPrivate))
/* globals */
static GtkVBoxClass *parent_class = NULL;


static guint signals[LAST_SIGNAL] = {0};

G_DEFINE_TYPE (MuMsgNormalView, mu_msg_normal_view, GTK_TYPE_VBOX);

static void
mu_msg_normal_view_class_init (MuMsgNormalViewClass *klass)
{
	GObjectClass *gobject_class;
	gobject_class = (GObjectClass*) klass;

	parent_class            = g_type_class_peek_parent (klass);
	gobject_class->finalize = mu_msg_normal_view_finalize;

	g_type_class_add_private (gobject_class, sizeof(MuMsgNormalViewPrivate));
	
	signals[ACTION_REQUESTED] =
		g_signal_new ("action-requested",
			      G_TYPE_FROM_CLASS (gobject_class),
			      G_SIGNAL_RUN_FIRST,
			      G_STRUCT_OFFSET (MuMsgNormalViewClass,
					       action_requested),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__STRING,
			      G_TYPE_NONE, 1, G_TYPE_STRING);
}


static GtkWidget*
get_header_widget (MuMsgNormalView *self)
{
	GtkWidget *scrolledwin;
	
	self->_priv->_headers = mu_msg_header_view_new ();
	
	scrolledwin = gtk_scrolled_window_new (NULL, NULL);
	gtk_scrolled_window_set_policy (
		GTK_SCROLLED_WINDOW(scrolledwin),
		GTK_POLICY_AUTOMATIC, GTK_POLICY_NEVER);
	gtk_scrolled_window_add_with_viewport
		(GTK_SCROLLED_WINDOW(scrolledwin), self->_priv->_headers);

	gtk_widget_show_all (scrolledwin);
	
	return scrolledwin;
}

static void
on_body_action_requested (MuMsgBodyView *body, const char* action,
			  MuMsgNormalView *self)
{
	/* forward the signal */
	g_signal_emit (G_OBJECT(self),
		       signals[ACTION_REQUESTED], 0, action);
}


static GtkWidget*
get_body_widget (MuMsgNormalView *self)
{
	self->_priv->_body = mu_msg_body_view_new ();
	g_signal_connect (self->_priv->_body, "action-requested",
			  G_CALLBACK(on_body_action_requested), self);

	gtk_widget_show (self->_priv->_body);
	return self->_priv->_body;
}


static void
on_attach_activated (GtkWidget *w, guint partnum, MuMsg *msg)
{
	gchar *filepath;
        
	filepath = mu_msg_part_filepath_cache (msg, partnum);
	if (filepath) {
		mu_msg_part_save (msg, filepath, partnum, FALSE, TRUE);
		mu_util_play (filepath, TRUE, FALSE);
		g_free (filepath);
	}
}


static GtkWidget*
get_attach_widget (MuMsgNormalView *self)
{
	self->_priv->_attach = mu_msg_attach_view_new ();
	self->_priv->_attacharea = gtk_expander_new_with_mnemonic
		("_Attachments");
	
	gtk_container_add (GTK_CONTAINER(self->_priv->_attacharea),
			   self->_priv->_attach);	
	g_signal_connect (self->_priv->_attach, "attach-activated",
			  G_CALLBACK(on_attach_activated),
			  self);
	
	return self->_priv->_attacharea;
}

static void
mu_msg_normal_view_init (MuMsgNormalView *self)
{
	self->_priv = MU_MSG_NORMAL_VIEW_GET_PRIVATE(self);
	
	gtk_box_pack_start (GTK_BOX(self),get_header_widget (self),
			    TRUE, FALSE, 0);
	gtk_box_pack_start (GTK_BOX(self), get_attach_widget (self),
			    FALSE, FALSE, 1);
	gtk_box_pack_start (GTK_BOX(self), get_body_widget (self),
			    TRUE, TRUE, 0);
}

static void
mu_msg_normal_view_finalize (GObject *obj)
{
/* 	free/unref instance resources here */
	G_OBJECT_CLASS(parent_class)->finalize (obj);
}

GtkWidget*
mu_msg_normal_view_new (void)
{
	return GTK_WIDGET(g_object_new(MU_TYPE_MSG_NORMAL_VIEW, NULL));
}



static void
update_attachment_area (MuMsgNormalView *self, MuMsg *msg)
{
	GtkExpander *exp;
	gint attach_num;
	gchar *label;
	
	attach_num = 0;
	if (msg)
		attach_num = mu_msg_attach_view_set_message
			(MU_MSG_ATTACH_VIEW(self->_priv->_attach), msg);

	if (attach_num <= 0) {
		gtk_widget_hide_all (self->_priv->_attacharea);
		return;
	}
	
	exp = GTK_EXPANDER(self->_priv->_attacharea);
	
	gtk_expander_set_use_markup (exp,TRUE);
	label = g_strdup_printf ("<b>_Attachments (%d)</b>", attach_num);
	gtk_expander_set_label (exp, label);
	g_free (label);
		
	gtk_expander_set_expanded (exp, FALSE);
	gtk_widget_show_all (self->_priv->_attacharea);
}


void
mu_msg_normal_view_set_message (MuMsgNormalView *self, MuMsg *msg)
{	
	g_return_if_fail (MU_IS_MSG_NORMAL_VIEW(self));
		
	mu_msg_header_view_set_message
		(MU_MSG_HEADER_VIEW(self->_priv->_headers), msg);
	mu_msg_body_view_set_message
		(MU_MSG_BODY_VIEW(self->_priv->_body), msg);

	update_attachment_area (self, msg);
}
