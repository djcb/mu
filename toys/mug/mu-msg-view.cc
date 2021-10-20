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

#include <config.h>

#include "mu-msg-view.hh"
#include "mu-msg-body-view.hh"
#include "mu-msg-header-view.hh"
#include "mu-msg-attach-view.hh"
#include <utils/mu-util.h>

#include <mu-msg.hh>
#include <mu-msg-part.hh>

using namespace Mu;

/* 'private'/'protected' functions */
static void mu_msg_view_class_init(MuMsgViewClass* klass);
static void mu_msg_view_init(MuMsgView* obj);
static void mu_msg_view_finalize(GObject* obj);

/* list my signals  */
enum {
	/* MY_SIGNAL_1, */
	/* MY_SIGNAL_2, */
	LAST_SIGNAL
};

struct _MuMsgViewPrivate {
	GtkWidget *_headers, *_attach, *_attachexpander, *_body;
	Mu::MuMsg* _msg;
};
#define MU_MSG_VIEW_GET_PRIVATE(o)                                                                 \
	(G_TYPE_INSTANCE_GET_PRIVATE((o), MU_TYPE_MSG_VIEW, MuMsgViewPrivate))
/* globals */
static GtkBoxClass* parent_class = NULL;

/* uncomment the following if you have defined any signals */
/* static guint signals[LAST_SIGNAL] = {0}; */

G_DEFINE_TYPE(MuMsgView, mu_msg_view, GTK_TYPE_BOX);

static void
set_message(MuMsgView* self, Mu::MuMsg* msg)
{
	if (self->_priv->_msg == msg)
		return; /* nothing to todo */

	if (self->_priv->_msg) {
		mu_msg_unref(self->_priv->_msg);
		self->_priv->_msg = NULL;
	}

	if (msg)
		self->_priv->_msg = mu_msg_ref(msg);
}

static void
mu_msg_view_class_init(MuMsgViewClass* klass)
{
	GObjectClass* gobject_class;
	gobject_class = (GObjectClass*)klass;

	parent_class            = (GtkBoxClass*)g_type_class_peek_parent(klass);
	gobject_class->finalize = mu_msg_view_finalize;

	g_type_class_add_private(gobject_class, sizeof(MuMsgViewPrivate));

	/* signal definitions go here, e.g.: */
	/* 	signals[MY_SIGNAL_1] = */
	/* 		g_signal_new ("my_signal_1",....); */
	/* 	signals[MY_SIGNAL_2] = */
	/* 		g_signal_new ("my_signal_2",....); */
	/* 	etc. */
}

static void
on_body_action_requested(GtkWidget* w, const char* action, MuMsgView* self)
{
	if (g_strcmp0(action, "view-source") == 0) {
		if (self->_priv->_msg)
			mu_msg_view_set_message_source(self, self->_priv->_msg);

	} else if (g_strcmp0(action, "view-message") == 0) {
		if (self->_priv->_msg)
			mu_msg_view_set_message(self, self->_priv->_msg);

	} else if (g_strcmp0(action, "reindex") == 0)
		g_warning("reindex");
	else
		g_warning("unknown action '%s'", action);
}

static void
on_attach_activated(GtkWidget* w, guint partnum, Mu::MuMsg* msg)
{
	gchar*  filepath;
	GError* err;

	err      = NULL;
	filepath = mu_msg_part_get_cache_path(msg, MU_MSG_OPTION_NONE, partnum, &err);
	if (!filepath) {
		g_warning("failed to get cache path: %s",
		          err && err->message ? err->message : "error");
		g_clear_error(&err);
		return;
	}

	if (!mu_msg_part_save(msg, MU_MSG_OPTION_USE_EXISTING, filepath, partnum, &err)) {
		g_warning("failed to save %s: %s",
		          filepath,
		          err && err->message ? err->message : "error");
		g_clear_error(&err);
		return;

	} else
		mu_util_play(filepath, TRUE, FALSE, NULL);

	g_free(filepath);
}

static void
mu_msg_view_init(MuMsgView* self)
{
	gtk_orientable_set_orientation(GTK_ORIENTABLE(self), GTK_ORIENTATION_VERTICAL);

	self->_priv = MU_MSG_VIEW_GET_PRIVATE(self);

	self->_priv->_msg            = NULL;
	self->_priv->_headers        = mu_msg_header_view_new();
	self->_priv->_attach         = mu_msg_attach_view_new();
	self->_priv->_attachexpander = gtk_expander_new_with_mnemonic("_Attachments");

	gtk_container_add(GTK_CONTAINER(self->_priv->_attachexpander), self->_priv->_attach);
	g_signal_connect(self->_priv->_attach,
	                 "attach-activated",
	                 G_CALLBACK(on_attach_activated),
	                 self);

	self->_priv->_body = mu_msg_body_view_new();
	g_signal_connect(self->_priv->_body,
	                 "action-requested",
	                 G_CALLBACK(on_body_action_requested),
	                 self);

	gtk_box_pack_start(GTK_BOX(self), self->_priv->_headers, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(self), self->_priv->_attachexpander, FALSE, FALSE, 2);
	gtk_box_pack_start(GTK_BOX(self), self->_priv->_body, TRUE, TRUE, 2);
}

static void
mu_msg_view_finalize(GObject* obj)
{
	set_message(MU_MSG_VIEW(obj), NULL);

	G_OBJECT_CLASS(parent_class)->finalize(obj);
}

GtkWidget*
mu_msg_view_new(void)
{
	return GTK_WIDGET(g_object_new(MU_TYPE_MSG_VIEW, NULL));
}

void
mu_msg_view_set_message(MuMsgView* self, Mu::MuMsg* msg)
{
	gint attachnum;

	g_return_if_fail(MU_IS_MSG_VIEW(self));

	set_message(self, msg);

	mu_msg_header_view_set_message(MU_MSG_HEADER_VIEW(self->_priv->_headers), msg);
	attachnum = mu_msg_attach_view_set_message(MU_MSG_ATTACH_VIEW(self->_priv->_attach), msg);

	mu_msg_body_view_set_message(MU_MSG_BODY_VIEW(self->_priv->_body), msg);

	gtk_widget_set_visible(self->_priv->_headers, TRUE);
	gtk_widget_set_visible(self->_priv->_attachexpander, attachnum > 0);
	gtk_widget_set_visible(self->_priv->_body, TRUE);
}

void
mu_msg_view_set_message_source(MuMsgView* self, Mu::MuMsg* msg)
{
	g_return_if_fail(MU_IS_MSG_VIEW(self));

	set_message(self, msg);

	mu_msg_body_view_set_message_source(MU_MSG_BODY_VIEW(self->_priv->_body), msg);

	gtk_widget_set_visible(self->_priv->_headers, FALSE);
	gtk_widget_set_visible(self->_priv->_attachexpander, FALSE);
	gtk_widget_set_visible(self->_priv->_body, TRUE);
}

void
mu_msg_view_set_note(MuMsgView* self, const gchar* html)
{
	g_return_if_fail(MU_IS_MSG_VIEW(self));

	gtk_widget_set_visible(self->_priv->_headers, FALSE);
	gtk_widget_set_visible(self->_priv->_attachexpander, FALSE);
	gtk_widget_set_visible(self->_priv->_body, TRUE);

	mu_msg_body_view_set_note(MU_MSG_BODY_VIEW(self->_priv->_body), html);
}
