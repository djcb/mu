/*
** Copyright (C) 2008-2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include "config.h"

#include <unistd.h>
#include "mu-msg-view.hh"
#include "mug-msg-view.h"
#include "mu-msg.hh"
#include "utils/mu-str.h"

using namespace Mu;

/* 'private'/'protected' functions */
static void mug_msg_view_class_init(MugMsgViewClass* klass);
static void mug_msg_view_init(MugMsgView* obj);
static void mug_msg_view_finalize(GObject* obj);

/* list my signals  */
enum {
	/* MY_SIGNAL_1, */
	/* MY_SIGNAL_2, */
	LAST_SIGNAL
};

typedef struct _MugMsgViewPrivate MugMsgViewPrivate;
struct _MugMsgViewPrivate {
	GtkWidget* _view;
};
#define MUG_MSG_VIEW_GET_PRIVATE(o)                                                                \
	(G_TYPE_INSTANCE_GET_PRIVATE((o), MUG_TYPE_MSG_VIEW, MugMsgViewPrivate))
/* globals */

static GtkBoxClass* parent_class = NULL;
G_DEFINE_TYPE(MugMsgView, mug_msg_view, GTK_TYPE_BOX);

/* uncomment the following if you have defined any signals */
/* static guint signals[LAST_SIGNAL] = {0}; */

static void
mug_msg_view_class_init(MugMsgViewClass* klass)
{
	GObjectClass* gobject_class;
	gobject_class = (GObjectClass*)klass;

	parent_class            = (GtkBoxClass*)g_type_class_peek_parent(klass);
	gobject_class->finalize = mug_msg_view_finalize;

	g_type_class_add_private(gobject_class, sizeof(MugMsgViewPrivate));

	/* signal definitions go here, e.g.: */
	/* 	signals[MY_SIGNAL_1] = */
	/* 		g_signal_new ("my_signal_1",....); */
	/* 	signals[MY_SIGNAL_2] = */
	/* 		g_signal_new ("my_signal_2",....); */
	/* 	etc. */
}

static void
mug_msg_view_init(MugMsgView* obj)
{
	MugMsgViewPrivate* priv;
	GtkWidget*         scrolled;

	priv = MUG_MSG_VIEW_GET_PRIVATE(obj);

	priv->_view = mu_msg_view_new();

	scrolled = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled),
	                               GTK_POLICY_AUTOMATIC,
	                               GTK_POLICY_AUTOMATIC);
	gtk_container_add(GTK_CONTAINER(scrolled), priv->_view);

	gtk_box_pack_start(GTK_BOX(obj), scrolled, TRUE, TRUE, 0);
}

static void
mug_msg_view_finalize(GObject* obj)
{
	/* 	free/unref instance resources here */
	G_OBJECT_CLASS(parent_class)->finalize(obj);
}

GtkWidget*
mug_msg_view_new(void)
{
	return GTK_WIDGET(g_object_new(MUG_TYPE_MSG_VIEW, NULL));
}

gboolean
mug_msg_view_set_msg(MugMsgView* self, const char* msgpath)
{
	MugMsgViewPrivate* priv;
	g_return_val_if_fail(MUG_IS_MSG_VIEW(self), FALSE);

	priv = MUG_MSG_VIEW_GET_PRIVATE(self);

	if (!msgpath)
		mu_msg_view_set_message(MU_MSG_VIEW(priv->_view), NULL);
	else {
		Mu::MuMsg* msg;

		if (access(msgpath, R_OK) == 0) {
			msg = mu_msg_new_from_file(msgpath, NULL, NULL);
			mu_msg_view_set_message(MU_MSG_VIEW(priv->_view), msg);
			if (msg)
				mu_msg_unref(msg);
		} else {
			gchar* note;
			note = g_strdup_printf("<h1>Note</h1><hr>"
			                       "<p>Message <tt>%s</tt> does not seem to be present "
			                       "on the file system."
			                       "<p>Maybe you need to run <tt>mu index</tt>?",
			                       msgpath);
			mu_msg_view_set_note(MU_MSG_VIEW(priv->_view), note);
			g_free(note);
		}
	}

	return TRUE;
}

void
mug_msg_view_set_note(MugMsgView* self, const char* html)
{
	MugMsgViewPrivate* priv;
	g_return_if_fail(MUG_IS_MSG_VIEW(self));

	priv = MUG_MSG_VIEW_GET_PRIVATE(self);

	mu_msg_view_set_note(MU_MSG_VIEW(priv->_view), html);
}
