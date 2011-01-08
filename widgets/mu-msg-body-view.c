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
#include <webkit/webkitwebview.h>
#include "mu-msg-body-view.h"

/* include other impl specific header files */

/* 'private'/'protected' functions */
static void mu_msg_body_view_class_init (MuMsgBodyViewClass *klass);
static void mu_msg_body_view_init       (MuMsgBodyView *obj);
static void mu_msg_body_view_finalize   (GObject *obj);

/* list my signals  */
enum {
	/* MY_SIGNAL_1, */
	/* MY_SIGNAL_2, */
	LAST_SIGNAL
};

struct _MuMsgBodyViewPrivate {
	WebKitWebSettings *_settings;
};
#define MU_MSG_BODY_VIEW_GET_PRIVATE(o)      (G_TYPE_INSTANCE_GET_PRIVATE((o), \
                                              MU_TYPE_MSG_BODY_VIEW, \
                                              MuMsgBodyViewPrivate))
/* globals */
static WebKitWebViewClass *parent_class = NULL;

/* uncomment the following if you have defined any signals */
/* static guint signals[LAST_SIGNAL] = {0}; */

G_DEFINE_TYPE (MuMsgBodyView, mu_msg_body_view, WEBKIT_TYPE_WEB_VIEW);

static void
mu_msg_body_view_class_init (MuMsgBodyViewClass *klass)
{
	GObjectClass *gobject_class;
	gobject_class = (GObjectClass*) klass;

	parent_class            = g_type_class_peek_parent (klass);
	gobject_class->finalize = mu_msg_body_view_finalize;

	g_type_class_add_private (gobject_class, sizeof(MuMsgBodyViewPrivate));

	/* signal definitions go here, e.g.: */
/* 	signals[MY_SIGNAL_1] = */
/* 		g_signal_new ("my_signal_1",....); */
/* 	signals[MY_SIGNAL_2] = */
/* 		g_signal_new ("my_signal_2",....); */
/* 	etc. */
}

static void
mu_msg_body_view_init (MuMsgBodyView *obj)
{
	obj->_priv = MU_MSG_BODY_VIEW_GET_PRIVATE(obj);

	obj->_priv->_settings = webkit_web_settings_new ();
	g_object_set (G_OBJECT(obj->_priv->_settings),
		      "enable-scripts", FALSE,
		      "auto-load-images", TRUE,
		      "enable-plugins", FALSE,
		      NULL);

	webkit_web_view_set_settings (WEBKIT_WEB_VIEW(obj),
				      obj->_priv->_settings);
	webkit_web_view_set_editable (WEBKIT_WEB_VIEW(obj), FALSE);
	/* other settings */
}

static void
mu_msg_body_view_finalize (GObject *obj)
{
	MuMsgBodyViewPrivate *priv;

	priv = MU_MSG_BODY_VIEW_GET_PRIVATE(obj);
	if (priv && priv->_settings)
		g_object_unref (priv->_settings);
	
	G_OBJECT_CLASS(parent_class)->finalize (obj);
}

GtkWidget*
mu_msg_body_view_new (void)
{
	return GTK_WIDGET(g_object_new(MU_TYPE_MSG_BODY_VIEW, NULL));
}


void
mu_msg_body_view_set_html (MuMsgBodyView *self, const char* html)
{
	g_return_if_fail (MU_IS_MSG_BODY_VIEW(self));

	webkit_web_view_load_string (WEBKIT_WEB_VIEW(self),
				     html ? html : "",
				     "text/html",
				     "utf-8",
				     "");
}

void
mu_msg_body_view_set_text (MuMsgBodyView *self, const char* txt)
{
	g_return_if_fail (MU_IS_MSG_BODY_VIEW(self));

	webkit_web_view_load_string (WEBKIT_WEB_VIEW(self),
				     txt ? txt : "",
				     "text/plain",
				     "utf-8",
				     "");
}
