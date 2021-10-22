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

#include "mu-msg-body-view.hh"
#include <mu-msg-part.hh>

using namespace Mu;

enum _ViewMode {
	VIEW_MODE_MSG,
	VIEW_MODE_SOURCE,
	VIEW_MODE_NOTE,

	VIEW_MODE_NONE
};
typedef enum _ViewMode ViewMode;

/* 'private'/'protected' functions */
static void mu_msg_body_view_class_init(MuMsgBodyViewClass* klass);
static void mu_msg_body_view_init(MuMsgBodyView* obj);
static void mu_msg_body_view_finalize(GObject* obj);

/* list my signals  */
enum {
	ACTION_REQUESTED,
	/* MY_SIGNAL_2, */
	LAST_SIGNAL
};

struct _MuMsgBodyViewPrivate {
	WebKitSettings* _settings;
	MuMsg*          _msg;
	ViewMode        _view_mode;
};

#define MU_MSG_BODY_VIEW_GET_PRIVATE(o)                                                            \
	(G_TYPE_INSTANCE_GET_PRIVATE((o), MU_TYPE_MSG_BODY_VIEW, MuMsgBodyViewPrivate))
/* globals */
static WebKitWebViewClass* parent_class = NULL;

static guint signals[LAST_SIGNAL] = {0};

G_DEFINE_TYPE(MuMsgBodyView, mu_msg_body_view, WEBKIT_TYPE_WEB_VIEW);

static void
set_message(MuMsgBodyView* self, MuMsg* msg)
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
mu_msg_body_view_class_init(MuMsgBodyViewClass* klass)
{
	GObjectClass* gobject_class;
	gobject_class = (GObjectClass*)klass;

	parent_class            = (WebKitWebViewClass*)g_type_class_peek_parent(klass);
	gobject_class->finalize = mu_msg_body_view_finalize;

	g_type_class_add_private(gobject_class, sizeof(MuMsgBodyViewPrivate));

	signals[ACTION_REQUESTED] =
	    g_signal_new("action-requested",
	                 G_TYPE_FROM_CLASS(gobject_class),
	                 G_SIGNAL_RUN_FIRST,
	                 G_STRUCT_OFFSET(MuMsgBodyViewClass, action_requested),
	                 NULL,
	                 NULL,
	                 g_cclosure_marshal_VOID__STRING,
	                 G_TYPE_NONE,
	                 1,
	                 G_TYPE_STRING);
}

static char*
save_file_for_cid(MuMsg* msg, const char* cid)
{
	gint     idx;
	gchar*   filepath;
	gboolean rv;
	GError*  err;

	g_return_val_if_fail(msg, NULL);
	g_return_val_if_fail(cid, NULL);

	idx = mu_msg_find_index_for_cid(msg, MU_MSG_OPTION_NONE, cid);
	if (idx < 0) {
		g_warning("%s: cannot find %s", __func__, cid);
		return NULL;
	}

	filepath = mu_msg_part_get_cache_path(msg, MU_MSG_OPTION_NONE, idx, NULL);
	if (!filepath) {
		g_warning("%s: cannot create filepath", filepath);
		return NULL;
	}

	err = NULL;
	rv  = mu_msg_part_save(msg, MU_MSG_OPTION_USE_EXISTING, filepath, idx, &err);
	if (!rv) {
		g_warning("%s: failed to save %s: %s",
		          __func__,
		          filepath,
		          err && err->message ? err->message : "error");
		g_clear_error(&err);
		g_free(filepath);
		filepath = NULL;
	}

	return filepath;
}

static void
on_resource_load_started(MuMsgBodyView*     self,
                         WebKitWebResource* resource,
                         WebKitURIRequest*  request,
                         gpointer           data)
{
	const char* uri;
	MuMsg*      msg;

	msg = self->_priv->_msg;
	uri = webkit_uri_request_get_uri(request);

	/* g_warning ("%s: %s", __func__, uri); */

	if (g_ascii_strncasecmp(uri, "cid:", 4) == 0) {
		gchar* filepath;
		filepath = save_file_for_cid(msg, uri);
		if (filepath) {
			gchar* fileuri;
			fileuri = g_strdup_printf("file://%s", filepath);
			webkit_uri_request_set_uri(request, fileuri);
			g_free(fileuri);
			g_free(filepath);
		}
	}
}

static void
on_menu_item_activate(GtkMenuItem* item, MuMsgBodyView* self)
{
	g_signal_emit(G_OBJECT(self),
	              signals[ACTION_REQUESTED],
	              0,
	              g_object_get_data(G_OBJECT(item), "action"));
}

static void
popup_menu(MuMsgBodyView* self, guint button, guint32 activate_time)
{
	GtkWidget* menu;
	int        i;
	struct {
		const char* title;
		const char* action;
		ViewMode    mode;
	} actions[] = {
	    {"View source...", "view-source", VIEW_MODE_MSG},
	    {"View message...", "view-message", VIEW_MODE_SOURCE},
	};

	menu = gtk_menu_new();

	for (i = 0; i != G_N_ELEMENTS(actions); ++i) {
		GtkWidget* item;

		if (self->_priv->_view_mode != actions[i].mode)
			continue;

		item = gtk_menu_item_new_with_label(actions[i].title);
		g_object_set_data(G_OBJECT(item), "action", (gpointer)actions[i].action);
		g_signal_connect(item, "activate", G_CALLBACK(on_menu_item_activate), self);
		gtk_menu_attach(GTK_MENU(menu), item, 0, 1, i, i + 1);
		gtk_widget_show(item);
	}
	gtk_menu_popup(GTK_MENU(menu), NULL, NULL, NULL, NULL, 0, 0);
}

static gboolean
on_button_press_event(MuMsgBodyView* self, GdkEventButton* event, gpointer data)
{
	/* ignore all but the first (typically, left) mouse button */
	switch (event->button) {
	case 1: return FALSE; /* propagate, let widget handle it */
	case 3:
		/* no popup menus for notes */
		if (self->_priv->_view_mode != VIEW_MODE_NOTE)
			popup_menu(self, event->button, event->time);
		break;
	default: return TRUE; /* ignore */
	}

	return (event->button > 1) ? TRUE : FALSE;
}

static void
mu_msg_body_view_init(MuMsgBodyView* obj)
{
	obj->_priv = MU_MSG_BODY_VIEW_GET_PRIVATE(obj);

	obj->_priv->_msg       = NULL;
	obj->_priv->_view_mode = VIEW_MODE_NONE;

	obj->_priv->_settings = webkit_settings_new();
	g_object_set(G_OBJECT(obj->_priv->_settings),
	             "enable-javascript",
	             FALSE,
	             "auto-load-images",
	             TRUE,
	             "enable-plugins",
	             FALSE,
	             NULL);

	webkit_web_view_set_settings(WEBKIT_WEB_VIEW(obj), obj->_priv->_settings);

	/* to support cid: */
	g_signal_connect(obj, "resource-load-started", G_CALLBACK(on_resource_load_started), NULL);
	g_signal_connect(obj, "button-press-event", G_CALLBACK(on_button_press_event), NULL);
}

static void
mu_msg_body_view_finalize(GObject* obj)
{
	MuMsgBodyViewPrivate* priv;

	priv = MU_MSG_BODY_VIEW_GET_PRIVATE(obj);
	if (priv && priv->_settings)
		g_object_unref(priv->_settings);

	set_message(MU_MSG_BODY_VIEW(obj), NULL);

	G_OBJECT_CLASS(parent_class)->finalize(obj);
}

GtkWidget*
mu_msg_body_view_new(void)
{
	return GTK_WIDGET(g_object_new(MU_TYPE_MSG_BODY_VIEW, NULL));
}

static void
set_html(MuMsgBodyView* self, const char* html)
{
	g_return_if_fail(MU_IS_MSG_BODY_VIEW(self));

	webkit_web_view_load_html(WEBKIT_WEB_VIEW(self), html ? html : "", NULL);
}

static void
set_text(MuMsgBodyView* self, const char* txt)
{
	g_return_if_fail(MU_IS_MSG_BODY_VIEW(self));

	webkit_web_view_load_plain_text(WEBKIT_WEB_VIEW(self), txt ? txt : "");
}

void
mu_msg_body_view_set_message(MuMsgBodyView* self, MuMsg* msg)
{
	const char* data;

	g_return_if_fail(self);

	set_message(self, msg);

	data = msg ? mu_msg_get_body_html(msg, MU_MSG_OPTION_NONE) : "";
	if (data)
		set_html(self, data);
	else
		set_text(self, mu_msg_get_body_text(msg, MU_MSG_OPTION_NONE));

	self->_priv->_view_mode = VIEW_MODE_MSG;
}

void
mu_msg_body_view_set_message_source(MuMsgBodyView* self, MuMsg* msg)
{
	const gchar* path;
	gchar*       data;

	g_return_if_fail(MU_IS_MSG_BODY_VIEW(self));
	g_return_if_fail(msg);

	set_message(self, NULL);

	path = msg ? mu_msg_get_path(msg) : NULL;

	if (path && g_file_get_contents(path, &data, NULL, NULL)) {
		set_text(self, data);
		g_free(data);
	} else
		set_text(self, "");

	self->_priv->_view_mode = VIEW_MODE_SOURCE;
}

void
mu_msg_body_view_set_note(MuMsgBodyView* self, const gchar* html)
{
	g_return_if_fail(self);
	g_return_if_fail(html);

	set_message(self, NULL);

	set_html(self, html);

	self->_priv->_view_mode = VIEW_MODE_NOTE;
}
