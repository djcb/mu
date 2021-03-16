/*
** Copyright (C) 2011-2021 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-msg-attach-view.hh"
#include "mu-widget-util.h"
#include <mu-msg.hh>
#include <mu-msg-part.hh>

using namespace Mu;

enum {
	ICON_COL,
	NAME_COL,
	PARTNUM_COL,

	NUM_COL
};


/* 'private'/'protected' functions */
static void mu_msg_attach_view_class_init (MuMsgAttachViewClass *klass);
static void mu_msg_attach_view_init       (MuMsgAttachView *obj);
static void mu_msg_attach_view_finalize   (GObject *obj);

/* list my signals  */
enum {
	ATTACH_ACTIVATED,
	/* MY_SIGNAL_2, */
	LAST_SIGNAL
};

struct _MuMsgAttachViewPrivate {
	MuMsg *_msg;
};
#define MU_MSG_ATTACH_VIEW_GET_PRIVATE(o)      (G_TYPE_INSTANCE_GET_PRIVATE((o), \
						MU_TYPE_MSG_ATTACH_VIEW, \
						MuMsgAttachViewPrivate))
/* globals */
static GtkIconViewClass *parent_class = NULL;

static guint signals[LAST_SIGNAL] = {0};

G_DEFINE_TYPE (MuMsgAttachView, mu_msg_attach_view, GTK_TYPE_ICON_VIEW);


static void
set_message (MuMsgAttachView *self, MuMsg *msg)
{
	if (self->_priv->_msg == msg)
		return; /* nothing to todo */

	if (self->_priv->_msg)  {
		mu_msg_unref (self->_priv->_msg);
		self->_priv->_msg = NULL;
	}

	if (msg)
		self->_priv->_msg = mu_msg_ref (msg);
}


static void
mu_msg_attach_view_class_init (MuMsgAttachViewClass *klass)
{
	GObjectClass *gobject_class;
	gobject_class = (GObjectClass*) klass;

	parent_class            = (GtkIconViewClass*)g_type_class_peek_parent (klass);
	gobject_class->finalize = mu_msg_attach_view_finalize;

	g_type_class_add_private (gobject_class, sizeof(MuMsgAttachViewPrivate));

	signals[ATTACH_ACTIVATED] =
		g_signal_new ("attach-activated",
			      G_TYPE_FROM_CLASS (gobject_class),
			      G_SIGNAL_RUN_FIRST,
			      G_STRUCT_OFFSET (MuMsgAttachViewClass,
					       attach_activated),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__UINT_POINTER,
			      G_TYPE_NONE, 2, G_TYPE_UINT, G_TYPE_POINTER);
}

static void
item_activated (MuMsgAttachView *self, GtkTreePath *tpath)
{
	GtkTreeModel *model;
	GtkTreeIter iter;
	guint partnum;

	model = gtk_icon_view_get_model (GTK_ICON_VIEW(self));
	if (!gtk_tree_model_get_iter (model, &iter, tpath)) {
		g_warning ("could not find path");
	}

	gtk_tree_model_get (model, &iter,
			    PARTNUM_COL, &partnum,
			    -1);

	g_signal_emit (G_OBJECT (self),
		       signals[ATTACH_ACTIVATED], 0,
		       partnum, self->_priv->_msg);
}

static void
accumulate_parts (MuMsgAttachView *self, GtkTreePath *path, GSList **lst)
{
	GtkTreeIter iter;
	GtkTreeModel *model;

	/* don't unref */
	model = gtk_icon_view_get_model (GTK_ICON_VIEW(self));

	if (gtk_tree_model_get_iter (model, &iter, path)) {
		gchar *filepath;
		gint idx;
		gtk_tree_model_get (model, &iter, PARTNUM_COL, &idx, -1);
		filepath = mu_msg_part_get_cache_path (self->_priv->_msg,
						       MU_MSG_OPTION_NONE,
						       idx, NULL);
		if (filepath) {
			if (mu_msg_part_save (self->_priv->_msg,
					      MU_MSG_OPTION_USE_EXISTING,
					      filepath,
					      idx, NULL)) {
				GFile *file;
				file = g_file_new_for_path (filepath);
				*lst = g_slist_prepend (*lst, g_file_get_uri(file));
				g_object_unref (file);
			} else
				g_warning ("error saving msg part");
			g_free (filepath);
		}
	}
}



static void
on_drag_data_get (MuMsgAttachView *self, GdkDragContext *drag_context,
		  GtkSelectionData *data, guint info, guint time, gpointer user_data)
{
	GSList *lst, *cur;
	char **uris;
	int i;

	lst = NULL;
	gtk_icon_view_selected_foreach (GTK_ICON_VIEW(self),
					(GtkIconViewForeachFunc)accumulate_parts,
					&lst);

	uris = g_new(char*, g_slist_length(lst) + 1);
	for (cur = lst, i = 0; cur; cur = g_slist_next(cur))
		uris[i++] = (gchar*)cur->data;

	uris[i] = NULL;
	gtk_selection_data_set_uris (data, uris);

	g_free (uris);
	g_slist_free_full (lst, g_free);
}

static void
mu_msg_attach_view_init (MuMsgAttachView *obj)
{
	GtkListStore *store;

	obj->_priv = MU_MSG_ATTACH_VIEW_GET_PRIVATE(obj);
	obj->_priv->_msg = NULL;

	store = gtk_list_store_new (NUM_COL,GDK_TYPE_PIXBUF,
				    G_TYPE_STRING, G_TYPE_UINT);
	gtk_icon_view_set_model (GTK_ICON_VIEW(obj), GTK_TREE_MODEL(store));
	g_object_unref (store);

	gtk_icon_view_set_pixbuf_column (GTK_ICON_VIEW(obj), ICON_COL);
	gtk_icon_view_set_text_column (GTK_ICON_VIEW(obj), NAME_COL);

	gtk_icon_view_set_margin (GTK_ICON_VIEW(obj), 0);
	gtk_icon_view_set_spacing (GTK_ICON_VIEW(obj), 0);
	gtk_icon_view_set_item_padding (GTK_ICON_VIEW(obj), 0);

	/* note: only since GTK+ 2.22 */
	/* gtk_icon_view_set_item_orientation (GTK_ICON_VIEW(obj), */
	/*				    GTK_ORIENTATION_HORIZONTAL); */

	g_signal_connect (G_OBJECT(obj), "item-activated",
			  G_CALLBACK(item_activated), NULL);

	gtk_icon_view_set_selection_mode (GTK_ICON_VIEW(obj),
					  GTK_SELECTION_MULTIPLE);
	/* drag & drop */
	gtk_icon_view_enable_model_drag_source (GTK_ICON_VIEW(obj),
						(GdkModifierType)0, NULL, 0,
						GDK_ACTION_COPY);
	gtk_drag_source_add_uri_targets(GTK_WIDGET(obj));
	g_signal_connect (obj, "drag-data-get", G_CALLBACK(on_drag_data_get), NULL);
}


static void
mu_msg_attach_view_finalize (GObject *obj)
{
	set_message (MU_MSG_ATTACH_VIEW(obj), NULL);

	G_OBJECT_CLASS(parent_class)->finalize (obj);
}

GtkWidget*
mu_msg_attach_view_new (void)
{
	return GTK_WIDGET(g_object_new(MU_TYPE_MSG_ATTACH_VIEW, NULL));
}

struct _CBData {
	GtkListStore *store;
	guint count;
};
typedef struct _CBData CBData;



static void
each_part (MuMsg *msg, MuMsgPart *part, CBData *cbdata)
{
	GtkTreeIter treeiter;
	GdkPixbuf *pixbuf;
	char ctype[128];

	if (!mu_msg_part_maybe_attachment(part))
		return;

	if (!part->type || !part->subtype)
		snprintf (ctype, sizeof(ctype), "%s",
			  "application/octet-stream");
	else
		snprintf (ctype, sizeof(ctype), "%s/%s",
			  part->type, part->subtype);

	pixbuf = mu_widget_util_get_icon_pixbuf_for_content_type (ctype, 16);
	if (!pixbuf) {
		g_debug ("%s: could not get icon pixbuf for '%s'",
			 __func__, ctype);
		pixbuf = mu_widget_util_get_icon_pixbuf_for_content_type
			("application/octet-stream", 16);
	}

	gtk_list_store_append (cbdata->store, &treeiter);
	gtk_list_store_set (cbdata->store, &treeiter,
			    NAME_COL, mu_msg_part_get_filename (part, TRUE),
			    ICON_COL, pixbuf,
			    PARTNUM_COL, part->index,
			    -1);
	if (pixbuf)
		g_object_unref (pixbuf);

	++cbdata->count;
}

gint
mu_msg_attach_view_set_message (MuMsgAttachView *self, MuMsg *msg)
{
	GtkListStore *store;
	CBData cbdata;

	g_return_val_if_fail (MU_IS_MSG_ATTACH_VIEW(self), -1);

	store = GTK_LIST_STORE (gtk_icon_view_get_model (GTK_ICON_VIEW(self)));
	gtk_list_store_clear (store);

	set_message (self, msg);

	if (!msg)
		return 0;


	cbdata.store = store;
	cbdata.count = 0;
	mu_msg_part_foreach (msg, MU_MSG_OPTION_NONE,
			     (MuMsgPartForeachFunc)each_part, &cbdata);

	return cbdata.count;
}
