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

#include "mug-msg-list-view.h"
#include "mu-query.h"
#include "mu-msg-str.h"
/* include other impl specific header files */

/* 'private'/'protected' functions */
static void mug_msg_list_view_class_init (MugMsgListViewClass *klass);
static void mug_msg_list_view_init       (MugMsgListView *obj);
static void mug_msg_list_view_finalize   (GObject *obj);

/* list my signals  */
enum {
	MUG_MSG_SELECTED,
	LAST_SIGNAL
};

enum {
	MUG_COL_DATE,
	MUG_COL_FROM,
	MUG_COL_TO,
	MUG_COL_SUBJECT,
	MUG_COL_PATH,
	
	MUG_N_COLS
};

typedef struct _MugMsgListViewPrivate MugMsgListViewPrivate;
struct _MugMsgListViewPrivate {
	GtkListStore *_store;
	char *_xpath;
	char *_query;
};
#define MUG_MSG_LIST_VIEW_GET_PRIVATE(o)      (G_TYPE_INSTANCE_GET_PRIVATE((o), \
                                          MUG_TYPE_MSG_LIST_VIEW, \
                                          MugMsgListViewPrivate))
/* globals */
static GtkTreeViewClass *parent_class = NULL;

/* uncomment the following if you have defined any signals */
static guint signals[LAST_SIGNAL] = {0};

GType
mug_msg_list_view_get_type (void)
{
	static GType my_type = 0;
	if (!my_type) {
		static const GTypeInfo my_info = {
			sizeof(MugMsgListViewClass),
			NULL,		/* base init */
			NULL,		/* base finalize */
			(GClassInitFunc) mug_msg_list_view_class_init,
			NULL,		/* class finalize */
			NULL,		/* class data */
			sizeof(MugMsgListView),
			0,		/* n_preallocs, ignored since 2.10 */
			(GInstanceInitFunc) mug_msg_list_view_init,
			NULL
		};
		my_type = g_type_register_static (GTK_TYPE_TREE_VIEW,
		                                  "MugMsgListView",
		                                  &my_info, 0);
	}
	return my_type;
}

static void
mug_msg_list_view_class_init (MugMsgListViewClass *klass)
{
	GObjectClass *gobject_class;
	gobject_class = (GObjectClass*) klass;

	parent_class            = g_type_class_peek_parent (klass);
	gobject_class->finalize = mug_msg_list_view_finalize;

	g_type_class_add_private (gobject_class, sizeof(MugMsgListViewPrivate));

	signals[MUG_MSG_SELECTED] = 
		g_signal_new ("msg-selected",
			      G_TYPE_FROM_CLASS(gobject_class),
			      G_SIGNAL_RUN_FIRST,
			      G_STRUCT_OFFSET(MugMsgListViewClass, msg_selected),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__STRING,
			      G_TYPE_NONE, 1, G_TYPE_STRING);
}


static void
on_cursor_changed (GtkTreeView *tview, MugMsgListView *lst)
{
	GtkTreeSelection *sel;
	GtkTreeIter iter;
	MugMsgListViewPrivate *priv;

	priv = MUG_MSG_LIST_VIEW_GET_PRIVATE(tview);
	
	sel = gtk_tree_view_get_selection (tview);
	if (!sel)
		return; /* hmmm */
	if (gtk_tree_selection_get_selected (sel, NULL, &iter)) {
		char *path;
		gtk_tree_model_get (GTK_TREE_MODEL(priv->_store), &iter,
			MUG_COL_PATH, &path, -1);
		g_signal_emit (G_OBJECT(lst),
			       signals[MUG_MSG_SELECTED], 0,
			       path);
		g_free (path);
	}
}

static GtkTreeViewColumn *
get_col (const char* label, int colidx)
{
	GtkTreeViewColumn *col;
	GtkCellRenderer *renderer;
	
	renderer = gtk_cell_renderer_text_new ();
	g_object_set (G_OBJECT(renderer),"ellipsize", PANGO_ELLIPSIZE_END, NULL);
	
	col = gtk_tree_view_column_new_with_attributes (label, renderer, "text",
							colidx, NULL);
 	g_object_set (G_OBJECT(col), "resizable", TRUE, NULL);
	
	//g_object_unref (renderer);

	return col;
}



static void
mug_msg_list_view_init (MugMsgListView *obj)
{
	GtkTreeViewColumn *col;
	MugMsgListViewPrivate *priv;
	
	priv = MUG_MSG_LIST_VIEW_GET_PRIVATE(obj);

	priv->_xpath = NULL;
	priv->_query = NULL;
	
	priv->_store = gtk_list_store_new (MUG_N_COLS,
					   G_TYPE_STRING,
					   G_TYPE_STRING,
					   G_TYPE_STRING,
					   G_TYPE_STRING,
					   G_TYPE_STRING);
	
	gtk_tree_view_set_model (GTK_TREE_VIEW (obj),
				 GTK_TREE_MODEL(priv->_store));

	gtk_tree_view_set_headers_clickable (GTK_TREE_VIEW(obj), TRUE);
	gtk_tree_view_set_grid_lines (GTK_TREE_VIEW(obj),
				      GTK_TREE_VIEW_GRID_LINES_VERTICAL);	
	gtk_tree_view_set_rules_hint (GTK_TREE_VIEW(obj), TRUE);
	

	col = get_col ("Date", MUG_COL_DATE);
	gtk_tree_view_append_column (GTK_TREE_VIEW (obj), col);

	col = get_col ("From", MUG_COL_FROM);
	gtk_tree_view_append_column (GTK_TREE_VIEW (obj), col);

	col = get_col ("To", MUG_COL_TO);
	gtk_tree_view_append_column (GTK_TREE_VIEW (obj), col);

	col = get_col ("Subject", MUG_COL_SUBJECT);
	gtk_tree_view_append_column (GTK_TREE_VIEW (obj), col);
	
	g_signal_connect (G_OBJECT(obj), "cursor-changed", G_CALLBACK(on_cursor_changed),
			  obj);
}

static void
mug_msg_list_view_finalize (GObject *obj)
{	
	MugMsgListViewPrivate *priv;
	priv = MUG_MSG_LIST_VIEW_GET_PRIVATE(obj);

	if (priv->_store) 
		g_object_unref (priv->_store);

	g_free (priv->_xpath);
	g_free (priv->_query);
	
	G_OBJECT_CLASS(parent_class)->finalize (obj);
}

void
mug_msg_list_view_move_first (MugMsgListView *self)
{
	GtkTreePath *path;
	
	g_return_val_if_fail (MUG_IS_MSG_LIST_VIEW(self), FALSE);

	path = gtk_tree_path_new_first ();
	gtk_tree_view_set_cursor (GTK_TREE_VIEW(self), path,
				  NULL, FALSE);
	
	gtk_tree_path_free (path);	
}


static gboolean
msg_list_view_move (MugMsgListView *self, gboolean next)
{
	GtkTreePath *path;
	
	gtk_tree_view_get_cursor (GTK_TREE_VIEW(self), &path, NULL);
	if (!path)
		return FALSE;

	if (next)
		gtk_tree_path_next (path);
	else
		gtk_tree_path_prev (path);
	
	gtk_tree_view_set_cursor (GTK_TREE_VIEW(self), path,
				  NULL, FALSE);

	gtk_tree_path_free (path);
	
	return TRUE;
}


gboolean
mug_msg_list_view_move_next (MugMsgListView *self)
{
	g_return_val_if_fail (MUG_IS_MSG_LIST_VIEW(self), FALSE);

	return msg_list_view_move (self, TRUE);
}


gboolean
mug_msg_list_view_move_prev (MugMsgListView *self)
{
	g_return_val_if_fail (MUG_IS_MSG_LIST_VIEW(self), FALSE);

	return msg_list_view_move (self, FALSE);
}

GtkWidget*
mug_msg_list_view_new (const char *xpath)
{
	GtkWidget *w;
	MugMsgListViewPrivate *priv;

	g_return_val_if_fail (xpath, NULL);
	
	w    = GTK_WIDGET(g_object_new(MUG_TYPE_MSG_LIST_VIEW, NULL));

	priv = MUG_MSG_LIST_VIEW_GET_PRIVATE(w);
	priv->_xpath = g_strdup (xpath);

	return w;
}


static int
update_model (GtkListStore *store, const char *xpath, const char *query)
{
	MuQuery *xapian;
	MuMsgIter *iter;
	int count;

	xapian = mu_query_new (xpath);
	if (!xapian) {
		g_printerr ("Failed to create a Xapian query\n");
		return -1;
	}

	iter = mu_query_run (xapian, query, NULL, TRUE, 0);
	if (!iter) {
		g_warning ("error: running query failed\n");
		mu_query_destroy (xapian);
		return -1;
	}

	for (count = 0; !mu_msg_iter_is_done (iter);
	     mu_msg_iter_next (iter), ++count) {

			GtkTreeIter treeiter;
			const gchar *date, *from, *subject, *path, *to;
			
			date	= mu_msg_str_date_s (mu_msg_iter_get_date (iter));
			from	= mu_msg_iter_get_from(iter);
			to      = mu_msg_iter_get_to (iter);
			subject = mu_msg_iter_get_subject (iter);
			path    = mu_msg_iter_get_path (iter);
			
			gtk_list_store_append (store, &treeiter);
			gtk_list_store_set (store, &treeiter,
					    MUG_COL_DATE, date,
					    MUG_COL_FROM, from,
					    MUG_COL_TO, to,
					    MUG_COL_SUBJECT, subject,
					    MUG_COL_PATH, path,
					    -1);
	}
	mu_query_destroy (xapian);
	
	return count;
}


int
mug_msg_list_view_query (MugMsgListView *self, const char *query)
{
	MugMsgListViewPrivate *priv;
	g_return_val_if_fail (MUG_IS_MSG_LIST_VIEW(self), FALSE);
	
	priv = MUG_MSG_LIST_VIEW_GET_PRIVATE(self);
	gtk_list_store_clear (priv->_store);

	g_free (priv->_query);
	priv->_query = query ? g_strdup(query) : NULL;
	
	if (!query)
		return TRUE;
	
	return update_model (priv->_store, priv->_xpath, query);
}


const gchar*
mug_msg_list_view_get_query (MugMsgListView *self)
{
	g_return_val_if_fail (MUG_IS_MSG_LIST_VIEW(self), NULL);
	
	return MUG_MSG_LIST_VIEW_GET_PRIVATE(self)->_query;
}
