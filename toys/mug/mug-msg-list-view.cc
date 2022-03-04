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

#include "mug-msg-list-view.h"
#include "mu-message-flags.hh"
#include "mu-query.hh"
#include "utils/mu-str.h"
#include "utils/mu-date.h"

using namespace Mu;

/* 'private'/'protected' functions */
static void mug_msg_list_view_finalize(GObject* obj);

/* list my signals  */
enum { MUG_MSG_SELECTED, MUG_ERROR_OCCURED, LAST_SIGNAL };

enum {
	MUG_COL_DATESTR,
	MUG_COL_MAILDIR,
	MUG_COL_FLAGSSTR,
	MUG_COL_FROM,
	MUG_COL_TO,
	MUG_COL_SUBJECT,
	MUG_COL_PATH,
	MUG_COL_PRIO,
	MUG_COL_FLAGS,
	MUG_COL_TIME,
	MUG_N_COLS
};

typedef struct _MugMsgListViewPrivate MugMsgListViewPrivate;
struct _MugMsgListViewPrivate {
	GtkTreeStore* _store;
	char*         _xpath;
	char*         _query;
};
#define MUG_MSG_LIST_VIEW_GET_PRIVATE(o)                                                           \
	(G_TYPE_INSTANCE_GET_PRIVATE((o), MUG_TYPE_MSG_LIST_VIEW, MugMsgListViewPrivate))
/* globals */
static GtkTreeViewClass* parent_class = NULL;

/* uncomment the following if you have defined any signals */
static guint signals[LAST_SIGNAL] = {0};

G_DEFINE_TYPE(MugMsgListView, mug_msg_list_view, GTK_TYPE_TREE_VIEW);

static void
mug_msg_list_view_class_init(MugMsgListViewClass* klass)
{
	GObjectClass* gobject_class;
	gobject_class = (GObjectClass*)klass;

	parent_class            = (GtkTreeViewClass*)g_type_class_peek_parent(klass);
	gobject_class->finalize = mug_msg_list_view_finalize;

	g_type_class_add_private(gobject_class, sizeof(MugMsgListViewPrivate));

	signals[MUG_MSG_SELECTED] = g_signal_new("msg-selected",
						 G_TYPE_FROM_CLASS(gobject_class),
						 G_SIGNAL_RUN_FIRST,
						 G_STRUCT_OFFSET(MugMsgListViewClass, msg_selected),
						 NULL,
						 NULL,
						 g_cclosure_marshal_VOID__STRING,
						 G_TYPE_NONE,
						 1,
						 G_TYPE_STRING);
	signals[MUG_ERROR_OCCURED] =
	    g_signal_new("error-occured",
			 G_TYPE_FROM_CLASS(gobject_class),
			 G_SIGNAL_RUN_FIRST,
			 G_STRUCT_OFFSET(MugMsgListViewClass, error_occured),
			 NULL,
			 NULL,
			 g_cclosure_marshal_VOID__UINT,
			 G_TYPE_NONE,
			 1,
			 G_TYPE_UINT);
}

static void
on_cursor_changed(GtkTreeView* tview, MugMsgListView* lst)
{
	GtkTreeSelection*      sel;
	GtkTreeIter            iter;
	MugMsgListViewPrivate* priv;

	priv = MUG_MSG_LIST_VIEW_GET_PRIVATE(tview);

	sel = gtk_tree_view_get_selection(tview);
	if (!sel)
		return; /* hmmm */
	if (gtk_tree_selection_get_selected(sel, NULL, &iter)) {
		char* path;
		gtk_tree_model_get(GTK_TREE_MODEL(priv->_store), &iter, MUG_COL_PATH, &path, -1);
		g_signal_emit(G_OBJECT(lst), signals[MUG_MSG_SELECTED], 0, path);
		g_free(path);
	}
}

static void
treecell_func(GtkTreeViewColumn* tree_column,
	      GtkCellRenderer*   renderer,
	      GtkTreeModel*      tree_model,
	      GtkTreeIter*       iter,
	      gpointer           data)
{

	MessageFlags flags;
	MessagePriority prio;

	gtk_tree_model_get(tree_model, iter, MUG_COL_FLAGS, &flags, MUG_COL_PRIO, &prio, -1);

	g_object_set(G_OBJECT(renderer),
		     "weight",
		     any_of(flags & MessageFlags::New) ? 800 : 400,
		     "weight",
		     any_of(flags & MessageFlags::Seen) ? 400 : 800,
		     "foreground",
		     prio == MessagePriority::High ? "red" : NULL,
		     NULL);
}

/* sortcolidx == -1 means 'sortcolidx = colidx' */
static void
append_col(GtkTreeView* treeview, const char* label, int colidx, int sortcolidx, gint maxwidth)
{
	GtkTreeViewColumn* col;
	GtkCellRenderer*   renderer;

	renderer = gtk_cell_renderer_text_new();
	g_object_set(G_OBJECT(renderer), "ellipsize", PANGO_ELLIPSIZE_END, NULL);

	col = gtk_tree_view_column_new_with_attributes(label, renderer, "text", colidx, NULL);
	g_object_set(G_OBJECT(col), "resizable", TRUE, NULL);

	gtk_tree_view_column_set_sort_indicator(col, TRUE);

	if (sortcolidx == -1)
		sortcolidx = colidx;
	gtk_tree_view_column_set_sort_column_id(col, sortcolidx);
	gtk_tree_view_column_set_sizing(col, GTK_TREE_VIEW_COLUMN_FIXED);

	if (maxwidth) {
		gtk_tree_view_column_set_fixed_width(col, maxwidth);
		gtk_tree_view_column_set_expand(col, FALSE);
	} else
		gtk_tree_view_column_set_expand(col, TRUE);

	gtk_tree_view_column_set_cell_data_func(col,
						renderer,
						(GtkTreeCellDataFunc)treecell_func,
						NULL,
						NULL);

	gtk_tree_view_append_column(treeview, col);

	gtk_tree_view_columns_autosize(treeview);
	gtk_tree_view_set_fixed_height_mode(treeview, TRUE);
}

static void
mug_msg_list_view_init(MugMsgListView* obj)
{
	MugMsgListViewPrivate* priv;
	GtkTreeView*           tview;

	priv = MUG_MSG_LIST_VIEW_GET_PRIVATE(obj);

	priv->_xpath = priv->_query = NULL;
	priv->_store                = gtk_tree_store_new(MUG_N_COLS,
					  G_TYPE_STRING, /* date */
					  G_TYPE_STRING, /* folder */
					  G_TYPE_STRING, /* flagstr */
					  G_TYPE_STRING, /* from */
					  G_TYPE_STRING, /* to */
					  G_TYPE_STRING, /* subject */
					  G_TYPE_STRING, /* path */
					  G_TYPE_UINT,   /* prio */
					  G_TYPE_UINT,   /* flags */
					  G_TYPE_INT);   /* timeval */

	tview = GTK_TREE_VIEW(obj);
	gtk_tree_view_set_model(tview, GTK_TREE_MODEL(priv->_store));
	gtk_tree_view_set_headers_clickable(GTK_TREE_VIEW(obj), TRUE);
	gtk_tree_view_set_grid_lines(GTK_TREE_VIEW(obj), GTK_TREE_VIEW_GRID_LINES_VERTICAL);
	gtk_tree_view_set_rules_hint(GTK_TREE_VIEW(obj), TRUE);

	append_col(tview, "Date", MUG_COL_DATESTR, MUG_COL_TIME, 80);
	append_col(tview, "Folder", MUG_COL_MAILDIR, -1, 60);
	append_col(tview, "F", MUG_COL_FLAGSSTR, -1, 25);
	append_col(tview, "From", MUG_COL_FROM, -1, 0);
	append_col(tview, "To", MUG_COL_TO, -1, 0);
	append_col(tview, "Subject", MUG_COL_SUBJECT, -1, 0);

	g_signal_connect(G_OBJECT(obj), "cursor-changed", G_CALLBACK(on_cursor_changed), obj);
}

static void
mug_msg_list_view_finalize(GObject* obj)
{
	MugMsgListViewPrivate* priv;
	priv = MUG_MSG_LIST_VIEW_GET_PRIVATE(obj);

	if (priv->_store)
		g_object_unref(priv->_store);

	g_free(priv->_xpath);
	g_free(priv->_query);

	G_OBJECT_CLASS(parent_class)->finalize(obj);
}

void
mug_msg_list_view_move_first(MugMsgListView* self)
{
	GtkTreePath* path;

	g_return_if_fail(MUG_IS_MSG_LIST_VIEW(self));

	path = gtk_tree_path_new_first();
	gtk_tree_view_set_cursor(GTK_TREE_VIEW(self), path, NULL, FALSE);

	gtk_tree_path_free(path);
}

static gboolean
msg_list_view_move(MugMsgListView* self, gboolean next)
{
	GtkTreePath* path;

	gtk_tree_view_get_cursor(GTK_TREE_VIEW(self), &path, NULL);
	if (!path)
		return FALSE;

	if (next)
		gtk_tree_path_next(path);
	else
		gtk_tree_path_prev(path);

	gtk_tree_view_set_cursor(GTK_TREE_VIEW(self), path, NULL, FALSE);
	gtk_tree_path_free(path);

	return TRUE;
}

gboolean
mug_msg_list_view_move_next(MugMsgListView* self)
{
	g_return_val_if_fail(MUG_IS_MSG_LIST_VIEW(self), FALSE);

	return msg_list_view_move(self, TRUE);
}

gboolean
mug_msg_list_view_move_prev(MugMsgListView* self)
{
	g_return_val_if_fail(MUG_IS_MSG_LIST_VIEW(self), FALSE);

	return msg_list_view_move(self, FALSE);
}

GtkWidget*
mug_msg_list_view_new(const char* xpath)
{
	GtkWidget*             w;
	MugMsgListViewPrivate* priv;

	g_return_val_if_fail(xpath, NULL);

	w = GTK_WIDGET(g_object_new(MUG_TYPE_MSG_LIST_VIEW, NULL));

	priv         = MUG_MSG_LIST_VIEW_GET_PRIVATE(w);
	priv->_xpath = g_strdup(xpath);

	return w;
}

static gchar*
empty_or_display_contact(const gchar* str)
{
	if (!str || *str == '\0')
		return g_strdup("-");
	else
		return mu_str_display_contact(str);
}

static Mu::Option<Mu::QueryResults>
run_query(const char* xpath, const char* expr, MugMsgListView* self)
{
	Mu::Store store{xpath};

	return store.run_query(expr,
				Field::Id::Date,
				Mu::QueryFlags::Descending | Mu::QueryFlags::SkipUnreadable |
				Mu::QueryFlags::SkipDuplicates | Mu::QueryFlags::IncludeRelated |
				Mu::QueryFlags::Threading);
}

static void
add_row(GtkTreeStore* store, MuMsg* msg, GtkTreeIter* treeiter)
{
	const gchar *datestr;
	gchar *      from, *to;
	time_t       timeval;
	std::string  flag_str;

	timeval = mu_msg_get_date(msg);
	datestr = timeval == 0 ? "-" : mu_date_display_s(timeval);
	from    = empty_or_display_contact(mu_msg_get_from(msg));
	to      = empty_or_display_contact(mu_msg_get_to(msg));

	flag_str =  message_flags_to_string(mu_msg_get_flags(msg));

	/* if (0) { */
	/*      GtkTreeIter myiter; */
	/*      if (!gtk_tree_model_get_iter_from_string (GTK_TREE_MODEL(store), */
	/*                                                &myiter, path)) */
	/*              g_warning ("%s: cannot get iter for %s",
	 *              __func__, path); */
	/* } */

	gtk_tree_store_set(store,
			   treeiter,
			   MUG_COL_DATESTR,
			   datestr,
			   MUG_COL_MAILDIR,
			   mu_msg_get_maildir(msg),
			   MUG_COL_FLAGSSTR,
			   flag_str.c_str(),
			   MUG_COL_FROM,
			   from,
			   MUG_COL_TO,
			   to,
			   MUG_COL_SUBJECT,
			   mu_msg_get_subject(msg),
			   MUG_COL_PATH,
			   mu_msg_get_path(msg),
			   MUG_COL_PRIO,
			   mu_msg_get_prio(msg),
			   MUG_COL_FLAGS,
			   mu_msg_get_flags(msg),
			   MUG_COL_TIME,
			   timeval,
			   -1);
	g_free(from);
	g_free(to);
}

static int
update_model(GtkTreeStore* store, const char* xpath, const char* query, MugMsgListView* self)
{
	const auto res{run_query(xpath, query, self)};
	if (!res) {
		g_warning("error: running query failed");
		return -1;
	}

	auto count{0};

	std::string prev_thread_path;
	for (auto&& it : *res) {
		GtkTreeIter treeiter, prev_treeiter;

		const auto thread_path{it.query_match().thread_path};

		if (prev_thread_path.find(thread_path) == 0)
			gtk_tree_store_append(store, &treeiter, &prev_treeiter);
		else
			gtk_tree_store_append(store, &treeiter, NULL);

		/* don't unref msg */
		add_row(store, it.floating_msg(), &treeiter);

		// prev_ti        = ti;
		prev_treeiter    = treeiter;
		prev_thread_path = thread_path;
		++count;
	}

	return count;
}

int
mug_msg_list_view_query(MugMsgListView* self, const char* query)
{
	MugMsgListViewPrivate* priv;
	gboolean               rv;

	g_return_val_if_fail(MUG_IS_MSG_LIST_VIEW(self), FALSE);

	priv = MUG_MSG_LIST_VIEW_GET_PRIVATE(self);
	gtk_tree_store_clear(priv->_store);

	g_free(priv->_query);
	priv->_query = query ? g_strdup(query) : NULL;

	if (!query)
		return TRUE;

	rv = update_model(priv->_store, priv->_xpath, query, self);

	gtk_tree_view_expand_all(GTK_TREE_VIEW(self));

	return rv;
}

const gchar*
mug_msg_list_view_get_query(MugMsgListView* self)
{
	g_return_val_if_fail(MUG_IS_MSG_LIST_VIEW(self), NULL);

	return MUG_MSG_LIST_VIEW_GET_PRIVATE(self)->_query;
}
