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
#include "mu-msg-header-view.hh"
#include "mu-msg.hh"

#include <utils/mu-str.h>
#include <utils/mu-utils.hh>

using namespace Mu;

/* 'private'/'protected' functions */
static void mu_msg_header_view_class_init(MuMsgHeaderViewClass* klass);
static void mu_msg_header_view_init(MuMsgHeaderView* obj);
static void mu_msg_header_view_finalize(GObject* obj);

/* list my signals  */
enum {
	/* MY_SIGNAL_1, */
	/* MY_SIGNAL_2, */
	LAST_SIGNAL
};

struct _MuMsgHeaderViewPrivate {
	GtkWidget* _grid;
};
#define MU_MSG_HEADER_VIEW_GET_PRIVATE(o)                                                          \
	(G_TYPE_INSTANCE_GET_PRIVATE((o), MU_TYPE_MSG_HEADER_VIEW, MuMsgHeaderViewPrivate))
/* globals */
static GtkBoxClass* parent_class = NULL;

/* uncomment the following if you have defined any signals */
/* static guint signals[LAST_SIGNAL] = {0}; */

G_DEFINE_TYPE(MuMsgHeaderView, mu_msg_header_view, GTK_TYPE_BOX);

static void
mu_msg_header_view_class_init(MuMsgHeaderViewClass* klass)
{
	GObjectClass* gobject_class;
	gobject_class = (GObjectClass*)klass;

	parent_class            = (GtkBoxClass*)g_type_class_peek_parent(klass);
	gobject_class->finalize = mu_msg_header_view_finalize;

	g_type_class_add_private(gobject_class, sizeof(MuMsgHeaderViewPrivate));

	/* signal definitions go here, e.g.: */
	/*	signals[MY_SIGNAL_1] = */
	/*		g_signal_new ("my_signal_1",....); */
	/*	signals[MY_SIGNAL_2] = */
	/*		g_signal_new ("my_signal_2",....); */
	/*	etc. */
}

static void
mu_msg_header_view_init(MuMsgHeaderView* obj)
{
	/*	static GtkBoxClass *parent_class = NULL; */
	obj->_priv        = MU_MSG_HEADER_VIEW_GET_PRIVATE(obj);
	obj->_priv->_grid = NULL;
}

static void
mu_msg_header_view_finalize(GObject* obj)
{
	G_OBJECT_CLASS(parent_class)->finalize(obj);
}

GtkWidget*
mu_msg_header_view_new(void)
{
	return GTK_WIDGET(g_object_new(MU_TYPE_MSG_HEADER_VIEW, NULL));
}

static GtkWidget*
get_label(const gchar* txt, gboolean istitle)
{
	GtkWidget* label;

	label = gtk_label_new(NULL);
	if (istitle) {
		char* markup;
		markup = g_strdup_printf("<b>%s</b>: ", txt);
		gtk_label_set_markup(GTK_LABEL(label), markup);
		gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_RIGHT);
		g_free(markup);
	} else {
		gtk_label_set_selectable(GTK_LABEL(label), TRUE);
		gtk_label_set_text(GTK_LABEL(label), txt ? txt : "");
		gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_LEFT);
	}

	return label;
}

G_GNUC_UNUSED static gboolean
add_row(GtkWidget* grid, guint row, const char* fieldname, const char* value, gboolean showempty)
{
	GtkWidget *label, *al;

	if (!value && !showempty)
		return FALSE;

	gtk_grid_insert_row(GTK_GRID(grid), row);

	label = get_label(fieldname, TRUE);
	al    = gtk_alignment_new(0.0, 0.0, 0.0, 0.0);
	gtk_container_add(GTK_CONTAINER(al), label);

	gtk_grid_attach(GTK_GRID(grid), al, 0, row, 1, 1);

	al = gtk_alignment_new(0.0, 1.0, 0.0, 0.0);

	label = get_label(value, FALSE);
	gtk_container_add(GTK_CONTAINER(al), label);
	gtk_grid_attach(GTK_GRID(grid), al, 1, row, 1, 1);

	return TRUE;
}

static GtkWidget*
get_grid(Mu::MuMsg* msg)
{
	GtkWidget* grid;
	int        row;

	row  = 0;
	grid = gtk_grid_new(); /* 5 2 */

	gtk_grid_insert_column(GTK_GRID(grid), 0);
	gtk_grid_insert_column(GTK_GRID(grid), 1);

	if (add_row(grid, row, "From", mu_msg_get_from(msg), TRUE))
		++row;
	if (add_row(grid, row, "To", mu_msg_get_to(msg), FALSE))
		++row;
	if (add_row(grid, row, "Cc", mu_msg_get_cc(msg), FALSE))
		++row;
	if (add_row(grid, row, "Subject", mu_msg_get_subject(msg), TRUE))
		++row;
	if (add_row(grid, row, "Date",
		    time_to_string("%c", mu_msg_get_date(msg)).c_str(), TRUE))
		++row;

	return grid;
}

void
mu_msg_header_view_set_message(MuMsgHeaderView* self, Mu::MuMsg* msg)
{
	g_return_if_fail(MU_IS_MSG_HEADER_VIEW(self));

	if (self->_priv->_grid) {
		gtk_container_remove(GTK_CONTAINER(self), self->_priv->_grid);
		self->_priv->_grid = NULL;
	}

	if (msg) {
		self->_priv->_grid = get_grid(msg);
		gtk_box_pack_start(GTK_BOX(self), self->_priv->_grid, TRUE, TRUE, 0);
		gtk_widget_show_all(self->_priv->_grid);
	}
}
