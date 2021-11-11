/*
** Copyright (C) 2010-2020 Dirk-Jan C. Binnema  <djcb@djcbsoftware.nl>
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

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include <string.h>		/* for memset */

#include <utils/mu-util.h>
#include <mu-store.hh>
#include <mu-runtime.hh>

#include "mug-msg-list-view.h"
#include "mug-query-bar.h"
#include "mug-msg-view.h"
#include "mug-shortcuts.h"

struct _MugData {
	GtkWidget *win;
	GtkWidget *statusbar;
	GtkWidget *mlist;
	GtkWidget *toolbar;
	GtkWidget *msgview;
	GtkWidget *querybar;
	GtkWidget *shortcuts;
	gchar *muhome;
};
typedef struct _MugData MugData;


static void
about_mug (MugData * mugdata)
{
	GtkWidget *about;
	about = gtk_message_dialog_new
	    (GTK_WINDOW (mugdata->win), GTK_DIALOG_MODAL,
	     GTK_MESSAGE_INFO, GTK_BUTTONS_OK,
	     "Mug version %s\n"
	     "A graphical frontend to the 'mu' e-mail search engine\n\n"
	     "(c) 2010-2013 Dirk-Jan C. Binnema\n"
	     "Released under the terms of the GPLv3+", VERSION);

	gtk_dialog_run (GTK_DIALOG (about));
	gtk_widget_destroy (about);
}

enum _ToolAction {
	ACTION_PREV_MSG = 1,
	ACTION_NEXT_MSG,
	ACTION_REINDEX,
	ACTION_DO_QUIT,
	ACTION_ABOUT,
	ACTION_SEPARATOR	/* pseudo action */
};
typedef enum _ToolAction ToolAction;

static void
on_tool_button_clicked (GtkToolButton * btn, MugData * mugdata)
{
	ToolAction action;
	action = (ToolAction)
	    GPOINTER_TO_UINT (g_object_get_data (G_OBJECT (btn), "action"));
	switch (action) {

	case ACTION_DO_QUIT:
		gtk_main_quit ();
		break;
	case ACTION_NEXT_MSG:
		mug_msg_list_view_move_next (MUG_MSG_LIST_VIEW
					     (mugdata->mlist));
		break;
	case ACTION_PREV_MSG:
		mug_msg_list_view_move_prev (MUG_MSG_LIST_VIEW
					     (mugdata->mlist));
		break;
	case ACTION_ABOUT:
		about_mug (mugdata);
		break;
	default:
		g_print ("%u\n", action);
	}
}



static GtkToolItem*
tool_button (const char *name)
{
	GtkWidget *icon;

	icon = gtk_image_new_from_icon_name
		(name, GTK_ICON_SIZE_SMALL_TOOLBAR);

	return gtk_menu_tool_button_new (icon, NULL);
}


static GtkToolItem*
get_connected_tool_button (const char* stock_id, ToolAction action,
			   MugData *mugdata)
{
	GtkToolItem *btn;

	btn = tool_button (stock_id);
	g_object_set_data (G_OBJECT (btn), "action",
			   GUINT_TO_POINTER (action));
	g_signal_connect (G_OBJECT (btn), "clicked",
			  G_CALLBACK (on_tool_button_clicked),
			  mugdata);
	return btn;
}

static GtkWidget *
mug_toolbar (MugData * mugdata)
{
	GtkWidget *toolbar;
	int i;
	struct {
		const char *stock_id;
		ToolAction action;
	} tools[] = {
		{"go-up", ACTION_PREV_MSG},
		{"go-down", ACTION_NEXT_MSG},
		{NULL, ACTION_SEPARATOR},
		{"view-refresh", ACTION_REINDEX},
		{NULL, ACTION_SEPARATOR},
		{"help-about", ACTION_ABOUT},
		{NULL, ACTION_SEPARATOR},
		{"application-exit", ACTION_DO_QUIT}};

	toolbar = gtk_toolbar_new ();
	for (i = 0; i != G_N_ELEMENTS (tools); ++i) {
		if (tools[i].action == ACTION_SEPARATOR) { /* separator? */
			gtk_toolbar_insert (GTK_TOOLBAR (toolbar),
					    gtk_separator_tool_item_new (), i);
			continue;
		} else /* nope: a real item */
			gtk_toolbar_insert (GTK_TOOLBAR (toolbar),
					    get_connected_tool_button
					    (tools[i].stock_id, tools[i].action,
					     mugdata), i);
	}

	return toolbar;
}

static void
on_shortcut_clicked (GtkWidget * w, const gchar * query, MugData * mdata)
{
	mug_query_bar_set_query (MUG_QUERY_BAR (mdata->querybar), query, TRUE);
}

static GtkWidget *
mug_shortcuts_bar (MugData * data)
{
	data->shortcuts = mug_shortcuts_new
		(mu_runtime_path(MU_RUNTIME_PATH_BOOKMARKS));

	g_signal_connect (G_OBJECT (data->shortcuts), "clicked",
			  G_CALLBACK (on_shortcut_clicked), data);

	return data->shortcuts;
}

static GtkWidget *
mug_statusbar (void)
{
	GtkWidget *statusbar;

	statusbar = gtk_statusbar_new ();

	return statusbar;
}

static void
on_query_changed (MugQueryBar * bar, const char *query, MugData * mugdata)
{
	int count;

	/* clear the old message */
	mug_msg_view_set_msg (MUG_MSG_VIEW (mugdata->msgview), NULL);

	count = mug_msg_list_view_query (MUG_MSG_LIST_VIEW (mugdata->mlist),
					 query);
	if (count >= 0) {
		gchar *msg =
		    g_strdup_printf ("%d message%s found matching '%s'",
				     count,
				     count > 1 ? "s" : "",
				     mug_msg_list_view_get_query
				     (MUG_MSG_LIST_VIEW (mugdata->mlist)));
		gtk_statusbar_push (GTK_STATUSBAR (mugdata->statusbar), 0, msg);
		g_free (msg);

		mug_msg_list_view_move_first (MUG_MSG_LIST_VIEW
					      (mugdata->mlist));
		gtk_widget_grab_focus (GTK_WIDGET (mugdata->mlist));
	}

	if (count == 0)		/* nothing found */
		mug_query_bar_grab_focus (MUG_QUERY_BAR (bar));
}

static void
on_msg_selected (MugMsgListView * mlist, const char *mpath, MugData * mugdata)
{
	mug_msg_view_set_msg (MUG_MSG_VIEW (mugdata->msgview), mpath);
}

static void
on_list_view_error (MugMsgListView * mlist, MugError err, MugData * mugdata)
{
	GtkWidget *errdialog;
	const char *msg;

	switch (err) {
	case MUG_ERROR_XAPIAN_NOT_UPTODATE:
		msg = "The Xapian Database has the wrong version\n"
		      "Please run 'mu init'";
		break;
	case MUG_ERROR_XAPIAN_DIR:
		msg = "Cannot find the Xapian database dir\n"
		    "Please restart mug with --muhome=... pointing\n"
		    "to your mu home directory";
		break;
	case MUG_ERROR_QUERY:
		msg = "Error in query";
		break;
	default:
		msg = "Some error occurred";
		break;
	}

	errdialog = gtk_message_dialog_new
	    (GTK_WINDOW (mugdata->win), GTK_DIALOG_MODAL,
	     GTK_MESSAGE_ERROR, GTK_BUTTONS_OK, "%s", msg);

	gtk_dialog_run (GTK_DIALOG (errdialog));
	gtk_widget_destroy (errdialog);

	if (err == MUG_ERROR_QUERY)
		mug_query_bar_grab_focus (MUG_QUERY_BAR (mugdata->querybar));
}

static GtkWidget *
mug_querybar (void)
{
	GtkWidget *querybar;

	querybar = mug_query_bar_new ();

	return querybar;
}

static GtkWidget *
mug_query_area (MugData * mugdata)
{
	GtkWidget *queryarea, *paned, *scrolled;

	queryarea = gtk_box_new (GTK_ORIENTATION_VERTICAL, 2);
	paned = gtk_paned_new (GTK_ORIENTATION_VERTICAL);

	mugdata->mlist = mug_msg_list_view_new
		(mu_runtime_path(MU_RUNTIME_PATH_XAPIANDB));
	scrolled = gtk_scrolled_window_new (NULL, NULL);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled),
					GTK_POLICY_AUTOMATIC,
					GTK_POLICY_AUTOMATIC);

	gtk_container_add (GTK_CONTAINER (scrolled), mugdata->mlist);
	gtk_paned_add1 (GTK_PANED (paned), scrolled);

	mugdata->msgview = mug_msg_view_new ();
	mug_msg_view_set_note (MUG_MSG_VIEW(mugdata->msgview),
			       "<h1>Welcome to <i>mug</i>!</h1><hr>"
			       "<tt>mug</tt> is an experimental UI for <tt>mu</tt>, which will "
			       "slowly evolve into something useful.<br><br>Enjoy the ride.");
	g_signal_connect (G_OBJECT (mugdata->mlist), "msg-selected",
			  G_CALLBACK (on_msg_selected), mugdata);
	g_signal_connect (G_OBJECT (mugdata->mlist), "error-occured",
			  G_CALLBACK (on_list_view_error), mugdata);
	gtk_paned_add2 (GTK_PANED (paned), mugdata->msgview);

	mugdata->querybar = mug_querybar ();
	g_signal_connect (G_OBJECT (mugdata->querybar), "query-changed",
			  G_CALLBACK (on_query_changed), mugdata);

	gtk_box_pack_start (GTK_BOX (queryarea),
			    mugdata->querybar, FALSE, FALSE, 2);
	gtk_box_pack_start (GTK_BOX (queryarea), paned, TRUE, TRUE, 2);

	gtk_widget_show_all (queryarea);
	return queryarea;
}

static GtkWidget *
mug_main_area (MugData * mugdata)
{
	GtkWidget *mainarea, *w;

	mainarea = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);

	w = mug_shortcuts_bar (mugdata);
	gtk_box_pack_start (GTK_BOX (mainarea), w, FALSE, FALSE, 0);
	gtk_widget_show (w);

	w = mug_query_area (mugdata);
	gtk_box_pack_start (GTK_BOX (mainarea), w, TRUE, TRUE, 0);
	gtk_widget_show (w);

	return mainarea;
}

static GtkWidget*
mug_shell (MugData *mugdata)
{
	GtkWidget *vbox;

	mugdata->win = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title (GTK_WINDOW (mugdata->win), "Mug Mail Search");

	vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 2);

	mugdata->toolbar = mug_toolbar (mugdata);
	gtk_box_pack_start (GTK_BOX (vbox), mugdata->toolbar, FALSE, FALSE, 2);
	gtk_box_pack_start (GTK_BOX (vbox), mug_main_area (mugdata), TRUE,
			    TRUE, 2);

	mugdata->statusbar = mug_statusbar ();
	gtk_box_pack_start (GTK_BOX (vbox), mugdata->statusbar, FALSE, FALSE,
			    2);

	gtk_container_add (GTK_CONTAINER (mugdata->win), vbox);
	gtk_widget_show_all (vbox);

	gtk_window_set_default_size (GTK_WINDOW (mugdata->win), 700, 500);
	gtk_window_set_resizable (GTK_WINDOW (mugdata->win), TRUE);

	{
		gchar *icon;
		icon = g_strdup_printf ("%s%cmug.svg",
					MUGDIR, G_DIR_SEPARATOR);
		gtk_window_set_icon_from_file (GTK_WINDOW (mugdata->win), icon, NULL);
		g_free (icon);
	}

	return mugdata->win;
}

static gint
on_focus_query_bar (GtkWidget* ignored, GdkEventKey *event, MugData* mugdata)
{
	if (event->type==GDK_KEY_RELEASE && event->keyval==GDK_KEY_Escape) {
		mug_query_bar_grab_focus (MUG_QUERY_BAR (mugdata->querybar));
		return 1;
	}
	return 0;
}

int
main (int argc, char *argv[])
{
	MugData mugdata;
	GtkWidget *mugshell;
	GOptionContext *octx;
	GOptionEntry entries[] = {
		{"muhome", 0, 0, G_OPTION_ARG_FILENAME, &mugdata.muhome,
		 "specify an alternative mu directory", NULL},
		{NULL, 0, 0, G_OPTION_ARG_NONE, NULL, NULL, NULL}	/* sentinel */
	};

	gtk_init (&argc, &argv);

	octx = g_option_context_new ("- mug options");
	g_option_context_add_main_entries (octx, entries, "Mug");

	memset (&mugdata, 0, sizeof (MugData));
	if (!g_option_context_parse (octx, &argc, &argv, NULL)) {
		g_option_context_free (octx);
		g_printerr ("mug: error in options\n");
		return 1;
	}

	g_option_context_free (octx);
	mu_runtime_init (mugdata.muhome, "mug", FALSE);

	mugshell = mug_shell (&mugdata);
	g_signal_connect (G_OBJECT (mugshell), "destroy",
			  G_CALLBACK (gtk_main_quit), NULL);
	g_signal_connect (G_OBJECT (mugshell), "key_release_event",
			  G_CALLBACK ( on_focus_query_bar ), (gpointer)&mugdata );

	gtk_widget_show (mugshell);
	mug_query_bar_grab_focus (MUG_QUERY_BAR (mugdata.querybar));

	gtk_main ();
	g_free (mugdata.muhome);

	mu_runtime_uninit ();

	return 0;
}
