/*
** Copyright (C) 2008-2010 Dirk-Jan C. Binnema  <djcb@djcbsoftware.nl>
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

#if HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG*/

#include <gtk/gtk.h>
#include <string.h> /* for memset */

#include "mu-config.h"
#include "mu-log.h"
#include "mu-util.h"

#include "mug-msg-list-view.h"
#include "mug-query-bar.h"
#include "mug-msg-view.h"

struct _MugData {
	GtkWidget *win;
	GtkWidget *statusbar;
	GtkWidget *mlist;
	GtkWidget *toolbar;
	GtkWidget *msgview;
	GtkWidget *querybar;
	gchar *muhome;
};
typedef struct _MugData MugData;
	

static void
about_mug (MugData *mugdata)
{
	GtkWidget *about;
	about = gtk_message_dialog_new
		(GTK_WINDOW(mugdata->win), GTK_DIALOG_MODAL,
		 GTK_MESSAGE_INFO,GTK_BUTTONS_OK,
		 "Mug version %s\n"
		 "A graphical frontend to the 'mu' e-mail search engine\n\n"
		 "(c) 2010 Dirk-Jan C. Binnema\n"
		 "Released under the terms of the GPLv3+", VERSION);
	
	gtk_dialog_run (GTK_DIALOG(about));
	gtk_widget_destroy (about);
}

enum _ToolAction {
	ACTION_PREV_MSG = 1,
	ACTION_NEXT_MSG,
	ACTION_DO_QUIT,
	ACTION_ABOUT,
	ACTION_SEPARATOR /* pseudo action */
};
typedef enum _ToolAction ToolAction;

static void
on_tool_button_clicked (GtkToolButton *btn, MugData *mugdata)
{
	ToolAction action;
	action = (ToolAction)GPOINTER_TO_UINT(g_object_get_data(G_OBJECT(btn),
								"action"));
	switch (action) {
		 
	case ACTION_DO_QUIT:
		gtk_main_quit();
		break;
	case ACTION_NEXT_MSG:
		mug_msg_list_view_move_next (MUG_MSG_LIST_VIEW(mugdata->mlist));
		break;
	case ACTION_PREV_MSG:
		mug_msg_list_view_move_prev (MUG_MSG_LIST_VIEW(mugdata->mlist));
		break;
	case ACTION_ABOUT:
		about_mug (mugdata);
		break;
	default:
		g_print ("%u\n", action);
	}
}

static GtkWidget*
mug_toolbar (MugData *mugdata)
{
	GtkWidget *toolbar;
	int i;
	struct {
		const char* stock_id;
		ToolAction action;
	} tools[] = {
		{ GTK_STOCK_GO_UP, ACTION_PREV_MSG },
		{ GTK_STOCK_GO_DOWN, ACTION_NEXT_MSG },
		{ NULL, ACTION_SEPARATOR },
		{ GTK_STOCK_ABOUT, ACTION_ABOUT },
		{ NULL, ACTION_SEPARATOR },
		{ GTK_STOCK_QUIT, ACTION_DO_QUIT },
	};	
	
	for (toolbar = gtk_toolbar_new(), i = 0; i != G_N_ELEMENTS(tools); ++i) {
		GtkToolItem *btn;

		/* separator item ? */
		if (tools[i].action == ACTION_SEPARATOR) {
			gtk_toolbar_insert (GTK_TOOLBAR(toolbar),
					    gtk_separator_tool_item_new(), i);
			continue;
		}

		/* nope: a real item */
		btn = gtk_tool_button_new_from_stock (tools[i].stock_id);
		g_object_set_data (G_OBJECT(btn), "action",
				   GUINT_TO_POINTER(tools[i].action));
		g_signal_connect (G_OBJECT(btn), "clicked",
				  G_CALLBACK(on_tool_button_clicked),
				  mugdata);
		gtk_toolbar_insert (GTK_TOOLBAR(toolbar), btn, i);
	}
	
	return toolbar;
}

static GtkWidget*
mug_statusbar (void)
{
	GtkWidget *statusbar;

	statusbar = gtk_statusbar_new ();

	return statusbar;
}

static void
on_query_changed (MugQueryBar *bar, const char* query, MugData *mugdata)
{
	int count;

	/* clear the old message */
	mug_msg_view_set_text (MUG_MSG_VIEW(mugdata->msgview), NULL);
		
	count = mug_msg_list_view_query (MUG_MSG_LIST_VIEW(mugdata->mlist),
					 query);
	if (count >= 0) {
		gchar *msg =
			g_strdup_printf ("%d message%s found matching '%s'",
					 count,
					 count > 1 ? "s" : "",
					 mug_msg_list_view_get_query
					 (MUG_MSG_LIST_VIEW(mugdata->mlist)));
		gtk_statusbar_push (GTK_STATUSBAR(mugdata->statusbar), 0, msg);
		g_free (msg);
		
		mug_msg_list_view_move_first (MUG_MSG_LIST_VIEW(mugdata->mlist));
		gtk_widget_grab_focus (GTK_WIDGET(mugdata->mlist));
	}	
}


static void
on_msg_selected (MugMsgListView *mlist, const char* mpath, MugData *mugdata)
{
	// g_warning ("msg selected: %s", mpath);
	mug_msg_view_set_msg (MUG_MSG_VIEW(mugdata->msgview), mpath);
}



static GtkWidget*
mug_querybar (void)
{
	GtkWidget *querybar;

	querybar = mug_query_bar_new ();
	
	return querybar;
}


static GtkWidget*
mug_query_area (MugData *mugdata)
{
	GtkWidget *queryarea;
	GtkWidget *paned;
	GtkWidget *scrolled;

	gchar* xdir;
	
	queryarea = gtk_vbox_new (FALSE, 2);
	
	paned = gtk_vpaned_new ();

	xdir = mu_util_guess_xapian_dir (mugdata->muhome);
	mugdata->mlist = mug_msg_list_view_new(xdir);
	g_free (xdir);
	
	scrolled = gtk_scrolled_window_new (NULL, NULL);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW(scrolled),
					GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	
	gtk_container_add (GTK_CONTAINER(scrolled), mugdata->mlist);
	gtk_paned_add1 (GTK_PANED (paned), scrolled);

	mugdata->msgview = mug_msg_view_new ();
	mug_msg_view_set_msg (MUG_MSG_VIEW(mugdata->msgview), NULL);
	g_signal_connect (G_OBJECT(mugdata->mlist), "msg-selected",
			  G_CALLBACK(on_msg_selected), mugdata);
	gtk_paned_add2 (GTK_PANED (paned), mugdata->msgview);

	mugdata->querybar = mug_querybar();	
	g_signal_connect (G_OBJECT(mugdata->querybar), "query_changed",
			  G_CALLBACK(on_query_changed),
			  mugdata);
	
	gtk_box_pack_start (GTK_BOX(queryarea),
			    mugdata->querybar, FALSE, FALSE, 2);
	gtk_box_pack_start (GTK_BOX(queryarea),
			    paned, TRUE, TRUE, 2);
	
	return queryarea;
}


static GtkWidget*
mug_main_area (MugData *mugdata)
{
	GtkWidget *mainarea;

	mainarea = gtk_hpaned_new ();
	//gtk_paned_add1 (GTK_PANED (mainarea), mug_shortcuts());
	gtk_paned_add2 (GTK_PANED (mainarea), mug_query_area (mugdata));

	return mainarea;
}


GtkWidget*
mug_shell (MugData *mugdata)
{
	GtkWidget *vbox;
	gchar *icon;
	
	mugdata->win = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title (GTK_WINDOW(mugdata->win), "Mug Mail Search");

	vbox = gtk_vbox_new (FALSE, 2);
	mugdata->toolbar = mug_toolbar(mugdata);
	gtk_box_pack_start (GTK_BOX(vbox), mugdata->toolbar, FALSE, FALSE, 2);
	gtk_box_pack_start (GTK_BOX(vbox), mug_main_area(mugdata), TRUE, TRUE, 2);
	
	mugdata->statusbar = mug_statusbar();
	gtk_box_pack_start (GTK_BOX(vbox), mugdata->statusbar, FALSE, FALSE, 2);
	
	gtk_container_add (GTK_CONTAINER(mugdata->win), vbox);
	gtk_widget_show_all (vbox);

	gtk_window_set_default_size (GTK_WINDOW(mugdata->win), 700, 500);
	gtk_window_set_resizable (GTK_WINDOW(mugdata->win), TRUE);

	icon = g_strdup_printf ("%s%cmug.svg", ICONDIR, G_DIR_SEPARATOR);
	gtk_window_set_icon_from_file (GTK_WINDOW(mugdata->win), icon, NULL);
	g_free (icon);
	
	return mugdata->win;
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
		{NULL, 0, 0, G_OPTION_ARG_NONE, NULL, NULL, NULL} /* sentinel */
	};

	gtk_init (&argc, &argv);

	octx = g_option_context_new ("- mug options");
	g_option_context_add_main_entries (octx, entries, "Mug");

	memset (&mugdata, 0, sizeof(MugData));
	if (!g_option_context_parse (octx, &argc, &argv, NULL)) {
		g_printerr ("mug: error in options\n");
		return 1;
	}
	
	mugshell = mug_shell (&mugdata);	
	g_signal_connect(G_OBJECT(mugshell), "destroy", 
			 G_CALLBACK(gtk_main_quit), NULL);
	
	gtk_widget_show (mugshell);
	mug_query_bar_grab_focus (MUG_QUERY_BAR(mugdata.querybar));
 	
	gtk_main ();
	g_free (mugdata.muhome);
	
	return 0;
}
