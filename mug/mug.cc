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
#include <config.h>
#endif /*HAVE_CONFIG_H*/

#include <gtk/gtk.h>

#include "mu-config.h"
#include "mu-log.h"
#include "mu-util.h"

#include "mug-msg-list-view.h"
#include "mug-query-bar.h"
#include "mug-msg-view.h"

struct _MugData {
	GtkWidget *statusbar;
	GtkWidget *mlist;
	GtkWidget *toolbar;
	GtkWidget *msgview;
	GtkWidget *querybar;
};
typedef struct _MugData MugData;
	

static GtkWidget*
mug_menu (void)
{
	GtkWidget *menu;

	menu = gtk_menu_bar_new ();

	return menu;
}

enum _ToolAction {
	ACTION_PREV_MSG = 1,
	ACTION_NEXT_MSG,
	ACTION_SHOW_PREFS,
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
		g_print ("About Mug\n");
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
		{GTK_STOCK_GO_UP, ACTION_PREV_MSG},
		{GTK_STOCK_GO_DOWN, ACTION_NEXT_MSG},
		{NULL, ACTION_SEPARATOR},
		{GTK_STOCK_PREFERENCES, ACTION_SHOW_PREFS},
		{GTK_STOCK_ABOUT, ACTION_ABOUT},
		{NULL, ACTION_SEPARATOR},
		{GTK_STOCK_QUIT, ACTION_DO_QUIT},
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

#if 0
static GtkWidget*
mug_shortcuts (void)
{
	GtkWidget *shortcuts;

	shortcuts = gtk_vbutton_box_new ();

	gtk_box_pack_start (GTK_BOX(shortcuts),
			    gtk_button_new_with_label ("Inbox"), FALSE, FALSE, 2);
	gtk_box_pack_start (GTK_BOX(shortcuts),
			    gtk_button_new_with_label ("Archive"), FALSE, FALSE, 2);
	gtk_box_pack_start (GTK_BOX(shortcuts),
			    gtk_button_new_with_label ("Sent"), FALSE, FALSE, 2);

	gtk_button_box_set_layout (GTK_BUTTON_BOX(shortcuts), GTK_BUTTONBOX_START);
	
	return shortcuts;
}
#endif 


static GtkWidget*
mug_query_area (MugData *mugdata)
{
	GtkWidget *queryarea;
	GtkWidget *paned;
	GtkWidget *scrolled;

	gchar* xdir;
	
	queryarea = gtk_vbox_new (FALSE, 2);
	
	paned = gtk_vpaned_new ();

	xdir = mu_util_guess_xapian_dir (NULL);
	mugdata->mlist = mug_msg_list_view_new(xdir);
	g_free (xdir);
	
	scrolled = gtk_scrolled_window_new (NULL, NULL);
	gtk_container_add (GTK_CONTAINER(scrolled), mugdata->mlist);
	gtk_paned_add1 (GTK_PANED (paned), scrolled);

	mugdata->msgview = mug_msg_view_new ();	
	g_signal_connect (G_OBJECT(mugdata->mlist), "msg-selected",
			  G_CALLBACK(on_msg_selected), mugdata);

	scrolled = gtk_scrolled_window_new (NULL, NULL);
	gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW(scrolled),
					       mugdata->msgview);
	gtk_paned_add2 (GTK_PANED (paned), scrolled);

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
	GtkWidget *win, *vbox;
	
	win = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title (GTK_WINDOW(win), "mu");

	vbox = gtk_vbox_new (FALSE, 2);

	gtk_box_pack_start (GTK_BOX(vbox), mug_menu(), FALSE, FALSE, 2);

	mugdata->toolbar = mug_toolbar(mugdata);
	gtk_box_pack_start (GTK_BOX(vbox), mugdata->toolbar, FALSE, FALSE, 2);
	gtk_box_pack_start (GTK_BOX(vbox), mug_main_area(mugdata), TRUE, TRUE, 2);
	
	mugdata->statusbar = mug_statusbar();
	gtk_box_pack_start (GTK_BOX(vbox), mugdata->statusbar, FALSE, FALSE, 2);
	
	gtk_container_add (GTK_CONTAINER(win), vbox);
	gtk_widget_show_all (vbox);

	gtk_window_set_default_size (GTK_WINDOW(win), 500, 500);
	gtk_window_set_resizable (GTK_WINDOW(win), TRUE);
	
	return win;
}

int
main (int argc, char *argv[])
{
	MugData mugdata;
	GtkWidget *mugshell;
	
	gtk_init (&argc, &argv);
			
	mugshell = mug_shell (&mugdata);	
	g_signal_connect(G_OBJECT(mugshell), "destroy", 
			 G_CALLBACK(gtk_main_quit), NULL);
	
	gtk_widget_show (mugshell);
	mug_query_bar_grab_focus (MUG_QUERY_BAR(mugdata.querybar));
 	
	gtk_main ();
	
	return 0;
}
