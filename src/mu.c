/*
** Copyright (C) 2008, 2009 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <glib.h>
#include <glib-object.h>
#include <string.h>

#include "mu-index.h"
#include "mu-query.h"
#include "mu-util.h"

#include "mu-msg-gmime.h"

static MuResult
msg_cb  (MuIndexStats* stats, void *user_data)
{
	char *kars="-\\|/";
	static int i = 0;

	g_print ("%s%c", (!i)?"":"\b", kars[i % 4]);
	++i;
	
	return MU_OK;
}

static void
show_help (const char* cmd)
{
	if (cmd)
		g_print ("Help about %s\n", cmd);
	else
		g_print ("General help");
}


static gboolean opt_debug;
static gboolean opt_quiet;

static GOptionEntry entries[] = {
	{ "debug", 'd', 0, G_OPTION_ARG_NONE, &opt_debug,
	  "print debug output to standard-error", NULL },
	{ "quiet", 'q', 0, G_OPTION_ARG_NONE, &opt_quiet,
	  "don't give any progress information", NULL },
	{ NULL }
};





int
main (int argc, char *argv[])
{
	const char* cmd;
	
	GError *error = NULL;
	GOptionContext *context;
	MuResult rv;
	
	g_type_init ();
	
	context = g_option_context_new ("- search your e-mail");
	g_option_context_add_main_entries (context, entries, "mu");
	g_option_context_add_group (context, mu_query_option_group());
	
	if (!g_option_context_parse (context, &argc, &argv, &error)) {
		g_printerr ("option parsing failed: %s\n",
			    error->message);
		g_error_free (error);
		return 1;
	}
	
	if (argc < 2 || !( strcmp(argv[1], "index") == 0 ||
			   strcmp(argv[1], "search") == 0 ||
			   strcmp(argv[1], "help") == 0)) {
		g_printerr ("usage: mu [options] command [parameters]\n"
			    "\twhere command is one of index, search, help\n");
		return 1;
	}
			    
	cmd = argv[1];

	if (strcmp (cmd, "help") == 0) {
		show_help (argc > 2 ? argv[2] : NULL);
		return 0;
	}
	

	mu_msg_gmime_init ();

	rv = MU_OK;
	if (strcmp(cmd, "index") == 0) {
		MuIndex *midx;
		MuIndexStats stats;
		
		midx = mu_index_new ("/home/djcb/.mu");
		rv = mu_index_run (midx,
				   "/home/djcb/Maildir",
			      FALSE, &stats, msg_cb, NULL, NULL);
		
		mu_index_destroy (midx);
		
	} else if (strcmp(cmd, "search") == 0) {

		GSList *args;
		if (argc < 3) { /* FIXME */
			g_printerr ("error!\n");
			return 1;
		}
		
		args = mu_util_strlist_from_args (argc-2, argv+2); 
		rv = mu_query_run (args);
		mu_util_strlist_free (args);
	}
	
	mu_msg_gmime_uninit ();
	return rv == MU_OK ? 0 : 1;

}
