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

#include <glib.h>
#include <gio/gio.h>

#include <libguile.h>
#include <libmuguile/mu-guile-msg.h>
#include <libmuguile/mu-guile-store.h>

#include "mu-runtime.h"
#include "mu-util.h"

struct _ChildData {
	char **shell_argv;
	int  shell_argc;
};
typedef struct _ChildData ChildData;

static ChildData *
child_data_new (const char *muhome)
{
	ChildData *data;
	
	data = g_new0 (ChildData, 1);

	data->shell_argv = g_new0 (char*,3);
	data->shell_argc = 0;
	
	data->shell_argv[data->shell_argc++] = g_strdup ("procmule");
	data->shell_argv[data->shell_argc++] = g_strdup ("-s");
	data->shell_argv[data->shell_argc++] =
		g_strdup_printf ("%s%cprocmule.scm", muhome, G_DIR_SEPARATOR);

	return data;
}

static void
child_data_destroy (ChildData *data)
{
	if (!data)
		return;

	g_strfreev (data->shell_argv);
	g_free (data);
}

static void
on_dir_change (GFileMonitor *mon, GFile *file, GFile *other_file,
	       GFileMonitorEvent event_type,  ChildData *data)
{
	gchar *path;

	path = g_file_get_path (file);

	/* ignore all except create events */
	if (event_type != G_FILE_MONITOR_EVENT_CREATED)
		return;

	if (fork() == 0) { /* run guile in child */
		
		scm_with_guile (&mu_guile_msg_init, NULL);
		scm_with_guile (&mu_guile_store_init, NULL);
		
		if (!(gboolean)scm_with_guile
		    ((MuGuileFunc*)&mu_guile_msg_load_current, path)) {
			g_warning ("failed to set message in guile env");
			return;
		}	
		scm_shell (data->shell_argc, data->shell_argv); /* never returns */
	}
		
	g_free (path);
}

static GFileMonitor*
create_monitor (const char *path, ChildData *data)
{
	GFile *dir;
	GFileMonitor *dirmon;
	GError *err;
	
	if (!mu_util_check_dir (path, TRUE, FALSE)) {
		g_warning ("must be a readable dir: '%s'", path);
		return NULL;
	}

	dir = g_file_new_for_path (path);

	err = NULL;
	dirmon = g_file_monitor_directory (dir, G_FILE_MONITOR_NONE,
					   NULL, &err);
	if (!dirmon) {
		g_warning ("error adding monitor: %s", err->message);
		g_error_free (err);
	}
	
	g_object_unref (dir);

	if (dirmon) 
		g_signal_connect (dirmon, "changed",
				  G_CALLBACK(on_dir_change), data);
	
	return dirmon;
}

static void
destroy_watchlist (GSList *lst)
{
	g_slist_foreach (lst, (GFunc)g_object_unref, NULL);
	g_slist_free (lst);
}


GSList*
create_watchlist  (char **dirs, ChildData *data)
{
	GSList *watchlist;
	char **cur;
	
	/* TODO: check for dups */
	for (watchlist = NULL, cur = dirs; cur && *cur; ++cur) {
		GFileMonitor *dirmon;
		dirmon = create_monitor (*cur, data);
		if (!dirmon) {
			destroy_watchlist (watchlist);
			return NULL;
		}
		watchlist = g_slist_prepend (watchlist, dirmon);
	}
	
	return watchlist;
}


struct _PMConfig {
	char *muhome;
	char **watchdirs;
	
};
typedef struct _PMConfig PMConfig;


static PMConfig *
pm_config_new (int *argcp, char ***argvp)
{
	GOptionContext *octx;
	char **cur;
	PMConfig *opts = g_new0 (PMConfig, 1);
	GOptionEntry entries[] = {
		{"muhome", 0, 0, G_OPTION_ARG_FILENAME, &opts->muhome,
		 "specify an alternative mu directory", NULL},
		{"watch", 0, 0, G_OPTION_ARG_FILENAME_ARRAY, &opts->watchdirs,
		 "directory to watch (may be specified multiple times)", NULL},
		{NULL, 0, 0, G_OPTION_ARG_NONE, NULL, NULL, NULL}/* sentinel */
	};

        octx = g_option_context_new ("- procmule options");
	g_option_context_add_main_entries (octx, entries, "Procmule");

	if (!g_option_context_parse (octx, argcp, argvp, NULL)) {
		g_printerr ("error in options\n");
		goto error;
	}

	if (!opts->watchdirs) {
		g_printerr ("specify at least one --watch=<dir>\n");
		goto error;
	} 

	for (cur = opts->watchdirs; cur && *cur; ++cur)
		*cur = mu_util_dir_expand (*cur);
	
	if (opts->muhome)
		opts->muhome = mu_util_dir_expand (opts->muhome);	
	
	g_option_context_free (octx);
	return opts;

error:
	if (octx)
		g_option_context_free (octx);
	g_free (opts);
	return NULL;	
}

static void
pm_config_destroy (PMConfig *conf)
{
	if (!conf)
		return;
	
	g_free     (conf->muhome);
	g_strfreev (conf->watchdirs);
	
	g_free (conf);
}


static void
usage (void)
{
	g_print ("usage: procmule [--muhome=<dir>] [--watch=<dir1>]\n");
	g_print ("also, see toys/procmule/README\n");
}


int
main (int argc, char *argv[])
{
	PMConfig *opts;
	GSList *watchlist;
	GMainLoop *loop;
	ChildData *child_data;
	
	g_type_init ();
	g_thread_init (NULL);
	
#ifdef HAVE_PRE2_GUILE	
	g_warning ("Note: pre-2.x version of guile: procmule will not function "
		   "correctly unless you're using UTF-8 locale.");
#endif /* HAVE_PRE2_GUILE */

	opts = pm_config_new (&argc, &argv);
	if (!opts) {
		usage ();
		goto error;
	}
		
	if (!mu_runtime_init (opts->muhome /* NULL is okay */,
			      "procmule")) {
		usage ();
		goto error;
	}

	child_data = child_data_new
		(mu_runtime_path(MU_RUNTIME_PATH_MUHOME));

	watchlist = create_watchlist (opts->watchdirs, child_data);
	if (!watchlist)
		goto error;
	
	loop = g_main_loop_new (NULL, TRUE);
	g_main_loop_run (loop);

	g_main_loop_unref (loop);

	destroy_watchlist (watchlist);
	mu_runtime_uninit ();
	pm_config_destroy (opts);
	child_data_destroy (child_data);
	
	return 0;

error:
	pm_config_destroy (opts);
	return 1;
	
}
