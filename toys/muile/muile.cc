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
#if HAVE_CONFIG_H
#include <config.h>
#endif /*HAVE_CONFIG_H*/

#include <mu-runtime.h>
#include <glib-object.h>

#include <libguile.h>
#include <libmuguile/mu-guile-msg.h>
#include <libmuguile/mu-guile-store.h>

struct _MuileConfig {
	const char *muhome;
	char *msgpath;
};
typedef struct _MuileConfig MuileConfig;


static MuileConfig *
muile_config_new (int *argcp, char ***argvp)
{
	GOptionContext *octx;
	MuileConfig *opts = g_new0 (MuileConfig, 1);
	GOptionEntry entries[] = {
		{"muhome", 0, 0, G_OPTION_ARG_FILENAME, &opts->muhome,
		 "specify an alternative mu directory", NULL},
		{"msg", 0, 0, G_OPTION_ARG_FILENAME, &opts->msgpath,
		 "specify path to a message to load as mu:current-msg)", NULL},
		{NULL, 0, 0, G_OPTION_ARG_NONE, NULL, NULL, NULL}/* sentinel */
	};

        octx = g_option_context_new ("- muile options");
	g_option_context_add_main_entries (octx, entries, "Muile");

	if (!g_option_context_parse (octx, argcp, argvp, NULL)) {
		g_option_context_free (octx);
		g_printerr ("muile: error in options\n");
		return NULL;
	}

	if (opts->msgpath)
		opts->msgpath = mu_util_dir_expand (opts->msgpath);
	
	g_option_context_free (octx);

	return opts;	
}

static void
muile_config_destroy (MuileConfig *conf)
{
	g_free (conf->msgpath);
	g_free (conf);
}


static void
usage (void)
{
	g_print ("usage: muile [--muhome=<dir>] [msgfile]\n");	
}


int
main (int argc, char *argv[])
{
	MuileConfig *opts;

	g_type_init ();
	
#ifdef HAVE_PRE2_GUILE	
	g_warning ("Note: muile will not function correctly unless you have a "
		   "UTF-8 locale.");
#endif /* HAVE_PRE2_GUILE */

	opts = muile_config_new (&argc, &argv);
	if (!opts) {
		usage ();
		goto error;
	}
		
	if (!mu_runtime_init (opts->muhome /* NULL is okay */)) {
		usage ();
		goto error;
	}

	scm_with_guile (&mu_guile_msg_init, NULL);
	scm_with_guile (&mu_guile_store_init, NULL);
	
	if (opts->msgpath) {
		if (!(gboolean)scm_with_guile
		    ((MuGuileFunc*)&mu_guile_msg_load_current, opts->msgpath))
			goto error;
	}

	
	scm_shell (argc, argv);

	mu_runtime_uninit ();
	muile_config_destroy (opts);
	
	return 0;

error:
	muile_config_destroy (opts);
	return 1;
	
}
