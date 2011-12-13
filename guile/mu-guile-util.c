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
#include "mu-guile-util.h"

SCM
mu_guile_util_error (const char *func_name, int status,
		const char *fmt, SCM args)
{
	scm_error_scm (scm_from_locale_symbol ("MuError"),
		       scm_from_utf8_string (func_name ? func_name : "<nameless>"),
		       scm_from_utf8_string (fmt), args,
		       scm_list_1 (scm_from_int (status)));

	return SCM_UNSPECIFIED;
}

SCM
mu_guile_util_g_error (const char *func_name, GError *err)
{
	scm_error_scm (scm_from_locale_symbol ("MuError"),
		       scm_from_utf8_string (func_name),
		       scm_from_utf8_string (err ? err->message : "error"),
		       SCM_UNDEFINED, SCM_UNDEFINED);

	return SCM_UNSPECIFIED;
}



#include <mu-runtime.h>
#include <glib-object.h>

#include "mu-guile-util.h"
#include "mu-guile-msg.h"

struct _GuileConfig {
	const char	*muhome;
	char		*msgpath;
};
typedef struct _GuileConfig GuileConfig;


static GuileConfig*
guile_config_new (int *argcp, char ***argvp)
{
	GOptionContext *octx;
	GuileConfig *opts = g_new0 (GuileConfig, 1);
	GOptionEntry entries[] = {
		{"muhome", 0, 0, G_OPTION_ARG_FILENAME, &opts->muhome,
		 "specify an alternative mu directory", NULL},
		{"msg", 0, 0, G_OPTION_ARG_FILENAME, &opts->msgpath,
		 "specify path to a message to load as mu:current-msg)", NULL},
		{NULL, 0, 0, G_OPTION_ARG_NONE, NULL, NULL, NULL}/* sentinel */
	};

        octx = g_option_context_new ("- mu guile options");
	g_option_context_add_main_entries (octx, entries, "mu guile");

	if (!g_option_context_parse (octx, argcp, argvp, NULL)) {
		g_option_context_free (octx);
		g_printerr ("mu guile: error in options\n");
		return NULL;
	}

	if (opts->msgpath)
		opts->msgpath = mu_util_dir_expand (opts->msgpath);

	g_option_context_free (octx);

	return opts;
}

static void
guile_config_destroy (GuileConfig *conf)
{
	g_free (conf->msgpath);
	g_free (conf);
}



gboolean
mu_guile_util_run (int *argcp, char **argvp[])
{
	GuileConfig *opts;

#ifdef HAVE_PRE2_GUILE
	g_warning ("Note: mu guile will not function properly unless you are using a"
		   "UTF-8 locale.");
#endif /* HAVE_PRE2_GUILE */

	opts = guile_config_new (argcp, argvp);
	if (!opts)
		goto error;

	if (!mu_runtime_init (opts->muhome /* NULL is okay */,
			      "mu-guile"))
		goto error;

	/* FIXME: mu_guile_init (); /\* initialize mu guile modules *\/ */

	if (opts->msgpath) {
		if (!(gboolean)scm_with_guile
		    ((MuGuileFunc*)&mu_guile_msg_load_current,
		     opts->msgpath))
			goto error;
	}

	scm_shell (*argcp, *argvp);

	mu_runtime_uninit ();
	guile_config_destroy (opts);

	return TRUE;
error:
	guile_config_destroy (opts);
	return FALSE;
}



/*
 * backward compat for pre-2.x guile - note, this will fail miserably
 * if you don't use a UTF8 locale
 */
#if HAVE_PRE2_GUILE

SCM
scm_from_utf8_string (const char* str)
{
	return scm_from_locale_string (str);
}

char*
scm_to_utf8_string (SCM scm)
{
	return scm_to_locale_string (scm);
}
#endif /*HAVE_PRE2_GUILE*/
