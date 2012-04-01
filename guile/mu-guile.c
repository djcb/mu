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

#include <locale.h>

#include <glib-object.h>
#include <libguile.h>

#include <mu-runtime.h>
#include <mu-query.h>
#include <mu-runtime.h>
#include <mu-store.h>
#include <mu-query.h>
#include <mu-msg.h>

#include "mu-guile.h"


SCM
mu_guile_scm_from_str (const char *str)
{
	if (!str)
		return SCM_BOOL_F;
	else
		return scm_from_stringn (str, strlen(str), "UTF-8",
					 SCM_FAILED_CONVERSION_QUESTION_MARK);
}


SCM
mu_guile_error (const char *func_name, int status,
		     const char *fmt, SCM args)
{
	scm_error_scm (scm_from_locale_symbol ("MuError"),
		       scm_from_utf8_string (func_name ? func_name : "<nameless>"),
		       scm_from_utf8_string (fmt), args,
		       scm_list_1 (scm_from_int (status)));

	return SCM_UNSPECIFIED;
}

SCM
mu_guile_g_error (const char *func_name, GError *err)
{
	scm_error_scm (scm_from_locale_symbol ("MuError"),
		       scm_from_utf8_string (func_name),
		       scm_from_utf8_string (err ? err->message : "error"),
		       SCM_UNDEFINED, SCM_UNDEFINED);

	return SCM_UNSPECIFIED;
}



/* there can be only one */

static MuGuile *_singleton = NULL;

static gboolean
mu_guile_init_instance (const char *muhome)
{
	MuStore *store;
	MuQuery *query;
	GError *err;

	setlocale (LC_ALL, "");

	if (!mu_runtime_init (muhome, "guile"))
		return FALSE;

	store = mu_store_new_read_only (mu_runtime_path(MU_RUNTIME_PATH_XAPIANDB),
					&err);
	if (!store) {
		mu_guile_g_error (__FUNCTION__, err);
		g_clear_error (&err);
		return FALSE;
	}

	query = mu_query_new (store, &err);
	mu_store_unref (store);
	if (!query) {
		mu_guile_g_error (__FUNCTION__, err);
		g_clear_error (&err);
		return FALSE;
	}

	_singleton        = g_new0 (MuGuile, 1);
	_singleton->query = query;

	return TRUE;
}

static void
mu_guile_uninit_instance (void)
{
	g_return_if_fail (_singleton);

	mu_query_destroy (_singleton->query);
	g_free (_singleton);

	_singleton = NULL;

	mu_runtime_uninit ();
}


MuGuile*
mu_guile_instance (void)
{
	g_return_val_if_fail (_singleton, NULL);
	return _singleton;
}

gboolean
mu_guile_initialized (void)
{
	return _singleton != NULL;
}



SCM_DEFINE_PUBLIC (mu_initialize, "mu:initialize", 0, 1, 0,
		   (SCM MUHOME),
		   "Initialize mu - needed before you call any of the other "
		   "functions. Optionally, you can provide MUHOME which should be an "
		   "absolute path to your mu home directory "
		   "-- typically, the default, ~/.mu, should be just fine\n.")
#define FUNC_NAME s_mu_initialize
{
	char *muhome;
	gboolean rv;

	SCM_ASSERT (scm_is_string (MUHOME) || MUHOME == SCM_BOOL_F || SCM_UNBNDP(MUHOME),
		    MUHOME, SCM_ARG1, FUNC_NAME);

	if (mu_guile_initialized())
		return mu_guile_error (FUNC_NAME, 0, "Already initialized",
				       SCM_UNSPECIFIED);

	if (SCM_UNBNDP(MUHOME) || MUHOME == SCM_BOOL_F)
		muhome = NULL;
	else
		muhome =  scm_to_utf8_string (MUHOME);

	rv = mu_guile_init_instance(muhome);
	free (muhome);

	if (!rv)
		return mu_guile_error (FUNC_NAME, 0, "Failed to initialize mu",
				       SCM_UNSPECIFIED);

	/* cleanup when we're exiting */
	atexit (mu_guile_uninit_instance);

	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE_PUBLIC (mu_initialized_p, "mu:initialized?", 0, 0, 0,
		   (void), "Whether mu is initialized or not.\n")
#define FUNC_NAME s_mu_initialized_p
{
	return mu_guile_initialized() ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME



static SCM
write_log (GLogLevelFlags level, SCM FRM, SCM ARGS)
#define FUNC_NAME __FUNCTION__
{
	SCM str;

	SCM_ASSERT (scm_is_string(FRM), FRM, SCM_ARG1, "<write_log>");
	SCM_VALIDATE_REST_ARGUMENT(ARGS);

	str = scm_simple_format (SCM_BOOL_F, FRM, ARGS);

	if (scm_is_string (str)) {

		gchar *output;
		output = scm_to_utf8_string (str);
		g_log (G_LOG_DOMAIN, level, "%s", output);
		free (output);
	}

	return SCM_UNSPECIFIED;

#undef FUNC_NAME
}


SCM_DEFINE_PUBLIC (log_info, "mu:log-info", 1, 0, 1,  (SCM FRM, SCM ARGS),
	    "log some message using a list of ARGS applied to FRM "
	    "(in 'simple-format' notation).\n")
#define FUNC_NAME s_info
{
	return write_log (G_LOG_LEVEL_INFO, FRM, ARGS);
}
#undef FUNC_NAME

SCM_DEFINE_PUBLIC (log_warning, "mu:log-warning", 1, 0, 1,  (SCM FRM, SCM ARGS),
	    "log some warning using a list of ARGS applied to FRM (in 'simple-format' "
	    "notation).\n")
#define FUNC_NAME s_warning
{
	return write_log (G_LOG_LEVEL_WARNING, FRM, ARGS);
}
#undef FUNC_NAME

SCM_DEFINE_PUBLIC (log_critical, "mu:log-critical", 1, 0, 1,  (SCM FRM, SCM ARGS),
	    "log some critical message using a list of ARGS applied to FRM "
	    "(in 'simple-format' notation).\n")
#define FUNC_NAME s_critical
{
	return write_log (G_LOG_LEVEL_CRITICAL, FRM, ARGS);
}
#undef FUNC_NAME


void*
mu_guile_init (void *data)
{
#include "mu-guile.x"
	return NULL;
}
