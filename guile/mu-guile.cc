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

#include "mu-guile.hh"

#include <config.h>
#include <locale.h>
#include <glib-object.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wredundant-decls"
#include <libguile.h>
#pragma GCC diagnostic pop

#include <mu-runtime.hh>
#include <mu-store.hh>
#include <mu-query.hh>
#include <mu-msg.h>

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

static std::unique_ptr<Mu::Query> QuerySingleton;

static gboolean
mu_guile_init_instance (const char *muhome) try
{
	setlocale (LC_ALL, "");
	if (!mu_runtime_init (muhome, "guile", FALSE))
		return FALSE;

        Mu::Store store{mu_runtime_path(MU_RUNTIME_PATH_XAPIANDB)};
        QuerySingleton = std::make_unique<Mu::Query>(store);

	return TRUE;

} catch (...) {
        return FALSE;
}

static void
mu_guile_uninit_instance ()
{
        QuerySingleton.reset();

	mu_runtime_uninit ();
}


Mu::Query&
mu_guile_query ()
{
        if (!QuerySingleton)
                g_error("mu guile not initialized");

        return *QuerySingleton.get();
}

gboolean
mu_guile_initialized ()
{
	return !!QuerySingleton;
}


SCM_DEFINE_PUBLIC (mu_initialize, "mu:initialize", 0, 1, 0,
		   (SCM MUHOME),
		   "Initialize mu - needed before you call any of the other "
		   "functions. Optionally, you can provide MUHOME which should be an "
		   "absolute path to your mu home directory "
		   "-- typically, the default, ~/.cache/mu, should be just fine.")
#define FUNC_NAME s_mu_initialize
{
	char *muhome;
	gboolean rv;

	SCM_ASSERT (scm_is_string (MUHOME) || MUHOME == SCM_BOOL_F ||
		    SCM_UNBNDP(MUHOME), MUHOME, SCM_ARG1, FUNC_NAME);

	if (mu_guile_initialized())
		return mu_guile_error (FUNC_NAME, 0, "Already initialized",
				       SCM_UNSPECIFIED);

	if (SCM_UNBNDP(MUHOME) || MUHOME == SCM_BOOL_F)
		muhome = NULL;
	else
		muhome =  scm_to_utf8_string (MUHOME);

	rv = mu_guile_init_instance (muhome);
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


SCM_DEFINE (log_func, "mu:c:log", 1, 0, 1, (SCM LEVEL, SCM FRM, SCM ARGS),
	    "log some message at LEVEL using a list of ARGS applied to FRM"
	    "(in 'simple-format' notation).\n")
#define FUNC_NAME s_log_func
{
	gchar *output;
	SCM str;
	int level;

	SCM_ASSERT (scm_integer_p(LEVEL), LEVEL, SCM_ARG1, FUNC_NAME);
	SCM_ASSERT (scm_is_string(FRM), FRM, SCM_ARG2, "<write_log>");
	SCM_VALIDATE_REST_ARGUMENT(ARGS);

	level = scm_to_int (LEVEL);
	if (level != G_LOG_LEVEL_MESSAGE &&
	    level != G_LOG_LEVEL_WARNING &&
	    level != G_LOG_LEVEL_CRITICAL)
		return mu_guile_error (FUNC_NAME, 0, "invalid log level",
				       SCM_UNSPECIFIED);

	str = scm_simple_format (SCM_BOOL_F, FRM, ARGS);

	if (!scm_is_string (str))
		return SCM_UNSPECIFIED;

	output = scm_to_utf8_string (str);
	g_log (G_LOG_DOMAIN, (GLogLevelFlags)level, "%s", output);
	free (output);

	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


static struct  {
	const char* name;
	unsigned val;
} VAR_PAIRS[] = {

	{ "mu:message",	 G_LOG_LEVEL_MESSAGE },
	{ "mu:warning",	 G_LOG_LEVEL_WARNING },
	{ "mu:critical", G_LOG_LEVEL_CRITICAL }
};

static void
define_vars (void)
{
	unsigned u;
	for (u = 0; u != G_N_ELEMENTS(VAR_PAIRS); ++u) {
		scm_c_define (VAR_PAIRS[u].name,
			      scm_from_uint (VAR_PAIRS[u].val));
		scm_c_export (VAR_PAIRS[u].name, NULL);
	}
}


void*
mu_guile_init (void *data)
{
	define_vars ();


#ifndef SCM_MAGIC_SNARFER
#include "mu-guile.x"
#endif /*SCM_MAGIC_SNARFER*/

	return NULL;
}
