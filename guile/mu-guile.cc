/*
** Copyright (C) 2011-2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-guile.hh"

#include <locale.h>
#include <glib-object.h>

#include <mu-store.hh>
#include <mu-query.hh>

#include <utils/mu-utils-file.hh>

using namespace Mu;

SCM
mu_guile_scm_from_string(const std::string& str)
{
	if (str.empty())
		return SCM_BOOL_F;
	else
		return scm_from_stringn(str.c_str(), str.size(),
					"UTF-8",
					SCM_FAILED_CONVERSION_QUESTION_MARK);
}

SCM
mu_guile_error(const char* func_name, int status, const char* fmt, SCM args)
{
	scm_error_scm(scm_from_locale_symbol("MuError"),
		      scm_from_utf8_string(func_name ? func_name : "<nameless>"),
		      scm_from_utf8_string(fmt),
		      args,
		      scm_list_1(scm_from_int(status)));

	return SCM_UNSPECIFIED;
}

SCM
mu_guile_g_error(const char* func_name, GError* err)
{
	scm_error_scm(scm_from_locale_symbol("MuError"),
		      scm_from_utf8_string(func_name),
		      scm_from_utf8_string(err ? err->message : "error"),
		      SCM_UNDEFINED,
		      SCM_UNDEFINED);

	return SCM_UNSPECIFIED;
}

/* there can be only one */

static Option<Mu::Store> StoreSingleton = Nothing;

static bool
mu_guile_init_instance(const std::string& muhome) try {
	setlocale(LC_ALL, "");

	const auto path{runtime_path(RuntimePath::XapianDb, muhome)};
	auto store = Store::make(path);
	if (!store) {
		mu_critical("error creating store @ %s: %s", path,
			    store.error().what());
		throw store.error();
	} else
		StoreSingleton.emplace(std::move(store.value()));

	mu_debug("mu-guile: opened store @ {} (n={}); maildir: {}",
		StoreSingleton->path(),
		StoreSingleton->size(),
		StoreSingleton->root_maildir());

	return true;

} catch (const Xapian::Error& xerr) {
	mu_critical("{}: xapian error '{}'", __func__, xerr.get_msg());
	return false;
} catch (const std::runtime_error& re) {
	mu_critical("{}: error: {}", __func__, re.what());
	return false;
} catch (const std::exception& e) {
	mu_critical("{}: caught exception: {}", __func__, e.what());
	return false;
} catch (...) {
	mu_critical("{}: caught exception", __func__);
	return false;
}

static void
mu_guile_uninit_instance()
{
	StoreSingleton.reset();
}

Mu::Store&
mu_guile_store()
{
	if (!StoreSingleton)
		mu_error("mu guile not initialized");

	return StoreSingleton.value();
}

gboolean
mu_guile_initialized()
{
	g_debug("initialized ? %u", !!StoreSingleton);

	return !!StoreSingleton;
}

SCM_DEFINE_PUBLIC(mu_initialize,
		  "mu:initialize",
		  0,
		  1,
		  0,
		  (SCM MUHOME),
		  "Initialize mu - needed before you call any of the other "
		  "functions. Optionally, you can provide MUHOME which should be an "
		  "absolute path to your mu home directory "
		  "-- typically, the default, ~/.cache/mu, should be just fine.")
#define FUNC_NAME s_mu_initialize
{
	char*    muhome;

	SCM_ASSERT(scm_is_string(MUHOME) || MUHOME == SCM_BOOL_F || SCM_UNBNDP(MUHOME),
		   MUHOME,
		   SCM_ARG1,
		   FUNC_NAME);

	if (mu_guile_initialized())
		return mu_guile_error(FUNC_NAME, 0, "Already initialized", SCM_UNSPECIFIED);

	if (SCM_UNBNDP(MUHOME) || MUHOME == SCM_BOOL_F)
		muhome = NULL;
	else
		muhome = scm_to_utf8_string(MUHOME);

	if (!mu_guile_init_instance(muhome ? muhome : "")) {
		free(muhome);
		mu_guile_error(FUNC_NAME, 0, "Failed to initialize mu", SCM_UNSPECIFIED);
		return SCM_UNSPECIFIED;
	}

	g_debug("mu-guile: initialized @ %s", muhome ? muhome : "<default>");
	free(muhome);

	/* cleanup when we're exiting */
	atexit(mu_guile_uninit_instance);

	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE_PUBLIC(mu_initialized_p,
		  "mu:initialized?",
		  0,
		  0,
		  0,
		  (void),
		  "Whether mu is initialized or not.\n")
#define FUNC_NAME s_mu_initialized_p
{
	return mu_guile_initialized() ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE(log_func,
	   "mu:c:log",
	   1,
	   0,
	   1,
	   (SCM LEVEL, SCM FRM, SCM ARGS),
	   "log some message at LEVEL using a list of ARGS applied to FRM"
	   "(in 'simple-format' notation).\n")
#define FUNC_NAME s_log_func
{
	gchar* output;
	SCM    str;
	int    level;

	SCM_ASSERT(scm_integer_p(LEVEL), LEVEL, SCM_ARG1, FUNC_NAME);
	SCM_ASSERT(scm_is_string(FRM), FRM, SCM_ARG2, "<write_log>");
	SCM_VALIDATE_REST_ARGUMENT(ARGS);

	level = scm_to_int(LEVEL);
	if (level != G_LOG_LEVEL_MESSAGE && level != G_LOG_LEVEL_WARNING &&
	    level != G_LOG_LEVEL_CRITICAL)
		return mu_guile_error(FUNC_NAME, 0, "invalid log level", SCM_UNSPECIFIED);

	str = scm_simple_format(SCM_BOOL_F, FRM, ARGS);

	if (!scm_is_string(str))
		return SCM_UNSPECIFIED;

	output = scm_to_utf8_string(str);
	g_log(G_LOG_DOMAIN, (GLogLevelFlags)level, "%s", output);
	free(output);

	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static struct {
	const char* name;
	unsigned    val;
} VAR_PAIRS[] = {

    {"mu:message", G_LOG_LEVEL_MESSAGE},
    {"mu:warning", G_LOG_LEVEL_WARNING},
    {"mu:critical", G_LOG_LEVEL_CRITICAL}};

static void
define_vars(void)
{
	unsigned u;
	for (u = 0; u != G_N_ELEMENTS(VAR_PAIRS); ++u) {
		scm_c_define(VAR_PAIRS[u].name, scm_from_uint(VAR_PAIRS[u].val));
		scm_c_export(VAR_PAIRS[u].name, NULL);
	}
}

void*
mu_guile_init(void* data)
{
	define_vars();

#ifndef SCM_MAGIC_SNARFER
#include "mu-guile.x"
#endif /*SCM_MAGIC_SNARFER*/

	return NULL;
}
