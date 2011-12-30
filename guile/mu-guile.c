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
#include <mu-store.h>
#include <mu-query.h>

#include "mu-guile-util.h"
#include "mu-guile-msg.h"

struct _MuData {
	MuQuery *_query;
};
typedef struct _MuData MuData;

static MuData *MU_DATA = NULL;

static gboolean
init_mu (const char *muhome)
{
	MuStore *store;
	MuQuery *query;
	GError *err;

	g_return_val_if_fail (!MU_DATA, FALSE);

	if (!mu_runtime_init (muhome, "guile"))
		return FALSE;

	store = mu_store_new_read_only (mu_runtime_path(MU_RUNTIME_PATH_XAPIANDB),
					&err);
	if (!store) {
		mu_guile_util_g_error (__FUNCTION__, err);
		g_clear_error (&err);
		return FALSE;
	}

	query = mu_query_new (store, &err);
	mu_store_unref (store);
	if (!query) {
		mu_guile_util_g_error (__FUNCTION__, err);
		g_clear_error (&err);
		return FALSE;
	}

	MU_DATA = g_new0 (MuData, 1);
	MU_DATA->_query = query;

	return TRUE;
}

static void
uninit_mu (void)
{
	g_return_if_fail (MU_DATA);

	mu_query_destroy (MU_DATA->_query);
	g_free (MU_DATA);

	MU_DATA = NULL;

	mu_runtime_uninit ();
}


SCM_DEFINE_PUBLIC (mu_initialize, "initialize-mu", 0, 1, 0,
		   (SCM MUHOME),
		   "Initialize mu - needed before you call any of the other "
		   "functions. Optionally, you can provide MUHOME which "
		   "should be an absolute path to your mu home directory "
		   "(typically, the default, ~/.mu, should be just fine)")
#define FUNC_NAME s_mu_initialize
{
	const char *muhome;

	SCM_ASSERT (scm_is_string (MUHOME) || SCM_UNBNDP(MUHOME),
		    MUHOME, SCM_ARG1, FUNC_NAME);

	if (MU_DATA)
		return mu_guile_util_error (FUNC_NAME, 0, "Already initialized",
					    SCM_BOOL_F);

	muhome = SCM_UNBNDP(MUHOME) ? NULL : scm_to_utf8_string (MUHOME);

	if (!init_mu (muhome))
		return mu_guile_util_error (FUNC_NAME, 0, "Failed to initialize mu",
					    SCM_BOOL_F);
	/* cleanup when we're exiting */
	g_atexit (uninit_mu);

	return SCM_BOOL_T;
}
#undef FUNC_NAME


SCM_DEFINE_PUBLIC (mu_initialized_p, "initialized-mu?", 0, 0, 0,
		   (void), "Whether mu is initialized or not.\n")
#define FUNC_NAME s_mu_initialized_p
{
	return MU_DATA ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME


static MuMsgIter*
get_query_iter (MuQuery *query, const char* expr)
{
	MuMsgIter *iter;
	GError *err;

	err = NULL;
	iter = mu_query_run (query, expr,
			     FALSE, MU_MSG_FIELD_ID_NONE, TRUE, -1, &err);
	if (!iter) {
		mu_guile_util_g_error ("<internal error>", err);
		g_clear_error (&err);
	}

	return iter;
}


static void
call_func (SCM FUNC, MuMsgIter *iter, const char* func_name)
{
	SCM msgsmob;
	MuMsg *msg;

	msg = mu_msg_iter_get_msg_floating (iter); /* don't unref */

	msgsmob = mu_guile_msg_to_scm (mu_msg_ref(msg));
	scm_call_1 (FUNC, msgsmob);

}


SCM_DEFINE_PUBLIC (for_each_message, "for-each-message", 1, 1, 0,
		   (SCM FUNC, SCM EXPR),
		   "Call FUNC for each message in the message store. If search expression EXPR "
		   "is specified, limit this to messages matching EXPR\n")
#define FUNC_NAME s_for_each_message
{
	MuMsgIter *iter;
	int count;
	const char* expr;

	SCM_ASSERT (scm_procedure_p (FUNC), FUNC, SCM_ARG1, FUNC_NAME);
	SCM_ASSERT (SCM_UNBNDP(EXPR) || scm_is_string (EXPR),
		    EXPR, SCM_ARG2, FUNC_NAME);

	if (!MU_DATA)
		return mu_guile_util_error (FUNC_NAME, 0, "mu not initialized",
					    SCM_UNDEFINED);

	/* note, "" matches *all* messages */
	expr = SCM_UNBNDP(EXPR) ? "" : scm_to_utf8_string(EXPR);

	iter = get_query_iter (MU_DATA->_query, expr);
	if (!iter)
		return SCM_UNSPECIFIED;

	for (count = 0; !mu_msg_iter_is_done(iter); mu_msg_iter_next (iter)) {
		call_func (FUNC, iter, FUNC_NAME);
		++count;
	}

	return scm_from_int (count);
}
#undef FUNC_NAME


enum _LogType {
	LOG_INFO,
	LOG_WARNING,
	LOG_CRITICAL
};
typedef enum _LogType LogType;


static SCM
write_log (LogType logtype, SCM FRM, SCM ARGS)
#define FUNC_NAME __FUNCTION__
{
	SCM str;

	SCM_ASSERT (scm_is_string(FRM), FRM, SCM_ARG1, "<write_log>");
	SCM_VALIDATE_REST_ARGUMENT(ARGS);

	str = scm_simple_format (SCM_BOOL_F, FRM, ARGS);

	if (scm_is_string (str)) {

		gchar *output;
		output = scm_to_utf8_string (str);

		switch (logtype) {
		case LOG_INFO:     g_message ("%s", output); break;
		case LOG_WARNING:  g_warning ("%s", output); break;
		case LOG_CRITICAL: g_critical ("%s", output); break;
		}
	}

	return SCM_UNSPECIFIED;

#undef FUNC_NAME
}


SCM_DEFINE_PUBLIC (log_info, "mu:log:info", 1, 0, 1,  (SCM FRM, SCM ARGS),
	    "log some message using a list of ARGS applied to FRM "
	    "(in 'simple-format' notation).\n")
#define FUNC_NAME s_info
{
	return write_log (LOG_INFO, FRM, ARGS);
}
#undef FUNC_NAME

SCM_DEFINE_PUBLIC (log_warning, "mu:log:warning", 1, 0, 1,  (SCM FRM, SCM ARGS),
	    "log some warning using a list of ARGS applied to FRM (in 'simple-format' "
	    "notation).\n")
#define FUNC_NAME s_warning
{
	return write_log (LOG_WARNING, FRM, ARGS);
}
#undef FUNC_NAME

SCM_DEFINE_PUBLIC (log_critical, "mu:log:critical", 1, 0, 1,  (SCM FRM, SCM ARGS),
	    "log some critical message using a list of ARGS applied to FRM "
	    "(in 'simple-format' notation).\n")
#define FUNC_NAME s_critical
{
	return write_log (LOG_CRITICAL, FRM, ARGS);
}
#undef FUNC_NAME



void*
mu_guile_init (void *data)
{
#include "mu-guile.x"

	return NULL;
}
