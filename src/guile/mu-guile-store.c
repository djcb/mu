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

#include <mu-query.h>
#include <mu-store.h>
#include <mu-runtime.h>

#include "mu-guile-msg.h"
#include "mu-guile-store.h"
#include "mu-guile-util.h"

static MuQuery*
get_query (void)
{
	MuQuery *query;
	MuStore *store;
	GError *err;

	err = NULL;
	store = mu_store_new_read_only (mu_runtime_path(MU_RUNTIME_PATH_XAPIANDB),
					&err);
	query = store ? mu_query_new (store, &err) : NULL;

	if (store)
		mu_store_unref (store);

	if (!query) {
		mu_guile_util_g_error ("<internal error>", err);
		g_clear_error (&err);
	}

	return query;
}


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


SCM_DEFINE_PUBLIC (store_foreach, "mu:store:for-each", 1, 1, 0,
	    (SCM FUNC, SCM EXPR),
	    "Call FUNC for each message in the store, or, if EXPR is specified, "
	    "for each message matching EXPR.\n")
#define FUNC_NAME s_store_foreach
{
	MuQuery *query;
	MuMsgIter *iter;
	int count;
	const char* expr;

	SCM_ASSERT (scm_procedure_p (FUNC), FUNC, SCM_ARG1, FUNC_NAME);
	SCM_ASSERT (scm_is_string (EXPR) || EXPR == SCM_UNDEFINED,
		    EXPR, SCM_ARG2, FUNC_NAME);

	query = get_query ();
	if (!query)
		return SCM_UNSPECIFIED;

	expr = SCM_UNBNDP(EXPR) ? NULL : scm_to_utf8_string(EXPR);

	iter = get_query_iter (query, expr);
	if (!iter)
		return SCM_UNSPECIFIED;

	for (count = 0; !mu_msg_iter_is_done(iter); mu_msg_iter_next (iter)) {
		call_func (FUNC, iter, FUNC_NAME);
		++count;
	}

	mu_query_destroy (query);

	return scm_from_int (count);
}
#undef FUNC_NAME


void*
mu_guile_store_init (void *data)
{
#include "mu-guile-store.x"

	return NULL;
}
