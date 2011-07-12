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
#include "mu-guile-utils.h"

static MuQuery*
get_query (void)
{
	MuQuery *query;
	GError *err;
	
	err = NULL;
	query = mu_query_new (mu_runtime_path(MU_RUNTIME_PATH_XAPIANDB), &err);
	if (err) {
		g_warning ("error creating query: %s", err->message);
		g_error_free (err);
		return NULL;
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
			     FALSE, MU_MSG_FIELD_ID_NONE, TRUE, &err);
	if (err) {
		g_warning ("error running query: %s", err->message);
		g_error_free (err);
		return NULL;
	}
	
	return iter;
}


SCM_DEFINE (store_for_each, "mu:store:foreach", 2, 0, 0,
	    (SCM EXPR, SCM FUNC),
	    "Call FUNC for each message matching EXPR.\n")
#define FUNC_NAME s_msg_make_from_file
{
	MuQuery *query;
	MuMsgIter *iter;
	//guint count;

	query = get_query ();
	if (!query)
		return SCM_UNSPECIFIED;

	iter = get_query_iter (query, scm_to_utf8_string(EXPR));
	if (!iter)
		return SCM_UNSPECIFIED;
	
	while (!mu_msg_iter_is_done(iter)) {

		SCM msgsmob;
		
		msgsmob = mu_guile_msg_to_scm (mu_msg_iter_get_msg (iter, NULL));

		scm_call_1 (FUNC, msgsmob);
		
		mu_msg_iter_next (iter);
	}
	
	mu_query_destroy (query);

	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


void*
mu_guile_store_init (void *data)
{	
#include "mu-guile-store.x"

	return NULL;
}


