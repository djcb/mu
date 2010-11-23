/*
** Copyright (C) 2008-2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <signal.h>

#include "mu-msg.h"
#include "mu-maildir.h"
#include "mu-index.h"
#include "mu-query.h"
#include "mu-msg-iter.h"
#include "mu-bookmarks.h"
#include "mu-runtime.h"

#include "mu-util.h"
#include "mu-util-db.h"
#include "mu-cmd.h"
#include "mu-output-plain.h"
#include "mu-output-link.h"


static void
update_warning (void)
{
	g_warning ("the database needs to be updated to version %s\n",
		   MU_XAPIAN_DB_VERSION);
	g_message ("please run 'mu index --rebuild' (see the man page)");
}



static gboolean
print_xapian_query (MuQuery *xapian, const gchar *query)
{
	char *querystr;
	GError *err;
	
	MU_WRITE_LOG ("query: '%s' (xquery)", query); 

	err = NULL;
	querystr = mu_query_as_string (xapian, query, &err);
	if (!querystr) {
		g_warning ("Error: %s", err->message);
		g_error_free (err);
		return FALSE;
	} 

	g_print ("%s\n", querystr);
	g_free (querystr);

	return TRUE;
}

/* returns NULL if there is an error */
static MuMsgFieldId
sort_field_from_string (const char* fieldstr)
{
	MuMsgFieldId mfid;
		
	mfid = mu_msg_field_id_from_name (fieldstr, FALSE);

	/* not found? try a shortcut */
	if (mfid == MU_MSG_FIELD_ID_NONE &&
	    strlen(fieldstr) == 1)
		mfid = mu_msg_field_id_from_shortcut(fieldstr[0],
						     FALSE);	
	if (mfid == MU_MSG_FIELD_ID_NONE)
		g_warning ("Not a valid sort field: '%s'\n",
			   fieldstr);
	return mfid;
}


static size_t
print_rows (MuMsgIter *iter, const char *fields, size_t summary_len)
{
	size_t count = 0;

	if (mu_msg_iter_is_done (iter))
		return 0;
	
	do {
		if (mu_output_plain_row (iter, fields, summary_len))
			++count;
		
	} while (mu_msg_iter_next (iter));
	
	return count;
}


static size_t
make_links (MuMsgIter *iter, const char* linksdir, gboolean clearlinks)
{
	size_t count = 0;

	if (!mu_output_link_create_dir (linksdir, clearlinks))
		return 0;

	if (mu_msg_iter_is_done (iter))
		return 0;
	
	/* iterate over the found iters */
	do {
		/* ignore errors...*/
		if (mu_output_link_row (iter, linksdir))
			++count;

	} while (mu_msg_iter_next (iter));
		 
	return count;
}



static gboolean
run_query (MuQuery *xapian, const gchar *query, MuConfigOptions *opts)
{
	GError *err;
	MuMsgIter *iter;
	MuMsgFieldId sortid;
	size_t matches;
	
	sortid = MU_MSG_FIELD_ID_NONE;
	if (opts->sortfield) {
		sortid = sort_field_from_string (opts->sortfield);
		if (sortid == MU_MSG_FIELD_ID_NONE) /* error occured? */
			return FALSE;
	}

	err  = NULL;
	iter = mu_query_run (xapian, query, sortid,
			     opts->descending ? FALSE : TRUE, 0, &err);
	if (!iter) {
		g_warning ("Error: %s", err->message);
		g_error_free (err);
		return FALSE;
	}

	if (opts->linksdir)
		matches = make_links (iter, opts->linksdir,
				      opts->clearlinks);
	else
		matches = print_rows (iter, opts->fields,
				      opts->summary_len);
	
	if (matches == 0) 
		g_printerr ("No matches found\n");

	mu_msg_iter_destroy (iter);

	return matches > 0;
}


static gboolean
query_params_valid (MuConfigOptions *opts)
{
	const gchar *xpath;
	
	if (opts->linksdir) 
		if (opts->xquery) {
			g_printerr ("Invalid option for '--linksdir'\n");
			return FALSE;
		}

	xpath = mu_runtime_xapian_dir();
	
	if (mu_util_check_dir (xpath, TRUE, FALSE))
		return TRUE;
	
	g_warning ("'%s' is not a readable Xapian directory\n", xpath);
	g_message ("Did you run 'mu index'?");
	
	return FALSE;
}

static gchar*
resolve_bookmark (MuConfigOptions *opts)
{
	MuBookmarks *bm;
	char* val;
	const gchar *bmfile;
	
	bmfile = mu_runtime_bookmarks_file();
	bm = mu_bookmarks_new (bmfile);
	if (!bm) {
		g_warning ("Failed to open bookmarks file '%s'", bmfile);
		return FALSE;
	}
	
	val = (gchar*)mu_bookmarks_lookup (bm, opts->bookmark); 
	if (!val) 
		g_warning ("Bookmark '%s' not found", opts->bookmark);
	else
		val = g_strdup (val);
	
	mu_bookmarks_destroy (bm);

	return val;
}


static gchar*
get_query (MuConfigOptions *opts)
{
	gchar *query, *bookmarkval;

	/* params[0] is 'find', actual search params start with [1] */
	if (!opts->bookmark && !opts->params[1]) {
		g_warning ("Empty search query");
		return FALSE;
	}

	bookmarkval = NULL;
	if (opts->bookmark) {
		bookmarkval = resolve_bookmark (opts);
		if (!bookmarkval)
			return NULL;
	}
	
	query = mu_util_str_from_strv ((const gchar**)&opts->params[1]);
	if (bookmarkval) {
		gchar *tmp;
		tmp = g_strdup_printf ("%s %s", bookmarkval, query);
		g_free (query);
		query = tmp;
	}

	g_free (bookmarkval);
	
	return query;
}

static gboolean
db_is_ready (const char *xpath)
{	
	if (mu_util_db_is_empty (xpath)) {
		g_warning ("Database is empty; use 'mu index' to "
			   "add messages");
		return FALSE;
	}
		
	if (!mu_util_db_version_up_to_date (xpath)) {
		update_warning ();
		return FALSE;
	}

	return TRUE;
}


gboolean
mu_cmd_find (MuConfigOptions *opts)
{
	GError *err;
	MuQuery *xapian;
	gboolean rv;
	gchar *query;
	const gchar *xpath;
	
	g_return_val_if_fail (opts, FALSE);
	g_return_val_if_fail (mu_cmd_equals (opts, "find"), FALSE);
	
	if (!query_params_valid (opts))
		return FALSE;

	xpath = mu_runtime_xapian_dir ();
	if (!db_is_ready(xpath))
		return FALSE;
	
	/* first param is 'query', search params are after that */
	query = get_query (opts);
	if (!query) 
		return FALSE;

	err = NULL;
	xapian = mu_query_new (xpath, &err);
	if (!xapian) {
		g_warning ("Error: %s", err->message);
		g_error_free (err);
		return FALSE;
	}

	if (opts->xquery) 
		rv = print_xapian_query (xapian, query);
	else
		rv = run_query (xapian, query, opts);

	mu_query_destroy (xapian);
	g_free (query);
	
	return rv;
}

