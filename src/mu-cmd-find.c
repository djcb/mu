/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

/*
** Copyright (C) 2008-2011 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include "mu-cmd.h"
#include "mu-output.h"


enum _OutputFormat {
	FORMAT_JSON,
	FORMAT_LINKS,
	FORMAT_PLAIN,
	FORMAT_SEXP,
	FORMAT_XML,
	FORMAT_XQUERY,

	FORMAT_NONE
};
typedef enum _OutputFormat OutputFormat;

static OutputFormat
get_output_format (const char *formatstr)
{
	int i;
	struct {
		const char*	name;
		OutputFormat	format;
	} formats [] = {
		{MU_CONFIG_FORMAT_JSON,		 FORMAT_JSON},
		{MU_CONFIG_FORMAT_LINKS,	 FORMAT_LINKS},
		{MU_CONFIG_FORMAT_PLAIN,	 FORMAT_PLAIN},
		{MU_CONFIG_FORMAT_SEXP,		 FORMAT_SEXP},
		{MU_CONFIG_FORMAT_XML,		 FORMAT_XML},
		{MU_CONFIG_FORMAT_XQUERY,	 FORMAT_XQUERY}
	};

	for (i = 0; i != G_N_ELEMENTS(formats); i++)
		if (strcmp (formats[i].name, formatstr) == 0)
			return formats[i].format;

	return FORMAT_NONE;
}


static void
upgrade_warning (void)
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
	
	err = NULL;
	querystr = mu_query_as_string (xapian, query, &err);
	if (!querystr) {
		g_warning ("error: %s", err->message);
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
		g_warning ("not a valid sort field: '%s'\n",
			   fieldstr);
	return mfid;
}


static gboolean
run_query_format (MuMsgIter *iter, MuConfig *opts,
		  OutputFormat format, size_t *count)
{
	switch (format) {

	case FORMAT_LINKS:
		return mu_output_links (iter, opts->linksdir, opts->clearlinks,
					count);
	case FORMAT_PLAIN: 
		return mu_output_plain (iter, opts->fields, opts->summary,
					count);
	case FORMAT_XML:
		return mu_output_xml (iter, count);
	case FORMAT_JSON:
		return mu_output_json (iter, count);
	case FORMAT_SEXP:
		return mu_output_sexp (iter, count);	
	default:
		g_assert_not_reached ();
		return FALSE;
	}
}


static gboolean
run_query (MuQuery *xapian, const gchar *query, MuConfig *opts,
	   OutputFormat format, size_t *count)
{
	GError *err;
	MuMsgIter *iter;
	MuMsgFieldId sortid;
	gboolean rv;
	
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
		g_warning ("error: %s", err->message);
		g_error_free (err);
		return FALSE;
	}

	rv = run_query_format (iter, opts, format, count);
		
	if (rv && count && *count == 0)
		g_warning ("no matching messages found");
	
	mu_msg_iter_destroy (iter);

	return rv;
}



static gboolean
format_params_valid (MuConfig *opts)
{
	OutputFormat format;
		
	format = get_output_format (opts->formatstr);
	if (format == FORMAT_NONE) {
		g_warning ("invalid output format %s",
			   opts->formatstr ? opts->formatstr : "<none>");
		return FALSE;
	}
	
	if (format == FORMAT_LINKS && !opts->linksdir) {
		g_warning ("missing --linksdir argument");
		return FALSE;
	}

	if (opts->linksdir && format != FORMAT_LINKS) {
		g_warning ("--linksdir is only valid with --format=links");
		return FALSE;
	}

	return TRUE;
}

static gboolean
query_params_valid (MuConfig *opts)
{
	const gchar *xpath;
	
	xpath = mu_runtime_path (MU_RUNTIME_PATH_XAPIANDB);
	
	if (mu_util_check_dir (xpath, TRUE, FALSE))
		return TRUE;
	
	g_warning ("'%s' is not a readable Xapian directory\n", xpath);
	g_message ("did you run 'mu index'?");
	
	return FALSE;
}

static gchar*
resolve_bookmark (MuConfig *opts)
{
	MuBookmarks *bm;
	char* val;
	const gchar *bmfile;
	
	bmfile = mu_runtime_path (MU_RUNTIME_PATH_BOOKMARKS);
	bm = mu_bookmarks_new (bmfile);
	if (!bm) {
		g_warning ("failed to open bookmarks file '%s'", bmfile);
		return FALSE;
	}
	
	val = (gchar*)mu_bookmarks_lookup (bm, opts->bookmark); 
	if (!val) 
		g_warning ("bookmark '%s' not found", opts->bookmark);
	else
		val = g_strdup (val);
	
	mu_bookmarks_destroy (bm);

	return val;
}


static gchar*
get_query (MuConfig *opts)
{
	gchar *query, *bookmarkval;

	/* params[0] is 'find', actual search params start with [1] */
	if (!opts->bookmark && !opts->params[1]) {
		g_warning ("usage: mu find [options] search-expression");
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
	if (mu_util_xapian_is_empty (xpath)) {
		g_warning ("database is empty; use 'mu index' to "
			   "add messages");
		return FALSE;
	}
		
	if (mu_util_xapian_needs_upgrade (xpath)) {
		upgrade_warning ();
		return FALSE;
	}

	return TRUE;
}

static MuQuery*
get_query_obj (void)
{
	GError *err;
	const char* xpath;
	MuQuery *mquery;
	
	xpath = mu_runtime_path (MU_RUNTIME_PATH_XAPIANDB);
	if (!db_is_ready(xpath)) {
		g_warning ("database '%s' is not ready", xpath);
		return NULL;
	}
		
	err = NULL;
	mquery = mu_query_new (xpath, &err);
	if (!mquery) {
		g_warning ("error: %s", err->message);
		g_error_free (err);
		return NULL;
	}

	return mquery;
}
	

MuExitCode
mu_cmd_find (MuConfig *opts)
{
	MuQuery *xapian;
	gboolean rv;
	gchar *query;
	size_t count;
	OutputFormat format;
	
	g_return_val_if_fail (opts, FALSE);
	g_return_val_if_fail (opts->cmd == MU_CONFIG_CMD_FIND, FALSE);
	
	if (!query_params_valid (opts) || !format_params_valid(opts))
		return MU_EXITCODE_ERROR;

	format = get_output_format (opts->formatstr);
	
	xapian = get_query_obj ();
	if (!xapian)
		return MU_EXITCODE_ERROR;
	
	/* first param is 'query', search params are after that */
	query = get_query (opts);
	if (!query) 
		return MU_EXITCODE_ERROR;
	
	if (format == FORMAT_XQUERY) 
		rv = print_xapian_query (xapian, query);
	else
		rv = run_query (xapian, query, opts, format, &count);

	mu_query_destroy (xapian);
	g_free (query);

	if (!rv)
		return MU_EXITCODE_ERROR;
	else
		return (count == 0) ?
			MU_EXITCODE_NO_MATCHES : MU_EXITCODE_OK;
}
