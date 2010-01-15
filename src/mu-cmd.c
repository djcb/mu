/*
** Copyright (C) 2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "mu-msg-gmime.h"
#include "mu-maildir.h"
#include "mu-index.h"
#include "mu-query-xapian.h"
#include "mu-msg-xapian.h"
#include "mu-msg-str.h"
#include "mu-cmd.h"
#include "mu-util.h"

static MuCmd 
cmd_from_string (const char* cmd)
{
	if (!cmd)
		return MU_CMD_UNKNOWN;

	if (strcmp (cmd, "index") == 0)
		return MU_CMD_INDEX;

	/* support some synonyms... */
	if ((strcmp (cmd, "query") == 0) ||
	    (strcmp (cmd, "find")  == 0) ||
	    (strcmp (cmd, "search") == 0))
		return MU_CMD_FIND;

	if (strcmp (cmd, "cleanup") == 0)
		return MU_CMD_CLEANUP;
	
	if ((strcmp (cmd, "mkmdir") == 0) ||
	    (strcmp (cmd, "mkdir") == 0)) 
		return MU_CMD_MKDIR;
	
	/* if (strcmp (cmd, "link") == 0) */
	/* 	return MU_CMD_LINK; */
	
	/* if ((strcmp (cmd, "help") == 0) || */
	/*     (strcmp (cmd, "info") == 0)) */
	/* 	return MU_CMD_HELP; */
	
	return MU_CMD_UNKNOWN;
}


static gboolean
print_query (MuQueryXapian *xapian, const gchar *query)
{
	char *querystr;

	MU_WRITE_LOG ("query: '%s' (xquery)", query); 
	
	querystr = mu_query_xapian_as_string (xapian, query);
	g_print ("%s\n", querystr);
	g_free (querystr);

	return TRUE;
}


static const gchar*
display_field (MuMsgXapian *row, const MuMsgField* field)
{
	gint64 val;

	switch (mu_msg_field_type(field)) {
	case MU_MSG_FIELD_TYPE_STRING:
		return mu_msg_xapian_get_field (row, field);

	case MU_MSG_FIELD_TYPE_INT:
	
		if (mu_msg_field_id(field) == MU_MSG_FIELD_ID_PRIORITY) {
			val = mu_msg_xapian_get_field_numeric (row, field);
			return mu_msg_str_prio ((MuMsgPriority)val);
		}
		
		if (mu_msg_field_id(field) == MU_MSG_FIELD_ID_FLAGS) {
			val = mu_msg_xapian_get_field_numeric (row, field);
			return mu_msg_str_flags_s ((MuMsgPriority)val);
		}

		return mu_msg_xapian_get_field (row, field); /* as string */

	case MU_MSG_FIELD_TYPE_TIME_T: 
		val = mu_msg_xapian_get_field_numeric (row, field);
		return mu_msg_str_date_s ((time_t)val);

	case MU_MSG_FIELD_TYPE_BYTESIZE: 
		val = mu_msg_xapian_get_field_numeric (row, field);
		return mu_msg_str_size_s ((time_t)val);
	default:
		g_return_val_if_reached (NULL);
	}
}


/* returns NULL if there is an error */
const MuMsgField*
sort_field_from_string (const char* fieldstr)
{
	const MuMsgField *field;
		
	field = mu_msg_field_from_name (fieldstr);

	if (!field && strlen(fieldstr) == 1)
		field = mu_msg_field_from_shortcut(fieldstr[0]);

	if (!field)
		g_printerr ("not a valid sort field: '%s'\n",
			    fieldstr);
	return field;
}




static gboolean
print_rows (MuQueryXapian *xapian, const gchar *query, MuConfigOptions *opts)
{
	MuMsgXapian *row;
	const MuMsgField *sortfield;

	MU_WRITE_LOG ("query: '%s' (rows)", query); 
	
	sortfield = NULL;
	if (opts->sortfield) {
		sortfield = sort_field_from_string (opts->sortfield);
		if (!sortfield) /* error occured? */
			return FALSE;
	}
	
	row = mu_query_xapian_run (xapian, query, sortfield,
				   !opts->descending);
	if (!row) {
		g_printerr ("error: running query failed\n");
		return FALSE;
	} else if (mu_msg_xapian_is_done (row)) {
		g_printerr ("No matches found\n");
		mu_msg_xapian_destroy (row);
		return FALSE;
	}

	/* iterate over the found rows */
	do  {
	 	const char*	fields		= opts->fields;
		int		printlen	= 0;

		while (*fields) {
			
			const MuMsgField* field;
			field =	mu_msg_field_from_shortcut (*fields);
			if (!field || 
			    !mu_msg_field_is_xapian_enabled (field)) 
				printlen += printf ("%c", *fields);
			else
				printlen += printf ("%s",
						    display_field(row, field));
			++fields;
		}
		
		if (printlen > 0)
			printf ("\n");
		
		mu_msg_xapian_next (row);
		
	} while (!mu_msg_xapian_is_done (row));
	
	mu_msg_xapian_destroy (row);

	return TRUE;
}


static gboolean
do_output_text (MuQueryXapian *xapian, MuConfigOptions* opts,
		 const gchar **params)
{
	gchar *query;
	gboolean retval = TRUE;
	
	query = mu_query_xapian_combine (params, FALSE);
		
	/* if xquery is set, we print the xapian query instead of the
	 * output; this is for debugging purposes */
	if (opts->xquery) 
		retval = print_query (xapian, query);
	else
		retval = print_rows (xapian, query, opts);
	
	g_free (query);

	return retval;
}


/* create a linksdir if it not exist yet; if it already existed,
 * remove old links if opts->clearlinks was specified */
static gboolean
create_or_clear_linksdir_maybe (MuConfigOptions* opts)
{
	if (access (opts->linksdir, F_OK) != 0) {

		if (!mu_maildir_mkmdir (opts->linksdir, 0700, TRUE)) 
			return FALSE;

	} else if (opts->clearlinks)
		mu_maildir_clear_links (opts->linksdir);
	
	return TRUE;
}

static gboolean
do_output_links (MuQueryXapian *xapian, MuConfigOptions* opts,
		  const gchar **params)
{
	gchar *query;
	gboolean retval = TRUE;
	MuMsgXapian *row;
	const MuMsgField *pathfield;

	if (!create_or_clear_linksdir_maybe (opts))
		return FALSE;
	
	query = mu_query_xapian_combine (params, FALSE);
	
	MU_WRITE_LOG ("query: '%s' (links)", query); 
	row = mu_query_xapian_run (xapian, query, NULL, FALSE);
	if (!row) {
		g_printerr ("error: running query failed\n");
		return FALSE;
	} else if (mu_msg_xapian_is_done (row)) {
		g_printerr ("No matches found\n");
		mu_msg_xapian_destroy (row);
		return FALSE;
	}
	
	pathfield = mu_msg_field_from_id (MU_MSG_FIELD_ID_PATH);
	
	/* iterate over the found rows */
	for (; !mu_msg_xapian_is_done (row); mu_msg_xapian_next (row)) {

		const char *path;
		
		path = mu_msg_xapian_get_field (row, pathfield);
		if (!path)
			continue;
			
		/* this might happen  if the database is not up-to-date */
		if (access (path, R_OK) != 0) {
			g_warning ("Cannot read source message %s: %s",
				   path, strerror (errno));
			continue;
		} 
		
		if (!mu_maildir_link (path, opts->linksdir))
			break;
	}
	
	mu_msg_xapian_destroy (row);
	g_free (query);

	return retval;
}


static gboolean
query_params_valid (MuConfigOptions *opts)
{
	if (opts->linksdir) 
		if (opts->xquery) {
			g_warning ("Invalid option for '--linksdir'");
			return FALSE;
		}
		
	if (!opts->params[0] || !opts->params[1]) {
		g_warning ("Missing search expression");
		return FALSE;
	}

	if (mu_util_check_dir (opts->xpath, TRUE, FALSE))
		return TRUE;

	g_warning ("%s is not a readable Xapian directory", opts->xpath);
	g_message ("Did you run 'mu index'?");
	
	return FALSE;
}


gboolean
cmd_find (MuConfigOptions *opts)
{
	MuQueryXapian *xapian;
	gboolean rv;
	const gchar **params;
		
	if (!query_params_valid (opts))
		return FALSE;
	
	/* first param is 'query', search params are after that */
	params = (const gchar**)&opts->params[1];

	mu_msg_gmime_init();
	
	xapian = mu_query_xapian_new (opts->xpath);
	if (!xapian) {
		g_warning ("Failed to create Xapian query");
		mu_msg_gmime_uninit ();
		return FALSE;
	}

	if (opts->linksdir)
		rv = do_output_links (xapian, opts, params);
	else
		rv = do_output_text (xapian, opts, params);
	
	mu_query_xapian_destroy (xapian);
	mu_msg_gmime_uninit();
	
	return rv;
}


static gboolean
check_index_params (MuConfigOptions *opts)
{
	if (opts->linksdir || opts->xquery) {
		g_warning ("Invalid option(s) for command");
		return FALSE;
	}
	
	
	if (!mu_util_check_dir (opts->maildir, TRUE, TRUE)) {
		g_message ("Please provide a valid Maildir");
		return FALSE;
	}
	
	return TRUE;
}
	

static MuResult
index_msg_cb  (MuIndexStats* stats, void *user_data)
{
	char *kars="-\\|/";
	char output[314];
	
	static int i = 0;
	static int len = 0;

	while (len --> 0) /* note the --> operator :-) */
		printf ("\b");
	
	len = snprintf (output, sizeof(output),
			"%c indexing mail; processed: %d; "
			"updated/new: %d, cleaned-up: %d",
			kars[i % 4], stats->_processed,
			stats->_updated, stats->_cleaned_up);
	g_print ("%s", output);
	++i;
	
	return MU_OK;
}


static gboolean
cmd_index (MuConfigOptions *opts)
{
	int rv;

	if (!check_index_params (opts))
		return FALSE;

	mu_msg_gmime_init ();
	{
		MuIndex *midx;
		MuIndexStats stats;
		
		mu_index_stats_clear (&stats);
		midx = mu_index_new (opts->xpath);
		
		if (!midx) {
			g_warning ("Indexing failed");
			return FALSE;
		} 

		g_message ("Indexing messages from %s", opts->maildir);
		g_message ("Database: %s", opts->xpath);
		
		rv = mu_index_run (midx, opts->maildir,
				   opts->reindex, &stats,
				   opts->quiet ? NULL : index_msg_cb,
				   NULL, NULL);
		if (!opts->nocleanup) {
			stats._processed = 0; /* start over */
			mu_index_cleanup (midx, &stats,
					  opts->quiet ? NULL : index_msg_cb,
					  NULL);
		}

		if (!opts->quiet) {
			index_msg_cb (&stats, NULL);
			g_print ("\n");
		}

		MU_WRITE_LOG ("processed: %d; updated/new: %d, cleaned-up: %d",
			      stats._processed, stats._updated,
			      stats._cleaned_up);
		
		mu_index_destroy (midx);
	}
	mu_msg_gmime_uninit ();
	
	return rv == MU_OK ? TRUE : FALSE;
}


MuResult
cleanup_cb (MuIndexStats *stats, void *user_data)
{
	char *kars="-\\|/";
	char output[100];
	
	static int i = 0;
	static int len = 0;

	while (len --> 0) 
		printf ("\b");
	
	len = snprintf (output, sizeof(output),
			"%c mu is cleaning up the message database; "
			"processed: %d; cleaned-up: %d",
			kars[i % 4], stats->_processed, stats->_cleaned_up);
	g_print ("%s", output);
	++i;

	return MU_OK;
}




static int
cmd_mkdir (MuConfigOptions *opts)
{
	int i;
	
	if (!opts->params[0])
		return FALSE;  /* shouldn't happen */
 	
	if (!opts->params[1]) {
		g_printerr ("usage: mu mkdir <dir> [more dirs]\n");
		return FALSE;
	}
	
	i = 1;
	while (opts->params[i]) {
		MU_WRITE_LOG ("mu_maildir_mkdir (%s, 0755, FALSE)",
			     opts->params[i]);
		if (!mu_maildir_mkmdir (opts->params[i], 0755, FALSE))
			return FALSE;
		++i;
	}

	return TRUE;
}



#if 0 /* currently, turned off */
static gboolean
cmd_link (MuConfigOptions *opts)
{
	if (!opts->params[0])
		return FALSE;  /* shouldn't happen */
 	
	if (!opts->params[1] || !opts->params[2]) {
		g_printerr ("usage: mu link <src> <targetdir>\n");
		return FALSE;
	}

	return mu_maildir_link (opts->params[1], opts->params[2]);
}


static gboolean
cmd_help (MuConfigOptions *opts)
{
	/* FIXME: get context-sensitive help */
	_show_version ();
	return _show_usage (FALSE);
}

static gboolean
cmd_cleanup (MuConfigOptions *opts)
{
	int rv;
	
	if (!_check_index_params (opts))
		return FALSE;
	
	mu_msg_gmime_init ();
	{
		MuIndex *midx;
		MuIndexStats stats;
		
		mu_index_stats_clear (&stats);
		
		midx = mu_index_new (opts->xpath);
		if (!midx) {
			g_warning ("Cleanup failed");
			return FALSE;
		}
		
		g_message ("Cleaning up removed messages from %s",
			   opts->xpath);
		mu_index_cleanup (midx, &stats,
				  opts->quiet ? NULL :_cleanup_cb,
				  NULL);
		mu_index_destroy (midx);

		if (!opts->quiet) {
			_cleanup_cb (&stats, NULL);
			g_print ("\n");
		}
	}
	
	mu_msg_gmime_uninit ();
	
	return rv == MU_OK ? TRUE : FALSE;
}
#endif /* 0 */



static gboolean
show_usage (gboolean noerror)
{
	const char* usage=
		"usage: mu [options] command [parameters]\n"
		"\twhere command is one of index, find, cleanup, mkdir\n"
		"see mu(1) for more information\n";

	if (noerror)
		g_print ("%s", usage);
	else
		g_printerr ("%s", usage);

	return noerror;
}

static gboolean
show_version (void)
{
	const char* msg =
		"mu (mail indexer / searcher version) " VERSION "\n\n"
		"Copyright (C) 2010 Dirk-Jan C. Binnema\n"
		"License GPLv3+: GNU GPL version 3 or later "
		"<http://gnu.org/licenses/gpl.html>.\n\n"
		"This is free software: you are free to change "
		"and redistribute it.\n"
		"There is NO WARRANTY, to the extent permitted by law.";

	g_print ("%s\n", msg);

	return TRUE;
}


gboolean
mu_cmd_execute (MuConfigOptions *opts)
{
	MuCmd cmd;
	
	if (opts->version)
		return show_version ();
	
	if (!opts->params||!opts->params[0]) /* no command? */
		return show_usage (FALSE);
	
	cmd = cmd_from_string (opts->params[0]);

	switch (cmd) {

	case MU_CMD_INDEX:   return cmd_index (opts);
	case MU_CMD_FIND:    return cmd_find (opts);

	case MU_CMD_MKDIR:   return cmd_mkdir (opts);

		/* case MU_CMD_CLEANUP: return _cmd_cleanup (opts); */
		/* case MU_CMD_HELP:    return _cmd_help  (opts); */
		/* case MU_CMD_LINK:    return _cmd_link  (opts); */

	case MU_CMD_UNKNOWN: return show_usage (FALSE);
	default:
		g_return_val_if_reached (FALSE);
	}	
}
