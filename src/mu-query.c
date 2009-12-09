/* 
** Copyright (C) 2008 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 3 of the License, or
** (at your option) any later version.
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

#include <glib.h>
#include <glib/gstdio.h>
#include <stdio.h>
#include <string.h>

#include "mu-util.h"
#include "mu-result.h"
#include "mu-msg-str.h"
#include "mu-msg-flags.h"
#include "mu-query-xapian.h"
#include "mu-query.h"

static gboolean
print_query (MuQueryXapian *xapian, const gchar *query)
{
	char *querystr;
	
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
	
	if (fieldstr)
		return NULL;
	
	field = mu_msg_field_from_name (fieldstr);
	if (!field && strlen(fieldstr) == 1)
		field = mu_msg_field_from_shortcut(fieldstr[0]);
	if (!field)
		g_printerr ("not a valid sort field: '%s'\n",
			    fieldstr);
	return field;
}


/* FIXME */
static gboolean
handle_options (MuConfigOptions *opts)
{
	if (opts->ascending_flag && opts->descending_flag) {
		g_printerr ("ignoring option '--descending'\n");
		opts->sortdir_ascending = TRUE;
	} else if (!opts->descending_flag)
		opts->sortdir_ascending = !opts->descending_flag;
	
	return TRUE;
}


static gboolean
print_rows (MuQueryXapian *xapian, const gchar *query, MuConfigOptions *opts)
{
	MuMsgXapian		*row;
	const MuMsgField	*sortfield;

	sortfield	  = NULL;
	if (opts->sortfield_str) {
		sortfield = sort_field_from_string (opts->sortfield_str);
		if (!sortfield) /* error occured? */
			return FALSE;
	}
	
	row = mu_query_xapian_run (xapian, query,
				   sortfield,
				   opts->sortdir_ascending);
	if (!row) {
		g_printerr ("error: running query failed\n");
		return FALSE;
	}

	/* iterate over the found rows */
	while (!mu_msg_xapian_is_done (row)) {
	 	const char*	fields		= opts->fields;
		int		printlen	= 0;
		while (*fields) {
			const MuMsgField* field = 
				mu_msg_field_from_shortcut (*fields);
			if (!field || 
			    !mu_msg_field_is_xapian_enabled (field)) 
				printlen += printf ("%c", *fields);
			else
				printlen += printf ("%s",
						    display_field(row, field));
			++fields;
		}	
		if (printlen >0)
			printf ("\n");
		
		mu_msg_xapian_next (row);
	}
	
	mu_msg_xapian_destroy (row);
	return TRUE;
}


static gboolean
do_output (MuQueryXapian *xapian, GSList *args, MuConfigOptions* opts)
{
	gchar *query;
	gboolean retval = TRUE;
	
	query = mu_query_xapian_combine (args, FALSE);
	
	/* if xquery is set, we print the xapian query instead of the
	 * output; this is for debugging purposes */
	if (opts->xquery) 
		retval = print_query (xapian, query);
	else
		retval = print_rows (xapian, query, opts);
	
	g_free (query);

	return retval;
}



MuResult
mu_query_run (MuConfigOptions *opts, GSList *args)
{
	MuQueryXapian *xapian;
	MuResult rv;
	
	rv = MU_OK;

	handle_options (opts);
	
	xapian = mu_query_xapian_new (opts->muhome);
	if (!xapian)
		return MU_ERROR;

	rv = do_output (xapian, args, opts) ? 0 : 1;
	
	mu_query_xapian_destroy (xapian);
	
	return rv;
}
