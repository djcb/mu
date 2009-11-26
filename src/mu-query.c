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

struct _FindOptions {
	gboolean		_print;
	gboolean		_xquery;
	char*                   _fields;

	char*			_sortfield_str;
	const MuMsgField*	_sortfield;

	gboolean		_ascending_flag, _descending_flag;
	gboolean		_sortdir_ascending;

};
typedef struct _FindOptions FindOptions;

static FindOptions FIND_OPTIONS;
static GOptionEntry OPTION_ENTRIES[] = {
	{"print", 'p', 0, G_OPTION_ARG_NONE, &FIND_OPTIONS._print,
	 "print matching messages to screen (default)", NULL},
	{"xquery", 'x', 0, G_OPTION_ARG_NONE, &FIND_OPTIONS._xquery,
	 "print a string representation of the Xapian query", NULL},
	{"fields", 'f', 0, G_OPTION_ARG_STRING, &FIND_OPTIONS._fields,
	 "fields to display in output", NULL},
	{"sortfield", 's', 0, G_OPTION_ARG_STRING, &FIND_OPTIONS._sortfield_str,
	 "field to sort on", NULL},
	{"ascending", 'a', 0, G_OPTION_ARG_NONE, &FIND_OPTIONS._ascending_flag,
	 "sort ascending", NULL},
	{"descending", 'c', 0, G_OPTION_ARG_NONE, &FIND_OPTIONS._descending_flag,
	 "sort ascending", NULL},
	{NULL}
};

GOptionGroup*
mu_query_option_group (void)
{
	GOptionGroup *og;

	og = g_option_group_new ("query",
				 "options for quering the e-mail database",
				 "", NULL, NULL);
	g_option_group_add_entries (og, OPTION_ENTRIES);
	
	return og;
}


static gboolean
handle_options_sort_field_dir (void)
{
	const MuMsgField *field;
	
	if (FIND_OPTIONS._sortfield_str) {
		field = mu_msg_field_from_name (FIND_OPTIONS._sortfield_str);
		if (!field && strlen(FIND_OPTIONS._sortfield_str) == 1)
			field = mu_msg_field_from_shortcut
				(FIND_OPTIONS._sortfield_str[0]);
		if (!field) {
			g_warning ("not a valid sort field: '%s'",
				   FIND_OPTIONS._sortfield_str);
			return FALSE;
		}
		FIND_OPTIONS._sortfield = field;
	}
	
	if (FIND_OPTIONS._ascending_flag && FIND_OPTIONS._descending_flag) {
		g_warning ("ignoring option '--descending'");
		FIND_OPTIONS._sortdir_ascending = TRUE;
	} else if (!FIND_OPTIONS._descending_flag)
		FIND_OPTIONS._sortdir_ascending = !FIND_OPTIONS._descending_flag;
	
	return TRUE;
}


static gboolean
handle_options (void)
{
	//GError *err = NULL;
	/* if (!mu_conf_handle_options (mu_app_conf(),OPTION_ENTRIES, argcp, argvp,  */
	/* 			     &err)) { */
	/* 	g_printerr ("option parsing failed: %s\n",  */
	/* 		    (err && err->message) ? err->message : "?" ); */
	/* 	if (err) */
	/* 		g_error_free (err); */
	/* 	return FALSE; */
	/* } */
	
	/* if nothing specified, or fields are specified use print */
	if ((!FIND_OPTIONS._xquery)||FIND_OPTIONS._fields)
		FIND_OPTIONS._print = TRUE;

	 /* if no fields are specified, use 'd f s' */
	if (FIND_OPTIONS._print && !FIND_OPTIONS._fields) {
		FIND_OPTIONS._fields = "d f s"; /* default: date-from-subject.. */
		if (!FIND_OPTIONS._ascending_flag) /* ... and sort descending */
			FIND_OPTIONS._sortdir_ascending = FALSE;
	}
	
	if (!handle_options_sort_field_dir ())
		return FALSE;
	
	return TRUE;
}

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


static gboolean
print_rows (MuQueryXapian *xapian, const gchar *query, FindOptions *opts)
{
	MuMsgXapian *row;
	
	row = mu_query_xapian_run (xapian, query,
				   opts->_sortfield, opts->_sortdir_ascending);
	if (!row) {
		g_printerr ("error: running query failed\n");
		return FALSE;
	}
	
	while (!mu_msg_xapian_is_done (row)) {
		const char* fields = opts->_fields;
		int printlen = 0;
		while (*fields) {
			const MuMsgField* field = 
				mu_msg_field_from_shortcut (*fields);
			if (!field) 
				printlen += printf ("%c", *fields);
			else
				printlen += printf ("%s", display_field(row, 
									field)); 
			
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
do_output (MuQueryXapian *xapian, GSList *args, FindOptions* opts)
{
	gchar *query;
	gboolean retval = TRUE;
	
	query = mu_query_xapian_combine (args, FALSE); 
	if (opts->_xquery)
		retval = print_query (xapian, query);
	
	if (retval && opts->_print) 
		retval = print_rows (xapian, query, opts);
	
	g_free (query);
	return retval;
}



MuResult
mu_query_run (GSList *args)
{
	GError *err = 0;
	MuQueryXapian *xapian;
	MuResult rv;
	
	rv = MU_OK;

	handle_options ();
	xapian = mu_query_xapian_new ("/home/djcb/.mu", &err);

	if (!xapian) {
		if (err) {
			g_printerr ("error: %s\n", err->message);
			g_error_free (err);
		}
		return MU_ERROR;
	}
	
	rv = do_output (xapian, args, &FIND_OPTIONS) ? 0 : 1;
	mu_query_xapian_destroy (xapian);
	
	return rv;
}
