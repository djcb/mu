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
#include "mu-str.h"
#include "mu-maildir.h"
#include "mu-index.h"
#include "mu-query.h"
#include "mu-msg-iter.h"
#include "mu-bookmarks.h"
#include "mu-runtime.h"

#include "mu-util.h"
#include "mu-cmd.h"

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

static gboolean output_links (MuMsgIter *iter, const char* linksdir,
				      gboolean clearlinks, size_t *count);
static gboolean output_sexp (MuMsgIter *iter, size_t *count);
static gboolean output_json (MuMsgIter *iter, size_t *count);
static gboolean output_xml (MuMsgIter *iter, size_t *count);
static gboolean output_plain (MuMsgIter *iter, const char *fields,
				      gboolean summary,gboolean color,
				      size_t *count);

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
print_xapian_query (MuQuery *xapian, const gchar *query, size_t *count)
{
	char *querystr;
	GError *err;
	
	err    = NULL;
	
	querystr = mu_query_as_string (xapian, query, &err);
	if (!querystr) {
		g_warning ("error: %s", err->message);
		g_error_free (err);
		return FALSE;
	} 

	g_print ("%s\n", querystr);
	g_free (querystr);

	*count = 1;
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
		return output_links (iter, opts->linksdir, opts->clearlinks,
					count);
	case FORMAT_PLAIN: 
		return output_plain (iter, opts->fields, opts->summary,
					opts->color, count);
	case FORMAT_XML:
		return output_xml (iter, count);
	case FORMAT_JSON:
		return output_json (iter, count);
	case FORMAT_SEXP:
		return output_sexp (iter, count);	
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
			     opts->descending ? FALSE : TRUE,
			     &err);
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

/* create a linksdir if it not exist yet; if it already existed,
 * remove old links if opts->clearlinks was specified */
static gboolean
create_linksdir_maybe (const char *linksdir, gboolean clearlinks)
{
	GError *err;
	
	err = NULL;
	if (access (linksdir, F_OK) != 0) {
		if (!mu_maildir_mkdir (linksdir, 0700, TRUE, &err))
			goto fail;
	} else if (clearlinks)
		if (!mu_maildir_clear_links (linksdir, &err))
			goto fail;

	return TRUE;
	
fail:
	if (err) {
		g_warning ("%s", err->message ? err->message : "unknown error");
		g_error_free (err);
	}

	return FALSE;	
}

static gboolean
link_message (const char *src, const char *destdir)
{
	GError *err;
	
	if (access (src, R_OK) != 0) {
		if (errno == ENOENT)
			g_warning ("cannot find source message %s", src);
		else 
			g_warning ("cannot read source message %s: %s", src,
				   strerror (errno));
		return FALSE;
	}

	err = NULL;
	if (!mu_maildir_link (src, destdir, &err)) {
		if (err) {
			g_warning ("%s", err->message ?
				   err->message : "unknown error");
			g_error_free (err);
		}
		return FALSE;	
	}
	
	return TRUE;
}



static gboolean
output_links (MuMsgIter *iter, const char* linksdir,
	      gboolean clearlinks, size_t *count)
{
	size_t mycount;
	gboolean errseen;
	MuMsgIter *myiter;
	
	g_return_val_if_fail (iter, FALSE);
	g_return_val_if_fail (linksdir, FALSE);	

	/* note: we create the linksdir even if there are no search results */
	if (!create_linksdir_maybe (linksdir, clearlinks))
		return FALSE;
	
	for (myiter = iter, errseen = FALSE, mycount = 0;
	     !mu_msg_iter_is_done (myiter);
	     mu_msg_iter_next (myiter), ++mycount) {

		MuMsg *msg;
		const char* path;
	
		msg = mu_msg_iter_get_msg (iter, NULL); /* don't unref */
		if (!msg)
			return FALSE;

		path = mu_msg_get_field_string (msg, MU_MSG_FIELD_ID_PATH);
		if (!path)
			return FALSE;
		
		if (!link_message (path, linksdir))
			errseen = TRUE;
	}

	if (errseen) 
		g_warning ("error linking some of the messages; maybe the "
			   "database needs to be updated");

	if (count)
		*count = mycount;
		
	return TRUE;
}



static void
ansi_color_maybe (MuMsgFieldId mfid, gboolean color)
{
	const char* ansi;

	if (!color)
		return; /* nothing to do */
	
	switch (mfid) {

	case MU_MSG_FIELD_ID_FROM:
		ansi = MU_COLOR_CYAN; break;

	case MU_MSG_FIELD_ID_TO:
	case MU_MSG_FIELD_ID_CC:
	case MU_MSG_FIELD_ID_BCC:
		ansi = MU_COLOR_BLUE; break;

	case MU_MSG_FIELD_ID_SUBJECT:
		ansi = MU_COLOR_GREEN; break;
		
	case MU_MSG_FIELD_ID_DATE:
		ansi = MU_COLOR_MAGENTA; break;

	default:
		if (mu_msg_field_type(mfid) == MU_MSG_FIELD_TYPE_STRING)
			ansi = MU_COLOR_YELLOW;
		else
			ansi = MU_COLOR_RED;
	}

	fputs (ansi, stdout);
}


static void
ansi_reset_maybe (MuMsgFieldId mfid, gboolean color)
{
	if (!color)
		return; /* nothing to do */
	
	fputs (MU_COLOR_DEFAULT, stdout);
	
}




static const char*
display_field (MuMsgIter *iter, MuMsgFieldId mfid)
{
	gint64 val;
	MuMsg *msg;
	
	msg = mu_msg_iter_get_msg (iter, NULL); /* don't unref */
	if (!msg)
		return NULL;
	
	switch (mu_msg_field_type(mfid)) {
	case MU_MSG_FIELD_TYPE_STRING: {
		const gchar *str;
		str = mu_msg_get_field_string (msg, mfid);
		return str ? str : "";
	}		
	case MU_MSG_FIELD_TYPE_INT:
	
		if (mfid == MU_MSG_FIELD_ID_PRIO) {
			val = mu_msg_get_field_numeric (msg, mfid);
			return mu_msg_prio_name ((MuMsgPrio)val);
 		} else if (mfid == MU_MSG_FIELD_ID_FLAGS) {
			val = mu_msg_get_field_numeric (msg, mfid);
			return mu_str_flags_s ((MuMsgFlags)val);
		} else  /* as string */
			return mu_msg_get_field_string (msg, mfid);
		
	case MU_MSG_FIELD_TYPE_TIME_T: 
		val = mu_msg_get_field_numeric (msg, mfid);
		return mu_str_date_s ("%c", (time_t)val);

	case MU_MSG_FIELD_TYPE_BYTESIZE: 
		val = mu_msg_get_field_numeric (msg, mfid);
		return mu_str_size_s ((unsigned)val);
	default:
		g_return_val_if_reached (NULL);
	}
}


static void
print_summary (MuMsgIter *iter)
{
	GError *err;
	char *summ;
	MuMsg *msg;
	const guint SUMMARY_LEN = 5; /* summary based on first 5
				      * lines */	
	err = NULL;
	msg = mu_msg_iter_get_msg (iter, &err); /* don't unref */
	if (!msg) {
		g_warning ("error get message: %s", err->message);
		g_error_free (err);
		return;
	}

	summ = mu_str_summarize (mu_msg_get_body_text(msg), SUMMARY_LEN);
	g_print ("Summary: %s\n", summ ? summ : "<none>");
	g_free (summ);
}


static void
indent (MuMsgIter *iter)
{
	const char* threadpath;
	int i;
	
	threadpath = mu_msg_iter_get_thread_path (iter);
	if (!threadpath)
		return;
	
	fputs (threadpath, stdout);
	
	/* count the colons... */
	for (i = 0; *threadpath; ++threadpath)
		i += (*threadpath == ':') ? 1 : 0;

	/* indent */
	while (i --> -1)
		fputs ("  ", stdout);
}

static gboolean
output_plain (MuMsgIter *iter, const char *fields, gboolean summary,
	      gboolean color, size_t *count)
{
	MuMsgIter *myiter;
	size_t mycount;
	
	g_return_val_if_fail (iter, FALSE);
	g_return_val_if_fail (fields, FALSE);
	
	for (myiter = iter, mycount = 0; !mu_msg_iter_is_done (myiter);
	     mu_msg_iter_next (myiter), ++mycount) {
		
		const char* myfields;
		int len;

		indent (myiter);
		
		for (myfields = fields, len = 0; *myfields; ++myfields) {
			MuMsgFieldId mfid;
			mfid =	mu_msg_field_id_from_shortcut (*myfields, FALSE);
			if (mfid == MU_MSG_FIELD_ID_NONE ||
			    (!mu_msg_field_xapian_value (mfid) &&
			     !mu_msg_field_xapian_contact (mfid)))
				len += printf ("%c", *myfields);
			else {
				ansi_color_maybe (mfid, color);
				len += mu_util_fputs_encoded
					(display_field (myiter, mfid), stdout);
				ansi_reset_maybe (mfid, color);
			}
		}
		
		g_print (len > 0 ? "\n" : "");
		if (summary)
			print_summary (myiter);
		
	}

	if (count)
		*count = mycount;
	
	return TRUE;
}

static void
print_attr_xml (const char* elm, const char *str)
{
	gchar *esc;
	
	if (mu_str_is_empty(str))
		return; /* empty: don't include */

	esc = g_markup_escape_text (str, -1);	
	g_print ("\t\t<%s>%s</%s>\n", elm, esc, elm);
	g_free (esc);
}

static gboolean
output_xml (MuMsgIter *iter, size_t *count)
{
	MuMsgIter *myiter;
	size_t mycount;
	
	g_return_val_if_fail (iter, FALSE);

	g_print ("<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n");
	g_print ("<messages>\n");
	
	for (myiter = iter, mycount = 0; !mu_msg_iter_is_done (myiter);
	     mu_msg_iter_next (myiter), ++mycount) {

		MuMsg *msg;
		
		msg = mu_msg_iter_get_msg (iter, NULL); /* don't unref */
		if (!msg)
			return FALSE;

		g_print ("\t<message>\n");
		print_attr_xml ("from", mu_msg_get_from (msg));
		print_attr_xml ("to", mu_msg_get_to (msg));
		print_attr_xml ("cc", mu_msg_get_cc (msg));
		print_attr_xml ("subject", mu_msg_get_subject (msg));
		g_print ("\t\t<date>%u</date>\n",
			 (unsigned) mu_msg_get_date (msg));
		g_print ("\t\t<size>%u</size>\n",
			 (unsigned) mu_msg_get_size (msg));
		print_attr_xml ("msgid", mu_msg_get_msgid (msg));
		print_attr_xml ("path", mu_msg_get_path (msg));
		print_attr_xml ("maildir", mu_msg_get_maildir (msg));
		g_print ("\t</message>\n");
	}

	g_print ("</messages>\n");
		
	if (count)
		*count = mycount;
	
	return TRUE;
}


static void
print_attr_json (const char* elm, const char *str, gboolean comma)
{
	gchar *esc;
	
	if (!str || strlen(str) == 0)
		return; /* empty: don't include */
	
	esc = mu_str_escape_c_literal (str);
	g_print ("\t\t\t\"%s\":\"%s\"%s\n", elm, esc, comma ? "," : "");
	g_free (esc);
}


static gboolean
output_json (MuMsgIter *iter, size_t *count)
{
	MuMsgIter *myiter;
	size_t mycount;
	
	g_return_val_if_fail (iter, FALSE);
	
	g_print ("{\n\t\"messages\":\n\t[\n");
	
	for (myiter = iter, mycount = 0; !mu_msg_iter_is_done (myiter);
	     mu_msg_iter_next (myiter), ++mycount) {

		MuMsg *msg;

		if (!(msg = mu_msg_iter_get_msg (iter, NULL)))
			return FALSE;
		
		if (mycount != 0)
			g_print (",\n");
				
		g_print ("\t\t{\n");
		print_attr_json ("from", mu_msg_get_from (msg), TRUE);
		print_attr_json ("to", mu_msg_get_to (msg),TRUE);
		print_attr_json ("cc", mu_msg_get_cc (msg),TRUE);
		print_attr_json ("subject", mu_msg_get_subject (msg), TRUE);
		g_print ("\t\t\t\"date\":%u,\n",
			 (unsigned) mu_msg_get_date (msg));
		g_print ("\t\t\t\"size\":%u,\n",
			 (unsigned) mu_msg_get_size (msg));
		print_attr_json ("msgid", mu_msg_get_msgid (msg),TRUE);
		print_attr_json ("path", mu_msg_get_path (msg),TRUE);
		print_attr_json ("maildir", mu_msg_get_maildir (msg), FALSE);
		g_print ("\t\t}");
	}
	g_print ("\t]\n}\n");
		
	if (count)
		*count = mycount;
	
	return TRUE;
}


static void
print_attr_sexp (const char* elm, const char *str, gboolean nl)
{
	gchar *esc;
	
	if (!str || strlen(str) == 0)
		return; /* empty: don't include */

	esc = mu_str_escape_c_literal (str);
	g_print ("    (:%s \"%s\")%s", elm, esc, nl ? "\n" : "");
	g_free (esc);
}



static gboolean
output_sexp (MuMsgIter *iter, size_t *count)
{
	MuMsgIter *myiter;
	size_t mycount;
	
	g_return_val_if_fail (iter, FALSE);
	
	g_print ("(:messages\n");
	
	for (myiter = iter, mycount = 0; !mu_msg_iter_is_done (myiter);
	     mu_msg_iter_next (myiter), ++mycount) {

		MuMsg *msg;
		if (!(msg = mu_msg_iter_get_msg (iter, NULL))) /* don't unref */
			return FALSE;
		
		if (mycount != 0)
			g_print ("\n");
		
		g_print ("  (:message\n");
		print_attr_sexp ("from", mu_msg_get_from (msg),TRUE);
		print_attr_sexp ("to", mu_msg_get_to (msg),TRUE);
		print_attr_sexp ("cc", mu_msg_get_cc (msg),TRUE);
		print_attr_sexp ("subject", mu_msg_get_subject (msg),TRUE);
		g_print ("    (:date %u)\n", (unsigned) mu_msg_get_date (msg));
		g_print ("    (:size %u)\n", (unsigned) mu_msg_get_size (msg));
		print_attr_sexp ("msgid", mu_msg_get_msgid (msg),TRUE);
		print_attr_sexp ("path", mu_msg_get_path (msg),TRUE);
		print_attr_sexp ("maildir", mu_msg_get_maildir (msg),FALSE);
		g_print (")");
	}
	g_print (")\n");
		
	if (count)
		*count = mycount;
	
	return TRUE;
}



MuExitCode
mu_cmd_find (MuConfig *opts)
{
	MuQuery *xapian;
	gboolean rv;
	gchar *query;
	size_t count = 0;
	OutputFormat format;
	
	g_return_val_if_fail (opts, FALSE);
	g_return_val_if_fail (opts->cmd == MU_CONFIG_CMD_FIND, FALSE);
	
	if (!query_params_valid (opts) || !format_params_valid(opts))
		return MU_EXITCODE_ERROR;

	format = get_output_format (opts->formatstr);
	xapian = get_query_obj ();
	query  = get_query (opts);

	if (!xapian ||!query)
		return MU_EXITCODE_ERROR;
	
	if (format == FORMAT_XQUERY)
		rv = print_xapian_query (xapian, query, &count);
	else
		rv = run_query (xapian, query, opts, format, &count);

	mu_query_destroy (xapian);
	g_free (query);

	if (!rv)
		return MU_EXITCODE_ERROR;

	return count == 0 ? MU_EXITCODE_NO_MATCHES : MU_EXITCODE_OK;
}
