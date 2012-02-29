/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

/*
** Copyright (C) 2008-2012 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include "mu-date.h"
#include "mu-maildir.h"
#include "mu-index.h"
#include "mu-query.h"
#include "mu-msg-iter.h"
#include "mu-bookmarks.h"
#include "mu-runtime.h"


#include "mu-util.h"
#include "mu-cmd.h"
#include "mu-threader.h"


static gboolean output_links (MuMsgIter *iter, const char* linksdir,
			      gboolean clearlinks, GError **err);
static gboolean output_sexp (MuMsgIter *iter, gboolean threads,
			     gboolean include_unreadable, GError **err);
static gboolean output_xml (MuMsgIter *iter,gboolean include_unreadable,
			    GError **err);
static gboolean output_plain (MuMsgIter *iter, const char *fields,
			      gboolean summary,gboolean threads,
			      gboolean color,  gboolean include_unreadable,
			      GError **err);

static gboolean
print_xapian_query (MuQuery *xapian, const gchar *query, GError **err)
{
	char *querystr;

	querystr = mu_query_as_string (xapian, query, err);
	if (!querystr)
		return FALSE;

	g_print ("%s\n", querystr);
	g_free (querystr);

	return TRUE;
}

/* returns MU_MSG_FIELD_ID_NONE if there is an error */
static MuMsgFieldId
sort_field_from_string (const char* fieldstr, GError **err)
{
	MuMsgFieldId mfid;

	mfid = mu_msg_field_id_from_name (fieldstr, FALSE);

	/* not found? try a shortcut */
	if (mfid == MU_MSG_FIELD_ID_NONE &&
	    strlen(fieldstr) == 1)
		mfid = mu_msg_field_id_from_shortcut(fieldstr[0],
						     FALSE);
	if (mfid == MU_MSG_FIELD_ID_NONE)
		g_set_error (err, 0, MU_ERROR_IN_PARAMETERS,
			     "not a valid sort field: '%s'\n", fieldstr);
	return mfid;
}


static gboolean
output_query_results (MuMsgIter *iter, MuConfig *opts, GError **err)
{
	switch (opts->format) {
	case MU_CONFIG_FORMAT_LINKS:
		return output_links (iter, opts->linksdir, opts->clearlinks, err);
	case MU_CONFIG_FORMAT_PLAIN:
		return output_plain (iter, opts->fields, opts->summary,
				     opts->threads, !opts->nocolor,
				     opts->include_unreadable, err);
	case MU_CONFIG_FORMAT_XML:
		return output_xml (iter, opts->include_unreadable, err);
	case MU_CONFIG_FORMAT_SEXP:
		return output_sexp (iter, opts->threads,
				    opts->include_unreadable, err);
	default:
		g_assert_not_reached ();
		return FALSE;
	}
}


static MuMsgIter*
run_query (MuQuery *xapian, const gchar *query, MuConfig *opts,
	   GError **err)
{
	MuMsgIter *iter;
	MuMsgFieldId sortid;

	sortid = MU_MSG_FIELD_ID_NONE;
	if (opts->sortfield) {
		sortid = sort_field_from_string (opts->sortfield, err);
		if (sortid == MU_MSG_FIELD_ID_NONE) /* error occured? */
			return FALSE;
	}

	iter = mu_query_run (xapian, query, opts->threads, sortid,
			     opts->reverse, -1, err);
	return iter;
}


static gboolean
process_query (MuQuery *xapian, const gchar *query, MuConfig *opts, GError **err)
{
	MuMsgIter *iter;
	gboolean rv;

	iter = run_query (xapian, query, opts, err);
	if (!iter)
		return FALSE;

	rv = output_query_results (iter, opts, err);
	mu_msg_iter_destroy (iter);

	return rv;
}


static gboolean
exec_cmd (const char *path, const char *cmd, GError **err)
{
	gint status;
	char *cmdline, *escpath;
	gboolean rv;

	if (access (path, R_OK) != 0) {
		g_set_error (err, 0, MU_ERROR_FILE_CANNOT_READ,
			     "cannot read %s: %s", path, strerror(errno));
		return FALSE;
	}

	escpath = g_strescape (path, NULL);

	cmdline = g_strdup_printf ("%s %s", cmd, escpath);
	err = NULL;
	rv = g_spawn_command_line_sync (cmdline, NULL, NULL,
					&status, err);
	g_free (cmdline);
	g_free (escpath);

	return rv;
}


static gboolean
exec_cmd_on_query (MuQuery *xapian, const gchar *query, MuConfig *opts,
		   GError **err)
{
	MuMsgIter *iter;
	gboolean rv;
	size_t count;

	if (!(iter = run_query (xapian, query, opts, err)))
		return FALSE;

	for (rv = TRUE, count = 0; !mu_msg_iter_is_done (iter);
	     mu_msg_iter_next(iter)) {
		const char *path;
		MuMsg *msg;

		msg = mu_msg_iter_get_msg_floating (iter);
		if (!msg)
			continue;

		path = mu_msg_get_path (msg);
		if (!msg) {
			g_warning ("cannot get path for msg");
			continue;
		}

		rv = exec_cmd (path, opts->exec, err);
		if (rv)
			++count;
	}

	if (count == 0) {
		g_set_error (err, 0, MU_ERROR_NO_MATCHES,
			     "no matches for search expression");
		return FALSE;
	}

	mu_msg_iter_destroy (iter);
	return rv;
}



static gboolean
format_params_valid (MuConfig *opts, GError **err)
{
	switch (opts->format) {
	case MU_CONFIG_FORMAT_PLAIN:
	case MU_CONFIG_FORMAT_SEXP:
	case MU_CONFIG_FORMAT_LINKS:
	case MU_CONFIG_FORMAT_XML:
	case MU_CONFIG_FORMAT_XQUERY:
		break;
	default:
		g_set_error (err, 0, MU_ERROR_IN_PARAMETERS,
			     "invalid output format %s",
			     opts->formatstr ? opts->formatstr : "<none>");
		return FALSE;
	}

	if (opts->format == MU_CONFIG_FORMAT_LINKS && !opts->linksdir) {
		g_set_error (err, 0, MU_ERROR_IN_PARAMETERS,
			     "missing --linksdir argument");
		return FALSE;
	}

	if (opts->linksdir && opts->format != MU_CONFIG_FORMAT_LINKS) {
		g_set_error (err, 0, MU_ERROR_IN_PARAMETERS,
			     "--linksdir is only valid with --format=links");
		return FALSE;
	}

	return TRUE;
}

static gboolean
query_params_valid (MuConfig *opts, GError **err)
{
	const gchar *xpath;

	xpath = mu_runtime_path (MU_RUNTIME_PATH_XAPIANDB);

	if (mu_util_check_dir (xpath, TRUE, FALSE))
		return TRUE;

	g_set_error (err, 0, MU_ERROR_FILE_CANNOT_READ,
		     "'%s' is not a readable Xapian directory", xpath);
	return FALSE;
}

static gchar*
resolve_bookmark (MuConfig *opts, GError **err)
{
	MuBookmarks *bm;
	char* val;
	const gchar *bmfile;

	bmfile = mu_runtime_path (MU_RUNTIME_PATH_BOOKMARKS);
	bm = mu_bookmarks_new (bmfile);
	if (!bm) {
		g_set_error (err, 0, MU_ERROR_FILE_CANNOT_OPEN,
			     "failed to open bookmarks file '%s'", bmfile);
		return FALSE;
	}

	val = (gchar*)mu_bookmarks_lookup (bm, opts->bookmark);
	if (!val)
		g_set_error (err, 0, MU_ERROR_NO_MATCHES,
			     "bookmark '%s' not found", opts->bookmark);
	else
		val = g_strdup (val);

	mu_bookmarks_destroy (bm);
	return val;
}



gchar*
str_quoted_from_strv (const gchar **params)
{
	GString *str;
	int i;

	g_return_val_if_fail (params, NULL);

	if (!params[0])
		return g_strdup ("");

	str = g_string_sized_new (64); /* just a guess */

	for (i = 0; params[i]; ++i) {

		if (i > 0)
			g_string_append_c (str, ' ');

		g_string_append_c (str, '"');
		g_string_append (str, params[i]);
		g_string_append_c (str, '"');
	}

	return g_string_free (str, FALSE);
}



static gchar*
get_query (MuConfig *opts, GError **err)
{
	gchar *query, *bookmarkval;

	/* params[0] is 'find', actual search params start with [1] */
	if (!opts->bookmark && !opts->params[1]) {
		g_set_error (err, 0, MU_ERROR_IN_PARAMETERS,
			     "error in parameters");
		return NULL;
	}

	bookmarkval = NULL;
	if (opts->bookmark) {
		bookmarkval = resolve_bookmark (opts, err);
		if (!bookmarkval)
			return NULL;
	}

	query = str_quoted_from_strv ((const gchar**)&opts->params[1]);
	if (bookmarkval) {
		gchar *tmp;
		tmp = g_strdup_printf ("%s %s", bookmarkval, query);
		g_free (query);
		query = tmp;
	}

	g_free (bookmarkval);

	return query;
}


static MuQuery*
get_query_obj (MuStore *store, GError **err)
{
	MuQuery *mquery;
	unsigned count;

	count = mu_store_count (store, err);

	if (count == (unsigned)-1)
		return NULL;

	if (count == 0) {
		g_set_error (err, 0, MU_ERROR_XAPIAN_IS_EMPTY,
			     "the database is empty");
		return NULL;
	}

	if (mu_store_needs_upgrade (store)) {
		g_set_error (err, 0, MU_ERROR_XAPIAN_NOT_UP_TO_DATE,
			     "the database is not up-to-date");
		return NULL;
	}

	mquery = mu_query_new (store, err);
	if (!mquery)
		return NULL;

	return mquery;
}

/* create a linksdir if it not exist yet; if it already existed,
 * remove old links if opts->clearlinks was specified */
static gboolean
create_linksdir_maybe (const char *linksdir, gboolean clearlinks)
{
	GError *err;

	err = NULL;
	/* note, mu_maildir_mkdir simply ignores whatever part of the
	 * mail dir already exists */
	if (!mu_maildir_mkdir (linksdir, 0700, TRUE, &err))
		goto fail;

	if (clearlinks && !mu_maildir_clear_links (linksdir, &err))
		goto fail;

	return TRUE;

fail:
	g_warning ("%s", err->message ? err->message : "unknown error");
	g_clear_error (&err);

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
		g_warning ("%s", err->message ? err->message :
			   "unknown error");
		g_clear_error (&err);
		return FALSE;
	}

	return TRUE;
}



static gboolean
output_links (MuMsgIter *iter, const char* linksdir, gboolean clearlinks,
	      GError **err)
{
	size_t count, errcount;
	MuMsgIter *myiter;

	g_return_val_if_fail (iter, FALSE);
	g_return_val_if_fail (linksdir, FALSE);

	/* note: we create the linksdir even if there are no search results */
	if (!create_linksdir_maybe (linksdir, clearlinks))
		return FALSE;

	for (myiter = iter, count = errcount = 0; !mu_msg_iter_is_done (myiter);
	     mu_msg_iter_next (myiter)) {

		const char* path;
		MuMsg *msg;

		msg = mu_msg_iter_get_msg_floating (iter);
		if (!msg)
			continue;

		path = mu_msg_get_path (msg);
		if (access (path, R_OK) == 0)  /* only link to readable */
			link_message (path, linksdir) ? ++count : ++errcount;
	}

	if (errcount > 0) {
		g_set_error (err, 0, MU_ERROR_FILE_CANNOT_LINK,
			     "error linking %u message(s)", errcount);
		return FALSE;
	}

	if (count == 0) {
		g_set_error (err, 0, MU_ERROR_NO_MATCHES,
			     "no matches for search expression");
		return FALSE;
	}

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
display_field (MuMsg *msg, MuMsgFieldId mfid)
{
	gint64 val;

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
			return mu_str_flags_s ((MuFlags)val);
		} else  /* as string */
			return mu_msg_get_field_string (msg, mfid);

	case MU_MSG_FIELD_TYPE_TIME_T:
		val = mu_msg_get_field_numeric (msg, mfid);
		return mu_date_str_s ("%c", (time_t)val);

	case MU_MSG_FIELD_TYPE_BYTESIZE:
		val = mu_msg_get_field_numeric (msg, mfid);
		return mu_str_size_s ((unsigned)val);
	default:
		g_return_val_if_reached (NULL);
	}
}


static void
print_summary (MuMsg *msg)
{
	char *summ;

	const guint SUMMARY_LEN = 5; /* summary based on first 5
				      * lines */
	summ = mu_str_summarize (mu_msg_get_body_text(msg), SUMMARY_LEN);
	g_print ("Summary: %s\n", summ ? summ : "<none>");
	g_free (summ);
}


static void
thread_indent (MuMsgIter *iter)
{
	const MuMsgIterThreadInfo *ti;
	const char* threadpath;
	int i;
	gboolean is_root, first_child, empty_parent, is_dup;

	ti = mu_msg_iter_get_thread_info (iter);
	if (!ti) {
		g_warning ("cannot get thread-info for message %u",
			   mu_msg_iter_get_docid (iter));
		return;
	}

	threadpath = ti->threadpath;
	/* fputs (threadpath, stdout); */
	/* fputs ("  ", stdout); */

	is_root      = ti->prop & MU_MSG_ITER_THREAD_PROP_ROOT;
	first_child  = ti->prop & MU_MSG_ITER_THREAD_PROP_FIRST_CHILD;
	empty_parent = ti->prop & MU_MSG_ITER_THREAD_PROP_EMPTY_PARENT;
	is_dup       = ti->prop & MU_MSG_ITER_THREAD_PROP_DUP;

	/* FIXME: count the colons... */
	for (i = 0; *threadpath; ++threadpath)
		i += (*threadpath == ':') ? 1 : 0;

	/* indent */
	while (i --> 0)
		fputs ("  ", stdout);

	if (!is_root) {
		fputs (first_child ? "`" : "|", stdout);
		fputs (empty_parent ? "*> " : is_dup ? "=> " : "-> ", stdout);
	}
}



static void
output_plain_fields (MuMsg *msg, const char *fields,
		     gboolean color, gboolean threads)
{
	const char* myfields;
	int nonempty;

	for (myfields = fields, nonempty = 0; *myfields; ++myfields) {

		MuMsgFieldId mfid;
		mfid =	mu_msg_field_id_from_shortcut (*myfields, FALSE);

		if (mfid == MU_MSG_FIELD_ID_NONE ||
		    (!mu_msg_field_xapian_value (mfid) &&
		     !mu_msg_field_xapian_contact (mfid)))
		  nonempty += printf ("%c", *myfields);

		else {
			ansi_color_maybe (mfid, color);
			nonempty += mu_util_fputs_encoded
			  (display_field (msg, mfid), stdout);
			ansi_reset_maybe (mfid, color);
		}
	}

	if (nonempty)
		fputs ("\n", stdout);
}

static gboolean
output_plain (MuMsgIter *iter, const char *fields, gboolean summary,
	      gboolean threads, gboolean color, gboolean include_unreadable,
	      GError **err)
{
	MuMsgIter *myiter;
	size_t count;

	g_return_val_if_fail (iter, FALSE);
	g_return_val_if_fail (fields, FALSE);

	for (myiter = iter, count = 0; !mu_msg_iter_is_done (myiter);
	     mu_msg_iter_next (myiter)) {

		MuMsg *msg;
		msg = mu_msg_iter_get_msg_floating (iter); /* don't unref */
		if (!msg)
			continue;

		/* only return messages if they're actually
		 * readable (ie, live also outside the database) */
		if (!include_unreadable && !mu_msg_is_readable (msg))
			continue;

		/* we reuse the color (whatever that may be)
		 * for message-priority for threads, too */
		ansi_color_maybe (MU_MSG_FIELD_ID_PRIO, color);
		if (threads)
			thread_indent (iter);

		output_plain_fields (msg, fields, color, threads);

		if (summary)
			print_summary (msg);

		++count;
	}

	if (count == 0) {
		g_set_error (err, 0, MU_ERROR_NO_MATCHES,
			     "no existing matches for search expression");
		return FALSE;
	}

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
output_sexp (MuMsgIter *iter, gboolean threads,
	     gboolean include_unreadable, GError **err)
{
	MuMsgIter *myiter;
	size_t count;

	g_return_val_if_fail (iter, FALSE);

	for (myiter = iter, count = 0; !mu_msg_iter_is_done (myiter);
	     mu_msg_iter_next (myiter)) {

		MuMsg *msg;
		char *sexp;
		const MuMsgIterThreadInfo *ti;

		msg = mu_msg_iter_get_msg_floating (iter);
		if (!msg)
			return FALSE;

		/* only return messages if they're actually
		 * readable (ie, live also outside the database) */
		if (!include_unreadable && !mu_msg_is_readable (msg))
			continue;

		ti   = threads ? mu_msg_iter_get_thread_info (iter) : NULL;
		sexp = mu_msg_to_sexp (msg,
				       mu_msg_iter_get_docid (iter),
				       ti, TRUE);

		fputs (sexp, stdout);
		g_free (sexp);

		++count;
	}

	if (count == 0) {
		g_set_error (err, 0, MU_ERROR_NO_MATCHES,
			     "no existing matches for search expression");
		return FALSE;
	}

	return TRUE;
}


static void
output_xml_msg (MuMsg *msg)
{
	g_print ("\t<message>\n");
	print_attr_xml ("from", mu_msg_get_from (msg));
	print_attr_xml ("to", mu_msg_get_to (msg));
	print_attr_xml ("cc", mu_msg_get_cc (msg));
	print_attr_xml ("subject", mu_msg_get_subject (msg));
	g_print ("\t\t<date>%u</date>\n",
		 (unsigned)mu_msg_get_date (msg));
	g_print ("\t\t<size>%u</size>\n", mu_msg_get_size (msg));
	print_attr_xml ("msgid", mu_msg_get_msgid (msg));
	print_attr_xml ("path", mu_msg_get_path (msg));
	print_attr_xml ("maildir", mu_msg_get_maildir (msg));
	g_print ("\t</message>\n");
}


static gboolean
output_xml (MuMsgIter *iter, gboolean include_unreadable, GError **err)
{
	MuMsgIter *myiter;
	size_t count;

	g_return_val_if_fail (iter, FALSE);

	g_print ("<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n");
	g_print ("<messages>\n");

	for (myiter = iter, count = 0; !mu_msg_iter_is_done (myiter);
	     mu_msg_iter_next (myiter)) {
		MuMsg *msg;
		msg = mu_msg_iter_get_msg_floating (iter); /* don't unref */
		if (!msg)
			return FALSE;

		/* only return messages if they're actually
		 * readable (ie, live also outside the database) */
		if (!include_unreadable && !mu_msg_is_readable (msg))
			continue;

		output_xml_msg (msg);
		++count;

	}
	g_print ("</messages>\n");

	if (count == 0) {
		g_set_error (err, 0, MU_ERROR_NO_MATCHES,
			     "no existing matches for search expression");
		return FALSE;
	}

	return TRUE;
}

static gboolean
execute_find (MuStore *store, MuConfig *opts, GError **err)
{
	char *query_str;
	MuQuery *oracle;
	gboolean rv;

	oracle = get_query_obj(store, err);
	if (!oracle)
		return FALSE;

	query_str = get_query (opts, err);
	if (!query_str) {
		mu_query_destroy (oracle);
		return FALSE;
	}

	if (opts->format == MU_CONFIG_FORMAT_XQUERY)
		rv = print_xapian_query (oracle, query_str, err);
	else if (opts->exec)
		rv = exec_cmd_on_query (oracle, query_str, opts, err);
	else
		rv = process_query (oracle, query_str, opts, err);

	mu_query_destroy (oracle);
	g_free (query_str);

	return rv;
}

static void
show_usage (void)
{
	const char *usage_str =
		"usage: mu find [options] <search expression>\n";
	g_message ("%s", usage_str);
}





MuError
mu_cmd_find (MuStore *store, MuConfig *opts, GError **err)
{
	g_return_val_if_fail (opts, MU_ERROR_INTERNAL);
	g_return_val_if_fail (opts->cmd == MU_CONFIG_CMD_FIND,
			      MU_ERROR_INTERNAL);

	if (!query_params_valid (opts, err) || !format_params_valid(opts, err)) {

		if (MU_G_ERROR_CODE(err) == MU_ERROR_IN_PARAMETERS)
			show_usage ();

		return MU_G_ERROR_CODE (err);
	}

	if (!execute_find (store, opts, err))
		return MU_G_ERROR_CODE(err);
	else
		return MU_OK;
}
