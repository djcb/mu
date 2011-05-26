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
#include <stdlib.h>
#include <errno.h>

#include "mu-output.h"

#include "mu-msg.h"
#include "mu-maildir.h"
#include "mu-index.h"
#include "mu-msg-iter.h"
#include "mu-str.h"
#include "mu-util.h"


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



gboolean
mu_output_links (MuMsgIter *iter, const char* linksdir,
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
ansi_color (MuMsgFieldId mfid)
{
	const char* ansi;
	
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
ansi_reset (MuMsgFieldId mfid)
{
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



gboolean
mu_output_plain (MuMsgIter *iter, const char *fields, gboolean summary,
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
		
		for (myfields = fields, len = 0; *myfields; ++myfields) {
			MuMsgFieldId mfid;
			mfid =	mu_msg_field_id_from_shortcut (*myfields, FALSE);
			if (mfid == MU_MSG_FIELD_ID_NONE ||
			    (!mu_msg_field_xapian_value (mfid) &&
			     !mu_msg_field_xapian_contact (mfid)))
				len += printf ("%c", *myfields);
			else {
				if (color) ansi_color (mfid);
				len += fputs (display_field (myiter, mfid), stdout);
				if (color) ansi_reset (mfid);
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

gboolean
mu_output_xml (MuMsgIter *iter, size_t *count)
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


gboolean
mu_output_json (MuMsgIter *iter, size_t *count)
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



gboolean
mu_output_sexp (MuMsgIter *iter, size_t *count)
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
