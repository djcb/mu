/*
** Copyright (C) 2012-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <mu-msg.h>
#include <mu-date.h>
#include <mu-msg-part.h>

#include <gtk/gtk.h>
#include <webkit/webkitwebview.h>
#include <webkit/webkitwebresource.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>

static gboolean
print_to_pdf (WebKitWebFrame *frame, GError **err)
{
	GtkPrintOperation *op;
	GtkPrintOperationResult res;
	char *path;
	gboolean rv;

	path = g_strdup_printf ("%s%c%x.pdf",mu_util_cache_dir(),
				G_DIR_SEPARATOR, (unsigned)random());
	if (!mu_util_create_dir_maybe (mu_util_cache_dir(),0700,FALSE)) {
		g_warning ("Couldn't create tempdir");
		return FALSE;
	}


	op = gtk_print_operation_new ();
	gtk_print_operation_set_export_filename
		(GTK_PRINT_OPERATION(op), path);

	res = webkit_web_frame_print_full (frame, op,
					   GTK_PRINT_OPERATION_ACTION_EXPORT,
					   err);
	g_object_unref (op);
	rv = (res != GTK_PRINT_OPERATION_RESULT_ERROR);
	if (rv)
		g_print ("%s\n", path);

	g_free (path);
	return rv;

}


static char*
save_file_for_cid (MuMsg *msg, const char* cid)
{
	gint idx;
	gchar *filepath;
	GError *err;

	g_return_val_if_fail (msg, NULL);
	g_return_val_if_fail (cid, NULL);

	idx = mu_msg_find_index_for_cid (msg, MU_MSG_OPTION_NONE, cid);
	if (idx < 0) {
		g_warning ("%s: cannot find %s", __func__, cid);
		return NULL;
	}

	err = NULL;
	filepath = mu_msg_part_get_cache_path (msg, MU_MSG_OPTION_NONE, idx, NULL);
	if (!filepath)
		goto errexit;

	if (!mu_msg_part_save (msg, MU_MSG_OPTION_USE_EXISTING, filepath, idx,
			       &err))
		goto errexit;

	return filepath;

errexit:
	g_warning ("%s: failed to save %s: %s", __func__,
		   filepath,
		   err&&err->message?err->message:"error");
	g_clear_error (&err);
	g_free (filepath);

	return NULL;
}

static void
on_resource_request_starting (WebKitWebView *self, WebKitWebFrame *frame,
			      WebKitWebResource *resource,
			      WebKitNetworkRequest *request,
			      WebKitNetworkResponse *response, MuMsg *msg)
{
	const char* uri;
	uri = webkit_network_request_get_uri (request);

	if (g_ascii_strncasecmp (uri, "cid:", 4) == 0) {
		gchar *filepath;
		filepath = save_file_for_cid (msg, uri);
		if (filepath) {
			gchar *fileuri;
			fileuri = g_strdup_printf ("file://%s", filepath);
			webkit_network_request_set_uri (request, fileuri);
			g_free (fileuri);
			g_free (filepath);
		}
	}
}


/* return the path to the output file, or NULL in case of error */
static gboolean
generate_pdf (MuMsg *msg, const char *str, GError **err)
{
	GtkWidget *view;
	WebKitWebFrame *frame;
	WebKitWebSettings *settings;
	WebKitLoadStatus status;
	time_t started;
	const int max_time = 3; /* max 3 seconds to download stuff */

	settings = webkit_web_settings_new ();
	g_object_set (G_OBJECT(settings),
		      "enable-scripts", FALSE,
		      "auto-load-images", TRUE,
		      "enable-plugins", FALSE,  NULL);

	view = webkit_web_view_new ();

	/* to support cid: */
	g_signal_connect (G_OBJECT(view), "resource-request-starting",
			  G_CALLBACK (on_resource_request_starting), msg);

	webkit_web_view_set_settings (WEBKIT_WEB_VIEW(view), settings);
 	webkit_web_view_load_string (WEBKIT_WEB_VIEW(view),
				     str, "text/html", "utf-8", "");
	g_object_unref (settings);

	frame = webkit_web_view_get_main_frame (WEBKIT_WEB_VIEW(view));
	if (!frame) {
		g_set_error (err, 0, MU_ERROR, "cannot get web frame");
		return FALSE;
	}

	started = time (NULL);
	do {
		status = webkit_web_view_get_load_status (WEBKIT_WEB_VIEW(view));
		gtk_main_iteration_do (FALSE);
	} while (status != WEBKIT_LOAD_FINISHED &&
		 (time(NULL) - started) <= max_time);

	return print_to_pdf (frame, err);
}


static void
add_header (GString *gstr, const char* header, const char *val)
{
	char *esc;

	if (!val)
		return;

	esc = g_markup_escape_text (val, -1);
	g_string_append_printf (gstr, "<b>%s</b>: %s<br>", header, esc);
	g_free (esc);
}

 /* return the path to the output file, or NULL in case of error */
static gboolean
convert_to_pdf (MuMsg *msg, GError **err)
{
	GString *gstr;
	const char *body;
	gchar *data;
	gboolean rv;

	gstr = g_string_sized_new (4096);

	add_header (gstr, "From", mu_msg_get_from (msg));
	add_header (gstr, "To", mu_msg_get_to (msg));
	add_header (gstr, "Cc", mu_msg_get_cc (msg));
	add_header (gstr, "Subject", mu_msg_get_subject (msg));
	add_header (gstr, "Date", mu_date_str_s
		    ("%c", mu_msg_get_date(msg)));

	gstr =	g_string_append (gstr, "<hr>\n");

	body = mu_msg_get_body_html (msg, MU_MSG_OPTION_NONE);
	if (body)
 		g_string_append_printf (gstr, "%s", body);
	else {
		body = mu_msg_get_body_text (msg, MU_MSG_OPTION_NONE);
		if (body) {
			gchar *esc;
			esc = g_markup_escape_text (body, -1);
			g_string_append_printf (gstr, "<pre>\n%s\n</pre>",
						esc);
			g_free (esc);
		} else
			gstr = g_string_append
				(gstr, "<i>No body</i>\n");
	}

	data = g_string_free (gstr, FALSE);
	rv = generate_pdf (msg, data, err);
	g_free (data);

	return rv;
}


int
main(int argc, char *argv[])
{
	MuMsg *msg;
	GError *err;

	if (argc != 2) {
		g_print ("msg2pdf: generate pdf files from e-mail messages\n"
		 "usage: msg2pdf <msgfile>\n");
		return 1;
	}

	gtk_init (&argc, &argv);

	if (access (argv[1], R_OK) != 0) {
		g_printerr ("%s is not a readable file\n", argv[1]);
		return 1;
	}

	err = NULL;
	msg = mu_msg_new_from_file (argv[1], NULL, &err);
	if (!msg) {
		g_printerr ("failed to create msg for %s\n", argv[1]);
		goto err;
	}

	if (!convert_to_pdf (msg, &err)) {
		g_printerr ("failed to create pdf from %s\n", argv[1]);
		goto err;
	}

	/* it worked! */
	mu_msg_unref (msg);
	return 0;

err:
	/* some error occurred */
	mu_msg_unref (msg);

	if (err)
		g_printerr ("error: %s", err->message);

	g_clear_error (&err);
	return 1;

}
