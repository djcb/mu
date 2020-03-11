/*
** Copyright (C) 2012-2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <utils/mu-date.h>
#include <mu-msg-part.h>

#include <gtk/gtk.h>
#include <webkit2/webkit2.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>

typedef enum { UNKNOWN, OK, FAILED } Result;

static void
on_failed (WebKitPrintOperation *print_operation,
           GError               *error,
           Result               *result)
{
        if (error)
                g_warning ("%s", error->message);

        *result = FAILED;
}


static void
on_finished (WebKitPrintOperation *print_operation,
             Result               *result)
{
        if (*result == UNKNOWN)
                *result = OK;
}


static gboolean
print_to_pdf (GtkWidget *webview, GError **err)
{
        GtkWidget            *win;
	WebKitPrintOperation *op;
        GtkPrintSettings     *settings;
	char                 *path, *uri;
        Result                res;
        time_t                started;
	const int             max_time = 3; /* max 3 seconds to download stuff */


	path = g_strdup_printf ("%s%c%x.pdf", mu_util_cache_dir(),
				G_DIR_SEPARATOR, (unsigned)random());
	if (!mu_util_create_dir_maybe (mu_util_cache_dir(), 0700, FALSE)) {
		g_warning ("Couldn't create tempdir");
                g_free (path);
		return FALSE;
	}
        uri = g_filename_to_uri (path, NULL, NULL);
        g_print ("%s\n", path);
        g_free(path);
        if (!uri) {
                g_warning ("failed to create uri");
                return FALSE;
        }

        win = gtk_window_new (GTK_WINDOW_TOPLEVEL);
        gtk_container_add(GTK_CONTAINER(win), webview);
        gtk_widget_show_all(win);


	op       = webkit_print_operation_new (WEBKIT_WEB_VIEW(webview));
        settings = gtk_print_settings_new();
        gtk_print_settings_set(settings,
                               GTK_PRINT_SETTINGS_OUTPUT_URI, uri);
        gtk_print_settings_set(settings,
                               GTK_PRINT_SETTINGS_OUTPUT_FILE_FORMAT, "PDF");
        g_free(uri);

        webkit_print_operation_set_print_settings(op, settings);


        webkit_print_operation_run_dialog(op, NULL);


        res = UNKNOWN;
        g_signal_connect(op, "failed", G_CALLBACK(on_failed), &res);
        g_signal_connect(op, "finished", G_CALLBACK(on_finished), &res);

        webkit_print_operation_print(op);
        started = time (NULL);
        do {
                gtk_main_iteration_do (FALSE);
	} while (res == UNKNOWN /*&& (time(NULL) - started) <= max_time*/);

        g_object_unref (op);

        return res == OK;
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
on_resource_load_started (WebKitWebView     *self,
                          WebKitWebResource *resource,
                          WebKitURIRequest  *request,
                          MuMsg             *msg)
{
	const char* uri;

	uri = webkit_uri_request_get_uri (request);

	if (g_ascii_strncasecmp (uri, "cid:", 4) == 0) {
		gchar *filepath;
		filepath = save_file_for_cid (msg, uri);
		if (filepath) {
			gchar *fileuri;
			fileuri = g_strdup_printf ("file://%s", filepath);
			webkit_uri_request_set_uri (request, fileuri);
                        g_debug("printing %s", fileuri);
			g_free (fileuri);
			g_free (filepath);
		}
	}
}


/* return the path to the output file, or NULL in case of error */
static gboolean
generate_pdf (MuMsg *msg, const char *str, GError **err)
{
	GtkWidget      *view;
	WebKitSettings *settings;
	time_t          started;
        gboolean        loading;
	const int       max_time = 3; /* max 3 seconds to download stuff */

	settings = webkit_settings_new ();
	g_object_set (G_OBJECT(settings),
		      "enable-javascript", FALSE,
		      "auto-load-images", TRUE,
		      "enable-plugins", FALSE,
                      NULL);

	view = webkit_web_view_new ();

	/* to support cid: */
	g_signal_connect (G_OBJECT(view), "resource-load-started",
			  G_CALLBACK (on_resource_load_started), msg);

	webkit_web_view_set_settings (WEBKIT_WEB_VIEW(view), settings);
 	webkit_web_view_load_html (WEBKIT_WEB_VIEW(view), str, NULL);
	g_object_unref (settings);

	started = time (NULL);
	do {
		loading = webkit_web_view_is_loading (WEBKIT_WEB_VIEW(view));
		gtk_main_iteration_do (FALSE);
	} while (loading && (time(NULL) - started) <= max_time);

	return print_to_pdf (view, err);
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
	GString    *gstr;
	const char *body;
	gchar      *data;
	gboolean    rv;

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
	MuMsg  *msg;
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
