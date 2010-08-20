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

#include "config.h"

#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>

#include "mu-msg-gmime.h"
#include "mu-msg-str.h"
#include "mu-cmd.h"

#include "mu-util.h"


/* we ignore fields for now */
static gboolean
view_file (const gchar *path, const gchar *fields, size_t summary_len)
{
	MuMsgGMime* msg;
	const char *field;
	time_t date;
	
	msg = mu_msg_gmime_new (path, NULL);
	if (!msg)
		return FALSE;
	
	field = mu_msg_gmime_get_from (msg);
	if (field)
		g_print ("From: %s\n", field);
	
	field = mu_msg_gmime_get_to (msg);
	if (field)
		g_print ("To: %s\n", field);

	field = mu_msg_gmime_get_cc (msg);
	if (field)
		g_print ("Cc: %s\n", field);

	field = mu_msg_gmime_get_subject (msg);
	if (field)
		g_print ("Subject: %s\n", field);
	
	date = mu_msg_gmime_get_date (msg);
	if (date)
		g_print ("Date: %s\n",
			 mu_msg_str_date_s (date));

	if (summary_len > 0) {
		field = mu_msg_gmime_get_summary (msg, summary_len);
		g_print ("Summary: %s\n", field ? field : "<none>");
	} else {
	
		field = mu_msg_gmime_get_body_text (msg);
		if (field) 
			g_print ("\n%s\n", field);
		else
			/* not really an error */
			g_debug ("No text body found for %s", path);
	}
		
	mu_msg_gmime_destroy (msg);

	return TRUE;
}

gboolean
mu_cmd_view (MuConfigOptions *opts)
{
	gboolean rv;
	int i;
	
	g_return_val_if_fail (opts, FALSE);

	/* note: params[0] will be 'view' */
	if (!opts->params[0] || !opts->params[1]) {
		g_printerr ("Missing files to view\n");
		return FALSE;
	}
	
	mu_msg_gmime_init();

	rv = TRUE;
	for (i = 1; opts->params[i] && rv; ++i) 	
		rv = view_file (opts->params[i], NULL,
				opts->summary_len);

	mu_msg_gmime_uninit();
	
	return rv;
}
