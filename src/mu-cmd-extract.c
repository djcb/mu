/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

/*
** Copyright (C) 2010-2012 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <stdlib.h>
#include <string.h>

#include "mu-msg.h"
#include "mu-msg-part.h"
#include "mu-cmd.h"
#include "mu-util.h"
#include "mu-str.h"

static gboolean
save_part (MuMsg *msg, const char *targetdir, guint partidx, gboolean overwrite,
	   gboolean play)
{
	GError *err;
	gchar *filepath;

	err = NULL;

	filepath = mu_msg_part_filepath (msg, targetdir, partidx, &err);
	if (!filepath) {
		if (err) {
			g_warning ("failed to save MIME-part: %s",
				   err->message);
			g_error_free (err);
		}
		g_free (filepath);
		return FALSE;
	}

	if (!mu_msg_part_save (msg, filepath, partidx, overwrite, FALSE, &err)) {
		if (err) {
			g_warning ("failed to save MIME-part: %s",
				   err->message);
			g_error_free (err);
		}
		g_free (filepath);
		return FALSE;
	}

	if (play)
		mu_util_play (filepath, TRUE, FALSE);

	g_free (filepath);
	return TRUE;
}



static gboolean
save_numbered_parts (MuMsg *msg, MuConfig *opts)
{
	gboolean rv;
	char **parts, **cur;

	parts = g_strsplit (opts->parts, ",", 0);

	for (rv = TRUE, cur = parts; cur && *cur; ++cur) {

		unsigned idx;
		int i;
		char *endptr;

		idx = (unsigned)(i = strtol (*cur, &endptr, 10));
		if (i < 0 || *cur == endptr) {
			g_warning ("invalid MIME-part index '%s'", *cur);
			rv = FALSE;
			break;
		}

		if (!save_part (msg, opts->targetdir, idx, opts->overwrite,
				opts->play)) {
			g_warning ("failed to save MIME-part %d", idx);
			rv = FALSE;
			break;
		}
	}

	g_strfreev (parts);
	return rv;
}

static GRegex*
anchored_regex (const char* pattern)
{
	GRegex *rx;
	GError *err;
	gchar *anchored;


	anchored = g_strdup_printf
		("%s%s%s",
		 pattern[0] == '^' ? "" : "^",
		 pattern,
		 pattern[strlen(pattern)-1] == '$' ? "" : "$");

	err = NULL;
	rx = g_regex_new (anchored, G_REGEX_CASELESS|G_REGEX_OPTIMIZE, 0,
			  &err);
	g_free (anchored);

	if (!rx) {
		g_warning ("error in regular expression '%s': %s",
			   pattern, err->message ? err->message : "error");
		g_error_free (err);
		return NULL;
	}

	return rx;
}


static gboolean
save_part_with_filename (MuMsg *msg, const char *pattern, MuConfig *opts)
{
	GSList *lst, *cur;
	GRegex *rx;
	gboolean rv;

	/* 'anchor' the pattern with '^...$' if not already */
	rx = anchored_regex (pattern);
	if (!rx)
		return FALSE;

	lst = mu_msg_part_find_files (msg, rx);
	g_regex_unref (rx);
	if (!lst) {
		g_warning ("no matching attachments found");
		return FALSE;
	}

	for (cur = lst, rv = TRUE; cur; cur = g_slist_next (cur))
		rv &= save_part (msg, opts->targetdir,
				 GPOINTER_TO_UINT(cur->data),
				 opts->overwrite, opts->play);
	g_slist_free (lst);

	return rv;
}



struct _SaveData {
	gboolean		 attachments_only;
	gboolean		 result;
	guint			 saved_num;
	const gchar*		 targetdir;
	gboolean		 overwrite;
	gboolean		 play;
};
typedef struct _SaveData	 SaveData;


static gboolean
ignore_part (MuMsg *msg, MuMsgPart *part, SaveData *sd)
{
	/* something went wrong somewhere; stop */
	if (!sd->result)
		return TRUE;

	/* filter out non-attachments if only want attachments */
	if (sd->attachments_only &&
	    !mu_msg_part_looks_like_attachment (part, TRUE))
		return TRUE;

	/* ignore multiparts */
	if (part->type &&
	    g_ascii_strcasecmp (part->type, "multipart") == 0)
		return TRUE;

	return FALSE;
}


static void
save_part_if (MuMsg *msg, MuMsgPart *part, SaveData *sd)
{
	gchar *filepath;
	gboolean rv;
	GError *err;

	if (ignore_part (msg, part, sd))
		return;

	rv	 = FALSE;
	filepath = NULL;

	err = NULL;
	filepath = mu_msg_part_filepath (msg, sd->targetdir, part->index, &err);
	if (!filepath) {
		g_warning ("failed to get file path: %s",
			   err&&err->message ? err->message : "error");
		g_clear_error (&err);
		goto leave;
	}

	if (!mu_msg_part_save (msg, filepath, part->index,
			       sd->overwrite, FALSE, &err)) {
		g_warning ("failed to save MIME-part: %s",
			   err&&err->message ? err->message : "error");
		g_clear_error (&err);
		goto leave;
	}

	if (sd->play && !mu_util_play (filepath, TRUE, FALSE))
		goto leave;

	rv = TRUE;
	++sd->saved_num;

leave:
	sd->result = rv;
	g_free (filepath);
}

static gboolean
save_certain_parts (MuMsg *msg, gboolean attachments_only,
		    const gchar *targetdir, gboolean overwrite, gboolean play)
{
	SaveData sd;

	sd.result	    = TRUE;
	sd.saved_num        = 0;
	sd.attachments_only = attachments_only;
	sd.overwrite	    = overwrite;
	sd.targetdir	    = targetdir;
	sd.play             = play;

	mu_msg_part_foreach (msg, FALSE,
			     (MuMsgPartForeachFunc)save_part_if,
			     &sd);

	if (sd.saved_num == 0) {
		g_warning ("no %s extracted from this message",
			   attachments_only ? "attachments" : "parts");
		sd.result = FALSE;
	}

	return sd.result;
}


static gboolean
save_parts (const char *path, const char *filename, MuConfig *opts)
{
	MuMsg* msg;
	gboolean rv;
	GError *err;

	err = NULL;
	msg = mu_msg_new_from_file (path, NULL, &err);
	if (!msg) {
		if (err) {
			g_warning ("error: %s", err->message);
			g_error_free (err);
		}
		return FALSE;
	}

	/* note, mu_cmd_extract already checks whether what's in opts
	 * is somewhat, so no need for extensive checking here */

	/* should we save some explicit parts? */
	if (opts->parts)
		rv = save_numbered_parts (msg, opts);
	else if (filename)
		rv = save_part_with_filename (msg, filename, opts);
	else if (opts->save_attachments)  /* all attachments */
		rv = save_certain_parts (msg, TRUE,
					 opts->targetdir, opts->overwrite,
					 opts->play);
	else if (opts->save_all)  /* all parts */
		rv = save_certain_parts (msg, FALSE,
					 opts->targetdir, opts->overwrite,
					 opts->play);
	else
		g_assert_not_reached ();

	mu_msg_unref (msg);

	return rv;
}

#define color_maybe(C)	do{ if (color) fputs ((C),stdout);}while(0)

static void
each_part_show (MuMsg *msg, MuMsgPart *part, gboolean color)
{
	/* index */
	g_print ("  %u ", part->index);

	/* filename */
	color_maybe (MU_COLOR_GREEN);
	mu_util_fputs_encoded (part->file_name ? part->file_name : "<none>",
			       stdout);
	/* content-type */
	color_maybe (MU_COLOR_BLUE);
	mu_util_print_encoded (
		" %s/%s ",
		part->type ? part->type : "<none>",
		part->subtype ? part->subtype : "<none>");

	/* disposition */
	color_maybe (MU_COLOR_MAGENTA);
	mu_util_print_encoded (
		"[%s]",	part->disposition ? part->disposition : "<none>");


	/* size */
	if (part->size > 0) {
		color_maybe (MU_COLOR_CYAN);
		g_print (" (%s)", mu_str_size_s (part->size));
	}

	color_maybe (MU_COLOR_DEFAULT);
	fputs ("\n", stdout);
}


static gboolean
show_parts (const char* path, MuConfig *opts, GError **err)
{
	MuMsg* msg;

	msg = mu_msg_new_from_file (path, NULL, err);
	if (!msg)
		return FALSE;

	g_print ("MIME-parts in this message:\n");

	mu_msg_part_foreach
		(msg, FALSE, (MuMsgPartForeachFunc)each_part_show,
		 GUINT_TO_POINTER(!opts->nocolor));

	mu_msg_unref (msg);

	return TRUE;

}


static gboolean
check_params (MuConfig *opts)
{
	size_t param_num;

	param_num = mu_config_param_num (opts);

	if (param_num < 2) {
		g_warning ("usage: mu extract [options] <file> [<pattern>]");
		return FALSE;
	}

	if (opts->save_attachments || opts->save_all)
		if (opts->parts || param_num == 3) {
			g_warning ("--save-attachments --save-all don't accept "
				   "a filename pattern or --parts");
			return FALSE;
		}

	if (opts->save_attachments && opts->save_all) {
		g_warning ("only one of --save-attachments and"
			   " --save-all is allowed");
		return FALSE;
	}

	return TRUE;
}

MuError
mu_cmd_extract (MuConfig *opts, GError **err)
{
	int rv;

	g_return_val_if_fail (opts, MU_ERROR_INTERNAL);
	g_return_val_if_fail (opts->cmd == MU_CONFIG_CMD_EXTRACT,
			      MU_ERROR_INTERNAL);

	if (!check_params (opts)) {
		g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_IN_PARAMETERS,
				     "error in parameters");
		return MU_ERROR_IN_PARAMETERS;
	}

	if (!opts->params[2] && !opts->parts &&
	    !opts->save_attachments && !opts->save_all)
		rv = show_parts (opts->params[1], opts, err); /* show, don't save */
	else {
		rv = mu_util_check_dir(opts->targetdir, FALSE, TRUE);
		if (!rv)
			g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_FILE_CANNOT_WRITE,
				     "target '%s' is not a writable directory",
				     opts->targetdir);
		else
			rv = save_parts (opts->params[1],
					 opts->params[2],
					 opts); /* save */
	}

	return rv ? MU_OK : MU_ERROR;
}
