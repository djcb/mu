/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

/*
** Copyright (C) 2010-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
save_part (MuMsg *msg, const char *targetdir, guint partidx, MuConfig *opts)
{
	GError *err;
	gchar *filepath;
	gboolean rv;
	MuMsgOptions msgopts;

	err = NULL;
	rv = FALSE;

	msgopts = mu_config_get_msg_options (opts);

	filepath = mu_msg_part_get_path (msg, msgopts, targetdir, partidx, &err);
	if (!filepath)
		goto exit;

	if (!mu_msg_part_save (msg, msgopts, filepath, partidx, &err))
		goto exit;

	if (opts->play)
		rv = mu_util_play (filepath, TRUE, FALSE, &err);
	else
		rv = TRUE;
exit:
	if (err) {
		g_warning ("error with MIME-part: %s", err->message);
		g_clear_error (&err);
	}

	g_free (filepath);
	return rv;
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

		if (!save_part (msg, opts->targetdir, idx, opts)) {
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
	MuMsgOptions msgopts;

	msgopts = mu_config_get_msg_options (opts);

	/* 'anchor' the pattern with '^...$' if not already */
	rx = anchored_regex (pattern);
	if (!rx)
		return FALSE;

	lst = mu_msg_find_files (msg, msgopts, rx);
	g_regex_unref (rx);
	if (!lst) {
		g_warning ("no matching attachments found");
		return FALSE;
	}

	for (cur = lst, rv = TRUE; cur; cur = g_slist_next (cur))
		rv = rv && save_part (msg, opts->targetdir,
				      GPOINTER_TO_UINT(cur->data), opts);
	g_slist_free (lst);

	return rv;
}

struct _SaveData {
	gboolean		 result;
	guint			 saved_num;
	MuConfig                 *opts;
};
typedef struct _SaveData	 SaveData;


static gboolean
ignore_part (MuMsg *msg, MuMsgPart *part, SaveData *sd)
{
	/* something went wrong somewhere; stop */
	if (!sd->result)
		return TRUE;

	/* only consider leaf parts */
	if (!(part->part_type & MU_MSG_PART_TYPE_LEAF))
		return TRUE;

	/* filter out non-attachments? */
	if (!sd->opts->save_all &&
	    !(mu_msg_part_maybe_attachment (part)))
		return TRUE;

	return FALSE;
}


static void
save_part_if (MuMsg *msg, MuMsgPart *part, SaveData *sd)
{
	gchar *filepath;
	gboolean rv;
	GError *err;
	MuMsgOptions msgopts;

	if (ignore_part (msg, part, sd))
		return;

	rv	 = FALSE;
	err      = NULL;

	msgopts = mu_config_get_msg_options (sd->opts);
	filepath = mu_msg_part_get_path (msg, msgopts,
					 sd->opts->targetdir,
					 part->index, &err);
	if (!filepath)
		goto exit;

	if (!mu_msg_part_save (msg, msgopts, filepath, part->index, &err))
		goto exit;

	if (sd->opts->play)
		rv = mu_util_play (filepath, TRUE, FALSE, &err);
	else
		rv = TRUE;

	++sd->saved_num;
exit:
	if (err)
		g_warning ("error saving MIME part: %s", err->message);

	g_free (filepath);
	g_clear_error (&err);

	sd->result = rv;

}

static gboolean
save_certain_parts (MuMsg *msg, MuConfig *opts)
{
	SaveData sd;
	MuMsgOptions msgopts;

	sd.result	    = TRUE;
	sd.saved_num        = 0;
	sd.opts             = opts;

	msgopts = mu_config_get_msg_options (opts);
	mu_msg_part_foreach (msg, msgopts,
			     (MuMsgPartForeachFunc)save_part_if, &sd);

	if (sd.saved_num == 0) {
		g_warning ("no %s extracted from this message",
			   opts->save_attachments ? "attachments" : "parts");
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
	else
		rv = save_certain_parts (msg, opts);

	mu_msg_unref (msg);

	return rv;
}

#define color_maybe(C)	do{ if (color) fputs ((C),stdout);}while(0)

static const char*
disp_str (MuMsgPartType ptype)
{
	if (ptype & MU_MSG_PART_TYPE_ATTACHMENT)
		return "attach";
	if (ptype & MU_MSG_PART_TYPE_INLINE)
		return "inline";
	return "<none>";
}

static void
each_part_show (MuMsg *msg, MuMsgPart *part, gboolean color)
{
	/* index */
	g_print ("  %u ", part->index);

	/* filename */
	color_maybe (MU_COLOR_GREEN); {
		gchar *fname;
		fname = mu_msg_part_get_filename (part, FALSE);
		mu_util_fputs_encoded (fname ? fname : "<none>", stdout);
		g_free (fname);
	}
	/* content-type */
	color_maybe (MU_COLOR_BLUE);
	mu_util_print_encoded (
		" %s/%s ",
		part->type ? part->type : "<none>",
		part->subtype ? part->subtype : "<none>");

	/* /\* disposition *\/ */
	color_maybe (MU_COLOR_MAGENTA);
	mu_util_print_encoded ("[%s]",	disp_str(part->part_type));

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
	MuMsg *msg;
	MuMsgOptions msgopts;

	msg = mu_msg_new_from_file (path, NULL, err);
	if (!msg)
		return FALSE;

	msgopts = mu_config_get_msg_options (opts);

	/* TODO: update this for crypto */
	g_print ("MIME-parts in this message:\n");
	mu_msg_part_foreach
		(msg, msgopts,
		 (MuMsgPartForeachFunc)each_part_show,
		 GUINT_TO_POINTER(!opts->nocolor));

	mu_msg_unref (msg);

	return TRUE;

}


static gboolean
check_params (MuConfig *opts, GError **err)
{
	size_t param_num;

	param_num = mu_config_param_num (opts);

	if (param_num < 2) {
		mu_util_g_set_error
			(err, MU_ERROR_IN_PARAMETERS,
			 "parameters missing");
		return FALSE;
	}

	if (opts->save_attachments || opts->save_all)
		if (opts->parts || param_num == 3) {
			mu_util_g_set_error
				(err, MU_ERROR_IN_PARAMETERS,
				 "--save-attachments and --save-all don't "
				 "accept a filename pattern or --parts");
			return FALSE;
		}

	if (opts->save_attachments && opts->save_all) {
		mu_util_g_set_error
			(err, MU_ERROR_IN_PARAMETERS,
			 "only one of --save-attachments and"
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

	if (!check_params (opts, err))
		return MU_ERROR_IN_PARAMETERS;

	if (!opts->params[2] && !opts->parts &&
	    !opts->save_attachments && !opts->save_all)
		/* show, don't save */
		rv = show_parts (opts->params[1], opts, err);
	else {
		rv = mu_util_check_dir(opts->targetdir, FALSE, TRUE);
		if (!rv)
			mu_util_g_set_error
				(err, MU_ERROR_FILE_CANNOT_WRITE,
				 "target '%s' is not a writable directory",
				 opts->targetdir);
		else
			rv = save_parts (opts->params[1],
					 opts->params[2],
					 opts); /* save */
	}

	return rv ? MU_OK : MU_ERROR;
}
