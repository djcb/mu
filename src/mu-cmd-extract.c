/*
** Copyright (C) 2010-2011 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-msg.h"
#include "mu-msg-part.h"
#include "mu-cmd.h"
#include "mu-util.h"
#include "mu-str.h"

static gboolean
save_part (MuMsg *msg, const char *targetdir, guint partidx, gboolean overwrite,
	   gboolean play)
{
	gchar *filepath;
	
	filepath = mu_msg_part_filepath (msg, targetdir, partidx);
	if (!filepath) {
		g_warning ("%s: failed to get filepath", __FUNCTION__);
		return FALSE;
	}
	
	if (!mu_msg_part_save (msg, filepath, partidx, overwrite, FALSE)) {
		g_warning ("%s: failed to save MIME-part %d at %s",
			   __FUNCTION__, partidx, filepath);
		g_free (filepath);
		return FALSE;
	}

	if (play)
		mu_util_play (filepath, TRUE, FALSE);

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
	
	if (ignore_part (msg, part, sd))
		return;

	rv	 = FALSE;
	filepath = NULL;
	
	filepath = mu_msg_part_filepath (msg, sd->targetdir, part->index);
	if (!filepath) 
		goto leave;
	
	if (!mu_msg_part_save (msg, filepath, part->index,
			       sd->overwrite, FALSE))
		goto leave;
	
	if (sd->play &&  !mu_util_play (filepath, TRUE, FALSE))
		goto leave;

	rv = TRUE;
	++sd->saved_num;

leave:
	if (!sd->result) 
		g_warning ("failed to save/play MIME-part %u to %s",
			   part->index, filepath ? filepath : "<none>");
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
	
	mu_msg_part_foreach (msg,
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
save_parts (const char *path, MuConfig *opts)
{	
	MuMsg* msg;
	gboolean rv;
	GError *err;
	
	err = NULL;
	msg = mu_msg_new (path, NULL, &err);
	if (!msg) {
		g_warning ("error: %s", err->message);
		g_error_free (err);
		return FALSE;
	}
		
	/* note, mu_cmd_extract already checks whether what's in opts
	 * is somewhat, so no need for extensive checking here */
	
	/* should we save some explicit parts? */
	if (opts->parts)
		rv = save_numbered_parts (msg, opts);	
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


static void
each_part_show (MuMsg *msg, MuMsgPart *part, gpointer user_data)
{
	g_print ("  %u %s %s/%s [%s]\n",
		 part->index,
		 part->file_name ? part->file_name : "<none>",
		 part->type ? part->type : "",
		 part->subtype ? part->subtype : "",
		 part->disposition ? part->disposition : "<none>");
}


static gboolean
show_parts (const char* path, MuConfig *opts)
{
	MuMsg* msg;
	GError *err;

	err = NULL;
	msg = mu_msg_new (path, NULL, &err);
	if (!msg) {
		g_warning ("error: %s", err->message);
		g_error_free (err);
		return FALSE;
	}

	g_print ("MIME-parts in this message:\n");
	mu_msg_part_foreach (msg, each_part_show, NULL);
	mu_msg_unref (msg);
	
	return TRUE;
	
}

static gboolean
check_params (MuConfig *opts)
{
	if (!opts->params[1] || opts->params[2]) {
		g_warning ("usage: mu extract [options] <file>");
		return FALSE;
	}
	
	if (opts->save_attachments && opts->save_all) {
		g_warning ("only one of --save-attachments and"
			   " --save-all is allowed");
		return FALSE;
	}
	
	if ((opts->save_attachments || opts->save_all) && opts->parts) {
		g_warning ("with --save-attachments/--save-all, " 
			   "parts should not be specified");
		return FALSE;
	}

	return TRUE;
}

MuExitCode
mu_cmd_extract (MuConfig *opts)
{
	int rv;
	
	g_return_val_if_fail (opts, MU_EXITCODE_ERROR);
	g_return_val_if_fail (opts->cmd == MU_CONFIG_CMD_EXTRACT,
			      MU_EXITCODE_ERROR);

	if (!check_params (opts))
		return MU_EXITCODE_ERROR;
	
	if (!opts->parts &&
	    !opts->save_attachments &&
	    !opts->save_all)  /* show, don't save */
		rv = show_parts (opts->params[1], opts);
	else {
		rv = mu_util_check_dir(opts->targetdir, FALSE, TRUE);
		if (!rv)
			g_warning ("target '%s' is not a writable directory",
				   opts->targetdir);
		else
			rv = save_parts (opts->params[1], opts); /* save */
	}
		
	return rv ? MU_EXITCODE_OK : MU_EXITCODE_ERROR;
}
