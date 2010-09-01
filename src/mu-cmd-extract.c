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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include <stdlib.h>

#include "mu-msg.h"
#include "mu-msg-part.h"
#include "mu-msg-str.h"
#include "mu-cmd.h"

static gboolean
save_numbered_parts (MuMsg *msg, MuConfigOptions *opts)
{
	gboolean rv;
	char **parts, **cur;

	parts = g_strsplit (opts->parts, ",", 0);
	
	for (rv = TRUE, cur = parts; cur && *cur; ++cur) {

		int idx;
		char *endptr;

		idx = (int)strtol (*cur, &endptr, 10);
		if (idx < 0 || *cur == endptr) {
			g_warning ("invalid MIME-part index '%s'", *cur);
			rv = FALSE;
			break;
		}

		if  (!mu_msg_mime_part_save
		     (msg, idx, opts->targetdir, opts->overwrite)) {
			g_warning ("failed to save MIME-part %d", idx);
			rv = FALSE;
			break;
		}			
	}

	g_strfreev (parts);
	
	return rv;
}

struct _SaveData {
	MuMsg			*msg;
	gboolean		 attachments_only;
	gboolean		 result;
	guint			 saved_num;
	const gchar*		 targetdir;
	gboolean		 overwrite;
};
typedef struct _SaveData	 SaveData;


static void
save_part_if (MuMsgPart *part, SaveData *sd)
{
	/* something went wrong somewhere; stop */
	if (!sd->result)
		return;
	
	/* filter out non-attachments if only want attachments. Note,
	 * the attachment check may be a bit too strict */
	if (sd->attachments_only) 
		if (!part->disposition ||
		    g_ascii_strcasecmp (part->disposition, "attachment") != 0)
			return;

	/* ignore multiparts */
	if (part->type && 
	    g_ascii_strcasecmp (part->type, "multipart") == 0)
		return;
	
	sd->result = mu_msg_mime_part_save (sd->msg, part->index,
					    sd->targetdir, sd->overwrite);
	if (!sd->result) 
		g_warning ("failed to save MIME-part %u", part->index);
	else
		++sd->saved_num;
	
}

static gboolean
save_certain_parts (MuMsg *msg, gboolean attachments_only, const gchar *targetdir,
		    gboolean overwrite)
{
	SaveData sd;

	sd.msg		    = msg;
	sd.result	    = TRUE;
	sd.saved_num        = 0;
	sd.attachments_only = attachments_only;
	sd.overwrite	    = overwrite;
	sd.targetdir	    = targetdir;
	
	mu_msg_msg_part_foreach (msg,
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
save_parts (const char *path, MuConfigOptions *opts)
{	
	MuMsg* msg;
	gboolean rv;
	
	msg = mu_msg_new (path, NULL);
	if (!msg)
		return FALSE;

	/* note, mu_cmd_extract already checks whether what's in opts
	 * is somewhat, so no need for extensive checking here */
	
	/* should we save some explicit parts? */
	if (opts->parts)
		rv = save_numbered_parts (msg, opts);	
	else if (opts->save_attachments)  /* all attachments */
		rv = save_certain_parts (msg, TRUE,
					 opts->targetdir, opts->overwrite);
	else if (opts->save_all)  /* all parts */
		rv = save_certain_parts (msg, FALSE,
					 opts->targetdir, opts->overwrite);
	else
		g_assert_not_reached ();
		
	mu_msg_destroy (msg);
	
	return rv;
}


static void
each_part_show (MuMsgPart *part, gpointer user_data)
{
	g_print ("  %u %s %s/%s [%s]\n",
		 part->index,
		 part->file_name ? part->file_name : "<none>",
		 part->type ? part->type : "",
		 part->subtype ? part->subtype : "",
		 part->disposition ? part->disposition : "<none>");
}


static gboolean
show_parts (const char* path, MuConfigOptions *opts)
{
	MuMsg* msg;
	
	msg = mu_msg_new (path, NULL);
	if (!msg)
		return FALSE;

	g_print ("MIME-parts in this message:\n");
	mu_msg_msg_part_foreach (msg, each_part_show, NULL);

	mu_msg_destroy (msg);
	
	return TRUE;
	
}

gboolean
mu_cmd_extract (MuConfigOptions *opts)
{
	gboolean rv;
	
	g_return_val_if_fail (opts, FALSE);
	g_return_val_if_fail (mu_cmd_equals (opts, "extract"), FALSE);
	
	if (!opts->params[1]) {
		g_warning ("missing mail file to extract something from");
		return FALSE;
	}
	
	if (opts->save_attachments && opts->save_all) {
		g_warning ("only one of --save-attachments and --save-all is allowed");
		return FALSE;
	}
	
	if ((opts->save_attachments || opts->save_all) && opts->parts) {
		g_warning ("with --save-attachments/--save-all, parts should not be specified");
		return FALSE;
	}

	mu_msg_init();

	if (!opts->parts &&
	    !opts->save_attachments &&
	    !opts->save_all)  /* show, don't save */
		rv = show_parts (opts->params[1], opts);
	else
		rv = save_parts (opts->params[1], opts); /* save */

 	mu_msg_uninit();
	
	return rv;
}
