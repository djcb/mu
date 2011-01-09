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

#include <string.h>
#include <unistd.h>

#include "mu-util.h"
#include "mu-msg-priv.h"
#include "mu-msg-part.h"

struct _PartData {
		unsigned				_idx;
		MuMsgPartForeachFunc	_func;
		gpointer				_user_data;
};
typedef struct _PartData		PartData;


static void
part_foreach_cb (GMimeObject *parent, GMimeObject *part, PartData *pdata)
{
		GMimeContentType *ct;
		MuMsgPart pi;

		memset (&pi, 0, sizeof pi);
		pi.index       = pdata->_idx++;
		pi.content_id  = (char*)g_mime_object_get_content_id (part);

		ct = g_mime_object_get_content_type (part);
		if (GMIME_IS_CONTENT_TYPE(ct)) {
				pi.type	   = (char*)g_mime_content_type_get_media_type (ct);
				pi.subtype = (char*)g_mime_content_type_get_media_subtype (ct);	
		}

		if (GMIME_IS_PART(part)) {
				pi.disposition = (char*)g_mime_object_get_disposition (part);
				pi.file_name   = (char*)g_mime_part_get_filename (GMIME_PART(part));
		}
	
		pdata->_func(&pi, pdata->_user_data);	
}



void
mu_msg_msg_part_foreach (MuMsg *msg,
						 MuMsgPartForeachFunc func,
						 gpointer user_data)
{
		PartData pdata;
	
		g_return_if_fail (msg);
		g_return_if_fail (GMIME_IS_OBJECT(msg->_mime_msg));

		pdata._idx       = 0;
		pdata._func		 = func;
		pdata._user_data = user_data;
	
		g_mime_message_foreach (msg->_mime_msg,
								(GMimeObjectForeachFunc)part_foreach_cb,
								&pdata);
}



struct _SavePartData {
		guint        idx, wanted_idx;
		const gchar* targetdir;
		gboolean     overwrite;
		gboolean     stream;
		guint        cookie;
		gboolean     result;
		gboolean     play;
};
typedef struct _SavePartData SavePartData;


static gboolean
write_to_stream (GMimeObject *part, int fd)
{
		GMimeStream *stream;
		GMimeDataWrapper *wrapper;
		gboolean rv;
		
		stream = g_mime_stream_fs_new (fd);
		if (!stream) {
				g_critical ("%s: failed to create stream",__FUNCTION__);
				return FALSE;
		}
		g_mime_stream_fs_set_owner (GMIME_STREAM_FS(stream), FALSE);
		
		wrapper = g_mime_part_get_content_object (GMIME_PART(part));
		if (!wrapper) {
				g_critical ("%s: failed to create wrapper", __FUNCTION__);
				g_object_unref (stream);
				return FALSE;
		}
		g_object_ref (part); /* FIXME: otherwise, the unrefs below
							  * give errors...*/
		
		rv = g_mime_data_wrapper_write_to_stream (wrapper, stream);
		if (!rv)
				g_critical ("%s: failed to write to stream", __FUNCTION__);
		
		g_object_unref (wrapper);
		g_object_unref (stream);
		
		return rv;
}


static gboolean
save_part (GMimeObject *part, const char *filename,
		   const char *targetdir, gboolean overwrite, gboolean tryplay)
{
		int fd;
		char *fullpath;
		gboolean rv;
		
		fullpath = g_strdup_printf ("%s%s%s",
									targetdir ? targetdir : "",
									targetdir ? G_DIR_SEPARATOR_S : "",
									filename);
		
		fd = mu_util_create_writeable_fd (fullpath, 0600, overwrite);
		if (fd == -1) {
				g_free (fullpath);
				return FALSE;
		}
		
		rv = write_to_stream (part, fd);
		if (close (fd) != 0)
				g_warning ("%s: failed to close %s: %s", __FUNCTION__,
						   fullpath, strerror(errno));
		
		if (rv && tryplay) {
				rv = mu_util_play (fullpath);
				if (!rv)
						g_warning ("%s: failed to play %s", __FUNCTION__,
								   fullpath);
		}
		
		return rv;
}

	

static void
part_foreach_save_cb (GMimeObject *parent, GMimeObject *part,
					  SavePartData *spd)
{
		const gchar* filename;
	
		/* did we find the right part yet? */
		if (spd->result || spd->wanted_idx != spd->idx++)
				return;
	
		if (!GMIME_IS_PART(part)) /* ie., multiparts are ignored */
				return;
	
		filename = g_mime_part_get_filename (GMIME_PART(part));
		if (filename) {
				spd->result = save_part (part, filename,
										 spd->targetdir,
										 spd->overwrite,
										 spd->play);				
		} else { /* make up a filename */
				gchar *my_filename;
				my_filename = g_strdup_printf ("%x-part-%u",
											   spd->cookie,
											   spd->wanted_idx);
				spd->result = save_part (part, my_filename,
										 spd->targetdir,
										 spd->overwrite,
										 spd->play);
				g_free (my_filename);
		}
}

gboolean
mu_msg_mime_part_save (MuMsg *msg, unsigned wanted_idx,
					   const char *targetdir, gboolean overwrite, gboolean play)
{
		SavePartData spd;
		const char *msgid;
	
		g_return_val_if_fail (msg, FALSE);
	
		spd.idx	       = 0;
		spd.wanted_idx = wanted_idx;
		spd.targetdir  = targetdir;
		spd.overwrite  = overwrite;
		spd.stream     = FALSE; /* not used yet */
		spd.result     = FALSE;
		spd.play       = play;
	
		/* get something fairly unique for building a filename;
		 * normally based on the message id, but if that is not
		 * available, a random number. basing it on the message id
		 * gives makes it easy to distinguish the parts of different
		 * messages */
		msgid = mu_msg_get_msgid (msg);
		spd.cookie = (guint)(msgid ? g_str_hash (msgid) : (guint)random());
	
		g_mime_message_foreach (msg->_mime_msg,
								(GMimeObjectForeachFunc)part_foreach_save_cb,
								&spd);
		return spd.result;
}

