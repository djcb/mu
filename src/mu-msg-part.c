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

#include <string.h>
#include <unistd.h>

#include "mu-util.h"
#include "mu-msg-priv.h"
#include "mu-msg-part.h"

struct _PartData {
	unsigned		_idx;
	MuMsgPartForeachFunc	_func;
	gpointer		_user_data;
};
typedef struct _PartData	PartData;


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
	pdata._func	 = func;
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
	gboolean     result;
};
typedef struct _SavePartData SavePartData;


static gboolean
save_part (GMimeObject *part, const char *filename,
	   const char *targetdir, gboolean overwrite)
{
	int fd, rv;
	GMimeDataWrapper *wrapper;
	GMimeStream *stream;

	rv = TRUE; 
	fd = mu_util_create_writeable_fd (filename, targetdir,
					  overwrite);
	if (fd == -1) {
		g_warning ("error saving file %s%s %s",
			   filename, errno != 0 ? ":" : "",
			   errno != 0 ? strerror(errno) : "");
		return FALSE;
	}

	stream = g_mime_stream_fs_new (fd);
	if (!stream) {
		g_warning ("%s: failed to create stream", __FUNCTION__);
		close (fd);
		return FALSE;
	}
	
	g_mime_stream_fs_set_owner (GMIME_STREAM_FS(stream),
				    TRUE); /* GMimeStream will close fd */
	
	wrapper = g_mime_part_get_content_object (GMIME_PART(part));
	if (!wrapper) {
		g_object_unref (G_OBJECT(stream));
		g_warning ("%s: failed to create wrapper", __FUNCTION__);
		return FALSE;
	}
	
	rv = g_mime_data_wrapper_write_to_stream (wrapper, stream);
	g_object_unref (G_OBJECT(stream));

	return rv == -1 ? FALSE : TRUE;
}

	

static void
part_foreach_save_cb (GMimeObject *parent, GMimeObject *part,
		      SavePartData *spd)
{
	const gchar* filename;
	
	/* did we find the right part yet? */
	if (spd->result || spd->wanted_idx != spd->idx++)
		return;
	
	if (!GMIME_IS_PART(part))
		return;
	
	filename = g_mime_part_get_filename (GMIME_PART(part));
	if (filename) {
		spd->result = save_part (part, filename,
					 spd->targetdir,
					 spd->overwrite);
	} else
		spd->result = FALSE;
}



gboolean
mu_msg_mime_part_save (MuMsg *msg, int wanted_idx,
		       const char *targetdir, gboolean overwrite)
{
	SavePartData	spd;
	spd.idx	       = 0;
	spd.wanted_idx = wanted_idx;
	spd.targetdir  = targetdir;
	spd.overwrite  = overwrite;
	spd.stream     = FALSE;
	spd.result     = FALSE;

	g_mime_message_foreach (msg->_mime_msg,
				(GMimeObjectForeachFunc)part_foreach_save_cb,
				&spd);	
	return spd.result;
}

