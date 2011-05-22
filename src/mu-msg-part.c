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

#include <string.h>
#include <unistd.h>

#include "mu-util.h"
#include "mu-msg-priv.h"
#include "mu-msg-part.h"

struct _FindPartData {
	guint			 idx, wanted_idx;
	GMimeObject		*part;
};
typedef struct _FindPartData FindPartData;


static void
find_part_cb (GMimeObject *parent, GMimeObject *part, FindPartData *fpdata)
{
	if (fpdata->part || fpdata->wanted_idx != fpdata->idx++)
		return; /* not yet found */

	fpdata->part = part;
}

static GMimeObject*
find_part (MuMsg* msg, guint partidx)
{
	FindPartData fpdata;

	fpdata.wanted_idx = partidx;
	fpdata.idx = 0;
	fpdata.part = NULL;
		
	g_mime_message_foreach (msg->_file->_mime_msg,
				(GMimeObjectForeachFunc)find_part_cb,
				&fpdata);
	return fpdata.part;
}

struct _PartData {
	MuMsg			*_msg;
	unsigned		_idx;
	MuMsgPartForeachFunc	_func;
	gpointer		_user_data;
};
typedef struct _PartData PartData;


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
	
	pdata->_func(pdata->_msg, &pi, pdata->_user_data);	
}

void
mu_msg_part_foreach (MuMsg *msg, MuMsgPartForeachFunc func,
		     gpointer user_data)
{
	PartData pdata;
	
	g_return_if_fail (msg);
	g_return_if_fail (GMIME_IS_OBJECT(msg->_file->_mime_msg));

	pdata._msg       = msg;
	pdata._idx       = 0;
	pdata._func	 = func;
	pdata._user_data = user_data;
	
	g_mime_message_foreach (msg->_file->_mime_msg,
				(GMimeObjectForeachFunc)part_foreach_cb,
				&pdata);
}


static gboolean
write_to_stream (GMimeObject *part, int fd)
{
	GMimeStream *stream;
	GMimeDataWrapper *wrapper;
	gboolean rv;
		
	stream = g_mime_stream_fs_new (fd);
	if (!GMIME_IS_STREAM(stream)) {
		g_critical ("%s: failed to create stream",__FUNCTION__);
		return FALSE;
	}
	g_mime_stream_fs_set_owner (GMIME_STREAM_FS(stream), FALSE);
		
	wrapper = g_mime_part_get_content_object (GMIME_PART(part));
	if (!GMIME_IS_DATA_WRAPPER(wrapper)) {
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


gboolean
save_part (GMimeObject *part, const char *fullpath,
	   gboolean overwrite, gboolean use_existing, GError **err)
{
	int fd;
	gboolean rv;
		
	/* don't try to overwrite when we already have it; useful when
	 * you're sure it's not a different file with the same name */
	if (use_existing && access (fullpath, F_OK) == 0)
		return TRUE;
		
	/* ok, try to create the file */
	fd = mu_util_create_writeable_fd (fullpath, 0600, overwrite);
	if (fd == -1) {
		g_set_error (err, 0, MU_ERROR_FILE,
			     "could not open '%s' for writing: %s",
			     fullpath, errno ? strerror(errno) : "error");
		return FALSE;
	}		

	rv = write_to_stream (part, fd);
	if (close (fd) != 0) {
		g_set_error (err, 0, MU_ERROR_FILE,
			     "could not close '%s': %s",
			     fullpath, errno ? strerror(errno) : "error");
		return FALSE;
	}

	return TRUE;
}


gchar*
mu_msg_part_filepath (MuMsg *msg, const char* targetdir, guint partidx)
{
	char *fname, *filepath;
	GMimeObject* part;

	part = find_part (msg, partidx);
	if (!part) {
		g_warning ("%s: cannot find part %u", __FUNCTION__, partidx);
		return NULL;
	}
		
	/* the easy case: the part has a filename */
	fname = (gchar*)g_mime_part_get_filename (GMIME_PART(part));
	if (fname) /* security: don't include any directory components... */
		fname = g_path_get_basename (fname);
	else
		fname = g_strdup_printf ("%x-part-%u",
					 g_str_hash (mu_msg_get_path (msg)),
					 partidx);

	filepath = g_build_path (G_DIR_SEPARATOR_S, targetdir ? targetdir : "",
				 fname, NULL);
	g_free (fname);

	return filepath;
}



gchar*
mu_msg_part_filepath_cache (MuMsg *msg, guint partid)
{
	char *dirname, *filepath;
	const char* path;
		
	g_return_val_if_fail (msg, NULL);
		
	path = mu_msg_get_path (msg);
	if (!path)
		return NULL;

	/* g_compute_checksum_for_string may be better, but requires
	 * rel. new glib (2.16) */
        dirname = g_strdup_printf ("%s%c%x%c%u",
				   mu_util_cache_dir(), G_DIR_SEPARATOR,
				   g_str_hash (path), G_DIR_SEPARATOR,
				   partid);
		
	if (!mu_util_create_dir_maybe (dirname, 0700)) {
		g_free (dirname);
		return NULL;
	}

	filepath = mu_msg_part_filepath (msg, dirname, partid);
	g_free (dirname);
	if (!filepath) 
		g_warning ("%s: could not get filename", __FUNCTION__);
	
	return filepath;
}


gboolean
mu_msg_part_save (MuMsg *msg, const char *fullpath, guint partidx,
		  gboolean overwrite, gboolean use_cached, GError **err)
{
	GMimeObject *part;
		
	g_return_val_if_fail (msg, FALSE);
	g_return_val_if_fail (fullpath, FALSE);
	g_return_val_if_fail (!overwrite||!use_cached, FALSE);
		
	part = find_part (msg, partidx);
	if (!GMIME_IS_PART(part)) {
		g_set_error (err, 0, MU_ERROR_GMIME,
			     "cannot find part %u", partidx);
		return FALSE;
	}
				
	if (!save_part (part, fullpath, overwrite, use_cached, err))
		return FALSE;

	return TRUE;
}

typedef gboolean (*MatchFunc) (GMimeObject *part, gpointer data);

struct _MatchData {
	MatchFunc _matcher;
	gpointer  _user_data;
	gint      _idx, _found_idx;
};
typedef struct _MatchData MatchData;

static void
part_match_foreach_cb (GMimeObject *parent, GMimeObject *part, MatchData *mdata)
{
	if (mdata->_found_idx < 0)
		if (mdata->_matcher (part, mdata->_user_data))
			mdata->_found_idx = mdata->_idx;
	
	++mdata->_idx;
}

static int
msg_part_find_idx (GMimeMessage *msg, MatchFunc func, gpointer user_data)
{
	MatchData mdata;
	
	g_return_val_if_fail (msg, -1);
	g_return_val_if_fail (GMIME_IS_MESSAGE(msg), -1);
	
	mdata._idx       = 0;
	mdata._found_idx = -1;
	mdata._matcher   = func;
	mdata._user_data = user_data;
	
	g_mime_message_foreach (msg,
				(GMimeObjectForeachFunc)part_match_foreach_cb,
				&mdata);

	return mdata._found_idx;
}


static gboolean
match_content_id (GMimeObject *part, const char *cid)
{
	return g_strcmp0 (g_mime_object_get_content_id (part),
			  cid) == 0 ? TRUE : FALSE;
}

int
mu_msg_part_find_cid (MuMsg *msg, const char* sought_cid)
{
	const char* cid;
		
	g_return_val_if_fail (msg, -1);
	g_return_val_if_fail (sought_cid, -1);
		
	cid = g_str_has_prefix (sought_cid, "cid:") ?
		sought_cid + 4 : sought_cid; 
		
	return msg_part_find_idx (msg->_file->_mime_msg,
				  (MatchFunc)match_content_id,
				  (gpointer)cid);
}

struct _MatchData2 {
	GSList       *_lst;
	const GRegex *_rx;
	guint         _idx;
};
typedef struct _MatchData2 MatchData2;


static void
match_filename_rx (GMimeObject *parent, GMimeObject *part, MatchData2 *mdata)
{
	const char *fname;
	
	if (!GMIME_IS_PART(part))
		goto leave;
	
	fname = g_mime_part_get_filename (GMIME_PART(part));
	if (!fname)
		goto leave;
	
	if (g_regex_match (mdata->_rx, fname, 0, NULL))
		mdata->_lst = g_slist_prepend (mdata->_lst,
					       GUINT_TO_POINTER(mdata->_idx));
leave:
	++mdata->_idx;
}


GSList*
mu_msg_part_find_files (MuMsg *msg, const GRegex *pattern)
{
	MatchData2 mdata;
	
	g_return_val_if_fail (msg, NULL);
	g_return_val_if_fail (pattern, NULL);

	mdata._lst = NULL;
	mdata._rx  = pattern;
	mdata._idx = 0;
	
	g_mime_message_foreach (msg->_file->_mime_msg,
				(GMimeObjectForeachFunc)match_filename_rx,
				&mdata);
	return mdata._lst;
}



gboolean
mu_msg_part_looks_like_attachment (MuMsgPart *part, gboolean include_inline)
{
	g_return_val_if_fail (part, FALSE);

	if (!part->disposition)
		return FALSE;

	if (g_ascii_strcasecmp (part->disposition,
				GMIME_DISPOSITION_ATTACHMENT) == 0)
		return TRUE;

	if (include_inline &&
	    g_ascii_strcasecmp (part->disposition,
				GMIME_DISPOSITION_INLINE) == 0)
		return TRUE;

	return FALSE;
}
