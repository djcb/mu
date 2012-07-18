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
#include "mu-str.h"
#include "mu-msg-priv.h"
#include "mu-msg-part.h"

#ifdef BUILD_CRYPTO
#include "mu-msg-crypto.h"
#endif /*BUILD_CRYPTO*/


struct _FindPartData {
	guint			 idx, wanted_idx;
	GMimeObject		*part;
};
typedef struct _FindPartData FindPartData;


/* is this either a leaf part or an embedded message? */
static gboolean
is_part_or_message_part (GMimeObject *part)
{
	return GMIME_IS_PART(part) || GMIME_IS_MESSAGE_PART (part);
}


static void
find_part_cb (GMimeObject *parent, GMimeObject *part, FindPartData *fpdata)
{
	/* ignore other parts */
	if (!is_part_or_message_part (part))
		return;

	/* g_printerr ("%u Type-name: %s\n", */
	/* 	    fpdata->idx, G_OBJECT_TYPE_NAME((GObject*)part)); */

	if (fpdata->part || fpdata->wanted_idx != fpdata->idx++)
		return; /* not yet found */

	fpdata->part = part;
}

static GMimeObject*
find_part (MuMsg* msg, guint partidx)
{
	FindPartData fpdata;

	fpdata.wanted_idx = partidx;
	fpdata.idx	  = 0;
	fpdata.part	  = NULL;

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
	GMimePart               *_body_part;
	MuMsgPartOptions        _opts;
};
typedef struct _PartData PartData;


char*
mu_msg_part_get_text (MuMsgPart *self, gboolean *err)
{
	GMimeObject *mobj;

	g_return_val_if_fail (self && self->data, NULL);

	mobj = (GMimeObject*)self->data;

	if (GMIME_IS_PART(mobj))
		return mu_msg_mime_part_to_string ((GMimePart*)mobj, err);
	else if (GMIME_IS_MESSAGE(mobj)) {
		/* when it's an (embedded) message, the text is just
		 * of the message metadata, so we can index it.
		 */
		GString *data;
		GMimeMessage *msg;
		InternetAddressList *addresses;
		gchar *adrs;

		msg = (GMimeMessage*)mobj;
		data = g_string_sized_new (512); /* just a guess */

		g_string_append (data, g_mime_message_get_sender(msg));
		g_string_append_c (data, '\n');
		g_string_append (data, g_mime_message_get_subject(msg));
		g_string_append_c (data, '\n');

		addresses = g_mime_message_get_all_recipients (msg);
		adrs = internet_address_list_to_string (addresses, FALSE);
		g_object_unref(G_OBJECT(addresses));

		g_string_append (data, adrs);
		g_free (adrs);

		return g_string_free (data, FALSE);
	} else

	return NULL;
}


/* note: this will return -1 in case of error or if the size is
 * unknown */
static ssize_t
get_part_size (GMimePart *part)
{
	GMimeDataWrapper *wrapper;
	GMimeStream *stream;

	wrapper = g_mime_part_get_content_object (part);
	if (!wrapper)
		return -1;

	stream = g_mime_data_wrapper_get_stream (wrapper);
	if (!stream)
		return -1; /* no stream -> size is 0 */
	else
		return g_mime_stream_length (stream);

	/* NOTE: it seems we shouldn't unref stream/wrapper */
}



static gboolean
init_msg_part_from_mime_part (GMimePart *part, MuMsgPart *pi)
{
	const gchar *fname, *descr;
	GMimeContentType *ct;

	ct = g_mime_object_get_content_type ((GMimeObject*)part);
	if (GMIME_IS_CONTENT_TYPE(ct)) {
		pi->type    = (char*)g_mime_content_type_get_media_type (ct);
		pi->subtype = (char*)g_mime_content_type_get_media_subtype (ct);
	}

	pi->disposition = (char*)g_mime_object_get_disposition
		((GMimeObject*)part);

	fname	      = g_mime_part_get_filename (part);
	pi->file_name = fname ? mu_str_utf8ify (fname) : NULL;

	descr		= g_mime_part_get_content_description (part);
	pi->description = descr ? mu_str_utf8ify (descr) : NULL;

	pi->size        = get_part_size (part);
	pi->is_leaf     = TRUE;
	pi->is_msg      = FALSE;

	return TRUE;
}

static gchar*
get_filename_for_mime_message_part (GMimeMessage *mmsg)
{
	gchar *name, *cur;

	name = (char*)g_mime_message_get_subject (mmsg);
	if (!name)
		name = "message";

	name = g_strconcat (name, ".eml", NULL);

	/* remove slashes... */
	for (cur = name ; *cur; ++cur) {
		if (*cur == '/' || *cur == ' ' || *cur == ':')
			*cur = '-';
	}

	return name;
}


static gboolean
init_msg_part_from_mime_message_part (GMimeMessage *mmsg, MuMsgPart *pi)
{
	pi->disposition = GMIME_DISPOSITION_ATTACHMENT;

	/* pseudo-file name... */
	pi->file_name	= get_filename_for_mime_message_part (mmsg);
	pi->description = g_strdup ("message");

	pi->type        = "message";
	pi->subtype     = "rfc822";

	pi->size        = 0;
	pi->is_leaf     = TRUE;
	pi->is_msg      = TRUE;

	return TRUE;
}



static void
msg_part_free (MuMsgPart *pi)
{
	if (!pi)
		return;

	g_free (pi->file_name);
	g_free (pi->description);

#ifdef BUILD_CRYPTO
	mu_msg_part_free_sig_infos (pi->sig_infos);
#endif /*BUILD_CRYPTO*/
}


static void
check_signature_maybe (GMimeObject *parent, GMimeObject *mobj, MuMsgPart *pi,
		       MuMsgPartOptions opts)
{
#ifdef BUILD_CRYPTO

	GMimeContentType *ctype;
	GError *err;
	gboolean pkcs7;

	if (!GMIME_IS_MULTIPART_SIGNED (parent))
		return;

	ctype = g_mime_object_get_content_type (mobj);
	if (g_mime_content_type_is_type
	    (ctype, "application", "pgp-signature"))
		pkcs7 = FALSE;
	else if (g_mime_content_type_is_type
		 (ctype, "application", "x-pkcs7-signature"))
		pkcs7 = TRUE;
	else return; /* don't know how to handle other kinds */

	if (pkcs7)
		opts |= MU_MSG_PART_OPTION_USE_PKCS7; /* gpg is the default */

	err = NULL;
	pi->sig_infos = mu_msg_mime_sig_infos
		(GMIME_MULTIPART_SIGNED (parent), opts, &err);
	if (err) {
		g_warning ("error verifying signature: %s", err->message);
		g_clear_error (&err);
	}
#endif /*BUILD_CRYPTO*/

	return;
}


static void
part_foreach_cb (GMimeObject *parent, GMimeObject *mobj, PartData *pdata)
{
	MuMsgPart pi;
	gboolean rv;

	/* ignore non-leaf / message parts */
	if (!is_part_or_message_part (mobj))
		return;

	memset (&pi, 0, sizeof(pi));
	pi.index      = pdata->_idx++;
	pi.content_id = (char*)g_mime_object_get_content_id (mobj);
	pi.data       = (gpointer)mobj;
	/* check if this is the body part */
	pi.is_body    = ((void*)pdata->_body_part == (void*)mobj);

	if (GMIME_IS_PART(mobj))
		rv = init_msg_part_from_mime_part ((GMimePart*)mobj, &pi);
	else if (GMIME_IS_MESSAGE_PART(mobj)) {
		GMimeMessage *mmsg;
		mmsg = g_mime_message_part_get_message ((GMimeMessagePart*)mobj);
		if (!mmsg)
			return;
		rv = init_msg_part_from_mime_message_part (mmsg, &pi);
		if (rv && (pdata->_opts && MU_MSG_PART_OPTION_RECURSE_RFC822))
			/* NOTE: this screws up the counting (pdata->_idx) */
			g_mime_message_foreach /* recurse */
				(mmsg, (GMimeObjectForeachFunc)part_foreach_cb,
				 pdata);
	} else
		rv = FALSE; /* ignore */

	/* if we have crypto support, check the signature if there is one */
	if (pdata->_opts & MU_MSG_PART_OPTION_CHECK_SIGNATURES)
		check_signature_maybe (parent, mobj, &pi, pdata->_opts);

	if (rv)
		pdata->_func(pdata->_msg, &pi, pdata->_user_data);

	msg_part_free (&pi);
}


void
mu_msg_part_foreach (MuMsg *msg, MuMsgPartForeachFunc func, gpointer user_data,
		     MuMsgPartOptions opts)
{
	PartData pdata;
	GMimeMessage *mime_msg;

	g_return_if_fail (msg);

	if (!mu_msg_load_msg_file (msg, NULL))
		return;

	mime_msg = msg->_file->_mime_msg;

	pdata._msg	      = msg;
	pdata._idx	      = 0;
	pdata._body_part      = mu_msg_mime_get_body_part (mime_msg, FALSE);
	pdata._func	      = func;
	pdata._user_data      = user_data;
	pdata._opts           = opts;

	g_mime_message_foreach (msg->_file->_mime_msg,
				(GMimeObjectForeachFunc)part_foreach_cb,
				&pdata);
}


static gboolean
write_part_to_fd (GMimePart *part, int fd, GError **err)
{
	GMimeStream *stream;
	GMimeDataWrapper *wrapper;
	gboolean rv;

	stream = g_mime_stream_fs_new (fd);
	if (!GMIME_IS_STREAM(stream)) {
		g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_GMIME,
			     "failed to create stream");
		return FALSE;
	}
	g_mime_stream_fs_set_owner (GMIME_STREAM_FS(stream), FALSE);

	wrapper = g_mime_part_get_content_object (part);
	if (!GMIME_IS_DATA_WRAPPER(wrapper)) {
		g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_GMIME,
			     "failed to create wrapper");
		g_object_unref (stream);
		return FALSE;
	}
	g_object_ref (part); /* FIXME: otherwise, the unrefs below
			      * give errors...*/

	if (g_mime_data_wrapper_write_to_stream (wrapper, stream) == -1) {
		rv = FALSE;
		g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_GMIME,
			     "failed to write to stream");
	} else
		rv = TRUE;

	g_object_unref (wrapper);
	g_object_unref (stream);

	return rv;
}



static gboolean
write_object_to_fd (GMimeObject *obj, int fd, GError **err)
{
	gchar *str;
	str = g_mime_object_to_string (obj);

	if (!str) {
		g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_GMIME,
			     "could not get string from object");
		return FALSE;
	}

	if (write (fd, str, strlen(str)) == -1) {
		g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_GMIME,
			     "failed to write object: %s",
			     strerror(errno));
		return FALSE;
	}

	return TRUE;
}



static gboolean
save_mime_object (GMimeObject *obj, const char *fullpath,
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
		g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_FILE,
			     "could not open '%s' for writing: %s",
			     fullpath, errno ? strerror(errno) : "error");
		return FALSE;
	}

	if (GMIME_IS_PART (obj))
		rv = write_part_to_fd ((GMimePart*)obj, fd, err);
	else
		rv = write_object_to_fd (obj, fd, err);

	if (close (fd) != 0 && !err) { /* don't write on top of old err */
		g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_FILE,
			     "could not close '%s': %s",
			     fullpath, errno ? strerror(errno) : "error");
		return FALSE;
	}

	return rv;
}


gchar*
mu_msg_part_filepath (MuMsg *msg, const char* targetdir, guint partidx,
		      GError **err)
{
	char *fname, *filepath;
	GMimeObject* mobj;

	if (!mu_msg_load_msg_file (msg, NULL))
		return NULL;

	if (!(mobj = find_part (msg, partidx))) {
		mu_util_g_set_error (err,
				     MU_ERROR_GMIME,
				     "cannot find part %u", partidx);
		return NULL;
	}

	if (GMIME_IS_PART (mobj)) {
		/* the easy case: the part has a filename */
		fname = (gchar*)g_mime_part_get_filename (GMIME_PART(mobj));
		if (fname) /* security: don't includ directory
			    * components */
			fname = g_path_get_basename (fname);
		else
			fname = g_strdup_printf ("%x-part-%u",
					 g_str_hash (mu_msg_get_path (msg)),
						 partidx);
	} else if (GMIME_IS_MESSAGE_PART(mobj))
		fname  = get_filename_for_mime_message_part
			(g_mime_message_part_get_message
			 ((GMimeMessagePart*)mobj));
	else {
		g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_GMIME,
			     "part %u cannot be saved", partidx);
		return NULL;
	}

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

	if (!mu_msg_load_msg_file (msg, NULL))
		return NULL;

	path = mu_msg_get_path (msg);
	if (!path)
		return NULL;

	/* g_compute_checksum_for_string may be better, but requires
	 * rel. new glib (2.16) */
        dirname = g_strdup_printf ("%s%c%x%c%u",
				   mu_util_cache_dir(), G_DIR_SEPARATOR,
				   g_str_hash (path), G_DIR_SEPARATOR,
				   partid);

	if (!mu_util_create_dir_maybe (dirname, 0700, FALSE)) {
		g_free (dirname);
		return NULL;
	}

	filepath = mu_msg_part_filepath (msg, dirname, partid, NULL);
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

	if (!mu_msg_load_msg_file (msg, err))
		return FALSE;

	part = find_part (msg, partidx);
	if (!is_part_or_message_part (part)) {
		g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_GMIME,
			     "unexpected type %s for part %u",
			     G_OBJECT_TYPE_NAME((GObject*)part),
			     partidx);
		return FALSE;
	} else
		return save_mime_object (part, fullpath, overwrite,
					 use_cached, err);

}


gchar*
mu_msg_part_save_temp (MuMsg *msg, guint partidx, GError **err)
{
	gchar *filepath;
	gboolean rv;

	filepath = mu_msg_part_filepath_cache (msg, partidx);
	if (!filepath) {
		g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_FILE,
			     "Could not get temp filepath");
		return NULL;
	}

	rv = mu_msg_part_save (msg, filepath, partidx, FALSE, TRUE, err);
	if (!rv) {
		g_free (filepath);
		return NULL;
	}

	return filepath;
}



typedef gboolean (*MatchFunc) (GMimeObject *part, gpointer data);

struct _MatchData {
	MatchFunc _matcher;
	gpointer  _user_data;
	gint      _idx, _found_idx;
};
typedef struct _MatchData MatchData;

static void
part_match_foreach_cb (GMimeObject *parent, GMimeObject *part,
		       MatchData *mdata)
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

	if (!mu_msg_load_msg_file (msg, NULL))
		return -1;

	cid = g_str_has_prefix (sought_cid, "cid:") ?
		sought_cid + 4 : sought_cid;

	return msg_part_find_idx (msg->_file->_mime_msg,
				  (MatchFunc)match_content_id,
				  (gpointer)(char*)cid);
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

	/* ignore other parts -- we need this guard so the counting of
	 * parts is the same as in other functions for dealing with
	 * msg parts (this is needed since we expose the numbers to
	 * the user) */
	if (!is_part_or_message_part (part))
		return;

	fname = g_mime_part_get_filename (GMIME_PART(part));
	if (!fname) {
		++mdata->_idx;
		return;
	}

	if (g_regex_match (mdata->_rx, fname, 0, NULL))
		mdata->_lst = g_slist_prepend (mdata->_lst,
					       GUINT_TO_POINTER(mdata->_idx++));
}


GSList*
mu_msg_part_find_files (MuMsg *msg, const GRegex *pattern)
{
	MatchData2 mdata;

	g_return_val_if_fail (msg, NULL);
	g_return_val_if_fail (pattern, NULL);


	if (!mu_msg_load_msg_file (msg, NULL))
		return NULL;

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

	if (part->is_body||!part->disposition||!part->type)
		return FALSE;

	if (include_inline ||
	    g_ascii_strcasecmp (part->disposition,
				GMIME_DISPOSITION_ATTACHMENT) == 0)
		return TRUE;

	return FALSE;
}
