/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/
/*
** Copyright (C) 2008-2012 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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


struct _DoData {
	GMimeObject *mime_obj;
	unsigned    index;
};
typedef struct _DoData DoData;

static void
do_it_with_index (MuMsg *msg, MuMsgPart *part, DoData *ddata)
{
	if (ddata->mime_obj)
		return;

	if (part->index == ddata->index)
		ddata->mime_obj = (GMimeObject*)part->data;
}

static GMimeObject*
get_mime_object_at_index (MuMsg *msg, MuMsgOptions opts, unsigned index)
{
	DoData ddata;

	ddata.mime_obj  = NULL;
	ddata.index     = index;

	mu_msg_part_foreach (msg, opts,
			     (MuMsgPartForeachFunc)do_it_with_index,
			     &ddata);

	return ddata.mime_obj;
}


typedef gboolean (*MuMsgPartMatchFunc) (MuMsgPart *, gpointer);
struct _MatchData {
	MuMsgPartMatchFunc match_func;
	gpointer user_data;
	int index;
};
typedef struct _MatchData MatchData;

static void
check_match (MuMsg *msg, MuMsgPart *part, MatchData *mdata)
{
	if (mdata->index != -1)
		return;

	if (mdata->match_func (part, mdata->user_data))
		mdata->index = part->index;
}

int
get_matching_part_index (MuMsg *msg, MuMsgOptions opts,
			 MuMsgPartMatchFunc func, gpointer user_data)
{
	MatchData mdata;

	mdata.match_func = func;
	mdata.user_data  = user_data;
	mdata.index      = -1;

	mu_msg_part_foreach (msg, opts,
			     (MuMsgPartForeachFunc)check_match,
			     &mdata);
	return mdata.index;
}


struct _TxtData {
	GString *gstr;
	gboolean decrypt;
};
typedef struct _TxtData TxtData;

static gchar *mime_message_to_string (GMimeMessage *mimemsg, gboolean decrypt);

static void
each_mime_part_get_text (GMimeObject *parent, GMimeObject *part, TxtData *tdata)
{
	char *txt;
	txt = NULL;

	if (GMIME_IS_MESSAGE(part)) {
		txt = mime_message_to_string (GMIME_MESSAGE(part),
					      tdata->decrypt);
		if (txt)
			tdata->gstr =
				g_string_append (tdata->gstr, txt);
	}

	if (GMIME_IS_PART (part)) {
		GMimeContentType *ctype;
		gboolean err;
		ctype = g_mime_object_get_content_type (part);
		if (!g_mime_content_type_is_type (ctype, "text", "plain"))
			return; /* not plain text */
		txt = mu_msg_mime_part_to_string
			((GMimePart*)part, &err);
	}

	if (txt) {
		tdata->gstr = g_string_append_c (tdata->gstr, '\n');
		tdata->gstr = g_string_append (tdata->gstr, txt);
		g_free (txt);
	}
}

static gchar*
mime_message_to_string (GMimeMessage *mimemsg, gboolean decrypt)
{
	TxtData tdata;
	InternetAddressList *addresses;
	gchar *adrs;
	const char *str;

	tdata.gstr = g_string_sized_new (2048); /* just a guess */

	/* put sender, recipients and subject in the string, so they
	 * can be indexed as well */
	str = g_mime_message_get_sender(mimemsg);
	if (str) {
		g_string_append (tdata.gstr, str);
		g_string_append_c (tdata.gstr, '\n');
	}
	str = g_mime_message_get_subject(mimemsg);
	if (str) {
		g_string_append (tdata.gstr, str);
		g_string_append_c (tdata.gstr, '\n');
	}
	addresses = g_mime_message_get_all_recipients (mimemsg);
	adrs = internet_address_list_to_string (addresses, FALSE);
	g_object_unref(G_OBJECT(addresses));
	if (adrs)
		g_string_append (tdata.gstr, adrs);
	g_free (adrs);

	/* recurse through all text parts */
	tdata.decrypt = decrypt;
	mu_mime_message_foreach
		(mimemsg, decrypt,
		 (GMimeObjectForeachFunc)each_mime_part_get_text,
		 &tdata);

	return g_string_free (tdata.gstr, FALSE);
}

char*
mu_msg_part_get_text (MuMsg *msg, MuMsgPart *self, gboolean *err)
{
	GMimeObject *mobj;

	g_return_val_if_fail (msg, NULL);
	g_return_val_if_fail (self && self->data, NULL);
	mobj = (GMimeObject*)self->data;

	if (GMIME_IS_PART(mobj)) {
		/* ignore all but plain text */
		if ((strcasecmp (self->type, "text") != 0) ||
		    (strcasecmp (self->subtype, "plain") != 0))
			return NULL;
		return mu_msg_mime_part_to_string ((GMimePart*)mobj, err);
	}

	if (GMIME_IS_MESSAGE(mobj))
		return mime_message_to_string ((GMimeMessage*)mobj,TRUE);

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
	if (!GMIME_IS_DATA_WRAPPER(wrapper))
		return -1;

	stream = g_mime_data_wrapper_get_stream (wrapper);
	if (!stream)
		return -1; /* no stream -> size is 0 */
	else
		return g_mime_stream_length (stream);

	/* NOTE: stream/wrapper are owned by gmime, no unreffing */
}


/* #ifdef BUILD_CRYPTO */
/* static void */
/* check_signature_maybe (GMimeObject *parent, GMimeObject *mobj, MuMsgPart *pi, */
/* 		       MuMsgOptions opts) */
/* { */
/* 	GMimeContentType *ctype; */
/* 	GError *err; */
/* 	gboolean pkcs7; */

/* 	if (!GMIME_IS_MULTIPART_SIGNED (parent)) */
/* 		return; */

/* 	ctype = g_mime_object_get_content_type (mobj); */
/* 	if (g_mime_content_type_is_type */
/* 	    (ctype, "application", "pgp-signature")) */
/* 		pkcs7 = FALSE; */
/* 	else if (g_mime_content_type_is_type */
/* 		 (ctype, "application", "x-pkcs7-signature")) */
/* 		pkcs7 = TRUE; */
/* 	else return; /\* don't know how to handle other kinds *\/ */

/* 	if (pkcs7) */
/* 		opts |= MU_MSG_OPTION_USE_PKCS7; /\* gpg is the default *\/ */

/* 	err = NULL; */
/* 	pi->sig_infos = mu_msg_mime_sig_infos */
/* 		(GMIME_MULTIPART_SIGNED (parent), opts, &err); */
/* 	if (err) { */
/* 		g_warning ("error verifying signature: %s", err->message); */
/* 		g_clear_error (&err); */
/* 	} */
/* } */



/* #endif /\*BUILD_CRYPTO*\/ */

/* static gboolean */
/* init_msg_part_from_mime_part (MuMsgOptions opts, GMimeObject *parent, */
/* 			      GMimePart *part, MuMsgPart *pi) */
/* { */
/* 	const gchar *fname, *descr; */
/* 	GMimeContentType *ct; */

/* 	ct = g_mime_object_get_content_type ((GMimeObject*)part); */
/* 	if (GMIME_IS_CONTENT_TYPE(ct)) { */
/* 		pi->type    = (char*)g_mime_content_type_get_media_type (ct); */
/* 		pi->subtype = (char*)g_mime_content_type_get_media_subtype (ct); */
/* 	} */

/* 	pi->disposition = (char*)g_mime_object_get_disposition */
/* 		((GMimeObject*)part); */

/* 	fname	      = g_mime_part_get_filename (part); */
/* 	pi->file_name = fname ? mu_str_utf8ify (fname) : NULL; */

/* 	descr		  = g_mime_part_get_content_description (part); */
/* 	pi->description   = descr ? mu_str_utf8ify (descr) : NULL; */
/* 	pi->size          = get_part_size (part); */
/* 	pi->part_type     = MU_MSG_PART_TYPE_LEAF; */

/* 	if (!pi->disposition || */
/* 	    g_ascii_strcasecmp (pi->disposition, */
/* 				GMIME_DISPOSITION_INLINE) == 0) */
/* 		pi->part_type |= MU_MSG_PART_TYPE_INLINE; */

/* 	if (GMIME_IS_MULTIPART_SIGNED (parent)) */
/* 		pi->part_type |= MU_MSG_PART_TYPE_SIGNED; */

/* 	/\* if we have crypto support, check the signature if there is one *\/ */
/* #ifdef BUILD_CRYPTO */
/* 	if (opts & MU_MSG_OPTION_CHECK_SIGNATURES) */
/*  		check_signature_maybe (parent, (GMimeObject*)part, */
/* 				       pi, opts); */
/* #endif /\*BUILD_CRYPTO*\/ */

/* 	return TRUE; */
/* } */

static char*
mime_part_get_filename (GMimeObject *mobj, unsigned index,
			gboolean construct_if_needed)
{
	gchar *fname, *cur;

	if (GMIME_IS_PART (mobj)) {
		/* the easy case: the part has a filename */
		fname = (gchar*)g_mime_part_get_filename (GMIME_PART(mobj));
		if (fname) /* don't include directory components */
			fname = g_path_get_basename (fname);
	}

	if (!fname && !construct_if_needed)
		return NULL;

	if (GMIME_IS_MESSAGE_PART(mobj)) {
		GMimeMessage *msg;
		const char *subj;
		msg  = g_mime_message_part_get_message
			(GMIME_MESSAGE_PART(mobj));
		subj = g_mime_message_get_subject (msg);
		fname = g_strdup_printf ("%s.eml", subj ? subj : "message");
	}

	if (!fname)
		fname =	g_strdup_printf ("%u.part", index);

	/* remove slashes, spaces, colons... */
	for (cur = fname; *cur; ++cur)
		if (*cur == '/' || *cur == ' ' || *cur == ':')
			*cur = '-';
	return fname;
}


char*
mu_msg_part_get_filename (MuMsgPart *mpart, gboolean construct_if_needed)
{
	g_return_val_if_fail (mpart, NULL);
	g_return_val_if_fail (GMIME_IS_OBJECT(mpart->data), NULL);

	return mime_part_get_filename ((GMimeObject*)mpart->data,
				       mpart->index, construct_if_needed);
}


static MuMsgPartType
get_disposition (GMimeObject *mobj)
{
	const char *disp;

	disp = g_mime_object_get_disposition (mobj);
	if (!disp)
		return MU_MSG_PART_TYPE_NONE;

	if (strcasecmp (disp, GMIME_DISPOSITION_ATTACHMENT) == 0)
		return MU_MSG_PART_TYPE_ATTACHMENT;

	if (strcasecmp (disp, GMIME_DISPOSITION_INLINE) == 0)
		return MU_MSG_PART_TYPE_INLINE;

	return MU_MSG_PART_TYPE_NONE;
}

static gboolean
handle_children (MuMsg *msg,
		 GMimeObject *mobj, MuMsgOptions opts,
		 unsigned index, MuMsgPartForeachFunc func,
		 gpointer user_data);

/* call 'func' with information about this MIME-part */
static gboolean
handle_signed_part (MuMsg *msg,
		    GMimeMultipartSigned *part, GMimeObject *parent,
		    MuMsgOptions opts, unsigned index,
		    MuMsgPartForeachFunc func, gpointer user_data)
{
	return TRUE;
}

/* call 'func' with information about this MIME-part */
static gboolean
handle_encrypted_part (MuMsg *msg,
		       GMimeMultipartEncrypted *part, GMimeObject *parent,
		       MuMsgOptions opts, unsigned index,
		       MuMsgPartForeachFunc func, gpointer user_data)
{
	return TRUE;
}

/* call 'func' with information about this MIME-part */
static gboolean
handle_part (MuMsg *msg, GMimePart *part, GMimeObject *parent,
	     MuMsgOptions opts, unsigned index,
	     MuMsgPartForeachFunc func, gpointer user_data)
{
	GMimeContentType *ct;
	MuMsgPart msgpart;

	memset (&msgpart, 0, sizeof(MuMsgPart));

	ct = g_mime_object_get_content_type ((GMimeObject*)part);
	if (GMIME_IS_CONTENT_TYPE(ct)) {
		msgpart.type    = g_mime_content_type_get_media_type (ct);
		msgpart.subtype = g_mime_content_type_get_media_subtype (ct);
	}

	msgpart.size        = get_part_size (part);

	msgpart.part_type   = MU_MSG_PART_TYPE_LEAF;
	msgpart.part_type |= get_disposition ((GMimeObject*)part);

	/* a top-level non-attachment text part is probably a body */
	if ((!parent || GMIME_IS_MESSAGE(parent)) &&
	    ((msgpart.part_type & MU_MSG_PART_TYPE_ATTACHMENT) == 0) &&
	    g_strcmp0 (msgpart.type, "text") == 0)
		msgpart.part_type |= MU_MSG_PART_TYPE_BODY;

	msgpart.data        = (gpointer)part;
	msgpart.index       = index;

	func (msg, &msgpart, user_data);

	return TRUE;
}


/* call 'func' with information about this MIME-part */
static gboolean
handle_message_part (MuMsg *msg, GMimeMessagePart *mmsg, GMimeObject *parent,
		MuMsgOptions opts, unsigned index,
		MuMsgPartForeachFunc func, gpointer user_data)
{
	MuMsgPart msgpart;
	memset (&msgpart, 0, sizeof(MuMsgPart));

	msgpart.type        = "message";
	msgpart.subtype     = "rfc822";
	msgpart.index       = index;

	/* msgpart.size        = 0; /\* maybe calculate this? *\/ */

	msgpart.part_type  = MU_MSG_PART_TYPE_MESSAGE;
	msgpart.part_type |= get_disposition ((GMimeObject*)mmsg);

	msgpart.data        = (gpointer)mmsg;

	func (msg, &msgpart, user_data);

	if (opts & MU_MSG_OPTION_RECURSE_RFC822) {
		GMimeMessage *mime_msg;
		mime_msg = g_mime_message_part_get_message (mmsg);
		return handle_children
			(msg,
			 (GMimeObject*)mime_msg,
			 opts, index, func, user_data);
	}
	return TRUE;
}


static gboolean
handle_mime_object (MuMsg *msg,
		    GMimeObject *mobj, GMimeObject *parent, MuMsgOptions opts,
		    unsigned index, MuMsgPartForeachFunc func, gpointer user_data)
{
	if (GMIME_IS_PART (mobj))
		return handle_part
			(msg, GMIME_PART(mobj), parent,
			 opts, index, func, user_data);
	else if (GMIME_IS_MESSAGE_PART (mobj))
		return handle_message_part
			(msg, GMIME_MESSAGE_PART(mobj),
			 parent, opts, index, func, user_data);
	else if (GMIME_IS_MULTIPART_SIGNED (mobj))
		return handle_signed_part
				(msg, GMIME_MULTIPART_SIGNED (mobj),
				 parent, opts, index, func, user_data);
	else if (GMIME_IS_MULTIPART_ENCRYPTED (mobj))
		return handle_encrypted_part
			(msg, GMIME_MULTIPART_ENCRYPTED (mobj),
			 parent, opts, index, func, user_data);

	return TRUE;
}


static gboolean
handle_children (MuMsg *msg,
		 GMimeObject *mobj, MuMsgOptions opts,
		 unsigned index, MuMsgPartForeachFunc func,
		 gpointer user_data)
{
	gboolean rv;
	GMimePartIter *iter;

	/* the children */
	iter = g_mime_part_iter_new (mobj);

	if (!iter)
		return FALSE;
	for (rv = TRUE; rv && g_mime_part_iter_is_valid (iter);
	     g_mime_part_iter_next (iter), index++)
		rv = handle_mime_object (
			msg, g_mime_part_iter_get_current (iter),
			g_mime_part_iter_get_parent (iter),
			opts, index, func, user_data);

	g_mime_part_iter_free (iter);

	return rv;
}

gboolean
mu_msg_part_foreach (MuMsg *msg, MuMsgOptions opts,
		     MuMsgPartForeachFunc func, gpointer user_data)
{
	GMimeObject *toplevel;
	unsigned idx;

	g_return_val_if_fail (msg, FALSE);

	if (!mu_msg_load_msg_file (msg, NULL))
		return FALSE;

	idx = 0;
	toplevel = g_mime_message_get_mime_part
		(GMIME_MESSAGE(msg->_file->_mime_msg));
	if (!toplevel || !handle_mime_object
	    (msg, toplevel, NULL, opts, idx++, func, user_data))
		return FALSE;

	return handle_children (msg, toplevel, opts, idx, func,
				user_data);
}


gboolean
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
save_object (GMimeObject *obj, MuMsgOptions opts, const char *fullpath,
	     GError **err)
{
	int fd;
	gboolean rv;
	gboolean use_existing, overwrite;

	use_existing = opts & MU_MSG_OPTION_USE_EXISTING;
	overwrite    = opts & MU_MSG_OPTION_OVERWRITE;

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
mu_msg_part_get_path (MuMsg *msg, MuMsgOptions opts,
		      const char* targetdir, unsigned index, GError **err)
{
	char *fname, *filepath;
	GMimeObject* mobj;

	g_return_val_if_fail (msg, NULL);

	if (!mu_msg_load_msg_file (msg, NULL))
		return NULL;

	mobj = get_mime_object_at_index (msg, opts, index);
	if (!mobj){
		mu_util_g_set_error (err, MU_ERROR_GMIME,
				     "cannot find part %u", index);
		return NULL;
	}

	fname = mime_part_get_filename (mobj, index, TRUE);
	filepath = g_build_path (G_DIR_SEPARATOR_S, targetdir ? targetdir : "",
				 fname, NULL);
	g_free (fname);

	return filepath;
}



gchar*
mu_msg_part_get_cache_path (MuMsg *msg, MuMsgOptions opts, guint partid,
			    GError **err)
{
	char *dirname, *filepath;
	const char* path;

	g_return_val_if_fail (msg, NULL);

	if (!mu_msg_load_msg_file (msg, NULL))
		return NULL;

	path = mu_msg_get_path (msg);

	/* g_compute_checksum_for_string may be better, but requires
	 * rel. new glib (2.16) */
        dirname = g_strdup_printf ("%s%c%x%c%u",
				   mu_util_cache_dir(), G_DIR_SEPARATOR,
				   g_str_hash (path), G_DIR_SEPARATOR,
				   partid);

	if (!mu_util_create_dir_maybe (dirname, 0700, FALSE)) {
		mu_util_g_set_error (err, MU_ERROR_FILE, "failed to create dir %s",
				     dirname);
		g_free (dirname);
		return NULL;
	}

	filepath = mu_msg_part_get_path (msg, opts, dirname, partid, err);
	g_free (dirname);

	return filepath;
}


gboolean
mu_msg_part_save (MuMsg *msg, MuMsgOptions opts,
		  const char *fullpath, guint partidx, GError **err)
{
	GMimeObject *part;

	g_return_val_if_fail (msg, FALSE);
	g_return_val_if_fail (fullpath, FALSE);
	g_return_val_if_fail (!((opts & MU_MSG_OPTION_OVERWRITE) &&
			        (opts & MU_MSG_OPTION_USE_EXISTING)), FALSE);

	if (!mu_msg_load_msg_file (msg, err))
		return FALSE;

	part = get_mime_object_at_index (msg, opts, partidx);
	if (!GMIME_IS_PART(part) || GMIME_IS_MESSAGE_PART(part)) {
		g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_GMIME,
			     "unexpected type %s for part %u",
			     G_OBJECT_TYPE_NAME((GObject*)part),
			     partidx);
		return FALSE;
	}

	return save_object (part, opts, fullpath, err);
}


gchar*
mu_msg_part_save_temp (MuMsg *msg, MuMsgOptions opts, guint partidx, GError **err)
{
	gchar *filepath;

	filepath = mu_msg_part_get_cache_path (msg, opts, partidx, err);
	if (!filepath)
		return NULL;

	if (!mu_msg_part_save (msg, opts, filepath, partidx, err)) {
		g_free (filepath);
		return NULL;
	}

	return filepath;
}

static gboolean
match_cid (MuMsgPart *mpart, const char *cid)
{
	const char *this_cid;

	this_cid = g_mime_object_get_content_id ((GMimeObject*)mpart->data);

	return g_strcmp0 (this_cid, cid) ? TRUE : FALSE;
}

int
mu_msg_find_index_for_cid (MuMsg *msg, MuMsgOptions opts, const char *sought_cid)
{
	const char* cid;

	g_return_val_if_fail (msg, -1);
	g_return_val_if_fail (sought_cid, -1);

	if (!mu_msg_load_msg_file (msg, NULL))
		return -1;

	cid = g_str_has_prefix (sought_cid, "cid:") ?
		sought_cid + 4 : sought_cid;

	return get_matching_part_index (msg, opts,
					(MuMsgPartMatchFunc)match_cid,
					(gpointer)(char*)cid);
}

struct _RxMatchData {
 	GSList       *_lst;
	const GRegex *_rx;
	guint         _idx;
};
typedef struct _RxMatchData RxMatchData;


static void
match_filename_rx (MuMsg *msg, MuMsgPart *mpart, RxMatchData *mdata)
{
	char *fname;

	fname = mu_msg_part_get_filename (mpart, FALSE);
	if (!fname)
		return;

	if (g_regex_match (mdata->_rx, fname, 0, NULL))
		mdata->_lst = g_slist_prepend (mdata->_lst,
					       GUINT_TO_POINTER(mpart->index));
	g_free (fname);
}


GSList*
mu_msg_find_files (MuMsg *msg, MuMsgOptions opts, const GRegex *pattern)
{
	RxMatchData mdata;

	g_return_val_if_fail (msg, NULL);
	g_return_val_if_fail (pattern, NULL);

	if (!mu_msg_load_msg_file (msg, NULL))
		return NULL;

	mdata._lst = NULL;
	mdata._rx  = pattern;
	mdata._idx = 0;

	mu_msg_part_foreach (msg, opts,
			     (MuMsgPartForeachFunc)match_filename_rx,
			     &mdata);
	return mdata._lst;
}


gboolean
mu_msg_part_looks_like_attachment (MuMsgPart *part, gboolean include_inline)
{
	g_return_val_if_fail (part, FALSE);

	if  (part->part_type & MU_MSG_PART_TYPE_BODY)
		return FALSE;
	if (!include_inline && (part->part_type & MU_MSG_PART_TYPE_INLINE))
		return FALSE;

	return TRUE;
}
