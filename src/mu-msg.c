/* -*- mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-
**
** Copyright (C) 2008-2011 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 3 of the License, or
** (at your option) any later version.
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

#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
#include <stdlib.h>
#include <ctype.h>

#include <gmime/gmime.h>

#include "mu-msg-priv.h" /* include before mu-msg.h */
#include "mu-msg.h"

#include "mu-util.h"
#include "mu-str.h"



static MuMsg*
msg_new (void)
{
<<<<<<< HEAD
	MuMsg *self;

	self = g_slice_new0 (MuMsg);

	self->_refcount = 1;
	self->_cache = mu_msg_cache_new ();
 
	return self;
}

MuMsg*
mu_msg_new_from_file (const char *path, const char *mdir, GError **err)
{
	MuMsg *self;
	MuMsgFile *msgfile;
	
	g_return_val_if_fail (path, NULL);
		
	msgfile = mu_msg_file_new (path, mdir, err);
	if (!msgfile) 
		return NULL;
	
	self = msg_new ();
	self->_file	= msgfile;
		
	return self;
}


MuMsg*
mu_msg_new_from_doc (const XapianDocument* doc, GError **err)
{
	MuMsg *self;
	MuMsgDoc *msgdoc;
		
	g_return_val_if_fail (doc, NULL);
				
	msgdoc = mu_msg_doc_new (doc, err);
	if (!msgdoc)
		return NULL;

	self = msg_new ();
	self->_doc	= msgdoc;
			
	return self;
=======
		g_return_if_fail (!_gmime_initialized);

#ifdef GMIME_ENABLE_RFC2047_WORKAROUNDS
		g_mime_init(GMIME_ENABLE_RFC2047_WORKAROUNDS);
#else
		g_mime_init(0);
#endif /* GMIME_ENABLE_RFC2047_WORKAROUNDS */

		_gmime_initialized = TRUE;
}


void
mu_msg_gmime_uninit (void)
{
		g_return_if_fail (_gmime_initialized);

		g_mime_shutdown();
		_gmime_initialized = FALSE;
}


static void 
mu_msg_destroy (MuMsg *msg)
{
		int i;
	
		if (!msg)
				return;
	
		if (G_IS_OBJECT(msg->_mime_msg)) {
				g_object_unref (msg->_mime_msg);
				msg->_mime_msg = NULL;
		}
		
		for (i = 0; i != FIELD_NUM; ++i)
				g_free (msg->_fields[i]);

		g_slist_foreach (msg->_refs, (GFunc)g_free, NULL);
		g_slist_free (msg->_refs);
	
		g_slice_free (MuMsg, msg);
}


static gboolean
init_file_metadata (MuMsg* msg, const char* path, const gchar* mdir,
					GError **err)
{
		struct stat statbuf;

		if (access (path, R_OK) != 0) {
				g_set_error (err, 0, MU_ERROR_FILE,
							 "cannot read file %s: %s",
							 path, strerror(errno));
				return FALSE;
		}

		if (stat (path, &statbuf) < 0) {
				g_set_error (err, 0, MU_ERROR_FILE,
							 "cannot stat %s: %s",
							 path, strerror(errno));
				return FALSE;
		}
	
		if (!S_ISREG(statbuf.st_mode)) {
				g_set_error (err, 0, MU_ERROR_FILE,
							 "not a regular file: %s", path);
				return FALSE;
		}
	
		msg->_timestamp            = statbuf.st_mtime;
		/* size_t should be enough for message size... */
		msg->_size                 = (size_t)statbuf.st_size; 
		msg->_fields[PATH_FIELD]   = strdup (path);

		/* FIXME: maybe try to derive it from the path? */
		if (mdir) 
				msg->_fields[MDIR_FIELD]   = strdup (mdir);
	
		return TRUE;
>>>>>>> f6ff982e921b60b7afcba19dd85f01d576057f42
}


static void 
mu_msg_destroy (MuMsg *self)
{
<<<<<<< HEAD
	if (!self)
		return;

	mu_msg_file_destroy (self->_file);
	mu_msg_doc_destroy (self->_doc);

	mu_msg_cache_destroy (self->_cache);
		
	g_slice_free (MuMsg, self);
=======
		FILE *file;
		GMimeStream *stream;
	
		file = fopen (mu_msg_get_path(msg), "r");
		if (!file) {
				g_set_error (err, 0, MU_ERROR_FILE,
							 "cannot open %s: %s", mu_msg_get_path(msg), 
							 strerror (errno));
				return NULL;
		}
	
		stream = g_mime_stream_file_new (file);
		if (!stream) {
				g_set_error (err, 0, MU_ERROR_GMIME,
							 "cannot create mime stream for %s",
							 mu_msg_get_path(msg));
				fclose (file);
				return NULL;
		}

		return stream;
}

static gboolean
init_mime_msg (MuMsg *msg, GError **err)
{
		GMimeStream *stream;
		GMimeParser *parser;
	
		stream = get_mime_stream (msg, err);
		if (!stream)
				return FALSE;
	
		parser = g_mime_parser_new_with_stream (stream);
		g_object_unref (stream);
		if (!parser) {
				g_set_error (err, 0, MU_ERROR_GMIME,
							 "cannot create mime parser for %s",
							 mu_msg_get_path(msg));
				return FALSE;
		}
	
		msg->_mime_msg = g_mime_parser_construct_message (parser);
		g_object_unref (parser);
		if (!msg->_mime_msg) {
				g_set_error (err, 0, MU_ERROR_GMIME,
							 "cannot construct mime message for %s",
							 mu_msg_get_path(msg));
				return FALSE;
		}

		return TRUE;
>>>>>>> f6ff982e921b60b7afcba19dd85f01d576057f42
}


MuMsg*
mu_msg_ref (MuMsg *self)
{
<<<<<<< HEAD
	g_return_val_if_fail (self, NULL);

	++self->_refcount;
	
	return self;
=======
		g_return_val_if_fail (msg, NULL);

		++msg->_refcount;

		return msg;
>>>>>>> f6ff982e921b60b7afcba19dd85f01d576057f42
}

void
mu_msg_unref (MuMsg *self)
{
<<<<<<< HEAD
	g_return_if_fail (self);
	g_return_if_fail (self->_refcount >= 1);
	
	if (--self->_refcount == 0) 
		mu_msg_destroy (self);
=======
		g_return_if_fail (msg);
		g_return_if_fail (msg->_refcount >= 1);
	
		if (--msg->_refcount == 0) 
				mu_msg_destroy (msg);
>>>>>>> f6ff982e921b60b7afcba19dd85f01d576057f42
}


/* use this instead of mu_msg_get_path so we don't get into infinite
 * regress...*/
static const char*
get_path (MuMsg *self)
{
<<<<<<< HEAD
	const char *path;
	char *val;
	gboolean do_free;
		
	/* try to get the path from the cache */
	path = mu_msg_cache_str (self->_cache, MU_MSG_FIELD_ID_PATH);
	if (path)
		return path;

	/* nothing found yet? try the doc in case we are using that
	 * backend */
	val = NULL;
	if (self->_doc)
		val = mu_msg_doc_get_str_field (self->_doc,
						MU_MSG_FIELD_ID_PATH,
						&do_free);
		
	/* not in the cache yet? try to get it from the file backend,
	 * in case we are using that */
	if (!val && self->_file)
		val = mu_msg_file_get_str_field (self->_file,
						 MU_MSG_FIELD_ID_PATH,
						 &do_free);
		
	/* this cannot happen unless there are bugs in mu */
	if (!val) {
		g_warning ("%s: cannot find path", __FUNCTION__);
		return NULL;
	}

	/* we found something */
	return mu_msg_cache_set_str (self->_cache,
				     MU_MSG_FIELD_ID_PATH, val,
				     do_free);
=======
		MuMsg *msg;
		
		g_return_val_if_fail (filepath, NULL);	
		g_return_val_if_fail (_gmime_initialized, NULL);
	
		msg	       = g_slice_new0 (MuMsg);	
		msg->_prio     = MU_MSG_PRIO_NONE;
		msg->_refcount = 1;
		msg->_refs     = NULL;
	
		if (!init_file_metadata(msg, filepath, mdir, err)) {
				mu_msg_unref (msg);
				return NULL;
		}
	
		if (!init_mime_msg(msg, err)) {
				mu_msg_unref (msg);
				return NULL;
		}
	
		return msg;
>>>>>>> f6ff982e921b60b7afcba19dd85f01d576057f42
}


/* for some data, we need to read the message file from disk */
static MuMsgFile*
get_msg_file (MuMsg *self)
{
<<<<<<< HEAD
	MuMsgFile *mfile;
	const char *path;
	GError *err;

	if (!(path = get_path (self)))
		return NULL;
		
	err = NULL;
	mfile = mu_msg_file_new (path, NULL, &err);
	if (!mfile) {
		g_warning ("%s: failed to create MuMsgFile: %s",
			   __FUNCTION__, err->message ? err->message : "?");
		g_error_free (err);
		return NULL;
	}
		
	return mfile;
}



static const char*
get_str_field (MuMsg *self, MuMsgFieldId mfid)
{
	gboolean do_free;
	char *val;

	/* first we try the cache */
	if (mu_msg_cache_cached (self->_cache, mfid))
		return mu_msg_cache_str (self->_cache, mfid);

	/* if it's not in the cache but it is a value retrievable from
	 * the doc backend, use that */
	val = NULL;
	if (self->_doc && mu_msg_field_xapian_value (mfid))
		val = mu_msg_doc_get_str_field (self->_doc, mfid, &do_free);
	else {
		/* if we don't have a file object yet, we need to
		 * create it from the file on disk */
		if (!self->_file)
			self->_file = get_msg_file (self);
		if (!self->_file && !(self->_file = get_msg_file (self)))
			return NULL;
		val = mu_msg_file_get_str_field (self->_file, mfid, &do_free);
	}
		
	/* if we get a string that needs freeing, we tell the cache to
	 * mark the string as such, so it will be freed when the cache
	 * is freed (or when the value is overwritten) */
	return mu_msg_cache_set_str (self->_cache, mfid, val, do_free);
=======
		g_return_val_if_fail (msg, NULL);

		return msg->_fields[PATH_FIELD];
}


const char*
mu_msg_get_subject (MuMsg *msg)
{
		g_return_val_if_fail (msg, NULL);

		return g_mime_message_get_subject (msg->_mime_msg);
>>>>>>> f6ff982e921b60b7afcba19dd85f01d576057f42
}


static gint64
get_num_field (MuMsg *self, MuMsgFieldId mfid)
{
<<<<<<< HEAD
	guint64 val;
		 
	if (mu_msg_cache_cached (self->_cache, mfid))
		return mu_msg_cache_num (self->_cache, mfid);
		
	/* if it's not in the cache but it is a value retrievable from
	 * the doc backend, use that */
	val = -1;
	if (self->_doc && mu_msg_field_xapian_value (mfid))
		val = mu_msg_doc_get_num_field (self->_doc, mfid);
	else {
		/* if we don't have a file object yet, we need to
		 * create it from the file on disk */
		if (!self->_file)
			self->_file = get_msg_file (self);
		if (!self->_file && !(self->_file = get_msg_file (self)))
			return -1;
		val = mu_msg_file_get_num_field (self->_file, mfid);
	}

	return mu_msg_cache_set_num (self->_cache, mfid, val);
=======
		g_return_val_if_fail (msg, NULL);
	
		return g_mime_message_get_message_id (msg->_mime_msg);
>>>>>>> f6ff982e921b60b7afcba19dd85f01d576057f42
}


const char*
mu_msg_get_header (MuMsg *self, const char *header)
{
<<<<<<< HEAD
	g_return_val_if_fail (self, NULL);
	g_return_val_if_fail (header, NULL);
=======
		g_return_val_if_fail (msg, NULL);
	
		return msg->_fields[MDIR_FIELD];
}

>>>>>>> f6ff982e921b60b7afcba19dd85f01d576057f42

	/* if we don't have a file object yet, we need to
	 * create it from the file on disk */
	if (!self->_file)
		self->_file = get_msg_file (self);
	if (!self->_file && !(self->_file = get_msg_file (self)))
		return NULL;

<<<<<<< HEAD
	return mu_msg_file_get_header (self->_file, header);
}


const char*    
mu_msg_get_path (MuMsg *self)
{
	g_return_val_if_fail (self, NULL);
	return get_str_field (self, MU_MSG_FIELD_ID_PATH);
=======
const char*    
mu_msg_get_from (MuMsg *msg)
{
		g_return_val_if_fail (msg, NULL);
	
		return g_mime_message_get_sender (msg->_mime_msg);
}


static const char*
get_recipient (MuMsg *msg, GMimeRecipientType rtype, StringFields field)
{
		/* can only be set once */
		if (!msg->_fields[field]) {

				char *recip;
				InternetAddressList *recips;
				recips = g_mime_message_get_recipients (msg->_mime_msg,
														rtype);
				/* FIXME: is there an internal leak in
				 * internet_address_list_to_string? */

				/* FALSE --> don't encode */
				recip = (char*)internet_address_list_to_string (recips,
																FALSE);
				
				if (mu_str_is_empty(recip))
						g_free (recip);
				else 
						msg->_fields[field] = recip;
		}

		return msg->_fields[field];
>>>>>>> f6ff982e921b60b7afcba19dd85f01d576057f42
}


const char*    
mu_msg_get_subject  (MuMsg *self)
{
<<<<<<< HEAD
	g_return_val_if_fail (self, NULL);
	return get_str_field (self, MU_MSG_FIELD_ID_SUBJECT);
=======
		g_return_val_if_fail (msg, NULL);
		return get_recipient (msg, GMIME_RECIPIENT_TYPE_TO, TO_FIELD);
>>>>>>> f6ff982e921b60b7afcba19dd85f01d576057f42
}

const char*    
mu_msg_get_msgid  (MuMsg *self)
{
<<<<<<< HEAD
	g_return_val_if_fail (self, NULL);
	return get_str_field (self, MU_MSG_FIELD_ID_MSGID);
=======
		g_return_val_if_fail (msg, NULL);
		return get_recipient (msg, GMIME_RECIPIENT_TYPE_CC, CC_FIELD);
>>>>>>> f6ff982e921b60b7afcba19dd85f01d576057f42
}

const char*    
mu_msg_get_maildir (MuMsg *self)
{
<<<<<<< HEAD
	g_return_val_if_fail (self, NULL);
	return get_str_field (self, MU_MSG_FIELD_ID_MAILDIR);
=======
		g_return_val_if_fail (msg, NULL);
		return get_recipient (msg, GMIME_RECIPIENT_TYPE_BCC, BCC_FIELD);
>>>>>>> f6ff982e921b60b7afcba19dd85f01d576057f42
}


const char*    
mu_msg_get_from (MuMsg *self)
{
<<<<<<< HEAD
	g_return_val_if_fail (self, NULL);
	return get_str_field (self, MU_MSG_FIELD_ID_FROM);
=======
		time_t t;
	
		g_return_val_if_fail (msg, 0);

		/* TODO: check: is the GMT-offset relevant? */
		g_mime_message_get_date(msg->_mime_msg, &t, NULL);
	
		return t;
>>>>>>> f6ff982e921b60b7afcba19dd85f01d576057f42
}


const char*    
mu_msg_get_to (MuMsg *self)
{
<<<<<<< HEAD
	g_return_val_if_fail (self, NULL);
	return get_str_field (self, MU_MSG_FIELD_ID_TO);
=======
		GMimeContentDisposition *disp;
		const char *str;
	
		disp  = g_mime_object_get_content_disposition (part);
		if (!GMIME_IS_CONTENT_DISPOSITION(disp))
				return FALSE; /* no content disp? prob not
							   * an attachment. */
	
		str = g_mime_content_disposition_get_disposition (disp);

		/* ok, it says it's an attachment, so it probably is... */
		if (!str)
				return TRUE;
		if (strcmp (str, GMIME_DISPOSITION_ATTACHMENT) == 0)
				return TRUE;
		else if (strcmp (str, GMIME_DISPOSITION_INLINE) == 0) {
				/* inline-images are also considered attachments... */
				GMimeContentType *ct;
				ct = g_mime_object_get_content_type (part);
				if (ct)
						return g_mime_content_type_is_type
								(ct, "image", "*");
		}
	
		return FALSE;
>>>>>>> f6ff982e921b60b7afcba19dd85f01d576057f42
}

const char*    
mu_msg_get_cc (MuMsg *self)
{
<<<<<<< HEAD
	g_return_val_if_fail (self, NULL);
	return get_str_field (self, MU_MSG_FIELD_ID_CC);
=======
		if (*flags & MU_MSG_FLAG_HAS_ATTACH)
				return;
	
		if (!GMIME_IS_PART(part))
				return;
	
		if (part_looks_like_attachment(part))
				*flags |= MU_MSG_FLAG_HAS_ATTACH;
>>>>>>> f6ff982e921b60b7afcba19dd85f01d576057f42
}


const char*    
mu_msg_get_bcc (MuMsg *self)
{
<<<<<<< HEAD
	g_return_val_if_fail (self, NULL);
	return get_str_field (self, MU_MSG_FIELD_ID_BCC);
}


time_t
mu_msg_get_date (MuMsg *self)
{
	g_return_val_if_fail (self, (time_t)-1);
	return (time_t)get_num_field (self, MU_MSG_FIELD_ID_DATE);
=======
		GMimeContentType *ctype;
		MuMsgFlags flags;
		GMimeObject *part;

		if (!GMIME_IS_MESSAGE(msg->_mime_msg))
				return MU_MSG_FLAG_NONE;

		flags = 0;
		g_mime_message_foreach (msg->_mime_msg,
								(GMimeObjectForeachFunc)msg_cflags_cb, 
								&flags);
	
		/* note: signed or encrypted status for a message is determined by
		 *  the top-level mime-part
		 */
		if ((part = g_mime_message_get_mime_part(msg->_mime_msg))) {
				ctype = g_mime_object_get_content_type
						(GMIME_OBJECT(part));
				if (!ctype) {
						g_warning ("not a content type!");
						return 0;
				}	
		
				if (ctype) {
						if (g_mime_content_type_is_type
							(ctype,"*", "signed")) 
								flags |= MU_MSG_FLAG_SIGNED;
						if (g_mime_content_type_is_type
							(ctype,"*", "encrypted")) 
								flags |= MU_MSG_FLAG_ENCRYPTED;
				}
		} else
				g_warning ("no top level mime part found");

		return flags;
>>>>>>> f6ff982e921b60b7afcba19dd85f01d576057f42
}



MuMsgFlags
mu_msg_get_flags (MuMsg *self)
{
<<<<<<< HEAD
	g_return_val_if_fail (self, MU_MSG_FLAG_NONE);
	return (MuMsgFlags)get_num_field (self, MU_MSG_FIELD_ID_FLAGS);
=======
		g_return_val_if_fail (msg, MU_MSG_FLAG_NONE);
	
		if (msg->_flags == MU_MSG_FLAG_NONE) {
				msg->_flags = mu_msg_file_get_flags_from_path (mu_msg_get_path(msg));
				msg->_flags |= get_content_flags (msg);
		}
	
		return msg->_flags;
>>>>>>> f6ff982e921b60b7afcba19dd85f01d576057f42
}

size_t
mu_msg_get_size (MuMsg *self)
{
<<<<<<< HEAD
	g_return_val_if_fail (self, (size_t)-1);
	return (size_t)get_num_field (self, MU_MSG_FIELD_ID_SIZE);
=======
		g_return_val_if_fail (msg, 0);

		return msg->_size;
>>>>>>> f6ff982e921b60b7afcba19dd85f01d576057f42
}


MuMsgPrio
mu_msg_get_prio (MuMsg *self)
{
<<<<<<< HEAD
	g_return_val_if_fail (self, MU_MSG_PRIO_NORMAL);
	return (MuMsgPrio)get_num_field (self, MU_MSG_FIELD_ID_PRIO);
}


/* const char*      */
/* mu_msg_get_header (MuMsg *msg, const char* header) */
/* { */
/* 	g_return_val_if_fail (self, NULL); */
/* 	g_return_val_if_fail (header, NULL); */

/* 	return g_mime_object_get_header (GMIME_OBJECT(self->_mime_msg),  */
/* 					 header); */
/* } */
=======
		char *t = s;
		while (t&&*t) {
				t[0] = g_ascii_tolower(t[0]);
				++t;
		}
		return s;
}


static char*
get_prio_header_field (MuMsg *msg)
{
		const char *str;
		GMimeObject *obj;

		obj = GMIME_OBJECT(msg->_mime_msg);

		str = g_mime_object_get_header (obj, "X-Priority");
		if (!str)
				str = g_mime_object_get_header (obj, "X-MSMail-Priority");
		if (!str)
				str = g_mime_object_get_header (obj, "Importance");
		if (!str)
				str = g_mime_object_get_header (obj, "Precedence");
		if (str) 
				return (to_lower(g_strdup(str)));
		else
				return NULL;
}
>>>>>>> f6ff982e921b60b7afcba19dd85f01d576057f42


time_t
mu_msg_get_timestamp (MuMsg *self)
{
<<<<<<< HEAD
	g_return_val_if_fail (self, (time_t)-1);
	return (MuMsgPrio)get_num_field (self, MU_MSG_FIELD_ID_TIMESTAMP);
}




const char*
mu_msg_get_body_html (MuMsg *self)
{
	g_return_val_if_fail (self, NULL);
	return get_str_field (self, MU_MSG_FIELD_ID_BODY_HTML);
=======
		int i;
		struct {
				const char*   _str;
				MuMsgPrio _prio;
		} str_prio[] = {
				{ "high",	MU_MSG_PRIO_HIGH },
				{ "1",		MU_MSG_PRIO_HIGH },
				{ "2",		MU_MSG_PRIO_HIGH },
		
				{ "normal",	MU_MSG_PRIO_NORMAL },
				{ "3",		MU_MSG_PRIO_NORMAL },

				{ "low",	MU_MSG_PRIO_LOW },
				{ "list",	MU_MSG_PRIO_LOW },
				{ "bulk",	MU_MSG_PRIO_LOW },
				{ "4",		MU_MSG_PRIO_LOW },
				{ "5",		MU_MSG_PRIO_LOW }
		};

		for (i = 0; i != G_N_ELEMENTS(str_prio); ++i)
				if (g_strstr_len (priostr, -1, str_prio[i]._str) != NULL)
						return str_prio[i]._prio;
	
		/* e.g., last-fm uses 'fm-user'... as precedence */
		return MU_MSG_PRIO_NORMAL;
}


MuMsgPrio
mu_msg_get_prio (MuMsg *msg)
{
		char* priostr;

		g_return_val_if_fail (msg, 0);

		if (msg->_prio != MU_MSG_PRIO_NONE)
				return msg->_prio;

		priostr = get_prio_header_field (msg);
		if (!priostr)
				return MU_MSG_PRIO_NORMAL;
	
		msg->_prio = parse_prio_str (priostr);
		g_free (priostr);

		return msg->_prio;
}


const char*     
mu_msg_get_header (MuMsg *msg, const char* header)
{
		g_return_val_if_fail (msg, NULL);
		g_return_val_if_fail (header, NULL);

		return g_mime_object_get_header (GMIME_OBJECT(msg->_mime_msg), 
										 header);
>>>>>>> f6ff982e921b60b7afcba19dd85f01d576057f42
}


const char*
mu_msg_get_body_text (MuMsg *self)
{
<<<<<<< HEAD
	g_return_val_if_fail (self, NULL);
	return get_str_field (self, MU_MSG_FIELD_ID_BODY_TEXT);
}

=======
		g_return_val_if_fail (msg, 0);
		
		return msg->_timestamp;
}

struct _GetBodyData {
		GMimeObject *_txt_part, *_html_part;
		gboolean _want_html;
};
typedef struct _GetBodyData GetBodyData;
>>>>>>> f6ff982e921b60b7afcba19dd85f01d576057f42

const char*
mu_msg_get_references_str (MuMsg *self)
{
<<<<<<< HEAD
	g_return_val_if_fail (self, NULL);
	return get_str_field (self, MU_MSG_FIELD_ID_REFS);
}


const char*
mu_msg_get_field_string (MuMsg *self, MuMsgFieldId mfid)
{
	g_return_val_if_fail (self, NULL);
	return get_str_field (self, mfid);
=======
		const char *str;
		GMimeContentDisposition *disp;
	
		disp = g_mime_object_get_content_disposition (GMIME_OBJECT(part));
		if (!GMIME_IS_CONTENT_DISPOSITION(disp))
				return FALSE;  

		str = g_mime_content_disposition_get_disposition (disp);
		if (!str)
				return FALSE;
	
		if (strcmp(str,GMIME_DISPOSITION_INLINE) == 0)
				return FALSE; /* inline, so it's not an attachment */
	
		return TRUE; /* looks like an attachment */
}

static void
get_body_cb (GMimeObject *parent, GMimeObject *part, GetBodyData *data)
{
		GMimeContentType *ct;		

		/* already found what we're looking for? */
		if ((data->_want_html && data->_html_part != NULL) ||
			(!data->_want_html && data->_txt_part != NULL))
				return;
	
		ct = g_mime_object_get_content_type (part);
		if (!GMIME_IS_CONTENT_TYPE(ct)) {
				g_warning ("not a content type!");
				return;
		}
	
		if (looks_like_attachment (part))
				return; /* not the body */
	
		/* is it right content type? */
		if (g_mime_content_type_is_type (ct, "text", "plain"))
				data->_txt_part = part;
		else if (g_mime_content_type_is_type (ct, "text", "html"))
				data->_html_part = part;
		else
				return; /* wrong type */
}	


/* turn \0-terminated buf into ascii (which is a utf8 subset); convert
 *   any non-ascii into '.'
 */
static void
asciify (char *buf)
{
		char *c;
		for (c = buf; c && *c; ++c)
				if (!isascii(*c))
						c[0] = '.';
>>>>>>> f6ff982e921b60b7afcba19dd85f01d576057f42
}

gint64
mu_msg_get_field_numeric (MuMsg *self, MuMsgFieldId mfid)
{
<<<<<<< HEAD
	g_return_val_if_fail (self, -1);
	return get_num_field (self, mfid);
=======
		GError *err;
		gchar * utf8;

		err = NULL;
		utf8 = g_convert_with_fallback (buffer, -1, "UTF-8",
										charset, (gchar*)".", 
										NULL, NULL, &err);
		if (!utf8) {
				MU_WRITE_LOG ("%s: conversion failed from %s: %s",
							  __FUNCTION__, charset,
							  err ? err ->message : "");
				if (err)
						g_error_free (err);
		}
	
		return utf8;
>>>>>>> f6ff982e921b60b7afcba19dd85f01d576057f42
}



MuMsgContact *
mu_msg_contact_new (const char *name, const char *address,
		    MuMsgContactType type)
{
<<<<<<< HEAD
	MuMsgContact *self;
	
	g_return_val_if_fail (name, NULL);
	g_return_val_if_fail (address, NULL);
	g_return_val_if_fail (!mu_msg_contact_type_is_valid(type),
			      NULL);
=======
		GMimeContentType *ctype;
		const char* charset;
		unsigned char *cur;
	
		/* optimization: if the buffer is plain ascii, no conversion
		 * is done... */
		for (cur = (unsigned char*)buffer; *cur && *cur < 0x80; ++cur);
		if (*cur == '\0')
				return buffer;
	
		ctype = g_mime_object_get_content_type (GMIME_OBJECT(part));
		g_return_val_if_fail (GMIME_IS_CONTENT_TYPE(ctype), NULL);
	
		charset = g_mime_content_type_get_parameter (ctype, "charset");
		if (charset) 
				charset = g_mime_charset_iconv_name (charset);
	
		/* of course, the charset specified may be incorrect... */
		if (charset) {
				char *utf8 = text_to_utf8 (buffer, charset);
				if (utf8) {
						g_free (buffer);
						return utf8;
				}
		}

		/* hmmm.... no charset at all, or conversion failed; ugly
		 *  hack: replace all non-ascii chars with '.'
		 *  instead... TODO: come up with something better */
		asciify (buffer);
		return buffer;
}
>>>>>>> f6ff982e921b60b7afcba19dd85f01d576057f42

	self = g_slice_new (MuMsgContact);

<<<<<<< HEAD
	self->name	  = g_strdup (name);
	self->address = g_strdup (address);
	self->type    = type;

	return self;	
=======
static gchar*
stream_to_string (GMimeStream *stream, size_t buflen)
{
		char *buffer;
		ssize_t bytes;
	
		buffer = g_new(char, buflen + 1);
		g_mime_stream_reset (stream);
	
		/* we read everything in one go */
		bytes = g_mime_stream_read (stream, buffer, buflen);
		if (bytes < 0) {
				g_warning ("%s: failed to read from stream", __FUNCTION__);
				g_free (buffer);
				return NULL;
		}
	
		buffer[bytes]='\0'; 

		return buffer;
>>>>>>> f6ff982e921b60b7afcba19dd85f01d576057f42
}


void
mu_msg_contact_destroy (MuMsgContact *self)
{
<<<<<<< HEAD

	if (!self)
		return;

	g_free ((void*)self->name);
	g_free ((void*)self->address);
	g_slice_free (MuMsgContact, self);
}
	
=======
		GMimeDataWrapper *wrapper;
		GMimeStream *stream = NULL;
		ssize_t buflen;
		char *buffer = NULL;

		*err = TRUE;
		g_return_val_if_fail (GMIME_IS_PART(part), NULL);
	
		wrapper = g_mime_part_get_content_object (part);
		if (!wrapper) {
				/* this happens with invalid mails */
				g_debug ("failed to create data wrapper");
				goto cleanup;
		}

		stream = g_mime_stream_mem_new ();
		if (!stream) {
				g_warning ("failed to create mem stream");
				goto cleanup;
		}

		buflen = g_mime_data_wrapper_write_to_stream (wrapper, stream);
		if (buflen <= 0)  {/* empty buffer, not an error */
				*err = FALSE;
				goto cleanup;
		}
	
		buffer = stream_to_string (stream, (size_t)buflen);
	
		/* convert_to_utf8 will free the old 'buffer' if needed */
		buffer = convert_to_utf8 (part, buffer);
	
		*err = FALSE;
	
cleanup:				
		if (stream)
				g_object_unref (G_OBJECT(stream));
	
		return buffer;
}


static char*
get_body (MuMsg *msg, gboolean want_html)
{
		GetBodyData data;
		char *str;
		gboolean err;
	
		g_return_val_if_fail (msg, NULL);
		g_return_val_if_fail (GMIME_IS_MESSAGE(msg->_mime_msg), NULL);
	
		memset (&data, 0, sizeof(GetBodyData));
		data._want_html = want_html;

		err = FALSE;
		g_mime_message_foreach (msg->_mime_msg,
								(GMimeObjectForeachFunc)get_body_cb,
								&data);
		if (want_html)
				str = data._html_part ?
						part_to_string (GMIME_PART(data._html_part), &err) :
						NULL; 
		else
				str = data._txt_part ?
						part_to_string (GMIME_PART(data._txt_part), &err) :
						NULL;

		/* note, str may be NULL (no body), but that's not necessarily
		 * an error; we only warn when an actual error occured */
		if (err) 
				g_warning ("error occured while retrieving %s body" 
						   "for message %s",
						   want_html ? "html" : "text",
						   mu_msg_get_path(msg));
		
		return str;
}
>>>>>>> f6ff982e921b60b7afcba19dd85f01d576057f42

static gboolean
fill_contact (MuMsgContact *self, InternetAddress *addr,
	      MuMsgContactType ctype)
{
<<<<<<< HEAD
	if (!addr)
		return FALSE;
	
	self->name = internet_address_get_name (addr);
	self->type = ctype;  
	
	/* we only support internet mailbox addresses; if we don't
	 * check, g_mime hits an assert
	 */
	if (INTERNET_ADDRESS_IS_MAILBOX(addr))
		self->address = internet_address_mailbox_get_addr
			(INTERNET_ADDRESS_MAILBOX(addr));
	else
		self->address  = NULL;
	
	return TRUE;
=======
		g_return_val_if_fail (msg, NULL);
	
		if (msg->_fields[HTML_FIELD])
				return msg->_fields[HTML_FIELD];
		else
				return msg->_fields[HTML_FIELD] = get_body (msg, TRUE);
}


const char*
mu_msg_get_body_text (MuMsg *msg)
{
		g_return_val_if_fail (msg, NULL);
	
		if (msg->_fields[TEXT_FIELD])
				return msg->_fields[TEXT_FIELD];
		else
				return msg->_fields[TEXT_FIELD] = get_body (msg, FALSE);
}

const char*
mu_msg_get_summary (MuMsg *msg, size_t max_lines)
{
		const char *body;
	
		g_return_val_if_fail (msg, NULL);
		g_return_val_if_fail (max_lines > 0, NULL);
	
		/* do we have a summary cached already? */
		if (msg->_fields[SUMMARY_FIELD])
				return msg->_fields[SUMMARY_FIELD];

		/* nope; calculate it */
		body = mu_msg_get_body_text (msg);
		if (!body)
				return NULL; /* there was no text body */

		return msg->_fields[SUMMARY_FIELD] =
				mu_str_summarize (body, max_lines);
>>>>>>> f6ff982e921b60b7afcba19dd85f01d576057f42
}


static void
address_list_foreach (InternetAddressList *addrlist,
		      MuMsgContactType     ctype,
		      MuMsgContactForeachFunc func, 
		      gpointer user_data)
{
<<<<<<< HEAD
	int i;
	
	if (!addrlist)
		return;
	
	for (i = 0; i != internet_address_list_length(addrlist); ++i) {
		
		MuMsgContact contact;
		if (!fill_contact(&contact,
				  internet_address_list_get_address (addrlist, i),
				  ctype)) {
			MU_WRITE_LOG ("ignoring contact");
			continue;
		}
		
		if (!(func)(&contact, user_data))
			break;
	}
}

=======
		GSList *msgids;
		const char *str;

		msgids = NULL;
		str = g_mime_object_get_header (GMIME_OBJECT(msg->_mime_msg),
										header);
	
		/* get stuff from the 'references' header */
		if (str) {
				const GMimeReferences *cur;
				GMimeReferences *mime_refs;
				mime_refs = g_mime_references_decode (str);
				for (cur = mime_refs; cur; cur = g_mime_references_get_next(cur)) {
						const char* msgid;
						msgid = g_mime_references_get_message_id (cur);
						if (msgid)
								msgids = g_slist_prepend (msgids, g_strdup (msgid));
				}
				g_mime_references_free (mime_refs);
		}

		return g_slist_reverse (msgids);
}

const char*
mu_msg_get_references_str (MuMsg *msg)
{
		const GSList *refs;
		gchar *refsstr;

		g_return_val_if_fail (msg, NULL);
	
		if (msg->_fields[REFS_FIELD])
				return msg->_fields[REFS_FIELD];

		refsstr = NULL;
		refs = mu_msg_get_references (msg);
		if (refs) {
				const GSList *cur;
				for (cur = refs; cur; cur = g_slist_next(cur)) {
						char *tmp;
						tmp = g_strdup_printf ("%s%s%s",
											   refsstr ? refsstr : "",
											   refsstr ? "," : "",
											   g_strdup((gchar*)cur->data));
						g_free (refsstr);
						refsstr = tmp;
				}
		}			
	
		return msg->_fields[REFS_FIELD] = refsstr;
}
>>>>>>> f6ff982e921b60b7afcba19dd85f01d576057f42


static void
get_contacts_from (MuMsg *msg, MuMsgContactForeachFunc func, 
		   gpointer user_data)
{
<<<<<<< HEAD
	InternetAddressList *lst;
	
	/* we go through this whole excercise of trying to get a *list*
	 * of 'From:' address (usually there is only one...), because
	 * internet_address_parse_string has the nice side-effect of
	 * splitting in names and addresses for us */
	lst = internet_address_list_parse_string (
		g_mime_message_get_sender (msg->_file->_mime_msg));

	if (lst) {
		address_list_foreach (lst, MU_MSG_CONTACT_TYPE_FROM,
				      func, user_data);
		g_object_unref (G_OBJECT(lst));
	} 
=======
		GSList *refs, *inreply;
	
		g_return_val_if_fail (msg, NULL);

		if (msg->_refs)
				return msg->_refs;

		refs = get_msgids_from_header (msg, "References");
	
		/* now, add in-reply-to:, we only take the first one if there
		 * are more */
		inreply = get_msgids_from_header (msg, "In-reply-to");
		if (inreply) {
				refs = g_slist_prepend (refs, g_strdup ((gchar*)inreply->data));
				g_slist_foreach (inreply, (GFunc)g_free, NULL);
				g_slist_free (inreply);
		}
				 
		/* put in proper order */
		msg->_refs = g_slist_reverse (refs);

		return msg->_refs;
>>>>>>> f6ff982e921b60b7afcba19dd85f01d576057f42
}


void
mu_msg_contact_foreach (MuMsg *msg, MuMsgContactForeachFunc func, 
			gpointer user_data)
{
	int i;		
	struct { 
		GMimeRecipientType     _gmime_type;
		MuMsgContactType       _type;
	} ctypes[] = {
		{GMIME_RECIPIENT_TYPE_TO,  MU_MSG_CONTACT_TYPE_TO},
		{GMIME_RECIPIENT_TYPE_CC,  MU_MSG_CONTACT_TYPE_CC},
		{GMIME_RECIPIENT_TYPE_BCC, MU_MSG_CONTACT_TYPE_BCC},
	};

<<<<<<< HEAD
	g_return_if_fail (func && msg);

	/* first, get the from address(es) */
	get_contacts_from (msg, func, user_data);

	/* get to, cc, bcc */
	for (i = 0; i != G_N_ELEMENTS(ctypes); ++i) {
		InternetAddressList *addrlist;
		addrlist = g_mime_message_get_recipients (msg->_file->_mime_msg,
							  ctypes[i]._gmime_type);
		address_list_foreach (addrlist, ctypes[i]._type, func, user_data);
	}
=======
const char*
mu_msg_get_field_string (MuMsg *msg, MuMsgFieldId mfid)
{
		g_return_val_if_fail (msg, NULL);

		switch (mfid) {
		case MU_MSG_FIELD_ID_BCC:        return mu_msg_get_bcc (msg);
		case MU_MSG_FIELD_ID_BODY_TEXT:  return mu_msg_get_body_text (msg);
		case MU_MSG_FIELD_ID_BODY_HTML:  return mu_msg_get_body_html (msg);
		case MU_MSG_FIELD_ID_CC:         return mu_msg_get_cc (msg);
		case MU_MSG_FIELD_ID_FROM:       return mu_msg_get_from (msg);
		case MU_MSG_FIELD_ID_PATH:       return mu_msg_get_path (msg);	
		case MU_MSG_FIELD_ID_SUBJECT:    return mu_msg_get_subject (msg);
		case MU_MSG_FIELD_ID_TO:         return mu_msg_get_to (msg);
		case MU_MSG_FIELD_ID_MSGID:      return mu_msg_get_msgid (msg);
		case MU_MSG_FIELD_ID_MAILDIR:    return mu_msg_get_maildir (msg);
		case MU_MSG_FIELD_ID_REFS:       return mu_msg_get_references_str (msg);
		default:
				g_return_val_if_reached (NULL);
		}
}

gint64
mu_msg_get_field_numeric (MuMsg *msg, const MuMsgFieldId mfid)
{
		g_return_val_if_fail (msg, 0);
	
		switch (mfid) {
		case MU_MSG_FIELD_ID_DATE: return mu_msg_get_date(msg);
		case MU_MSG_FIELD_ID_FLAGS: return mu_msg_get_flags(msg);
		case MU_MSG_FIELD_ID_PRIO: return mu_msg_get_prio(msg);
		case MU_MSG_FIELD_ID_SIZE: return mu_msg_get_size(msg);
		default: g_return_val_if_reached (-1);
		}
>>>>>>> f6ff982e921b60b7afcba19dd85f01d576057f42
}

