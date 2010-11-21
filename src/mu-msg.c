/* 
** Copyright (C) 2008-2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <gmime/gmime.h>
#include <stdlib.h>
#include <ctype.h>

#include "mu-util.h"
#include "mu-msg-str.h"

#include "mu-msg-priv.h" /* include before mu-msg.h */
#include "mu-msg.h"


static guint _refcount = 0;

static void
ref_gmime (void)
{
	if (G_UNLIKELY(_refcount == 0)) {
		srandom ((unsigned)(getpid()*time(NULL)));
		g_mime_init(0);
	}
	++_refcount;	
}


static void
unref_gmime (void)
{
	g_return_if_fail (_refcount > 0);

	--_refcount;
	if (G_UNLIKELY(_refcount == 0))
		g_mime_shutdown();
}

void 
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
	
	g_slice_free (MuMsg, msg);
	unref_gmime ();
}


static gboolean
init_file_metadata (MuMsg* msg, const char* path, const gchar* mdir)
{
	struct stat statbuf;

	if (access (path, R_OK) != 0) {
		g_warning ("%s: cannot read file %s: %s", 
			   __FUNCTION__, path, strerror(errno));
		return FALSE;
	}

	if (stat (path, &statbuf) < 0) {
		g_warning ("%s: cannot stat %s: %s", 
			   __FUNCTION__, path, strerror(errno));
		return FALSE;
	}
	
	if (!S_ISREG(statbuf.st_mode)) {
		g_warning ("%s: not a regular file: %s",
			   __FUNCTION__, path);
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
}


static gboolean
init_mime_msg (MuMsg *msg)
{
	FILE *file;
	GMimeStream *stream;
	GMimeParser *parser;
	
	file = fopen (mu_msg_get_path(msg), "r");
	if (!file) {
		g_warning ("%s:cannot open %s: %s", 
			   __FUNCTION__, mu_msg_get_path(msg), 
			   strerror (errno));
		return FALSE;
	}
	
	stream = g_mime_stream_file_new (file);
	if (!stream) {
		g_warning ("%s: cannot create mime stream", __FUNCTION__);
		fclose (file);
		return FALSE;
	}
	
	parser = g_mime_parser_new_with_stream (stream);
	g_object_unref (stream);
	if (!parser) {
		g_warning ("%s: cannot create mime parser", __FUNCTION__);
		return FALSE;
	}
	
	msg->_mime_msg = g_mime_parser_construct_message (parser);
	g_object_unref (parser);
	if (!msg->_mime_msg) {
		g_warning ("%s: cannot create mime message", __FUNCTION__);
		return FALSE;
	}

	return TRUE;
}


MuMsg*   
mu_msg_new (const char* filepath, const gchar* mdir)
{
	MuMsg *msg;
		
	g_return_val_if_fail (filepath, NULL);

	ref_gmime ();
	
	msg = g_slice_new0 (MuMsg);	
	msg->_prio      = MU_MSG_PRIO_NONE;
	
	if (!init_file_metadata(msg, filepath, mdir)) {
		mu_msg_destroy (msg);
		return NULL;
	}
	
	if (!init_mime_msg(msg)) {
		mu_msg_destroy (msg);
		return NULL;
	}
	
	return msg;
}


const char*    
mu_msg_get_path  (MuMsg *msg)
{
	g_return_val_if_fail (msg, NULL);

	return msg->_fields[PATH_FIELD];
}


const char*
mu_msg_get_subject (MuMsg *msg)
{
	g_return_val_if_fail (msg, NULL);

	return g_mime_message_get_subject (msg->_mime_msg);
}

const char*
mu_msg_get_msgid (MuMsg *msg)
{
	g_return_val_if_fail (msg, NULL);
	
	return g_mime_message_get_message_id (msg->_mime_msg);
}


const char*
mu_msg_get_maildir (MuMsg *msg)
{
	g_return_val_if_fail (msg, NULL);
	
	return msg->_fields[MDIR_FIELD];
}



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

		char *recep;
		InternetAddressList *receps;
		receps = g_mime_message_get_recipients (msg->_mime_msg,
							rtype);
		/* FIXME: is there an internal leak in
		 * internet_address_list_to_string? */
		recep = (char*)internet_address_list_to_string (receps,
								TRUE);
		if (recep && recep[0]=='\0')
			g_free (recep);
		else 
			msg->_fields[field] = recep;
	}

	return msg->_fields[field];
}


const char*
mu_msg_get_to (MuMsg *msg)
{
	g_return_val_if_fail (msg, NULL);
	return get_recipient (msg, GMIME_RECIPIENT_TYPE_TO, TO_FIELD);
}

const char*
mu_msg_get_cc (MuMsg *msg)
{
	g_return_val_if_fail (msg, NULL);
	return get_recipient (msg, GMIME_RECIPIENT_TYPE_CC, CC_FIELD);
}


time_t
mu_msg_get_date (MuMsg *msg)
{
	time_t t;
	
	g_return_val_if_fail (msg, 0);

	/* TODO: check: is the GMT-offset relevant? */
	g_mime_message_get_date(msg->_mime_msg, &t, NULL);
	
	return t;
}

static gboolean
part_looks_like_attachment (GMimeObject *part)
{
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
}
					  

static void
msg_cflags_cb (GMimeObject *parent, GMimeObject *part, MuMsgFlags *flags)
{
	if (*flags & MU_MSG_FLAG_HAS_ATTACH)
		return;
	
	if (!GMIME_IS_PART(part))
		return;
	
	if (part_looks_like_attachment(part))
		*flags |= MU_MSG_FLAG_HAS_ATTACH;
}



static MuMsgFlags
get_content_flags (MuMsg *msg)
{
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
		g_warning ("No top level mime part ?!");

	return flags;
}


MuMsgFlags
mu_msg_get_flags (MuMsg *msg)
{
	g_return_val_if_fail (msg, MU_MSG_FLAG_NONE);
	
	if (msg->_flags == MU_MSG_FLAG_NONE) {
		msg->_flags = mu_msg_flags_from_file (mu_msg_get_path(msg));
		msg->_flags |= get_content_flags (msg);
	}
	
	return msg->_flags;
}


size_t
mu_msg_get_size (MuMsg *msg)
{
	g_return_val_if_fail (msg, 0);

	return msg->_size;
}


static char*
to_lower (char *s)
{
	char *t = s;
	while (t&&*t) {
		t[0] = g_ascii_tolower(t[0]);
		++t;
	}
	return s;
}


static char*
get_prio_str (MuMsg *msg)
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


static MuMsgPrio
parse_prio_str (const char* priostr)
{
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
	MuMsgPrio prio;

	g_return_val_if_fail (msg, 0);

	if (msg->_prio != MU_MSG_PRIO_NONE)
		return msg->_prio;

	priostr = get_prio_str (msg);
	if (!priostr)
		return MU_MSG_PRIO_NORMAL;
	
	prio = parse_prio_str (priostr);

	g_free (priostr);

	return prio;
}


const char*     
mu_msg_get_header (MuMsg *msg, const char* header)
{
	g_return_val_if_fail (msg, NULL);
	g_return_val_if_fail (header, NULL);

	return g_mime_object_get_header (GMIME_OBJECT(msg->_mime_msg), 
					 header);
}


time_t
mu_msg_get_timestamp (MuMsg *msg)
{
	g_return_val_if_fail (msg, 0);
		
	return msg->_timestamp;
}

struct _GetBodyData {
	GMimeObject *_txt_part, *_html_part;
	gboolean _want_html;
};
typedef struct _GetBodyData GetBodyData;


static gboolean
looks_like_attachment (GMimeObject *part)
{
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


/* turn \0-terminated buf into ascii (which is a utf8 subset);
 *   convert any non-ascii into '.'
 */
static void
asciify (char *buf)
{
	char *c;
	for (c = buf; c && *c; ++c)
		if (!isascii(*c))
			c[0] = '.';
}



static gchar*
text_to_utf8 (const char* buffer, const char *charset)
{
	GError *err;
	gchar * utf8;

	err = NULL;
	utf8 = g_convert_with_fallback (buffer, -1, "UTF-8",
					charset, (gchar*)".", 
					NULL, NULL, &err);
	if (!utf8) {
		MU_WRITE_LOG ("%s: conversion failed from %s: %s",
			      __FUNCTION__, charset, err ? err ->message : "");
		if (err)
			g_error_free (err);
	}
	
	return utf8;
}


/* NOTE: buffer will be *freed* or returned unchanged */
static char*
convert_to_utf8 (GMimePart *part, char *buffer)
{
	GMimeContentType *ctype;
	const char* charset;
		
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

	/* hmmm.... no charset at all, or conversion failed; ugly hack:
	 *  replace all non-ascii chars with '.' instead... TODO: come up
	 * with something better */
	asciify (buffer);
	return buffer;
}


static gchar*
stream_to_string (GMimeStream *stream, size_t buflen, gboolean convert_utf8)
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
}


static gchar*
part_to_string (GMimePart *part, gboolean convert_utf8, gboolean *err)
{
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
	
	buffer = stream_to_string (stream, (size_t)buflen, convert_utf8);
	
	/* convert_to_utf8 will free the old 'buffer' if needed */
	if (convert_utf8) 
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
			part_to_string (GMIME_PART(data._html_part), FALSE, &err) :
			NULL; 
	else
		str = data._txt_part ?
			part_to_string (GMIME_PART(data._txt_part), TRUE, &err) :
			NULL;

	/* note, str may be NULL (no body), but that's not necessarily
	 * an error; we only warn when an actual error occured */
	if (err) 
		g_warning ("error occured while retrieving %s body for message %s",
			   want_html ? "html" : "text",
			   mu_msg_get_path(msg));

	return str;	
}

const char*
mu_msg_get_body_html (MuMsg *msg)
{
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
		mu_msg_str_summarize (body, max_lines);
}


const char*
mu_msg_get_field_string (MuMsg *msg, MuMsgFieldId mfid)
{
	g_return_val_if_fail (msg, NULL);

	switch (mfid) {
	case MU_MSG_FIELD_ID_BODY_TEXT:  return mu_msg_get_body_text (msg);
	case MU_MSG_FIELD_ID_BODY_HTML:  return mu_msg_get_body_html (msg);
	case MU_MSG_FIELD_ID_CC:         return mu_msg_get_cc (msg);
	case MU_MSG_FIELD_ID_FROM:       return mu_msg_get_from (msg);
	case MU_MSG_FIELD_ID_PATH:       return mu_msg_get_path (msg);	
	case MU_MSG_FIELD_ID_SUBJECT:    return mu_msg_get_subject (msg);
	case MU_MSG_FIELD_ID_TO:         return mu_msg_get_to (msg);
	case MU_MSG_FIELD_ID_MSGID:      return mu_msg_get_msgid (msg);
	case MU_MSG_FIELD_ID_MAILDIR:    return mu_msg_get_maildir (msg);
	default:
		g_return_val_if_reached (NULL);
	}
}

gint64
mu_msg_get_field_numeric (MuMsg *msg, const MuMsgFieldId mfid)
{
	g_return_val_if_fail (msg, 0);
	
	switch (mfid) {
	case MU_MSG_FIELD_ID_DATE:    
		return mu_msg_get_date(msg);
	case MU_MSG_FIELD_ID_FLAGS:   
		return mu_msg_get_flags(msg);
	case MU_MSG_FIELD_ID_PRIO:
		return mu_msg_get_prio(msg);
	case MU_MSG_FIELD_ID_SIZE:    
		return mu_msg_get_size(msg);
	default:
		g_warning ("%s: %u", __FUNCTION__, mfid);
		g_return_val_if_reached (0);
	}
}
