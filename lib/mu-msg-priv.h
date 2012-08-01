/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

/*
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

#ifndef __MU_MSG_PRIV_H__
#define __MU_MSG_PRIV_H__

#if HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include <gmime/gmime.h>
#include <stdlib.h>

#include <mu-msg.h>
#include <mu-msg-file.h>
#include <mu-msg-doc.h>
#include <mu-msg-cache.h>
#include "mu-msg-part.h"

G_BEGIN_DECLS

struct _MuMsgFile {
	GMimeMessage	*_mime_msg;
	time_t		 _timestamp;
	size_t		 _size;
	char		 _path    [PATH_MAX + 1];
	char		 _maildir [PATH_MAX + 1];

	/* list where we push allocated strings so we can
	 * free them when the struct gets destroyed
	 */
	GSList          *_free_later;
};


/* we put the the MuMsg definition in this separate -priv file, so we
 * can split the mu_msg implementations over separate files */
struct _MuMsg {

	guint		 _refcount;

	/* our two backend */
	MuMsgFile	*_file; /* based on GMime, ie. a file on disc */
	MuMsgDoc        *_doc;  /* based on Xapian::Document */

	MuMsgCache      *_cache;
};


/**
 * convert a GMimePart to a string
 *
 * @param part a GMimePart
 * @param err will receive TRUE if there was an error, FALSE otherwise
 *
 * @return utf8 string for this MIME part, to be freed by caller
 */
gchar* mu_msg_mime_part_to_string (GMimePart *part, gboolean *err);


/* /\** */
/*  * write a GMimeObject to a file */
/*  * */
/*  * @param obj a GMimeObject */
/*  * @param fullpath full file path */
/*  * @param overwrite allow overwriting existing file */
/*  * @param if file already exist, don't bother to write */
/*  * @param err receives error information */
/*  * */
/*  * @return TRUE if writing succeeded, FALSE otherwise. */
/*  *\/ */
/* gboolean mu_msg_part_mime_save_object (GMimeObject *obj, const char *fullpath, */
/* 				       gboolean overwrite, gboolean use_existing, */
/* 				       GError **err); */



/**
 * get the MIME part that's probably the body of the message (heuristic)
 *
 * @param self a MuMsg
 * @param decrypt whether decryption should be attempted, if needed
 * @param want_html whether it should be a html type of body
 *
 * @return the MIME part, or NULL in case of error.
 */
GMimePart* mu_msg_mime_get_body_part (GMimeMessage *msg, gboolean decrypt,
				      gboolean want_html);


/**
 * Like g_mime_message_foreach, but will recurse into encrypted parts
 * if @param decrypt is TRUE and mu was built with crypto support
 *
 * @param msg a GMimeMessage
 * @param decrypt whether to try to automatically decrypt
 * @param func user callback function for each part
 * @param user_data user point passed to callback function
 * @param err receives error information
 *
 */
void mu_mime_message_foreach (GMimeMessage *msg, gboolean decrypt,
			      GMimeObjectForeachFunc func,
			      gpointer user_data);

#ifdef BUILD_CRYPTO
/**
 * get signature information for the mime part
 *
 * @param part a multipart/sigde part
 * @param opts options for the signature verification (we only use the
 * crypto-related options in opts)
 * @param err receives error info
 *
 * @return a list of MuMsgPartSig, or NULL
 */
GSList* mu_msg_mime_sig_infos (GMimeMultipartSigned *sigmpart,
			       MuMsgOptions opts, GError **err);

/**
 * callback function to retrieve a password from the user
 *
 * @param user_id the user name / id to get the password for
 * @param prompt_ctx a string containing some helpful context for the prompt
 * @param reprompt whether this is a reprompt after an earlier, incorrect password
 * @param user_data the user_data pointer passed to mu_msg_part_decrypt_foreach
 *
 * @return a newly allocated (g_free'able) string
 */
typedef char* (*MuMsgPartPasswordFunc)   (const char *user_id, const char *prompt_ctx,
					  gboolean reprompt, gpointer user_data);

/**
 * decrypt the given encrypted mime multipart
 *
 * @param enc encrypted part
 * @param opts options
 * @param password_func callback function to retrieve as password (or NULL)
 * @param user_data pointer passed to the password func
 * @param err receives error data
 *
 * @return the decrypted part, or NULL in case of error
 */
GMimeObject* mu_msg_crypto_decrypt_part (GMimeMultipartEncrypted *enc, MuMsgOptions opts,
					 MuMsgPartPasswordFunc func, gpointer user_data,
					 GError **err);
#endif /*BUILD_CRYPTO*/

G_END_DECLS

#endif /*__MU_MSG_PRIV_H__*/
