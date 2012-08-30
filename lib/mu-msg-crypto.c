/*
** Copyright (C) 2012 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-msg.h"
#include "mu-msg-priv.h"
#include "mu-msg-part.h"
#include "mu-msg-crypto.h"
#include "mu-date.h"

#include <gmime/gmime.h>
#include <gmime/gmime-multipart-signed.h>

#define CALLBACK_DATA "callback-data"

struct _CallbackData {
	MuMsgPartPasswordFunc  pw_func;
	gpointer               user_data;
};
typedef struct _CallbackData CallbackData;


static gboolean
password_requester (GMimeCryptoContext *ctx, const char *user_id,
		    const char* prompt_ctx, gboolean reprompt,
		    GMimeStream *response, GError **err)
{
	CallbackData *cbdata;
	gchar *password;
	ssize_t written;

	cbdata = g_object_get_data (G_OBJECT(ctx), CALLBACK_DATA);
	if (!cbdata || !cbdata->pw_func)
		return FALSE;

	password = cbdata->pw_func (user_id, prompt_ctx, reprompt,
				    cbdata->user_data);
	if (!password) {
		mu_util_g_set_error (err, MU_ERROR_CRYPTO,
				     "failed to get password");
		return FALSE;
	}

	written = g_mime_stream_write_string (response, password);
	if (written != -1)
		written = g_mime_stream_write_string (response, "\n");
	if (written == -1)
		mu_util_g_set_error (err, MU_ERROR_CRYPTO,
			             "writing password to mime stream failed");

	if (g_mime_stream_flush (response) != 0)
		g_printerr ("error flushing stream\n");

	memset (password, 0, strlen(password));
	g_free (password);

	return written != -1 ? TRUE : FALSE;
}


static char*
dummy_password_func (const char *user_id, const char *prompt_ctx,
		     gboolean reprompt, gpointer user_data)
{
	g_print ("password requested for %s (%s) %s\n",
		 user_id, prompt_ctx, reprompt ? "again" : "");

	return NULL;
}


static GMimeCryptoContext*
get_gpg_crypto_context (MuMsgOptions opts, GError **err)
{
	GMimeCryptoContext *cctx;
	const char *prog;

	cctx  = NULL;

	prog = g_getenv ("MU_GPG_PATH");
	if (prog)
		cctx = g_mime_gpg_context_new (
		(GMimePasswordRequestFunc)password_requester, prog);
	else {
		char *path;
		path  = g_find_program_in_path ("gpg");
		if (path)
			cctx = g_mime_gpg_context_new (
				password_requester, path);
		g_free (path);
	}
	if (!cctx) {
		mu_util_g_set_error (err, MU_ERROR,
				     "failed to get GPG crypto context");
		return NULL;
	}

	/* always try to use the agent */
	g_mime_gpg_context_set_use_agent (GMIME_GPG_CONTEXT(cctx), TRUE);
 	g_mime_gpg_context_set_auto_key_retrieve
		(GMIME_GPG_CONTEXT(cctx),
		 opts & MU_MSG_OPTION_AUTO_RETRIEVE ? TRUE:FALSE);

	return cctx;
}


static GMimeCryptoContext*
get_crypto_context (MuMsgOptions opts, MuMsgPartPasswordFunc password_func,
		    gpointer user_data, GError **err)
{
	CallbackData *cbdata;
	GMimeCryptoContext *cctx;

	cctx = get_gpg_crypto_context (opts, err);

	/* use gobject to pass data to the callback func */
	cbdata = g_new0 (CallbackData, 1);
	cbdata->pw_func   = password_func ? password_func : dummy_password_func;
	cbdata->user_data = user_data;

	g_object_set_data_full (G_OBJECT(cctx), CALLBACK_DATA,
				cbdata, (GDestroyNotify)g_free);
	return cctx;
}


static MuMsgPartSigStatus
get_verdict (GMimeSignatureList *sigs)
{
	int i;
	MuMsgPartSigStatus status;

	status = MU_MSG_PART_SIG_STATUS_GOOD; /* let's start positive! */

	for (i = 0; i != g_mime_signature_list_length (sigs); ++i) {

		GMimeSignature *msig;
		GMimeSignatureStatus sigstat;
		msig = g_mime_signature_list_get_signature (sigs, i);
		sigstat = g_mime_signature_get_status (msig);

		switch (sigstat) {
		case GMIME_SIGNATURE_STATUS_GOOD:  continue;
		case GMIME_SIGNATURE_STATUS_ERROR: return MU_MSG_PART_SIG_STATUS_ERROR;
		case GMIME_SIGNATURE_STATUS_BAD:   return MU_MSG_PART_SIG_STATUS_BAD;
		}
	}

	return status;
}



MuMsgPartSigStatus
mu_msg_crypto_verify_part (GMimeMultipartSigned *sig, MuMsgOptions opts,
			   GError **err)
{
	MuMsgPartSigStatus sigstat;
	GMimeCryptoContext *ctx;
	GMimeSignatureList *sigs;

	g_return_val_if_fail (GMIME_IS_MULTIPART_SIGNED(sig),
			      MU_MSG_PART_SIG_STATUS_FAIL);

	ctx = get_crypto_context (opts, NULL, NULL, err);
	if (!ctx) {
		mu_util_g_set_error (err, MU_ERROR_CRYPTO,
				     "failed to get crypto context");
		return MU_MSG_PART_SIG_STATUS_FAIL;
	}

	sigs = g_mime_multipart_signed_verify (sig, ctx, err);
	g_object_unref (ctx);
	if (!sigs) {
		if (err && !*err)
			mu_util_g_set_error (err, MU_ERROR_CRYPTO,
					     "verification failed");
		return MU_MSG_PART_SIG_STATUS_FAIL;
	}

	sigstat = get_verdict (sigs);
	g_mime_signature_list_clear (sigs);

	return sigstat;
}




GMimeObject* /* this is declared in mu-msg-priv.h */
mu_msg_crypto_decrypt_part (GMimeMultipartEncrypted *enc, MuMsgOptions opts,
			    MuMsgPartPasswordFunc func, gpointer user_data,
			    GError **err)
{
	GMimeObject *dec;
	GMimeCryptoContext *ctx;

	g_return_val_if_fail (GMIME_IS_MULTIPART_ENCRYPTED(enc), NULL);

	ctx = get_crypto_context (opts, func, user_data, err);
	if (!ctx) {
		mu_util_g_set_error (err, MU_ERROR_CRYPTO,
				     "failed to get crypto context");
		return NULL;
	}

	dec = g_mime_multipart_encrypted_decrypt (enc, ctx, NULL, err);
	g_object_unref (ctx);
	if (!dec) {
		if (err && !*err)
			mu_util_g_set_error (err, MU_ERROR_CRYPTO,
					     "decryption failed");
		return NULL;
	}

	return dec;
}
