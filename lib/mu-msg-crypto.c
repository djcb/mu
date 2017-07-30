/*
** Copyright (C) 2012-2016 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

	/* it seems that GMime tries to flush the fd; however, this
	 * does not work for pipes/sockets, causing getting a password
	 * to fail.
	 *
	 * I have reported this, and it has been fixed now:
	 *
	 * http://git.gnome.org/browse/gmime/commit/
	 *      ?id=bda4834d3d9a1fbefb6d97edfef2bc1da9357f58
	 *
	 * however, it may take a while before everybody has this
	 * version of GMime (ie. version > 2.6.10)
	 *
	 */

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


static const char*
get_pubkey_algo_name (GMimePubKeyAlgo algo)
{
	switch (algo) {
	case GMIME_PUBKEY_ALGO_DEFAULT:
		return "default";
	case GMIME_PUBKEY_ALGO_RSA:
		return "RSA";
	case GMIME_PUBKEY_ALGO_RSA_E:
		return "RSA (encryption only)";
	case GMIME_PUBKEY_ALGO_RSA_S:
		return "RSA (signing only)";
	case GMIME_PUBKEY_ALGO_ELG_E:
		return "ElGamal (encryption only)";
	case GMIME_PUBKEY_ALGO_DSA:
		return "DSA";
	case GMIME_PUBKEY_ALGO_ELG:
		return "ElGamal";
	default:
		return "unknown pubkey algorithm";
	}
}

static const gchar*
get_digestkey_algo_name (GMimeDigestAlgo algo)
{
	switch (algo) {
	case GMIME_DIGEST_ALGO_DEFAULT:
		return "default";
	case GMIME_DIGEST_ALGO_MD5:
		return "MD5";
	case GMIME_DIGEST_ALGO_SHA1:
		return "SHA-1";
	case GMIME_DIGEST_ALGO_RIPEMD160:
		return "RIPEMD160";
	case GMIME_DIGEST_ALGO_MD2:
		return "MD2";
	case GMIME_DIGEST_ALGO_TIGER192:
		return "TIGER-192";
	case GMIME_DIGEST_ALGO_HAVAL5160:
		return "HAVAL-5-160";
	case GMIME_DIGEST_ALGO_SHA256:
		return "SHA-256";
	case GMIME_DIGEST_ALGO_SHA384:
		return "SHA-384";
	case GMIME_DIGEST_ALGO_SHA512:
		return "SHA-512";
	case GMIME_DIGEST_ALGO_SHA224:
		return "SHA-224";
	case GMIME_DIGEST_ALGO_MD4:
		return "MD4";
	default:
		return "unknown digest algorithm";
	}
}


/* get data from the 'certificate' */
static char*
get_cert_data (GMimeCertificate *cert)
{
	const char /**email,*/ *name, *digest_algo, *pubkey_algo,
		*keyid, *trust;

	/* email         =  g_mime_certificate_get_email (cert); */
	name          =  g_mime_certificate_get_name (cert);
	keyid         =  g_mime_certificate_get_key_id (cert);

	digest_algo  =  get_digestkey_algo_name
		(g_mime_certificate_get_digest_algo (cert));
	pubkey_algo  =  get_pubkey_algo_name
		(g_mime_certificate_get_pubkey_algo (cert));

	switch (g_mime_certificate_get_trust (cert)) {
	case GMIME_TRUST_UNKNOWN:   trust = "unknown"; break;
	case GMIME_TRUST_UNDEFINED: trust = "undefined"; break;
	case GMIME_TRUST_NEVER:     trust = "never"; break;
	case GMIME_TRUST_MARGINAL:  trust = "marginal"; break;
	case GMIME_TRUST_FULL:      trust = "full"; break;
	case GMIME_TRUST_ULTIMATE:  trust = "ultimate"; break;
	default:
		g_return_val_if_reached (NULL);
	}

	return g_strdup_printf (
		"signer:%s, key:%s (%s,%s), trust:%s",
		name ? name : "?",
		/* email ? email : "?", */
		keyid, pubkey_algo, digest_algo,
		trust);
}



/* get a human-readable report about the signature */
static char*
get_verdict_report (GMimeSignature *msig)
{
	time_t t;
	const char *status, *created, *expires;
	gchar *certdata, *report;
	GMimeSignatureStatus sigstat;

	sigstat = g_mime_signature_get_status (msig);
	if (sigstat & GMIME_SIGNATURE_STATUS_ERROR_MASK)
	  status = "error";
	else if (sigstat & GMIME_SIGNATURE_STATUS_RED)
	  status = "bad";
	else if (sigstat & GMIME_SIGNATURE_STATUS_GREEN)
	  status = "good";
	else
	  g_return_val_if_reached (NULL);

	t = g_mime_signature_get_created (msig);
	created = (t == 0 || t == (time_t)-1) ? "?" : mu_date_str_s ("%x", t);

	t = g_mime_signature_get_expires (msig);
	expires = (t == 0 || t == (time_t)-1) ? "?" : mu_date_str_s ("%x", t);

	certdata = get_cert_data (g_mime_signature_get_certificate (msig));
	report = g_strdup_printf ("%s; created:%s, expires:%s, %s",
				  status, created, expires,
				  certdata ? certdata : "?");
	g_free (certdata);
	return report;
}


static MuMsgPartSigStatusReport*
get_status_report (GMimeSignatureList *sigs)
{
	int i;
	MuMsgPartSigStatus status;
	MuMsgPartSigStatusReport *status_report;
	char *report;

	status = MU_MSG_PART_SIG_STATUS_GOOD; /* let's start positive! */

	for (i = 0, report = NULL; i != g_mime_signature_list_length (sigs);
	     ++i) {

		GMimeSignature *msig;
		GMimeSignatureStatus sigstat;
		gchar *rep;

		msig = g_mime_signature_list_get_signature (sigs, i);
		sigstat = g_mime_signature_get_status (msig);

		/* downgrade our expectations */
		if ((sigstat & GMIME_SIGNATURE_STATUS_ERROR_MASK) &&
		    status != MU_MSG_PART_SIG_STATUS_ERROR)
		  status = MU_MSG_PART_SIG_STATUS_ERROR;
		else if ((sigstat & GMIME_SIGNATURE_STATUS_RED) &&
			 status == MU_MSG_PART_SIG_STATUS_GOOD)
		  status = MU_MSG_PART_SIG_STATUS_BAD;

		rep  = get_verdict_report (msig);
		report = g_strdup_printf ("%s%s%d: %s",
					  report ? report : "",
					  report ? "; " : "",  i + 1,
					  rep);
		g_free (rep);
	}

	status_report = g_slice_new (MuMsgPartSigStatusReport);
	status_report->verdict = status;
	status_report->report  = report;

	return status_report;
}

void
mu_msg_part_sig_status_report_destroy (MuMsgPartSigStatusReport *report)
{
	if (!report)
		return;

	g_free ((char*)report->report);
	g_slice_free (MuMsgPartSigStatusReport, report);
}


static inline void
tag_with_sig_status(GObject *part,
		    MuMsgPartSigStatusReport *report)
{
	g_object_set_data_full
		(part, SIG_STATUS_REPORT, report,
		 (GDestroyNotify)mu_msg_part_sig_status_report_destroy);
}


void
mu_msg_crypto_verify_part (GMimeMultipartSigned *sig, MuMsgOptions opts,
			   GError **err)
{
	/* the signature status */
	MuMsgPartSigStatusReport *report;
	GMimeSignatureList *sigs;

	g_return_if_fail (GMIME_IS_MULTIPART_SIGNED(sig));

	sigs = g_mime_multipart_signed_verify (sig, GMIME_VERIFY_NONE, err);
	if (!sigs) {
		if (err && !*err)
			mu_util_g_set_error (err, MU_ERROR_CRYPTO,
					     "verification failed");
		return;
	}

	report = get_status_report (sigs);
	g_mime_signature_list_clear (sigs);

	/* tag this part with the signature status check */
	tag_with_sig_status(G_OBJECT(sig), report);
}


static inline void
check_decrypt_result(GMimeMultipartEncrypted *part, GMimeDecryptResult *res,
		     GError **err)
{
	GMimeSignatureList *sigs;
	MuMsgPartSigStatusReport *report;

	if (res) {
		/* Check if the decrypted part had any embed signatures */
		sigs = res->signatures;
		if (sigs) {
			report = get_status_report (sigs);
			g_mime_signature_list_clear (sigs);

			/* tag this part with the signature status check */
			tag_with_sig_status(G_OBJECT(part), report);
		}
		else {
			if (err && !*err)
				mu_util_g_set_error (err, MU_ERROR_CRYPTO,
						     "verification failed");
		}
		g_object_unref (res);
	}

}


GMimeObject* /* this is declared in mu-msg-priv.h */
mu_msg_crypto_decrypt_part (GMimeMultipartEncrypted *enc, MuMsgOptions opts,
			    MuMsgPartPasswordFunc func, gpointer user_data,
			    GError **err)
{
	GMimeObject *dec;
	GMimeDecryptResult *res;

	g_return_val_if_fail (GMIME_IS_MULTIPART_ENCRYPTED(enc), NULL);

	res = NULL;
	dec = g_mime_multipart_encrypted_decrypt (enc, GMIME_DECRYPT_NONE, NULL, &res, err);
	check_decrypt_result(enc, res, err);

	if (!dec) {
		if (err && !*err)
			mu_util_g_set_error (err, MU_ERROR_CRYPTO,
					     "decryption failed");
		return NULL;
	}

	return dec;
}
