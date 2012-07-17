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

#include "mu-msg.h"
#include "mu-msg-priv.h"
#include "mu-msg-part.h"
#include "mu-msg-crypto.h"
#include "mu-date.h"


#include <gmime/gmime.h>
#include <gmime/gmime-multipart-signed.h>

static gboolean
dummy_password_requester (GMimeCryptoContext *ctx, const char *user_id,
			  const char* prompt_ctx, gboolean reprompt,
			  GMimeStream *response, GError **err)
{
	g_print ("password requested\n");
	return TRUE;

}


GMimeCryptoContext*
get_crypto_context (MuMsgPartOptions opts, GError **err)
{
	GMimeCryptoContext *ctx;
	const char *prog;

	ctx  = NULL;

	prog = g_getenv ("MU_GPG_PATH");
	if (prog)
		ctx = g_mime_gpg_context_new (
		(GMimePasswordRequestFunc)dummy_password_requester,
			prog);
	else {
		char *path;
		path  = g_find_program_in_path ("gpg");
		if (path)
			ctx = g_mime_gpg_context_new (
				(GMimePasswordRequestFunc)dummy_password_requester,
				path);
		g_free (path);
	}

	if (!ctx) {
		mu_util_g_set_error (err, MU_ERROR, "failed to get crypto context");
		return NULL;
	}

	g_mime_gpg_context_set_use_agent
		(GMIME_GPG_CONTEXT(ctx),
		 opts & MU_MSG_PART_OPTION_USE_AGENT);
	g_mime_gpg_context_set_always_trust
		(GMIME_GPG_CONTEXT(ctx), FALSE);
	g_mime_gpg_context_set_auto_key_retrieve
		(GMIME_GPG_CONTEXT(ctx),
		 opts & MU_MSG_PART_OPTION_AUTO_RETRIEVE_KEY);

	return ctx;

}

const char*
get_pubkey_algo_name (GMimePubKeyAlgo algo)
{
	switch (algo) {
	case GMIME_PUBKEY_ALGO_DEFAULT:
		return "default";
	case GMIME_PUBKEY_ALGO_RSA:
		return  "RSA";
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
		return "unknown algorithm";
	}
}

const gchar*
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
		return  "RIPEMD160";
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
		return "unknown algorithm";
	}
}


static void
harvest_certificate_info (GMimeSignature *sig, MuMsgPartSigInfo *siginfo)
{
	GMimeCertificate *cert;

	cert = g_mime_signature_get_certificate (sig);
	if (!cert)
		return; /* nothing to harvest */

	siginfo->_cert = cert;

	siginfo->issuer_serial = g_mime_certificate_get_issuer_serial (cert);
	siginfo->issuer_name   = g_mime_certificate_get_issuer_name (cert);
	siginfo->fingerprint   = g_mime_certificate_get_fingerprint (cert);
	siginfo->key_id        = g_mime_certificate_get_key_id (cert);
	siginfo->email         = g_mime_certificate_get_email (cert);
	siginfo->name          = g_mime_certificate_get_name (cert);

	siginfo->pubkey_algo   = get_pubkey_algo_name
		(g_mime_certificate_get_pubkey_algo (cert));
	siginfo->digest_algo   = get_digestkey_algo_name
		(g_mime_certificate_get_digest_algo (cert));
}


static MuMsgPartSigInfo*
sig_info_new (GMimeSignature *sig)
{
	MuMsgPartSigInfo *siginfo;
	MuMsgPartSigStatus status;

	switch (g_mime_signature_get_status (sig)) {
	case GMIME_SIGNATURE_STATUS_GOOD:
		status = MU_MSG_PART_SIG_STATUS_GOOD; break;
	case GMIME_SIGNATURE_STATUS_BAD:
		status = MU_MSG_PART_SIG_STATUS_BAD; break;
	default:
		status = MU_MSG_PART_SIG_STATUS_ERROR; break;
	}

	if (status != MU_MSG_PART_SIG_STATUS_GOOD) {
		GMimeSignatureError sigerr;
		sigerr = g_mime_signature_get_errors (sig);
		if (sigerr & GMIME_SIGNATURE_ERROR_EXPSIG)
			status |= MU_MSG_PART_SIG_STATUS_EXPSIG;
		if (sigerr & GMIME_SIGNATURE_ERROR_NO_PUBKEY)
			status |= MU_MSG_PART_SIG_STATUS_NO_PUBKEY;
		if (sigerr & GMIME_SIGNATURE_ERROR_EXPKEYSIG)
			status |= MU_MSG_PART_SIG_STATUS_EXPKEYSIG;
		if (sigerr & GMIME_SIGNATURE_ERROR_REVKEYSIG)
			status |= MU_MSG_PART_SIG_STATUS_REVKEYSIG;
		if (sigerr & GMIME_SIGNATURE_ERROR_UNSUPP_ALGO)
			status |= MU_MSG_PART_SIG_STATUS_UNSUPP_ALGO;
	}

	siginfo = g_new0 (MuMsgPartSigInfo, 1);
	siginfo->status = status;
	siginfo->created = g_mime_signature_get_created (sig);
	siginfo->expires = g_mime_signature_get_expires (sig);

	harvest_certificate_info (sig, siginfo);

	return siginfo;
}

static void
sig_info_destroy (MuMsgPartSigInfo *siginfo)
{
	if (!siginfo)
		return;

	g_clear_object (&siginfo->_cert);
	g_free (siginfo);
}



GSList*
mu_msg_mime_sig_infos (GMimeMultipartSigned *sigmpart, MuMsgPartOptions opts,
		       GError **err)
{
	int i;
	GMimeSignatureList *sigs;
	GMimeCryptoContext *gpgctx;
	GSList *siginfos;

	if (!GMIME_IS_MULTIPART_SIGNED (sigmpart)) {
		mu_util_g_set_error (err, MU_ERROR_IN_PARAMETERS,
				     "not a multipart/signed part");
		return NULL; /* error */
	}

 	gpgctx =  get_crypto_context (opts, err);
	if (!gpgctx)
		return NULL; /* error */

	sigs = g_mime_multipart_signed_verify (sigmpart, gpgctx, err);
	g_object_unref (gpgctx);

	if (!sigs)
		return NULL; /* error */

	for (i = 0, siginfos = NULL; i != g_mime_signature_list_length (sigs); ++i) {

		MuMsgPartSigInfo *siginfo;

		siginfo = sig_info_new
			(g_mime_signature_list_get_signature (sigs, i));

		siginfos = g_slist_prepend (siginfos, siginfo);
	}

	return siginfos;
}


void
mu_msg_part_free_sig_infos (GSList *siginfos)
{
	g_slist_foreach (siginfos,
			 (GFunc)sig_info_destroy, NULL);
	g_slist_free (siginfos);
}


const char*
mu_msg_part_sig_status_to_string (MuMsgPartSigStatus status)
{
	switch (status) {
	case MU_MSG_PART_SIG_STATUS_UNKNOWN:
		return "unknown";
	case MU_MSG_PART_SIG_STATUS_GOOD:
		return "good";
	case MU_MSG_PART_SIG_STATUS_BAD:
		return "bad signature";
	case MU_MSG_PART_SIG_STATUS_ERROR:
		return "error verifying signature";
	case MU_MSG_PART_SIG_STATUS_EXPSIG:
		return "signature is expired";
	case MU_MSG_PART_SIG_STATUS_NO_PUBKEY:
		return "no public key found";
	case MU_MSG_PART_SIG_STATUS_EXPKEYSIG:
		return "expired public key";
	case MU_MSG_PART_SIG_STATUS_REVKEYSIG:
		return "revoked public key";
	case MU_MSG_PART_SIG_STATUS_UNSUPP_ALGO:
		return "unsupported algorithm";
	default:
		g_warning ("%s: invalid status %d",
			   __FUNCTION__, status);
		return "invalid status";
	}
}


char*
mu_msg_part_sig_info_to_string (MuMsgPartSigInfo *info)
{
	unsigned u;
	GString *gstr;

	MuMsgPartSigStatus statuses[] = {
		MU_MSG_PART_SIG_STATUS_UNKNOWN,
		MU_MSG_PART_SIG_STATUS_GOOD,
		MU_MSG_PART_SIG_STATUS_BAD,
		MU_MSG_PART_SIG_STATUS_ERROR,
		MU_MSG_PART_SIG_STATUS_EXPSIG,
		MU_MSG_PART_SIG_STATUS_NO_PUBKEY,
		MU_MSG_PART_SIG_STATUS_EXPKEYSIG,
		MU_MSG_PART_SIG_STATUS_REVKEYSIG,
		MU_MSG_PART_SIG_STATUS_UNSUPP_ALGO
	};

	g_return_val_if_fail (info, NULL);

	gstr = g_string_sized_new (128);

	for (u = 0; u != G_N_ELEMENTS(statuses); ++u) {
		const gchar *statstr;

		if (!(info->status & statuses[u]))
			continue;

		statstr = mu_msg_part_sig_status_to_string (statuses[u]);
		if (gstr->len != 0)
			gstr = g_string_append (gstr, ", ");

		gstr = g_string_append (gstr, statstr);
	}

	gstr = g_string_prepend (gstr, "status: ");

	if (info->status & MU_MSG_PART_SIG_STATUS_ERROR)
		return g_string_free (gstr, FALSE);

	g_string_append_printf (gstr, "; algorithms (P/D) (%s, %s)",
				info->pubkey_algo, info->digest_algo);

	g_string_append_printf (gstr, "; created: %s, expires: %s",
				mu_date_str_s ("%c", info->created),
				mu_date_str_s ("%c", info->expires));

	if (info->name || info->email)
		g_string_append_printf (gstr, "; who:%s %s",
					info->name ? info-> name : "",
					info->email ? info->email : "");

	if (info->issuer_name && info->issuer_serial)
		g_string_append_printf (gstr, "; issuer: %s (%s)",
					info->issuer_name,
					info->issuer_serial);
	if (info->fingerprint)
		g_string_append_printf (gstr, "; fingerprint: %s",
					info->fingerprint);

	return g_string_free (gstr, FALSE);
}
