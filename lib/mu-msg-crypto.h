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

/* functions for verifying signatures, decrypting messages */

#ifndef __MU_MSG_CRYPTO_H__
#define __MU_MSG_CRYPTO_H__

#include <glib.h>
#include <mu-msg.h>

/* the signature status */
enum _MuMsgPartSigStatus {
	MU_MSG_PART_SIG_STATUS_UNKNOWN            = 0,
	MU_MSG_PART_SIG_STATUS_GOOD               = 1 << 0,

	MU_MSG_PART_SIG_STATUS_BAD                = 1 << 1,
	MU_MSG_PART_SIG_STATUS_ERROR              = 1 << 2,

	/* status when crypto does not work */
	MU_MSG_PART_SIG_STATUS_FAIL               = 1 << 3,

	MU_MSG_PART_SIG_STATUS_EXPSIG             = 1 << 4, /* expired sig */
	MU_MSG_PART_SIG_STATUS_NO_PUBKEY	  = 1 << 5, /* no public key */
	MU_MSG_PART_SIG_STATUS_EXPKEYSIG          = 1 << 6, /* key expired */
	MU_MSG_PART_SIG_STATUS_REVKEYSIG          = 1 << 7, /* revoked key */
	MU_MSG_PART_SIG_STATUS_UNSUPP_ALGO        = 1 << 8  /* unsupp'd algo */
};
typedef enum _MuMsgPartSigStatus MuMsgPartSigStatus;


struct _MuMsgPartSigInfo {
	time_t             created; /* creation time */
	time_t             expires; /* expiration time */
	MuMsgPartSigStatus status;  /* status of the signature */

	const char         *issuer_serial; /* issuer's serial #*/
	const char         *issuer_name;   /* issuer name */
	const char         *fingerprint;   /* fingerprint */
	const char         *key_id;        /* key id */
	const char         *email;
	const char         *name;

	const char         *pubkey_algo;   /* public key algorithm */
	const char         *digest_algo;   /* digest algorithm */

	const char         *errmsg;        /* errmsg when status ==
					    * MU_MSG_PART_SIG_STATUS_FAIL */
	/* don't touch */
	gpointer           _cert;
};
typedef struct _MuMsgPartSigInfo MuMsgPartSigInfo;


/**
 * get a human-readable string describing @param status; note, status
 * must match a _single_ status.
 *
 * @param status
 *
 * @return a constant string describing status
 */
const char* mu_msg_part_sig_status_to_string (MuMsgPartSigStatus status);


/**
 * summarize the signature checks to one status:
 *
 * - if there's any signature with MU_MSG_PART_SIG_STATUS_(ERROR|FAIL),
 *   the verdict is MU_MSG_PART_SIG_STATUS_ERROR
 * - if not, if there's any signature with MU_MSG_PART_SIG_STATUS_BAD
 *   the verdict is MU_MSG_PART_SIG_STATUS_BAD
 * - if not, if there's any signature with MU_MSG_PART_SIG_STATUS_GOOD
 *   the verdict is MU_MSG_PART_SIG_STATUS_GOOD
 * - if not, the verdic is MU_MSG_PART_SIG_STATUS_UNKNOWN
 *
 * @param sig_infos
 *
 * @return the status
 */
MuMsgPartSigStatus mu_msg_part_sig_infos_verdict (GSList *sig_infos);

/**
 * convert the bitwise-OR'ed statuses to a string
 *
 * @param statuses bitwise-OR'ed statuses
 *
 * @return newly allocated string (g_free)
 */
char* mu_msg_part_sig_statuses_to_string (MuMsgPartSigStatus statuses)
	G_GNUC_MALLOC G_GNUC_WARN_UNUSED_RESULT;


/**
 * get a human readable-description of siginfo
 *
 * @param info a MuMsgPartSigInfo ptr
 *
 * @return a newly allocated string (g_free)
 */
char* mu_msg_part_sig_info_to_string (MuMsgPartSigInfo *info)
	G_GNUC_MALLOC G_GNUC_WARN_UNUSED_RESULT;

/**
 * free the list of MuMsgPartSigInfo structures
 *
 * @param siginfo
 */
void mu_msg_part_free_sig_infos (GSList *siginfos);

struct _MuMsgDecryptedPart;
typedef struct _MuMsgDecryptedPart MuMsgDecryptedPart;


#endif /*__MU_MSG_CRYPTO_H__*/
