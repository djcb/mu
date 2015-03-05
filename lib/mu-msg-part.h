/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

/*
** Copyright (C) 2008-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef __MU_MSG_PART_H__
#define __MU_MSG_PART_H__

#include <glib.h>
#include <unistd.h> /* for ssize_t */

#define SIG_STATUS_REPORT "sig-status-report"

G_BEGIN_DECLS

enum _MuMsgPartType {
	MU_MSG_PART_TYPE_NONE		= 0,

	/* MIME part without children */
	MU_MSG_PART_TYPE_LEAF		= 1 << 1,
	/* an RFC822 message part? */
	MU_MSG_PART_TYPE_MESSAGE	= 1 << 2,
	/* disposition inline? */
	MU_MSG_PART_TYPE_INLINE		= 1 << 3,
	/* disposition attachment? */
	MU_MSG_PART_TYPE_ATTACHMENT	= 1 << 4,
	/* a signed part? */
	MU_MSG_PART_TYPE_SIGNED		= 1 << 5,
	/* an encrypted part? */
	MU_MSG_PART_TYPE_ENCRYPTED	= 1 << 6,
	/* a decrypted part? */
	MU_MSG_PART_TYPE_DECRYPTED	= 1 << 7,
	/* a text/plain part? */
	MU_MSG_PART_TYPE_TEXT_PLAIN     = 1 << 8,
	/* a text/html part? */
	MU_MSG_PART_TYPE_TEXT_HTML      = 1 << 9
};
typedef enum _MuMsgPartType MuMsgPartType;


/* the signature status */
enum _MuMsgPartSigStatus {
	MU_MSG_PART_SIG_STATUS_UNSIGNED         = 0,

	MU_MSG_PART_SIG_STATUS_GOOD,
	MU_MSG_PART_SIG_STATUS_BAD,
	MU_MSG_PART_SIG_STATUS_ERROR,
	MU_MSG_PART_SIG_STATUS_FAIL
};
typedef enum _MuMsgPartSigStatus MuMsgPartSigStatus;

struct _MuMsgPartSigStatusReport {
	MuMsgPartSigStatus verdict;
	const char        *report;
};
typedef struct _MuMsgPartSigStatusReport MuMsgPartSigStatusReport;

/**
 * destroy a MuMsgPartSignatureStatusReport object
 *
 * @param report a MuMsgPartSignatureStatusReport object
 */
void mu_msg_part_sig_status_report_destroy (MuMsgPartSigStatusReport *report);


struct _MuMsgPart {

	/* index of this message part */
	unsigned         index;

	/* cid */
	/* const char       *content_id; */

	/* content-type: type/subtype, ie. text/plain */
	const char       *type;
	const char       *subtype;

	/* size of the part; or < 0 if unknown */
	ssize_t		 size;

	gpointer         data; /* opaque data */

	MuMsgPartType            part_type;
	MuMsgPartSigStatusReport *sig_status_report;
 };
typedef struct _MuMsgPart MuMsgPart;

/**
 * get some appropriate file name for the mime-part
 *
 * @param mpart a MuMsgPart
 * @param construct_if_needed if there is no
 * real filename, construct one.
 *
 * @return the file name (free with g_free)
 */
char *mu_msg_part_get_filename (MuMsgPart *mpart, gboolean construct_if_needed)
	G_GNUC_WARN_UNUSED_RESULT;


/**
 * get the text in the MuMsgPart (ie. in its GMimePart)
 *
 * @param msg a MuMsg
 * @param part a MuMsgPart
 * @param opts MuMsgOptions
 *
 * @return utf8 string for this MIME part, to be freed by caller
 */
char* mu_msg_part_get_text (MuMsg *msg, MuMsgPart *part, MuMsgOptions opts)
	G_GNUC_WARN_UNUSED_RESULT;


/**
 * does this msg part look like an attachment?
 *
 * @param part a message part
 *
 * @return TRUE if it looks like an attachment, FALSE otherwise
 */
gboolean mu_msg_part_maybe_attachment (MuMsgPart *part);


/**
 * save a specific attachment to some targetdir
 *
 * @param msg a valid MuMsg instance
 * @param opts mu-message options (OVERWRITE/USE_EXISTING)
 * @gchar filepath the filepath to save
 * @param partidx index of the attachment you want to save
 * @param err receives error information (when function returns NULL)
 *
 * @return full path to the message part saved or NULL in case or
 * error; free with g_free
 */
gboolean mu_msg_part_save (MuMsg *msg, MuMsgOptions opts,
			   const char *filepath, guint partidx,
			   GError **err);


/**
 * save a message part to a temporary file and return the full path to
 * this file
 *
 * @param msg a MuMsg message
 * @param opts mu-message options (OVERWRITE/USE_EXISTING)
 * @param partidx index of the part to save
 * @param err receives error information if any
 *
 * @return the full path to the temp file, or NULL in case of error
 */
gchar* mu_msg_part_save_temp (MuMsg *msg, MuMsgOptions opts,
			      guint partidx, GError **err)
        G_GNUC_WARN_UNUSED_RESULT;



/**
 * get a filename for the saving the message part; try the filename
 * specified for the message part if any, otherwise determine a unique
 * name based on the partidx and the message path
 *
 * @param msg a msg
 * @param opts mu-message options
 * @param targetdir where to store the part
 * @param partidx the part for which to determine a filename
 * @param err receives error information (when function returns NULL)
 *
 * @return a filepath (g_free when done with it) or NULL in case of error
 */
gchar* mu_msg_part_get_path (MuMsg *msg, MuMsgOptions opts,
			     const char* targetdir,
			     guint partidx, GError **err)
	G_GNUC_WARN_UNUSED_RESULT;


/**
 * get a full path name for a file for saving the message part INDEX;
 * this path is unique (1:1) for this particular message and part for
 * this user. Thus, it can be used as a cache.
 *
 * Will create the directory if needed.
 *
 * @param msg a msg
 * @param opts mu-message options
 * @param partidx the part for which to determine a filename
 * @param err receives error information (when function returns NULL)
 *
 * @return a filepath (g_free when done with it) or NULL in case of error
 */
gchar* mu_msg_part_get_cache_path (MuMsg *msg, MuMsgOptions opts,
				   guint partidx, GError **err)
        G_GNUC_WARN_UNUSED_RESULT;


/**
 * get the part index for the message part with a certain content-id
 *
 * @param msg a message
 * @param content_id a content-id to search
 *
 * @return the part index number of the found part, or -1 if it was not found
 */
int mu_msg_find_index_for_cid (MuMsg *msg, MuMsgOptions opts, const char* content_id);



/**
 * retrieve a list of indices for mime-parts with filenames matching a regex
 *
 * @param msg a message
 * @param opts
 * @param a regular expression to match the filename with
 *
 * @return a list with indices for the files matching the pattern; the
 * indices are the GPOINTER_TO_UINT(lst->data) of the list. They must
 * be freed with g_slist_free
 */
GSList* mu_msg_find_files (MuMsg *msg, MuMsgOptions opts, const GRegex *pattern);


typedef void (*MuMsgPartForeachFunc) (MuMsg *msg, MuMsgPart*, gpointer);


/**
 * call a function for each of the mime part in a message
 *
 * @param msg a valid MuMsg* instance
 * @param func a callback function to call for each contact; when
 * the callback does not return TRUE, it won't be called again
 * @param user_data a user-provide pointer that will be passed to the callback
 * @param options, bit-wise OR'ed
 *
 * @return FALSE in case of error, TRUE otherwise
 */
gboolean mu_msg_part_foreach (MuMsg *msg, MuMsgOptions opts,
			      MuMsgPartForeachFunc func, gpointer user_data);

G_END_DECLS

#endif /*__MU_MSG_PART_H__*/
