/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

/*
** Copyright (C) 2008-2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

G_BEGIN_DECLS

struct _MuMsgPart {

	/* index of this message part */
	unsigned         index;

	/* cid */
	char             *content_id;

	/* content-type: type/subtype, ie. text/plain */
	char             *type;
	char             *subtype;
	/* full content-type, e.g. image/jpeg  */
	/* char             *content_type; */

	/* the file name (if any) */
	char             *file_name;

	/* description (if any) */
	char             *description;

	/* usually, "attachment" or "inline" */
	char             *disposition;

	/* size of the part; or < 0 if unknown */
	ssize_t		 size;

	gpointer         data; /* opaque data */

	gboolean         is_body; /* TRUE if this is probably the
				   * message body*/
	gboolean	 is_leaf; /* if the body is a leaf part (MIME
				   * Part), not eg. a multipart/ */
	gboolean         is_msg;  /* part is a message/rfc822 */

	/* crypto stuff */
	GSList           *sig_infos; /* list of MuMsgPartSig */

	/* if TRUE, mu_msg_part_destroy will free the member vars
	 * as well*/
	gboolean          own_members;
};
typedef struct _MuMsgPart MuMsgPart;

/**
 * macro to get the file name for this mime-part
 *
 * @param pi a MuMsgPart instance
 *
 * @return the file name
 */
#define mu_msg_part_file_name(pi)    ((pi)->file_name)


/**
 * macro to get the description for this mime-part
 *
 * @param pi a MuMsgPart instance
 *
 * @return the description
 */
#define mu_msg_part_description(pi)    ((pi)->description)


/**
 * macro to get the content-id (cid) for this mime-part
 *
 * @param pi a MuMsgPart instance
 *
 * @return the file name
 */
#define  mu_msg_part_content_id(pi) ((pi)->content_id)


/**
 * get the text in the MuMsgPart (ie. in its GMimePart)
 *
 * @param part a MuMsgPart
 * @param err will receive TRUE if there was an error, FALSE otherwise
 *
 * @return utf8 string for this MIME part, to be freed by caller
 */
char* mu_msg_part_get_text (MuMsgPart *part, gboolean *err);


/**
 * does this msg part look like an attachment?
 *
 * @param part a message part
 * @param include_inline consider 'inline' parts also as attachments
 *
 * @return TRUE if it looks like an attachment, FALSE otherwise
 */
gboolean mu_msg_part_looks_like_attachment (MuMsgPart *part,
					    gboolean include_inline);

/**
 * save a specific attachment to some targetdir
 *
 * @param msg a valid MuMsg instance
 * @gchar filepath the filepath to save
 * @param partidx index of the attachment you want to save
 * @param overwrite overwrite existing files?
 * @param don't raise error when the file already exists
 * @param err receives error information (when function returns NULL)
 *
 * @return full path to the message part saved or NULL in case or error; free with g_free
 */
gboolean mu_msg_part_save (MuMsg *msg, const char *filepath, guint partidx,
			   gboolean overwrite, gboolean use_cached, GError **err);


/**
 * save a message part to a temporary file and return the full path to
 * this file
 *
 * @param msg a MuMsg message
 * @param partidx index of the part to save
 * @param err receives error information if any
 *
 * @return the full path to the temp file, or NULL in case of error
 */
gchar* mu_msg_part_save_temp (MuMsg *msg, guint partidx, GError **err);




/**
 * get a filename for the saving the message part; try the filename
 * specified for the message part if any, otherwise determine a unique
 * name based on the partidx and the message path
 *
 * @param msg a msg
 * @param targetdir where to store the part
 * @param partidx the part for which to determine a filename
 * @param err receives error information (when function returns NULL)
 *
 * @return a filepath (g_free when done with it) or NULL in case of error
 */
gchar* mu_msg_part_filepath (MuMsg *msg, const char* targetdir,
			     guint partidx, GError **err) G_GNUC_WARN_UNUSED_RESULT;


/**
 * get a full path name for a file for saving the message part INDEX;
 * this path is unique (1:1) for this particular message and part for
 * this user. Thus, it can be used as a cache.
 *
 * Will create the directory if needed.
 *
 * @param msg a msg
 * @param partidx the part for which to determine a filename
 *
 * @return a filepath (g_free when done with it) or NULL in case of error
 */
gchar* mu_msg_part_filepath_cache (MuMsg *msg, guint partid)
        G_GNUC_WARN_UNUSED_RESULT;


/**
 * get the part index for the message part with a certain content-id
 *
 * @param msg a message
 * @param content_id a content-id to search
 *
 * @return the part index number of the found part, or -1 if it was not found
 */
int mu_msg_part_find_cid (MuMsg *msg, const char* content_id);

/**
 * retrieve a list of indices for mime-parts with filenames matching a regex
 *
 * @param msg a message
 * @param a regular expression to match the filename with
 *
 * @return a list with indices for the files matching the pattern; the
 * indices are the GPOINTER_TO_UINT(lst->data) of the list. They must
 * be freed with g_slist_free
 */
GSList* mu_msg_part_find_files (MuMsg *msg, const GRegex *pattern);


typedef void (*MuMsgPartForeachFunc) (MuMsg*, MuMsgPart*, gpointer);


enum _MuMsgPartOptions {
	MU_MSG_PART_OPTION_NONE              = 0,
	MU_MSG_PART_OPTION_RECURSE_RFC822    = 1 << 0, /* recurse into submessages */

	/* below options are for checking signatures; only effective
	 * if mu was built with crypto support */
	MU_MSG_PART_OPTION_CHECK_SIGNATURES  = 1 << 1,
	MU_MSG_PART_OPTION_AUTO_RETRIEVE_KEY = 1 << 2,
	MU_MSG_PART_OPTION_USE_AGENT         = 1 << 3
};
typedef enum _MuMsgPartOptions MuMsgPartOptions;

/**
 * call a function for each of the mime part in a message
 *
 * @param msg a valid MuMsg* instance
 * @param func a callback function to call for each contact; when
 * the callback does not return TRUE, it won't be called again
 * @param user_data a user-provide pointer that will be passed to the callback
 * @param options, bit-wise OR'ed
 *
 */
void mu_msg_part_foreach (MuMsg *msg, MuMsgPartForeachFunc func, gpointer user_data,
			  MuMsgPartOptions opts);

G_END_DECLS

#endif /*__MU_MSG_PART_H__*/
