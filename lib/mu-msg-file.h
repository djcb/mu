/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

/*
** Copyright (C) 2012-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef __MU_MSG_FILE_H__
#define __MU_MSG_FILE_H__

struct _MuMsgFile;
typedef struct _MuMsgFile MuMsgFile;

/**
 * create a new message from a file
 *
 * @param path full path to the message
 * @param mdir
 * @param err error to receive (when function returns NULL), or NULL
 *
 * @return a new MuMsg, or NULL in case of error
 */
MuMsgFile *mu_msg_file_new (const char *path,
			    const char* mdir, GError **err)
                            G_GNUC_MALLOC G_GNUC_WARN_UNUSED_RESULT;

/**
 * destroy a MuMsgFile object
 *
 * @param self object to destroy, or NULL
 */
void mu_msg_file_destroy (MuMsgFile *self);



/**
 * get a specific header
 *
 * @param self a MuMsgFile instance
 * @param header a header (e.g. 'X-Mailer' or 'List-Id')
 *
 * @return the value of the header or NULL if not found; free with g_free
 */
char* mu_msg_file_get_header (MuMsgFile *self, const char *header);


/**
 * get a string value for this message
 *
 * @param self a valid MuMsgFile
 * @param msfid the message field id to get (must be of type string)
 * @param do_free receives TRUE or FALSE, conveying if this string
 * should be owned & freed (TRUE) or not by caller. In case 'FALSE',
 * this function should be treated as if it were returning a const
 * char*, and note that in that case the string is only valid as long
 * as the MuMsgFile is alive, ie. before mu_msg_file_destroy
 *
 * @return a string, or NULL
 */
char* mu_msg_file_get_str_field (MuMsgFile *self,
				 MuMsgFieldId msfid,
				 gboolean *do_free)
	                         G_GNUC_WARN_UNUSED_RESULT;


/**
 * get a string-list value for this message
 *
 * @param self a valid MuMsgFile
 * @param msfid the message field id to get (must be of type string-list)
 *
 * @return a GSList*, or NULL; free with mu_str_free_list
 */
GSList* mu_msg_file_get_str_list_field (MuMsgFile *self, MuMsgFieldId msfid)
					G_GNUC_WARN_UNUSED_RESULT;



/**
 * get a numeric value for this message -- the return value should be
 * cast into the actual type, e.g., time_t, MuMsgPrio etc.
 *
 * @param self a valid MuMsgFile
 * @param msfid the message field id to get (must be string-based one)
 *
 * @return the numeric value, or -1 in case of error
 */
gint64 mu_msg_file_get_num_field (MuMsgFile *self, MuMsgFieldId mfid);


#endif /*__MU_MSG_FILE_H__*/
