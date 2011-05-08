/*
** Copyright (C) 2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
MuMsgFile *mu_msg_file_new (const char *path, const char* mdir, GError **err);


/** 
 * destroy a MuMsgFile object
 *
 * @param self object to destroy, or NULL
 */
void mu_msg_file_destroy (MuMsgFile *self);


/** 
 * get a string value for this message
 * 
 * @param self a valid MuMsgFile
 * @param msfid the message field id to get (must be string-based one)
 * 
 * @return a const string, or NULL
 */
const char* mu_msg_file_get_str_field (MuMsgFile *self,
				       MuMsgFieldId msfid);

/** 
 * get a numeric value for this message -- the return value should be
 * cast into the actual type, e.g., time_t, MuMsgPrio etc.
 * 
 * @param self a valid MuMsgFile
 * @param msfid the message field id to get (must be string-based one)
 * 
 * @return the numeric value, or -1
 */
gint64 mu_msg_file_get_num_field (MuMsgFile *self, MuMsgFieldId msfid);

#endif /*__MU_MSG_FILE_H__*/
