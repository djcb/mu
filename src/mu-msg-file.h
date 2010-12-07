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

#include <glib.h>
#include <mu-msg-flags.h>

G_BEGIN_DECLS

/**
 * get the Maildir flags from the full path of a mailfile. The flags
 * are as specified in http://cr.yp.to/proto/maildir.html, plus
 * MU_MSG_FLAG_NEW for new messages, ie the ones that live in
 * new/. The flags are logically OR'ed. Note that the file does not
 * have to exist; the flags are based on the path only.
 *
 * @param pathname of a mailfile; it does not have to refer to an
 * actual message
 * 
 * @return the flags, or MU_MSG_FILE_FLAG_UNKNOWN in case of error
 */
MuMsgFlags mu_msg_file_get_flags_from_path (const char* pathname);


/**
 * get the new pathname for a message, based on the old path and the
 * new flags. Note that setting/removing the MU_MSG_FLAG_NEW will
 * change the directory in which a message lives. The flags are as
 * specified in http://cr.yp.to/proto/maildir.html, plus
 * MU_MSG_FLAG_NEW for new messages, ie the ones that live in
 * new/. The flags are logically OR'ed. Note that the file does not
 * have to exist; the flags are based on the path only.
 * 
 * @param oldpath the old (current) full path to the message (including the filename) 
 * @param newflags the new flags for this message
 * 
 * @return a new path name; use g_free when done with. NULL in case of
 * error.
 */
char* mu_msg_file_get_path_from_flags (const char *oldpath, MuMsgFlags newflags);

G_END_DECLS

#endif /*__MU_MSG_FILE_H__*/
