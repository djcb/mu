/* 
** Copyright (C) 2008 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef __MU_PATH_H__
#define __MU_PATH_H__

#include <glib.h>
#include <sys/types.h> /* for mode_t */

#include "mu-result.h"          /* for MuResult */

/** 
 * MuPathWalkMsgCallback -- callback function for mu_path_walk_maildir; see the
 * documentation there. It will be called for each message found
 */
typedef MuResult (*MuPathWalkMsgCallback) (const char* fullpath,
					   time_t timestamp,
					   void *user_data);

/** 
 * MuPathWalkDirCallback -- callback function for mu_path_walk_maildir; see the
 * documentation there. It will be called each time a dir is entered or left.
 */
typedef MuResult (*MuPathWalkDirCallback) (const char* fullpath,
					   gboolean enter,       /* enter (TRUE) or leave (FALSE) dir?*/
					   void *user_data);

/** 
 * start a recursive scan of a maildir; for each file found, we call
 * callback with the path (with the Maildir path of scanner_new as
 * root), the filename, the timestamp (mtime) of the file,and the
 * *data pointer, for user data.  dot-files are ignored, as well as
 * files outside cur/ and new/ dirs and unreadable files; however,
 * dotdirs are visited (ie. '.dotdir/cur'), so this enables Maildir++.
 * (http://www.inter7.com/courierimap/README.maildirquota.html,
 *  search for 'Mission statement')
 *
 * mu_path_walk_maildir wills stop if the callbacks return something
 * != MU_OK. For example, it can return MU_STOP to stop the scan, or
 * some error.
 * 
 * @param path the maildir path to scan
 * @param cb_msg the callback function called for each msg
 * @param cb_dir the callback function called for each dir
 * @param data user data pointer
 * 
 * @return a scanner result; MU_OK if everything went ok, 
 * MU_STOP if we want to stop, or MU_ERROR in
 * case of error
 */
MuResult mu_path_walk_maildir (const char *path, 
			       MuPathWalkMsgCallback cb_msg, 
			       MuPathWalkDirCallback cb_dir, 
			       void *data);


#endif /*__MU_PATH_H__*/

