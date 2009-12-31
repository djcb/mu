/* 
** Copyright (C) 2009 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef __MU_MAILDIR_H__
#define __MU_MAILDIR_H__

#include <glib.h>

/** 
 * create a new maildir. Note, if the function fails 'halfway', it
 * will *not* try to remove the parts the were created.
 * 
 * @param path the path (missing components will be created, as in 'mkdir -p')
 * @param mode the file mode (e.g., 0755)
 * @param noindex add a .noindex file to the maildir, so it will be excluded
 * from indexing by 'mu index'
 * @param err a GError* to receive error info, or NULL
 * 
 * @return TRUE if creation succeeded, FALSE otherwise
 */
gboolean mu_maildir_mkmdir (const char* path, int mode,
			    gboolean noindex, GError **err);


/** 
 * create a symbolic link to a mail message; the function will
 * automatically
 * 
 * @param src the full path to the source message
 * @param targetpath the path to the target maildir; ie., *not*
 * MyMaildir/cur, but just MyMaildir/. The function will figure out
 * the correct subdir then. *
 * @param err a GError* to receive error info, or NULL
 * 
 * @return 
 */
gboolean mu_maildir_link   (const char* src, const char *targetpath,
			    GError **err);

#endif /*__MU_MAILDIR_H__*/
