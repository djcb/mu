/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

/* 
** Copyright (C) 2008-2011 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef __MU_LOG_H__
#define __MU_LOG_H__

#include <glib.h>

/* mu log is the global logging system */

G_BEGIN_DECLS

/**
 * write logging information to a log file
 * 
 * @param muhome the mu home directory
 * @param backup if TRUE and size of log file > MU_MAX_LOG_FILE_SIZE, move
 * the log file to <log file>.old and start a new one. The .old file will overwrite
 * existing files of that name
 * @param quiet don't log non-errors to stdout/stderr
 * @param debug include debug-level information.
 * 
 * @return TRUE if initialization succeeds, FALSE otherwise
 */
gboolean mu_log_init  (const char* muhome, gboolean backup,
		       gboolean quiet, gboolean debug)
	   G_GNUC_WARN_UNUSED_RESULT;

/**
 * write logging information to a file descriptor
 * 
 * @param fd an open file descriptor
 * @param doclose if true, mu-log will close it upon mu_log_uninit
 * @param quiet don't log non-errors to stdout/stderr
 * @param debug include debug-level info
 * 
 * @return TRUE if initialization succeeds, FALSE otherwise
 */
gboolean mu_log_init_with_fd    (int fd, gboolean doclose, gboolean quiet,
				 gboolean debug) G_GNUC_WARN_UNUSED_RESULT;

/**
 * be silent except for runtime errors, which will be written to
 * stderr.
 *  
 * @return TRUE if initialization succeeds, FALSE otherwise
 */
gboolean mu_log_init_silence    (void) G_GNUC_WARN_UNUSED_RESULT;

/**
 * unitialize the logging system, and free all resources
 */
void mu_log_uninit             (void);

G_END_DECLS

#endif /*__MU_LOG_H__*/
