/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset:
      8 -*-*/
/*
** Copyright (C) 2008-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

enum _MuLogOptions {
	MU_LOG_OPTIONS_NONE	= 0,

	/* when size of log file > MU_MAX_LOG_FILE_SIZE, move the log
	 * file to <log file>.old and start a new one. The .old file will
	 * overwrite existing files of that name */
	MU_LOG_OPTIONS_BACKUP   = 1 << 1,

	/* quiet: don't log non-errors to stdout/stderr */
	MU_LOG_OPTIONS_QUIET    = 1 << 2,

	/* should lines be preceded by \n? useful when errors come
	 * during indexing */
	MU_LOG_OPTIONS_NEWLINE  = 1 << 3,

	/* color in output (iff output is to a tty) */
	MU_LOG_OPTIONS_COLOR    = 1 << 4,

	/* log everything to stderr */
	MU_LOG_OPTIONS_STDERR   = 1 << 5,

	/* debug: debug include debug-level information */
	MU_LOG_OPTIONS_DEBUG    = 1 << 6
};
typedef enum _MuLogOptions MuLogOptions;


/**
 * write logging information to a log file
 *
 * @param full path to the log file (does not have to exist yet, but
 * it's directory must)
 * @param opts logging options
 *
 * @return TRUE if initialization succeeds, FALSE otherwise
 */
gboolean mu_log_init (const char *logfile, MuLogOptions opts)
	   G_GNUC_WARN_UNUSED_RESULT;

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

/**
 * set logging options, a logical-OR'd value of MuLogOptions
 *
 * @param opts the options (logically OR'd)
 */
void mu_log_options_set (MuLogOptions opts);

/**
 * get logging options, a logical-OR'd value of MuLogOptions
 *
 * @param opts the options (logically OR'd)
 */
MuLogOptions mu_log_options_get (void);

G_END_DECLS

#endif /*__MU_LOG_H__*/
