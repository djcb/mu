/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

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

#if HAVE_CONFIG_H
#include <config.h>
#endif /*HAVE_CONFIG_H*/

#include "mu-log.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>
#include <errno.h>
#include <string.h>

#include "mu-util.h"

#define MU_MAX_LOG_FILE_SIZE 1000 * 1000 /* 1 MB (SI units) */
#define MU_LOG_FILE "mu.log"

struct _MuLog {
	int _fd;           /* log file descriptor */

	MuLogOptions _opts;

	gboolean _color_stdout;   /* whether to use color */
	gboolean _color_stderr;

	GLogFunc _old_log_func;
};
typedef struct _MuLog MuLog;

/* we use globals, because logging is a global operation as it
 * globally modifies the behaviour of g_warning and friends
 */
static MuLog* MU_LOG = NULL;
static void log_write (const char* domain, GLogLevelFlags level,
		       const gchar *msg);

static void
try_close (int fd)
{
	if (fd < 0)
		return;

	if (close (fd) < 0)
		g_printerr ("%s: close() of fd %d failed: %s\n",
				    __func__, fd, strerror(errno));
}

static void
silence (void)
{
	return;
}

gboolean
mu_log_init_silence (void)
{
	g_return_val_if_fail (!MU_LOG, FALSE);

        MU_LOG	      = g_new0 (MuLog, 1);
        MU_LOG->_fd   = -1;

	mu_log_options_set (MU_LOG_OPTIONS_NONE);

	MU_LOG->_old_log_func =
		g_log_set_default_handler ((GLogFunc)silence, NULL);

	return TRUE;
}

static void
log_handler (const gchar* log_domain, GLogLevelFlags log_level,
	     const gchar* msg)
{
	if ((log_level & G_LOG_LEVEL_DEBUG) &&
	    !(MU_LOG->_opts & MU_LOG_OPTIONS_DEBUG))
	 	return;

	log_write (log_domain ? log_domain : "mu", log_level, msg);
}


void
mu_log_options_set (MuLogOptions opts)
{
	g_return_if_fail (MU_LOG);

	MU_LOG->_opts = opts;

	/* when color is, only enable it when output is to a tty */
	if (MU_LOG->_opts & MU_LOG_OPTIONS_COLOR) {
		MU_LOG->_color_stdout = isatty(fileno(stdout));
		MU_LOG->_color_stderr = isatty(fileno(stderr));

	}
}


MuLogOptions
mu_log_options_get (void)
{
	g_return_val_if_fail (MU_LOG, MU_LOG_OPTIONS_NONE);

	return MU_LOG->_opts;
}


static gboolean
move_log_file (const char *logfile)
{
	gchar *logfile_old;
	int rv;

	logfile_old = g_strdup_printf ("%s.old", logfile);
	rv = rename (logfile, logfile_old);
	g_free (logfile_old);

	if (rv != 0) {
		g_warning ("failed to move %s to %s.old: %s",
			   logfile, logfile, strerror(rv));
		return FALSE;
	} else
		return TRUE;

}


static gboolean
log_file_backup_maybe (const char *logfile)
{
	struct stat statbuf;

	if (stat (logfile, &statbuf) != 0) {
		if (errno == ENOENT)
			return TRUE; /* it did not exist yet, no problem */
		else {
			g_warning ("failed to stat(2) %s: %s",
					   logfile, strerror(errno));
			return FALSE;
		}
	}

	/* log file is still below the max size? */
	if (statbuf.st_size <= MU_MAX_LOG_FILE_SIZE)
		return TRUE;

	/* log file is too big!; we move it to <logfile>.old, overwriting */
	return move_log_file (logfile);
}


gboolean
mu_log_init (const char* logfile, MuLogOptions opts)
{
	int fd;

	/* only init once... */
	g_return_val_if_fail (!MU_LOG, FALSE);
	g_return_val_if_fail (logfile, FALSE);

	if (opts & MU_LOG_OPTIONS_BACKUP)
		if (!log_file_backup_maybe(logfile)) {
			g_warning ("failed to backup log file");
			return FALSE;
		}

	fd = open (logfile, O_WRONLY|O_CREAT|O_APPEND, 00600);
	if (fd < 0) {
		g_warning ("%s: open() of '%s' failed: %s",  __func__,
			   logfile, strerror(errno));
		return FALSE;
	}

	MU_LOG = g_new0 (MuLog, 1);
	MU_LOG->_fd = fd;

	mu_log_options_set (opts);

	MU_LOG->_old_log_func =
		g_log_set_default_handler ((GLogFunc)log_handler, NULL);

	MU_WRITE_LOG ("logging started");

	return TRUE;
}

void
mu_log_uninit (void)
{
	if (!MU_LOG)
		return;

	MU_WRITE_LOG ("logging stopped");

	try_close (MU_LOG->_fd);
	g_free (MU_LOG);

	MU_LOG = NULL;
}


static const char*
levelstr (GLogLevelFlags level)
{
	switch (level) {
	case G_LOG_LEVEL_WARNING:  return  " [WARN] ";
	case G_LOG_LEVEL_ERROR :   return  " [ERR ] ";
	case G_LOG_LEVEL_DEBUG:	   return  " [DBG ] ";
	case G_LOG_LEVEL_CRITICAL: return  " [CRIT] ";
	case G_LOG_LEVEL_MESSAGE:  return  " [MSG ] ";
	case G_LOG_LEVEL_INFO :	   return  " [INFO] ";
	default:		   return  " [LOG ] ";
	}
}



#define color_stdout_maybe(C)					    \
	do{if (MU_LOG->_color_stdout) fputs ((C),stdout);} while (0)
#define color_stderr_maybe(C)					    \
	do{if (MU_LOG->_color_stderr) fputs ((C),stderr);} while (0)



static void
log_write_fd (GLogLevelFlags level, const gchar *msg)
{
	time_t now;
	char timebuf [22];
	const char *mylevel;

	/* get the time/date string */
	now = time(NULL);
	strftime (timebuf, sizeof(timebuf), "%Y-%m-%d %H:%M:%S",
		  localtime(&now));

	if (write (MU_LOG->_fd, timebuf, strlen (timebuf)) < 0)
		goto err;

	mylevel = levelstr (level);
	if (write (MU_LOG->_fd, mylevel, strlen (mylevel)) < 0)
		goto err;

	if (write (MU_LOG->_fd, msg, strlen (msg)) < 0)
		goto err;

	if (write (MU_LOG->_fd, "\n", strlen ("\n")) < 0)
		goto err;

	return; /* all went well */

err:
	fprintf (stderr, "%s: failed to write to log: %s\n",
		 __func__,  strerror(errno));
}


static void
log_write_stdout_stderr (GLogLevelFlags level, const gchar *msg)
{
	const char *mu;

	mu = MU_LOG->_opts & MU_LOG_OPTIONS_NEWLINE ?
		"\nmu: " : "mu: ";

	if (!(MU_LOG->_opts & MU_LOG_OPTIONS_QUIET) &&
	    (level & G_LOG_LEVEL_MESSAGE)) {
		color_stdout_maybe (MU_COLOR_GREEN);
		fputs (mu, stdout);
		fputs (msg,    stdout);
		fputs ("\n",   stdout);
		color_stdout_maybe (MU_COLOR_DEFAULT);
	}

	/* for errors, log them to stderr as well */
	if (level & G_LOG_LEVEL_ERROR ||
	    level & G_LOG_LEVEL_CRITICAL ||
	    level & G_LOG_LEVEL_WARNING) {
		color_stderr_maybe (MU_COLOR_RED);
		fputs (mu,     stderr);
		fputs (msg,    stderr);
		fputs ("\n",   stderr);
		color_stderr_maybe (MU_COLOR_DEFAULT);
	}
}


static void
log_write (const char* domain, GLogLevelFlags level, const gchar *msg)
{
	g_return_if_fail (MU_LOG);

	log_write_fd (level, msg);
	log_write_stdout_stderr (level, msg);
}
