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

	gboolean _own;     /* close _fd with log_destroy? */
	gboolean _debug;   /* add debug-level stuff? */
	gboolean _quiet;   /* don't write non-error to stdout/stderr */
	
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
				    __FUNCTION__, fd, strerror(errno));
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
	
        MU_LOG	     = g_new(MuLog, 1);
        MU_LOG->_fd  = -1;
	MU_LOG->_own = FALSE;	/* nobody owns silence */
	
	MU_LOG->_old_log_func =
		g_log_set_default_handler ((GLogFunc)silence, NULL);

	return TRUE;
}

static void
log_handler (const gchar* log_domain, GLogLevelFlags log_level,
	     const gchar* msg)
{
	if (log_level == G_LOG_LEVEL_DEBUG && !MU_LOG->_debug)
	 	return;

	log_write (log_domain ? log_domain : "mu", log_level, msg);
}


gboolean
mu_log_init_with_fd (int fd, gboolean doclose,
		     gboolean quiet, gboolean debug)
{
	g_return_val_if_fail (!MU_LOG, FALSE);
	
        MU_LOG = g_new(MuLog, 1);

        MU_LOG->_fd		    = fd;
	MU_LOG->_quiet		    = quiet;
	MU_LOG->_newline_before_msg = FALSE;
	MU_LOG->_debug		    = debug;
        MU_LOG->_own		    = doclose; /* if we now own the fd, close it
				    * in _destroy */
	MU_LOG->_old_log_func	    = 
		g_log_set_default_handler ((GLogFunc)log_handler, NULL);
	
	return TRUE;
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
mu_log_init  (const char* muhome,
	      gboolean backup, gboolean quiet, gboolean debug)
{
	int fd;
	gchar *logfile;
	
	/* only init once... */
	g_return_val_if_fail (!MU_LOG, FALSE);	
	g_return_val_if_fail (muhome, FALSE);

	if (!mu_util_create_dir_maybe(muhome, 0700)) {
		g_warning ("failed to init log in %s", muhome);
		return FALSE;
	}
	
	logfile = g_strdup_printf ("%s%c%s", muhome,
				   G_DIR_SEPARATOR, MU_LOG_FILE);

	if (backup && !log_file_backup_maybe(logfile)) {
		g_warning ("failed to backup log file");
		return FALSE;
	}
	
	fd = open (logfile, O_WRONLY|O_CREAT|O_APPEND, 00600);
	if (fd < 0) 
		g_warning ("%s: open() of '%s' failed: %s",  __FUNCTION__,
			   logfile, strerror(errno));
	g_free (logfile);
	
	if (fd < 0 || !mu_log_init_with_fd (fd, FALSE, quiet, debug)) {
		try_close (fd);
		return FALSE;
	}
	
	return TRUE;
}

void 
mu_log_uninit (void)
{
	if (!MU_LOG)
		return;

	if (MU_LOG->_own)
		try_close (MU_LOG->_fd);

	g_free (MU_LOG);

	MU_LOG = NULL;
}


static const char*
pfx (GLogLevelFlags level)
{
	switch (level) {
	case G_LOG_LEVEL_WARNING:  return  "WARN";
	case G_LOG_LEVEL_ERROR :   return  "ERR ";
	case G_LOG_LEVEL_DEBUG:	   return  "DBG ";
	case G_LOG_LEVEL_CRITICAL: return  "CRIT";
	case G_LOG_LEVEL_MESSAGE:  return  "MSG ";
	case G_LOG_LEVEL_INFO :	   return  "INFO";
	default:		   return  "LOG "; 
	}
}

static void
log_write (const char* domain, GLogLevelFlags level, 
	   const gchar *msg)
{
	time_t now;
	ssize_t len;
	
	/* log lines will be truncated at 255 chars */
	char buf [512], timebuf [32];

	/* get the time/date string */
	now = time(NULL);
	strftime (timebuf, sizeof(timebuf), "%F %T", localtime(&now));
		
	/* now put it all together */
	len = snprintf (buf, sizeof(buf), "%s [%s] %s\n", timebuf, 
			pfx(level), msg);
	/* if the buffer is full, add a newline */ 
	if (len == sizeof(buf))
		buf[sizeof(buf)-2] = '\n';
	
	len = write (MU_LOG->_fd, buf, (size_t)len);
	if (len < 0)
		fprintf (stderr, "%s: failed to write to log: %s\n",
			 __FUNCTION__,  strerror(errno));
	
	if (!(MU_LOG->_quiet) && (level & G_LOG_LEVEL_MESSAGE)) {
		fputs ("mu: ", stdout);
		fputs (msg,    stdout);
		fputs ("\n",   stdout);
		fflush (stdout);
	}
		
	/* for serious errors, log them to stderr as well */
	if (level & G_LOG_LEVEL_ERROR ||
	    level & G_LOG_LEVEL_CRITICAL ||
	    level & G_LOG_LEVEL_WARNING) {
		fputs ("mu: ", stderr);
		fputs (msg,    stderr);
		fputs ("\n",   stderr);
		fflush (stderr);
	}
}
