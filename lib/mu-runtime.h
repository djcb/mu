/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-
**
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
#ifndef __MU_RUNTIME_H__
#define __MU_RUNTIME_H__

#include <glib.h>
#include <mu-log.h>

G_BEGIN_DECLS

/**
 * initialize the mu runtime system; initializes logging and other
 * systems. To uninitialize, use mu_runtime_uninit
 *
 * @param muhome path where to find the mu home directory (typicaly, ~/.mu)
 * @param name of the main program, ie. 'mu', 'mug' or
 * 'procmule'. this influences the name of the e.g. the logfile
 *
 * @return TRUE if succeeded, FALSE in case of error
 */
gboolean mu_runtime_init (const char *muhome, const char *name);


/**
 * initialize the mu runtime system with comand line argument; this
 * will parse the command line assuming the parameters of the 'mu'
 * program. Initializes logging and other systems. To uninitialize,
 * use mu_runtime_uninit
 *
 * @param ptr to the param count (typically, argc)
 * @param ptr to the params (typically, argv)
 * @param name of the main program, ie. 'mu', 'mug' or
 * 'procmule'. this influences the name of the e.g. the logfile
 *
 * @return TRUE if succeeded, FALSE in case of error
 */
gboolean mu_runtime_init_from_cmdline (int *pargc, char ***pargv,
				       const char *name);


/**
 * free all resources
 *
 */
void mu_runtime_uninit (void);


enum _MuRuntimePath {
	MU_RUNTIME_PATH_MUHOME,     /* mu home path */
	MU_RUNTIME_PATH_XAPIANDB,   /* mu xapian db path */
	MU_RUNTIME_PATH_BOOKMARKS,  /* mu bookmarks file path */
	MU_RUNTIME_PATH_CACHE,      /* mu cache path */
	MU_RUNTIME_PATH_LOG,        /* mu path for log files */
	MU_RUNTIME_PATH_CONTACTS,   /* mu path to the contacts cache */

	MU_RUNTIME_PATH_NUM
};
typedef enum _MuRuntimePath MuRuntimePath;

/**
 * get a file system path to some 'special' file or directory
 *
 * @return ma string which should be not be modified/freed, or NULL in
 * case of error.
 */
const char* mu_runtime_path   (MuRuntimePath path);

G_END_DECLS

#endif /*__MU_RUNTIME_H__*/
