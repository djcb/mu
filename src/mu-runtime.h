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
#ifndef __MU_RUNTIME_H__
#define __MU_RUNTIME_H__

#include <glib.h>
#include <mu-config.h>

G_BEGIN_DECLS

/**
 * initialize the mu runtime system; initializes logging and other
 * systems. To uninitialize, use mu_runtime_uninit
 * 
 * @param muhome path where to find the mu home directory (typicaly, ~/.mu)
 * 
 * @return TRUE if succeeded, FALSE in case of error
 */
gboolean mu_runtime_init              (const char* muhome);


/**
 * initialize the mu runtime system with comand line argument; this
 * will parse the command line assuming the parameters of the 'mu'
 * program. Initializes logging and other systems. To uninitialize,
 * use mu_runtime_uninit
 * 
 * @param ptr to the param count (typically, argc)
 * @param ptr to the params (typically, argv)
 * 
 * @return TRUE if succeeded, FALSE in case of error
 */
gboolean mu_runtime_init_from_cmdline (int *pargc, char ***pargv);


/**
 * free all resources 
 * 
 */
void mu_runtime_uninit (void);

/**
 * get the mu home directory (typically, ~/.mu); this can only be
 * called after mu_runtime_init and before mu_runtime_uninit
 * 
 * @return mu home directory as a string which should be not be
 * modified, or NULL in case of error.
 */
const char* mu_runtime_mu_home_dir     (void);

/**
 * get the xapian directory (typically, ~/.mu/xapian/); this can only
 * be called after mu_runtime_init and before mu_runtime_uninit
 * 
 * @return the xapian directory as a string which should be not be
 * modified, or NULL in case of error.
 */
const char* mu_runtime_xapian_dir      (void);


/**
 * get the mu bookmarks file (typically, ~/.mu/bookmarks); this can
 * only be called after mu_runtime_init and before mu_runtime_uninit
 * 
 * @return the bookmarks file as a string which should be not be
 * modified, or NULL in case of error.
 */
const char* mu_runtime_bookmarks_file  (void);

/**
 * get the mu contacts cache file name (typically,
 * ~/.mu/contacts.cache); this can only be called after
 * mu_runtime_init and before mu_runtime_uninit
 * 
 * @return the contacts cache file name as a string which should be not be
 * modified, or NULL in case of error.
 */
const char* mu_runtime_contacts_cache_file  (void);


/**
 * get the mu configuration options (ie., the parsed command line
 * parameters)
 * 
 * @return the configuration options
 */
MuConfig* mu_runtime_config (void);

G_END_DECLS

#endif /*__MU_RUNTIME_H__*/
