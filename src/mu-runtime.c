/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-
**
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

#include "mu-runtime.h"

#include <glib-object.h>
#include <locale.h> /* for setlocale() */
#include <stdio.h> /* for fileno() */
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <mu-msg.h>

#include "mu-config.h"
#include "mu-log.h"
#include "mu-util.h"

#define MU_XAPIAN_DIRNAME     "xapian"
#define MU_BOOKMARKS_FILENAME "bookmarks"

struct _MuRuntimeData {
	gchar			*_muhome;
	gchar			*_xapian_dir;
	gchar			*_bookmarks_file;
	MuConfig		*_config;
};
typedef struct _MuRuntimeData	 MuRuntimeData;

/* static, global data for this singleton */
static gboolean		 _initialized = FALSE;
static MuRuntimeData	*_data	      = NULL;

static void runtime_free (void);

static gboolean
mu_dir_is_readable_and_writable (const char* muhome)
{
	if (mu_util_create_dir_maybe (muhome))
		return TRUE;
	
	g_warning ("cannot use '%s' as a mu homedir", muhome);
	g_warning ("use --muhome= to set a different one");
	
	return FALSE;
}



gboolean
mu_runtime_init (const char* muhome_arg)
{
	gchar *muhome;

	g_return_val_if_fail (!_initialized, FALSE);

	if (!mu_util_init_system())
		return FALSE;
	
	if (muhome_arg)
		muhome = g_strdup (muhome_arg);
	else
		muhome = mu_util_guess_mu_homedir ();

	if (!mu_dir_is_readable_and_writable (muhome)) {
		runtime_free ();
		return FALSE;
	}
	
	if (!mu_log_init (muhome, TRUE, FALSE, FALSE)) {
		g_free (muhome);
		return FALSE;
	}
	
	_data = g_new0 (MuRuntimeData, 1);
 	_data->_muhome = muhome;

	mu_msg_gmime_init ();
	
	return _initialized = TRUE;
}


static gboolean
init_log (MuConfig *opts)
{
	if (opts->log_stderr)
		return mu_log_init_with_fd (fileno(stderr), FALSE,
					  opts->quiet, opts->debug);
	else 
		return mu_log_init (opts->muhome, TRUE, opts->quiet,
				    opts->debug);
}

gboolean
mu_runtime_init_from_cmdline (int *pargc, char ***pargv)
{
	g_return_val_if_fail (!_initialized, FALSE);

	if (!mu_util_init_system())
		return FALSE;

	_data	       = g_new0 (MuRuntimeData, 1);	
	_data->_config = mu_config_new (pargc, pargv);
	if (!_data->_config) {
		runtime_free ();
		return FALSE;
	 }
	
	if (!mu_dir_is_readable_and_writable (_data->_config->muhome)) {
		runtime_free ();
		return FALSE;
	}

	if (!init_log (_data->_config)) {
		runtime_free ();
		return FALSE;
	}
	
	_data->_muhome = g_strdup (_data->_config->muhome);

	mu_msg_gmime_init ();
	
	return _initialized = TRUE;
}


static void
runtime_free (void)
{
	g_free (_data->_xapian_dir);
	g_free (_data->_muhome);

	mu_config_destroy (_data->_config);

	mu_log_uninit();
	
	g_free (_data);	
}

void
mu_runtime_uninit (void)
{
	g_return_if_fail (_initialized);

	mu_msg_gmime_uninit ();
	
	runtime_free ();

	_initialized = FALSE;
}
	

const char*
mu_runtime_mu_home_dir (void)
{
	g_return_val_if_fail (_initialized, NULL);
  
	return _data->_muhome;
}

const char*
mu_runtime_xapian_dir (void)
{
	g_return_val_if_fail (_initialized, NULL);

	if (!_data->_xapian_dir)
		_data->_xapian_dir = g_strdup_printf ("%s%c%s",
						      _data->_muhome,
						      G_DIR_SEPARATOR,
						      MU_XAPIAN_DIRNAME);
	return _data->_xapian_dir;
}

const char*
mu_runtime_bookmarks_file  (void)
{
	g_return_val_if_fail (_initialized, NULL);

	if (!_data->_bookmarks_file)
		_data->_bookmarks_file = 
			g_strdup_printf ("%s%c%s",
					 _data->_muhome,
					 G_DIR_SEPARATOR,
					 MU_BOOKMARKS_FILENAME);
	
	return _data->_bookmarks_file;
}


MuConfig*
mu_runtime_config (void)
{
	g_return_val_if_fail (_initialized, NULL);
	
	return _data->_config;
}
