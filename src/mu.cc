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

#include <config.h>

#include <glib.h>
#include <stdio.h> /* for fileno() */

#include "mu-util.h"
#include "mu-config.h"
#include "mu-cmd.h"
#include "mu-log.h"

static gboolean
init_log (MuConfigOptions *opts)
{
	gboolean rv;
	
	if (opts->log_stderr)
		rv = mu_log_init_with_fd (fileno(stderr), FALSE,
					  opts->quiet, opts->debug);
	else
		rv = mu_log_init (opts->muhome, TRUE, opts->quiet,
				  opts->debug);

	/* we use g_printerr here because g_warning does not give
	 * the desired result when log initialization failed */
	if (!rv)
		g_printerr ("error: failed to initialize log\n");
	
	return rv;
}

int
main (int argc, char *argv[])
{
	MuConfigOptions config;
	gboolean rv;

	if (!mu_util_init_system()) 
		return 1;
	
	if (!mu_config_init (&config, &argc, &argv))
		return 1;

	if (!init_log (&config)) {
		mu_config_uninit (&config);
		return 1;
	}
			
	rv = mu_cmd_execute (&config);

	mu_log_uninit();

	mu_config_uninit (&config);
	
	return rv ? 0 : 1;
}

