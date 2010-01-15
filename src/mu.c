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
#include <glib-object.h>
#include <string.h>
#include <stdio.h> /* for fileno() */

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

	if (!rv)
		g_printerr ("error: failed to initialize log\n");
	
	return rv;
}

static gboolean
parse_params (MuConfigOptions *config, int *argcp, char ***argvp)
{
	GError *error = NULL;
	GOptionContext *context;
	gboolean rv;
	
	context = g_option_context_new ("- maildir utilities");

	g_option_context_set_main_group (context,
					 mu_config_options_group_mu (config));
	g_option_context_add_group (context,
				    mu_config_options_group_index (config));
	g_option_context_add_group (context,
				    mu_config_options_group_find (config));
	
	rv = g_option_context_parse (context, argcp, argvp, &error);
	if (!rv) {
		g_printerr ("error in options: %s\n", error->message);
		g_error_free (error);
	} else {
		g_option_context_free (context);
		mu_config_set_defaults (config);
	}
		
	return rv;
}


int
main (int argc, char *argv[])
{
	MuConfigOptions config;
	gboolean rv;
	
	g_type_init ();

	mu_config_init (&config);
	
	do {
		rv = FALSE;

		if (!parse_params (&config, &argc, &argv))
			break;

		if (!init_log (&config))
			break;
		
		rv = mu_cmd_execute (&config);

		mu_log_uninit();

	} while (0); 

	mu_config_uninit (&config);
	
	return rv ? 0 : 1;
}

