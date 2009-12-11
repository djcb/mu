/*
** Copyright (C) 2008, 2009 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-index.h"
#include "mu-query.h"
#include "mu-util.h"
#include "mu-config.h"
#include "mu-log.h"

#include "mu-msg-gmime.h"

enum _MuCmd {
	MU_CMD_INDEX,
	MU_CMD_QUERY,
	MU_CMD_HELP,
	MU_CMD_UNKNOWN
};
typedef enum _MuCmd MuCmd;



MuCmd 
parse_cmd (const char* cmd)
{
	if (!cmd)
		return MU_CMD_UNKNOWN;

	if (strcmp (cmd, "index") == 0)
		return MU_CMD_INDEX;

	/* support some synonyms... */
	if ((strcmp (cmd, "query") == 0) ||
	    (strcmp (cmd, "find")  == 0) ||
	    (strcmp (cmd, "search") == 0))
		return MU_CMD_QUERY;

	if ((strcmp (cmd, "help") == 0) ||
	    (strcmp (cmd, "info") == 0))
		return MU_CMD_HELP;

	return MU_CMD_UNKNOWN;
}



static MuResult
msg_cb  (MuIndexStats* stats, void *user_data)
{
	char *kars="-\\|/";
	char output[100];
	
	static int i = 0;
	static int len = 0;

	while (len --> 0) 
		printf ("\b");
	
	len = snprintf (output, sizeof(output),
			"%c mu is indexing your mails; processed: %d; "
			"updated/new: %d",
			kars[i % 4], stats->_processed, stats->_updated);
	g_print ("%s", output);
	
	++i;
	
	return MU_OK;
}

static int
show_help (const char* cmd)
{
	if (cmd)
		g_print ("Help about %s\n", cmd);
	else
		g_print ("General help\n");

	return 0;
}

static int
show_version (void)
{
	const char* msg =
		"mu (mail indexer / searcher version) " VERSION "\n\n"
		"Copyright (C) 2009 Dirk-Jan C. Binnema\n"
		"License GPLv3+: GNU GPL version 3 or later "
		"<http://gnu.org/licenses/gpl.html>.\n\n"
		"This is free software: you are free to change "
		"and redistribute it.\n"
		"There is NO WARRANTY, to the extent permitted by law.";

	g_print ("%s\n", msg);

	return 0;
}

static int
show_usage (gboolean noerror)
{
	const char* usage=
		"usage: mu [options] command [parameters]\n"
		"\twhere command is one of index, query, help\n"
		"see mu(1) for for information\n";

	if (noerror)
		g_print ("%s", usage);
	else
		g_printerr ("%s", usage);

	return noerror ? 0 : 1;
}

static gboolean
init_log (MuConfigOptions *opts)
{
	gboolean rv;

	/* TODO: check incompatible options (eg., silent + log_append) */
	
	if (opts->quiet)
		rv = mu_log_init_silence ();	
	else if (opts->log_stderr) 
		rv = mu_log_init_with_fd (fileno(stderr), FALSE,
					  opts->debug);
	else 
		rv = mu_log_init (opts->muhome, opts->log_append,
				  opts->debug);

	if (!rv)
		g_print ("error: failed to initialize log\n");
	
	return rv;
}


int
main (int argc, char *argv[])
{
	GError *error = NULL;
	GOptionContext *context;
	MuConfigOptions config;
	MuResult rv;
	MuCmd cmd;
	gboolean ok;
	
	g_type_init ();
	
	context = g_option_context_new ("- search your e-mail");
	
	g_option_context_set_main_group (context,
					 mu_config_options_group_mu(&config));
	g_option_context_add_group (context,
				    mu_config_options_group_index(&config));
	g_option_context_add_group (context,
				    mu_config_options_group_query(&config));

	mu_config_init (&config);	
	ok = g_option_context_parse (context, &argc, &argv, &error);
	g_option_context_free (context);

	if (!ok) {
		g_printerr ("error in options: %s\n",
			    error->message);
		g_error_free (error);
		return 1;
	}
	
	if (config.version)
		return show_version ();
	
	if (argc < 2)
		return show_usage (FALSE);
	
	cmd = parse_cmd (argv[1]);
	if (cmd == MU_CMD_UNKNOWN)
		return show_usage (FALSE);
	
	if (cmd == MU_CMD_HELP)
		return show_help (argc > 2 ? argv[2] : NULL);

	if (!init_log (&config))
		return 1;

	mu_msg_gmime_init ();
	rv = MU_OK;
	
	if (cmd == MU_CMD_INDEX) {
		MuIndex *midx;
		MuIndexStats stats;
		
		midx = mu_index_new (config.muhome);
		rv = mu_index_run (midx,
				   config.maildir,
				   config.reindex,
				   &stats,
				   config.quiet ? NULL : msg_cb,
				   NULL,
				   NULL);
		g_print ("\n");
		mu_index_destroy (midx);
		
	} else if (cmd == MU_CMD_QUERY) {
		
		GSList *args;
		if (argc < 3) {
			g_printerr ("error: missing something to search for\n");
			rv = 1;
		} else {
			args = mu_util_strlist_from_args (argc-2, argv+2); 
			rv = mu_query_run (&config, args);
			mu_util_strlist_free (args);
		}
	}
	
	mu_msg_gmime_uninit();
	mu_log_uninit();
	mu_config_uninit(&config);
	
	return rv == MU_OK ? 0 : 1;
}
