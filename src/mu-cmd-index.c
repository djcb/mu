/*
** Copyright (C) 2008-2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include "mu-cmd.h"

#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <signal.h>
#include <unistd.h>

#include "mu-util.h"
#include "mu-util-db.h"

#include "mu-msg.h"
#include "mu-index.h"

static gboolean MU_CAUGHT_SIGNAL;

static void
maybe_newline (gboolean quiet)
{
if (!quiet)
	g_print ("\n");
}

static void
update_warning (void)
{
	g_warning ("Note: the database needs to be upgraded to version %s",
		   MU_XAPIAN_DB_VERSION);
	g_warning ("Please run 'mu index --rebuild' (see the manpage)");
}

static void
sig_handler (int sig)
{
	if (!MU_CAUGHT_SIGNAL && sig == SIGINT) { /* Ctrl-C */
		g_print ("\n");
		g_warning ("Shutting down gracefully, "
			   "press again to kill immediately");
	}
	
        MU_CAUGHT_SIGNAL = TRUE;
}

static void
install_sig_handler (void)
{
        struct sigaction action;
        int i, sigs[] = { SIGINT, SIGHUP, SIGTERM };
	
        MU_CAUGHT_SIGNAL = FALSE;

        action.sa_handler = sig_handler;
        sigemptyset(&action.sa_mask);
        action.sa_flags = SA_RESETHAND;

        for (i = 0; i != G_N_ELEMENTS(sigs); ++i)
                if (sigaction (sigs[i], &action, NULL) != 0)
                        g_critical ("set sigaction for %d failed: %s",
				    sigs[i], strerror (errno));;
}


static gboolean
check_index_params (MuConfigOptions *opts)
{
	if (opts->linksdir || opts->xquery) {
		g_warning ("invalid option(s) for command");
		return FALSE;
	}
	
	if (!opts->maildir || !g_path_is_absolute (opts->maildir)) {
		g_warning ("maildir path '%s' is not valid",
			   opts->maildir);
		return FALSE;
	}
	
	if (!mu_util_check_dir (opts->maildir, TRUE, FALSE)) {
		g_warning ("not a valid Maildir: %s",
			   opts->maildir);
		return FALSE;
	}
	
	return TRUE;
}


static MuResult
index_msg_silent_cb  (MuIndexStats* stats, void *user_data)
{
	return MU_CAUGHT_SIGNAL ? MU_STOP: MU_OK;
}

static unsigned
print_stats (MuIndexStats* stats)
{
	char *kars="-\\|/";
	char output[120];
	
	static int i = 0;
	unsigned len = 0;

	len = (unsigned) snprintf (output, sizeof(output),
				   "%c processing mail; processed: %u; "
				   "updated/new: %u, cleaned-up: %u",
				   (unsigned)kars[++i % 4],
				   (unsigned)stats->_processed,
				   (unsigned)stats->_updated,
				   (unsigned)stats->_cleaned_up);

        g_print ("%s", output);
	return len;
}


static MuResult
index_msg_cb  (MuIndexStats* stats, void *user_data)
{
	static unsigned len = 0;
	
	if (MU_CAUGHT_SIGNAL)
		return MU_STOP;
	
	if (stats->_processed % 25)
	 	return MU_OK;
	
	while (len --> 0) /* note the --> operator :-) */
		g_print ("\b");

	len = print_stats (stats);
		
	return MU_CAUGHT_SIGNAL ? MU_STOP: MU_OK;
}



static gboolean
database_version_check_and_update (MuConfigOptions *opts)
{
	if (mu_util_db_is_empty (opts->xpath))
		return TRUE;
	
	/* we empty the database before doing anything */
	if (opts->rebuild) {
		opts->reindex = TRUE;
		g_message ("Clearing database %s", opts->xpath);
		return mu_util_clear_database (opts->xpath);
	}

	if (mu_util_db_version_up_to_date (opts->xpath))
		return TRUE; /* ok, nothing to do */
	
	/* ok, database is not up to date */
	if (opts->autoupgrade) {
		opts->reindex = TRUE;
		g_message ("Auto-upgrade: clearing old database first");
		return mu_util_clear_database (opts->xpath);
	}

	update_warning ();
	return FALSE;
}


static void
show_time (unsigned t, unsigned processed)
{
	if (t)
		g_message ("Elapsed: %u second(s), ~ %u msg/s",
			   t, processed/t);
	else
		g_message ("Elapsed: %u second(s)", t);
}

gboolean
mu_cmd_cleanup (MuConfigOptions *opts)
{
	MuResult rv;	
	MuIndex *midx;
	MuIndexStats stats;
	time_t t;
	gboolean quiet;
	
	g_return_val_if_fail (opts, FALSE);
	g_return_val_if_fail (mu_cmd_equals (opts, "cleanup") ||
			      mu_cmd_equals (opts, "index"),
			      FALSE);
	
	if (!check_index_params (opts))
		return FALSE;

	install_sig_handler ();
	
	midx = mu_index_new (opts->xpath);
	if (!midx) {
		g_warning ("Cleanup failed");
		return FALSE;
	}
	
	g_message ("Cleaning up removed messages from %s",
		   opts->xpath);
	mu_index_stats_clear (&stats);

	t = time (NULL);
	quiet = opts->quiet || !isatty(fileno(stdout));
	rv = mu_index_cleanup (midx, &stats,
			       quiet ? index_msg_silent_cb : index_msg_cb,
			       NULL);
	maybe_newline (opts->quiet);
	show_time ((unsigned)(time(NULL)-t),stats._processed);		
	
	mu_index_destroy (midx);

	return (rv == MU_OK || rv == MU_STOP) ? TRUE: FALSE;
}


gboolean
mu_cmd_index (MuConfigOptions *opts)
{
	MuResult rv;
	gboolean quiet;	
	MuIndex *midx;
	MuIndexStats stats;
	time_t t;
	
	g_return_val_if_fail (opts && mu_cmd_equals (opts, "index"), FALSE);
	
	if (!check_index_params (opts) || !database_version_check_and_update(opts))
		return FALSE;
	
	if (!(midx = mu_index_new (opts->xpath))) {
		g_warning ("Indexing failed");
		return FALSE;
	} 
	
	g_message ("Indexing messages under %s", opts->maildir);
	g_message ("Database: %s", opts->xpath);
	t = time (NULL);

	mu_index_stats_clear (&stats);
	quiet = opts->quiet || !isatty(fileno(stdout));
	install_sig_handler ();
	rv    = mu_index_run (midx, opts->maildir, opts->reindex, &stats,
			   quiet ? index_msg_silent_cb :index_msg_cb, NULL, NULL);
	maybe_newline (opts->quiet);
	show_time ((unsigned)(time(NULL)-t), stats._processed);	
	mu_index_destroy (midx);
	
	if (rv == MU_OK  && !opts->nocleanup)
		rv = mu_cmd_cleanup (opts);
	
	MU_WRITE_LOG ("processed: %u; updated/new: %u, cleaned-up: %u",
		      stats._processed, stats._updated, stats._cleaned_up);
	
	return (rv == MU_OK || rv == MU_STOP) ? TRUE: FALSE;
}
