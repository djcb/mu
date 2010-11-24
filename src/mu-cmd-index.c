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
#include "mu-runtime.h"

static gboolean MU_CAUGHT_SIGNAL;

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
			   opts->maildir ? opts->maildir : "<none>");
		return FALSE;
	}
	
	if (!mu_util_check_dir (opts->maildir, TRUE, FALSE)) {
		g_warning ("not a valid Maildir: %s",
			   opts->maildir ? opts->maildir : "<none>");
		return FALSE;
	}
	
	return TRUE;
}


static MuResult
index_msg_silent_cb  (MuIndexStats* stats, void *user_data)
{
	return MU_CAUGHT_SIGNAL ? MU_STOP: MU_OK;
}

static void
print_stats (MuIndexStats* stats, gboolean clear)
{
	char *kars="-\\|/";
	char output[120];
	
	static int i = 0;
	static unsigned len = 0;

	if (clear)
		while (len --> 0) /* note the --> operator :-) */
			g_print ("\b");
	
	len = (unsigned) snprintf (output, sizeof(output),
				   "%c processing mail; processed: %u; "
				   "updated/new: %u, cleaned-up: %u",
				   (unsigned)kars[++i % 4],
				   (unsigned)stats->_processed,
				   (unsigned)stats->_updated,
				   (unsigned)stats->_cleaned_up);

        g_print ("%s", output);
}


static MuResult
index_msg_cb  (MuIndexStats* stats, void *user_data)
{
	if (stats->_processed % 25)
	 	return MU_OK;
	
	print_stats (stats, TRUE);
		
	return MU_CAUGHT_SIGNAL ? MU_STOP: MU_OK;
}



static gboolean
database_version_check_and_update (MuConfigOptions *opts)
{
	const gchar *xpath;

	xpath = mu_runtime_xapian_dir ();
	
	if (mu_util_db_is_empty (xpath))
		return TRUE;
	
	/* when rebuilding, we empty the database before doing
	 * anything */
	if (opts->rebuild) {
		opts->reindex = TRUE;
		g_message ("Clearing database %s", xpath);
		return mu_util_clear_database (xpath);
	}

	if (mu_util_db_version_up_to_date (xpath))
		return TRUE; /* ok, nothing to do */
	
	/* ok, database is not up to date */
	if (opts->autoupgrade) {
		opts->reindex = TRUE;
		g_message ("Auto-upgrade: clearing old database first");
		return mu_util_clear_database (xpath);
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



static gboolean
cmd_cleanup (MuIndex *midx, MuConfigOptions *opts, MuIndexStats *stats,
	     gboolean show_progress)
{
	MuResult rv;
	time_t t;
	
	g_message ("Cleaning up messages [%s]", mu_runtime_xapian_dir());
	
	t = time (NULL);
	rv = mu_index_cleanup (midx, stats,
			       show_progress ? index_msg_cb : index_msg_silent_cb,
			       NULL);
	
	if (!opts->quiet) {
		print_stats (stats, TRUE);
		g_print ("\n");
		show_time ((unsigned)(time(NULL)-t),stats->_processed);
	}
	
	return (rv == MU_OK || rv == MU_STOP) ? TRUE: FALSE;
}



static gboolean
cmd_index (MuIndex *midx, MuConfigOptions *opts, MuIndexStats *stats,
	   gboolean show_progress)
{
	MuResult rv;
	time_t t;
	
	g_message ("Indexing messages under %s [%s]", opts->maildir,
		   mu_runtime_xapian_dir());

	t = time (NULL);
	rv = mu_index_run (midx, opts->maildir, opts->reindex, stats,
			   show_progress ? index_msg_cb : index_msg_silent_cb,
			   NULL, NULL);

	if (!opts->quiet) {
		print_stats (stats, TRUE);
		g_print ("\n");
		show_time ((unsigned)(time(NULL)-t),stats->_processed);
	}
	
	if (rv == MU_OK && !opts->nocleanup) {
		mu_index_stats_clear (stats);
		rv = cmd_cleanup (midx, opts, stats, show_progress);
	}
	
	if (rv == MU_OK || rv == MU_STOP) {
		MU_WRITE_LOG ("processed: %u; updated/new: %u, cleaned-up: %u",
			      stats->_processed, stats->_updated,
			      stats->_cleaned_up);
		return TRUE;
	}
	
	return FALSE;
}



static gboolean
cmd_index_or_cleanup (MuConfigOptions *opts)
{
	gboolean rv;
	MuIndex *midx;
	MuIndexStats stats;
	gboolean show_progress;
	GError *err;
	
	g_return_val_if_fail (opts, FALSE);
	g_return_val_if_fail (mu_cmd_equals (opts, "index") ||
			      mu_cmd_equals (opts, "cleanup"), FALSE);
			  
	if (!check_index_params (opts) ||
	    !database_version_check_and_update(opts))
		return FALSE;

	err = NULL;
	if (!(midx = mu_index_new (mu_runtime_xapian_dir(), &err))) {
		g_warning ("Indexing/Cleanup failed: %s",
			   err->message);
		g_error_free (err);
		return FALSE;
	} 

	/* note, 'opts->quiet' already cause g_message output not to
	 * be shown; here, we make sure we only print progress info if
	 * opts->quiet is false case and when stdout is a tty */
	show_progress = !opts->quiet && isatty(fileno(stdout));

	mu_index_stats_clear (&stats);
	install_sig_handler ();
	
	if (mu_cmd_equals (opts, "index"))
		rv = cmd_index (midx, opts, &stats, show_progress);
	else if (mu_cmd_equals (opts, "cleanup"))
		rv = cmd_cleanup (midx, opts, &stats, show_progress);
	else
		g_assert_not_reached ();
	
	mu_index_destroy (midx);
		
	return rv;
}


gboolean
mu_cmd_index (MuConfigOptions *opts)
{
	g_return_val_if_fail (opts && mu_cmd_equals (opts, "index"),
			      FALSE);
	
	return cmd_index_or_cleanup (opts);
}

gboolean
mu_cmd_cleanup (MuConfigOptions *opts)
{
	g_return_val_if_fail (opts && mu_cmd_equals (opts, "cleanup"),
			      FALSE);
	
	return cmd_index_or_cleanup (opts);
}




