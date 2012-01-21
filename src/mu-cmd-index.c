/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

/*
** Copyright (C) 2008-2012 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#if HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include "mu-cmd.h"

#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <signal.h>
#include <unistd.h>

#include "mu-util.h"
#include "mu-msg.h"
#include "mu-index.h"
#include "mu-store.h"
#include "mu-runtime.h"

static gboolean MU_CAUGHT_SIGNAL;

static void
sig_handler (int sig)
{
	if (!MU_CAUGHT_SIGNAL && sig == SIGINT) { /* Ctrl-C */
		g_print ("\n");
		g_warning ("shutting down gracefully, "
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
check_params (MuConfig *opts, GError **err)
{
	/* param[0] == 'index'  there should be no param[1] */
	if (opts->params[1]) {
		g_set_error (err, 0, MU_ERROR_IN_PARAMETERS,
				     "unexpected parameter");
		return FALSE;
	}

	if (opts->xbatchsize < 0) {
		g_set_error (err, 0, MU_ERROR_IN_PARAMETERS,
				     "the batch size must be non-negative");
		return FALSE;
	}

	if (opts->max_msg_size < 0) {
		g_set_error (err, 0, MU_ERROR_IN_PARAMETERS,
				     "the maximum message size must be non-negative");
		return FALSE;
	}

	return TRUE;
}

static gboolean
check_maildir (const char *maildir, GError **err)
{
	if (!maildir) {
		g_set_error (err, 0, MU_ERROR_IN_PARAMETERS,
			     "no maildir to work on; use --maildir=");
		return FALSE;
	}

	if (!g_path_is_absolute (maildir)) {
		g_set_error (err, 0, MU_ERROR_IN_PARAMETERS,
			     "maildir path '%s' is not absolute",
			     maildir);
		return FALSE;
	}

	if (!mu_util_check_dir (maildir, TRUE, FALSE)) {
		g_set_error (err, 0, MU_ERROR_IN_PARAMETERS,
			     "not a valid Maildir: %s", maildir);
		return FALSE;
	}

	return TRUE;
}


static MuError
index_msg_silent_cb (MuIndexStats* stats, void *user_data)
{
	return MU_CAUGHT_SIGNAL ? MU_STOP: MU_OK;
}


static void
backspace (unsigned u)
{
	static gboolean init = FALSE;
	static char backspace[80];

	if (G_UNLIKELY(!init)) {
		/* fill with backspaces */
		int i;
		for (i = 0; i != sizeof(backspace); ++i)
			backspace[i] = '\b';
		init = TRUE;
	}

	backspace[MIN(u,sizeof(backspace))] = '\0';
	fputs (backspace, stdout);
	backspace[u] = '\b';
}




static void
print_stats (MuIndexStats* stats, gboolean clear, gboolean color)
{
	const char *kars="-\\|/";
	char output[120];

	static unsigned i = 0, len = 0;

	if (clear)
		backspace (len);

	if (color)
		len = (unsigned)snprintf
			(output, sizeof(output),
			 MU_COLOR_YELLOW "%c " MU_COLOR_DEFAULT
			 "processing mail; "
			 "processed: " MU_COLOR_GREEN "%u; " MU_COLOR_DEFAULT
			 "updated/new: " MU_COLOR_GREEN "%u" MU_COLOR_DEFAULT
			 ", cleaned-up: " MU_COLOR_GREEN "%u" MU_COLOR_DEFAULT,
			 (unsigned)kars[++i % 4],
			 (unsigned)stats->_processed,
			 (unsigned)stats->_updated,
			 (unsigned)stats->_cleaned_up);
	else
		len = (unsigned)snprintf
			(output, sizeof(output),
			 "%c processing mail; processed: %u; "
			 "updated/new: %u, cleaned-up: %u",
			 (unsigned)kars[++i % 4],
			 (unsigned)stats->_processed,
			 (unsigned)stats->_updated,
			 (unsigned)stats->_cleaned_up);

	fputs (output, stdout);
	fflush (stdout);
}


struct _IndexData {
	gboolean color;
};
typedef struct _IndexData IndexData;


static MuError
index_msg_cb  (MuIndexStats* stats, IndexData *idata)
{
	if (stats->_processed % 25)
	 	return MU_OK;

	print_stats (stats, TRUE, idata->color);

	return MU_CAUGHT_SIGNAL ? MU_STOP: MU_OK;
}



static gboolean
database_version_check_and_update (MuStore *store, MuConfig *opts,
				   GError **err)
{
	if (mu_store_count (store, err) == 0)
		return TRUE;

	/* when rebuilding, we empty the database before doing
	 * anything */
	if (opts->rebuild) {
		opts->reindex = TRUE;
		g_debug ("clearing database");
		g_debug ("clearing contacts-cache");
		return mu_store_clear (store, err);
	}

	if (!mu_store_needs_upgrade (store))
		return TRUE; /* ok, nothing to do */

	/* ok, database is not up to date */
	if (opts->autoupgrade) {
		opts->reindex = TRUE;
		g_debug ("auto-upgrade: clearing old database and cache");
		return mu_store_clear (store, err);
	}

	return FALSE;
}


static void
show_time (unsigned t, unsigned processed, gboolean color)
{

	if (color) {
		if (t)
			g_message ("elapsed: "
				   MU_COLOR_GREEN "%u" MU_COLOR_DEFAULT
				   " second(s), ~ "
				   MU_COLOR_GREEN "%u" MU_COLOR_DEFAULT
				   " msg/s",
				   t, processed/t);
		else
			g_message ("elapsed: "
				   MU_COLOR_GREEN "%u" MU_COLOR_DEFAULT
				   " second(s)", t);
	} else {
		if (t)
			g_message ("elapsed: %u second(s), ~ %u msg/s",
				   t, processed/t);
		else
			g_message ("elapsed: %u second(s)", t);
	}
}



static MuError
cleanup_missing (MuIndex *midx, MuConfig *opts, MuIndexStats *stats,
		 gboolean show_progress, GError **err)
{
	MuError rv;
	time_t t;
	IndexData idata;

	g_message ("cleaning up messages [%s]",
		   mu_runtime_path (MU_RUNTIME_PATH_XAPIANDB));

	mu_index_stats_clear (stats);

	t = time (NULL);
	idata.color = !opts->nocolor;
	rv = mu_index_cleanup
		(midx, stats,
		 show_progress ?
		 (MuIndexCleanupDeleteCallback)index_msg_cb :
		 (MuIndexCleanupDeleteCallback)index_msg_silent_cb,
		 &idata, err);

	if (!opts->quiet) {
		print_stats (stats, TRUE, !opts->nocolor);
		g_print ("\n");
		show_time ((unsigned)(time(NULL)-t),stats->_processed,
			   !opts->nocolor);
	}

	return (rv == MU_OK || rv == MU_STOP) ? MU_OK: MU_G_ERROR_CODE(err);
}



static MuError
cmd_index (MuIndex *midx, MuConfig *opts, MuIndexStats *stats,
	   gboolean show_progress, GError **err)
{
	IndexData idata;
	MuError rv;
	time_t t;

	if (opts->nocolor)
		g_message ("indexing messages under %s [%s]", opts->maildir,
			   mu_runtime_path (MU_RUNTIME_PATH_XAPIANDB));
	else
		g_message ("indexing messages under "
			   MU_COLOR_BLUE "%s" MU_COLOR_DEFAULT
			   " ["
			   MU_COLOR_BLUE "%s" MU_COLOR_DEFAULT
			   "]", opts->maildir,
			   mu_runtime_path (MU_RUNTIME_PATH_XAPIANDB));

	t = time (NULL);
	idata.color = !opts->nocolor;
	rv = mu_index_run (midx, opts->maildir, opts->reindex, stats,
			   show_progress ?
			   (MuIndexMsgCallback)index_msg_cb :
			   (MuIndexMsgCallback)index_msg_silent_cb,
			   NULL, &idata);

	if (!opts->quiet) {
		print_stats (stats, TRUE, !opts->nocolor);
		g_print ("\n");
		show_time ((unsigned)(time(NULL)-t),
			   stats->_processed, !opts->nocolor);
	}

	if (rv == MU_OK || rv == MU_STOP)
		MU_WRITE_LOG ("index: processed: %u; updated/new: %u",
			      stats->_processed, stats->_updated);

	if (rv == MU_OK && !opts->nocleanup)
		rv = cleanup_missing (midx, opts, stats, show_progress, err);

	if (rv == MU_STOP)
		rv = MU_OK;

	if (rv != MU_OK && !err)
		g_set_error (err, 0, rv, "error while indexing");

	return rv;
}


static MuIndex*
init_mu_index (MuStore *store, MuConfig *opts, GError **err)
{
	MuIndex *midx;

	if (!check_params (opts, err))
		return NULL;

	if (!database_version_check_and_update(store, opts, err))
		return NULL;


	if (!check_maildir (opts->maildir, err))
		return NULL;

	midx = mu_index_new (store, err);
	if (!midx)
		return NULL;

	mu_index_set_max_msg_size (midx, opts->max_msg_size);
	mu_index_set_xbatch_size (midx, opts->xbatchsize);

	return midx;
}


MuError
mu_cmd_index (MuStore *store, MuConfig *opts, GError **err)
{
	MuIndex *midx;
	MuIndexStats stats;
	gboolean rv, show_progress;

	g_return_val_if_fail (opts, FALSE);
	g_return_val_if_fail (opts->cmd == MU_CONFIG_CMD_INDEX,
			      FALSE);

	/* create, and do error handling if needed */
	midx = init_mu_index (store, opts, err);
	if (!midx)
		return MU_G_ERROR_CODE(err);

	/* note, 'opts->quiet' already cause g_message output not to
	 * be shown; here, we make sure we only print progress info if
	 * opts->quiet is false case and when stdout is a tty */
	show_progress = !opts->quiet && isatty(fileno(stdout));

	mu_index_stats_clear (&stats);
	install_sig_handler ();

	rv = cmd_index (midx, opts, &stats, show_progress, err);
	mu_index_destroy (midx);

	return rv;
}
