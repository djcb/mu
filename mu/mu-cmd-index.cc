/*
** Copyright (C) 2008-2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "config.h"
#include "mu-cmd.hh"

#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <signal.h>
#include <unistd.h>

#include "mu-msg.h"
#include "mu-index.h"
#include "mu-store.hh"
#include "mu-runtime.h"

#include "utils/mu-util.h"

static gboolean MU_CAUGHT_SIGNAL;

static void
sig_handler (int sig)
{
	if (!MU_CAUGHT_SIGNAL && sig == SIGINT) /* Ctrl-C */
		g_print ("\nshutting down gracefully, "
			   "press again to kill immediately");

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
check_params (const MuConfig *opts, GError **err)
{
	/* param[0] == 'index'  there should be no param[1] */
	if (opts->params[1]) {
		mu_util_g_set_error (err, MU_ERROR_IN_PARAMETERS,
				     "unexpected parameter");
		return FALSE;
	}

	if (opts->max_msg_size < 0) {
		mu_util_g_set_error (err, MU_ERROR_IN_PARAMETERS,
				     "the maximum message size must >= 0");
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
print_stats (MuIndexStats* stats, gboolean clear, gboolean color)
{
	const char *kars="-\\|/";
	char output[120];

	static unsigned i = 0;

	if (clear)
		fputs ("\r", stdout);

	if (color)
		g_snprintf
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
		g_snprintf
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
	if (stats->_processed % 75)
	 	return MU_OK;

	print_stats (stats, TRUE, idata->color);

	return MU_CAUGHT_SIGNAL ? MU_STOP: MU_OK;
}

static void
show_time (unsigned t, unsigned processed, gboolean color)
{
	if (color) {
		if (t)
			g_print ("elapsed: "
				   MU_COLOR_GREEN "%u" MU_COLOR_DEFAULT
				   " second(s), ~ "
				   MU_COLOR_GREEN "%u" MU_COLOR_DEFAULT
				   " msg/s",
				   t, processed/t);
		else
			g_print ("elapsed: "
				   MU_COLOR_GREEN "%u" MU_COLOR_DEFAULT
				   " second(s)", t);
	} else {
		if (t)
			g_print ("elapsed: %u second(s), ~ %u msg/s",
				   t, processed/t);
		else
			g_print ("elapsed: %u second(s)", t);
	}

	g_print ("\n");
}

static MuError
cleanup_missing (MuIndex *midx, const MuConfig *opts, MuIndexStats *stats,
		 GError **err)
{
	MuError		rv;
	time_t		t;
	IndexData	idata;
	gboolean	show_progress;

	if (!opts->quiet)
		g_print ("cleaning up messages [%s]\n",
			 mu_runtime_path (MU_RUNTIME_PATH_XAPIANDB));

	show_progress = !opts->quiet && isatty(fileno(stdout));
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
cmd_index (MuIndex *midx, const MuConfig *opts, MuIndexStats *stats, GError **err)
{
	IndexData	idata;
	MuError		rv;
	gboolean	show_progress;

	show_progress = !opts->quiet && isatty(fileno(stdout));
	idata.color   = !opts->nocolor;

	rv = mu_index_run (midx,
			   opts->rebuild,
			   opts->lazycheck, stats,
			   show_progress ?
			   (MuIndexMsgCallback)index_msg_cb :
			   (MuIndexMsgCallback)index_msg_silent_cb,
			   NULL, &idata);
	if (rv == MU_OK || rv == MU_STOP) {
		g_message ("index: processed: %u; updated/new: %u",
			   stats->_processed, stats->_updated);
	} else
		mu_util_g_set_error (err, rv, "error while indexing");

	return rv;
}


static MuIndex*
init_mu_index (MuStore *store, const MuConfig *opts, GError **err)
{
	MuIndex *midx;

	if (!check_params (opts, err))
		return NULL;

	midx = mu_index_new (store, err);
	if (!midx)
		return NULL;

	mu_index_set_max_msg_size (midx, opts->max_msg_size);

	return midx;
}

MuError
mu_cmd_index (Mu::Store& store, const MuConfig *opts, GError **err)
{
	MuIndex		*midx;
	MuIndexStats	 stats;
	gboolean	 rv;
	time_t		 t;

	g_return_val_if_fail (opts, MU_ERROR);
	g_return_val_if_fail (opts->cmd == MU_CONFIG_CMD_INDEX, MU_ERROR);

	/* create, and do error handling if needed */
	midx = init_mu_index (reinterpret_cast<MuStore*>(&store), // ugh.
                              opts, err);
	if (!midx)
                throw Mu::Error(Mu::Error::Code::Internal, err/*consumes*/,
                                "error in index");

	mu_index_stats_clear (&stats);
	install_sig_handler ();

	t = time (NULL);
	rv = cmd_index (midx, opts, &stats, err);

	if (rv == MU_OK && !opts->nocleanup) {
		if (!opts->quiet)
			g_print ("\n");
		rv = cleanup_missing (midx, opts, &stats, err);
	}

	if (!opts->quiet)  {
		print_stats (&stats, TRUE, !opts->nocolor);
		g_print ("\n");
		show_time ((unsigned)(time(NULL)-t),
			   stats._processed, !opts->nocolor);
	}

	mu_index_destroy (midx);

        if (rv != MU_OK)
                throw Mu::Error(Mu::Error::Code::Internal, err/*consumes*/,
                                "error in index");

	return rv ? MU_OK : MU_ERROR;
}
