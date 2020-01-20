/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

/*
** Copyright (C) 2008-2016 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-msg.h"
#include "mu-index.h"
#include "mu-store.hh"
#include "mu-runtime.h"

#include "utils/mu-util.h"
#include "utils/mu-log.h"

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

static gboolean
check_maildir (const char *maildir, GError **err)
{
	if (!maildir) {
		mu_util_g_set_error (err, MU_ERROR_IN_PARAMETERS,
				     "no maildir to work on; use --maildir=");
		return FALSE;
	}

	if (!g_path_is_absolute (maildir)) {
		mu_util_g_set_error (err, MU_ERROR_IN_PARAMETERS,
				     "maildir path '%s' is not absolute",
				     maildir);
		return FALSE;
	}

	if (!mu_util_check_dir (maildir, TRUE, FALSE)) {
		mu_util_g_set_error (err, MU_ERROR_IN_PARAMETERS,
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

/* when logging to console, print a newline before doing so; this
 * makes it more clear when something happens during the
 * indexing/cleanup progress output */
#define newline_before_on()			                          \
	mu_log_options_set(mu_log_options_get() | MU_LOG_OPTIONS_NEWLINE)
#define newline_before_off()						  \
	mu_log_options_set(mu_log_options_get() & ~MU_LOG_OPTIONS_NEWLINE)

static MuError
cleanup_missing (MuIndex *midx, MuConfig *opts, MuIndexStats *stats,
		 GError **err)
{
	MuError rv;
	time_t t;
	IndexData idata;
	gboolean show_progress;

	if (!opts->quiet)
		g_print ("cleaning up messages [%s]\n",
			 mu_runtime_path (MU_RUNTIME_PATH_XAPIANDB));

	show_progress = !opts->quiet && isatty(fileno(stdout));
	mu_index_stats_clear (stats);

	t = time (NULL);
	idata.color = !opts->nocolor;
	newline_before_on();
	rv = mu_index_cleanup
		(midx, stats,
		 show_progress ?
		 (MuIndexCleanupDeleteCallback)index_msg_cb :
		 (MuIndexCleanupDeleteCallback)index_msg_silent_cb,
		 &idata, err);
	newline_before_off();

	if (!opts->quiet) {
		print_stats (stats, TRUE, !opts->nocolor);
		g_print ("\n");
		show_time ((unsigned)(time(NULL)-t),stats->_processed,
			   !opts->nocolor);
	}

	return (rv == MU_OK || rv == MU_STOP) ? MU_OK: MU_G_ERROR_CODE(err);
}


static void
index_title (MuStore *store, MuConfig *opts)
{
	const char	  *green, *def;
	char		**addrs;
	int		  i;
	time_t            created;
	struct tm	 *tstamp;
	char		  tbuf[40];

	green = opts->nocolor ? "" : MU_COLOR_GREEN;
	def   = opts->nocolor ? "" : MU_COLOR_DEFAULT;

	g_print ("database           : %s%s%s\n",
		 green, mu_store_database_path (store), def);
	g_print ("schema-version     : %s%s%s\n",
		 green, mu_store_schema_version(store), def);

	created = mu_store_created (store);
	tstamp = localtime (&created);
	strftime (tbuf, sizeof(tbuf), "%c", tstamp);

	g_print ("created            : %s%s%s\n", green, tbuf, def);
	g_print ("maildir            : %s%s%s\n",
		 green, mu_store_maildir (store), def);

	g_print ("personal-addresses : ");

	addrs = mu_store_personal_addresses (store);
	for (i = 0; addrs[i]; ++i) {
		if (i != 0)
			g_print ("                     ");
		g_print ("%s%s%s\n", green, addrs[i], def);
	}

	g_strfreev(addrs);

	g_print ("\n");
}


static MuError
cmd_index (MuIndex *midx, MuConfig *opts, MuIndexStats *stats, GError **err)
{
	IndexData	idata;
	MuError		rv;
	gboolean	show_progress;

	show_progress = !opts->quiet && isatty(fileno(stdout));
	idata.color   = !opts->nocolor;

	newline_before_on();

	rv = mu_index_run (midx,
			   opts->rebuild,
			   opts->lazycheck, stats,
			   show_progress ?
			   (MuIndexMsgCallback)index_msg_cb :
			   (MuIndexMsgCallback)index_msg_silent_cb,
			   NULL, &idata);
	newline_before_off();

	if (rv == MU_OK || rv == MU_STOP) {
		MU_WRITE_LOG ("index: processed: %u; updated/new: %u",
			      stats->_processed, stats->_updated);
	} else
		mu_util_g_set_error (err, rv, "error while indexing");

	return rv;
}


static MuIndex*
init_mu_index (MuStore *store, MuConfig *opts, GError **err)
{
	MuIndex *midx;

	if (!check_params (opts, err))
		return NULL;

	if (!check_maildir (opts->maildir, err))
		return NULL;

	midx = mu_index_new (store, err);
	if (!midx)
		return NULL;

	mu_index_set_max_msg_size (midx, opts->max_msg_size);

	return midx;
}


MuError
mu_cmd_index (MuStore *store, MuConfig *opts, GError **err)
{
	MuIndex		*midx;
	MuIndexStats	 stats;
	gboolean	 rv;
	time_t		 t;

	g_return_val_if_fail (opts, FALSE);
	g_return_val_if_fail (opts->cmd == MU_CONFIG_CMD_INDEX,
			      FALSE);

	/* create, and do error handling if needed */
	midx = init_mu_index (store, opts, err);
	if (!midx)
		return MU_G_ERROR_CODE(err);

	mu_index_stats_clear (&stats);
	install_sig_handler ();

	if (!opts->quiet)
		index_title (store, opts);

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

	return rv;
}
