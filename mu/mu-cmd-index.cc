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

#include <chrono>
#include <thread>
#include <atomic>

#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <signal.h>
#include <unistd.h>

#include "mu-msg.hh"
#include "index/mu-indexer.hh"
#include "mu-store.hh"
#include "mu-runtime.hh"

#include "utils/mu-util.h"

using namespace Mu;

static std::atomic<bool> CaughtSignal{};

static void
install_sig_handler (void)
{
	struct sigaction action;
	int i, sigs[] = { SIGINT, SIGHUP, SIGTERM };

	sigemptyset(&action.sa_mask);
	action.sa_flags   = SA_RESETHAND;
        action.sa_handler = [](int sig) {
                if (!CaughtSignal && sig == SIGINT) /* Ctrl-C */
                        g_print ("\nshutting down gracefully, "
                                 "press again to kill immediately");
                CaughtSignal = true;
        };

	for (i = 0; i != G_N_ELEMENTS(sigs); ++i)
		if (sigaction (sigs[i], &action, NULL) != 0)
			g_critical ("set sigaction for %d failed: %s",
				    sigs[i], strerror (errno));;
}


static void
print_stats (const Indexer::Progress& stats, bool color)
{
	const char  *kars = "-\\|/";
	static auto  i    = 0U;

        MaybeAnsi col{color};
        using Color = MaybeAnsi::Color;

        std::cout << col.fg(Color::Yellow) << kars[++i % 4] << col.reset()
                  << " indexing messages; "
                  << "processed: " << col.fg(Color::Green) << stats.processed << col.reset()
                  << "; updated/new: " << col.fg(Color::Green) << stats.updated << col.reset()
                  << "; cleaned-up: " << col.fg(Color::Green) << stats.removed << col.reset();
}


MuError
Mu::mu_cmd_index (Mu::Store& store, const MuConfig *opts, GError **err)
{
	g_return_val_if_fail (opts, MU_ERROR);
	g_return_val_if_fail (opts->cmd == MU_CONFIG_CMD_INDEX, MU_ERROR);

        /* param[0] == 'index'  there should be no param[1] */
	if (opts->params[1]) {
		mu_util_g_set_error (err, MU_ERROR_IN_PARAMETERS,
				     "unexpected parameter");
		return MU_ERROR;
	}
	if (opts->max_msg_size < 0) {
		mu_util_g_set_error (err, MU_ERROR_IN_PARAMETERS,
				     "the maximum message size must be >= 0");
		return MU_ERROR;
	}

        const auto mdir{store.metadata().root_maildir};
        if (G_UNLIKELY(access (mdir.c_str(), R_OK) != 0)) {
                mu_util_g_set_error(err, MU_ERROR_FILE,
                                   "'%s' is not readable: %s", mdir.c_str(), strerror (errno));
                return MU_ERROR;
        }

        MaybeAnsi col{!opts->nocolor};
        using Color = MaybeAnsi::Color;
        if (!opts->quiet) {

                if (opts->lazycheck)
                        std::cout << "lazily ";

                std::cout << "indexing maildir "
                          << col.fg(Color::Green) << store.metadata().root_maildir
                          << col.reset()
                          << " -> store "
                          << col.fg(Color::Green) << store.metadata().database_path
                          << col.reset()
                          << std::endl;
        }

        Mu::Indexer::Config conf{};
        conf.cleanup          = !opts->nocleanup;
        conf.lazy_check       = opts->lazycheck;

        install_sig_handler ();

        auto& indexer{store.indexer()};
        indexer.start(conf);
        while (!CaughtSignal && indexer.is_running()) {
                if (!opts->quiet)
                        print_stats (indexer.progress(), !opts->nocolor);

                std::this_thread::sleep_for(std::chrono::milliseconds(250));

                if (!opts->quiet) {
                        std::cout << "\r";
                        std::cout.flush();
                }
        }

        store.indexer().stop();

        if (!opts->quiet) {
                print_stats (store.indexer().progress(), !opts->nocolor);
                std::cout << std::endl;
        }

        return MU_OK;
}
