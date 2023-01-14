/*
** Copyright (C) 2008-2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "index/mu-indexer.hh"
#include "mu-store.hh"

using namespace Mu;

static std::atomic<bool> caught_signal;

static void
sig_handler(int _sig)
{
	caught_signal = true;
}

static void
install_sig_handler(void)
{
	struct sigaction action;
	int              i, sigs[] = {SIGINT, SIGHUP, SIGTERM};

	sigemptyset(&action.sa_mask);
	action.sa_flags   = SA_RESETHAND;
	action.sa_handler = sig_handler;

	for (i = 0; i != G_N_ELEMENTS(sigs); ++i)
		if (sigaction(sigs[i], &action, NULL) != 0)
			g_critical("set sigaction for %d failed: %s",
				   sigs[i], g_strerror(errno));
}

static void
print_stats(const Indexer::Progress& stats, bool color)
{
	const char* kars = "-\\|/";
	static auto i    = 0U;

	MaybeAnsi col{color};
	using Color = MaybeAnsi::Color;

	std::cout << col.fg(Color::Yellow) << kars[++i % 4] << col.reset() << " indexing messages; "
		  << "checked: " << col.fg(Color::Green) << stats.checked << col.reset()
		  << "; updated/new: " << col.fg(Color::Green) << stats.updated << col.reset()
		  << "; cleaned-up: " << col.fg(Color::Green) << stats.removed << col.reset();
}

Result<void>
Mu::mu_cmd_index(Store& store, const Options& opts)
{
	const auto mdir{store.properties().root_maildir};
	if (G_UNLIKELY(access(mdir.c_str(), R_OK) != 0))
		return Err(Error::Code::File, "'%s' is not readable: %s",
			   mdir.c_str(), g_strerror(errno));

	MaybeAnsi col{!opts.nocolor};
	using Color = MaybeAnsi::Color;
	if (!opts.quiet) {
		if (opts.index.lazycheck)
			std::cout << "lazily ";

		std::cout << "indexing maildir " << col.fg(Color::Green)
			  << store.properties().root_maildir << col.reset() << " -> store "
			  << col.fg(Color::Green) << store.properties().database_path << col.reset()
			  << std::endl;
	}

	Mu::Indexer::Config conf{};
	conf.cleanup    = !opts.index.nocleanup;
	conf.lazy_check = opts.index.lazycheck;
	// ignore .noupdate with an empty store.
	conf.ignore_noupdate = store.empty();

	install_sig_handler();

	auto& indexer{store.indexer()};
	indexer.start(conf);
	while (!caught_signal && indexer.is_running()) {
		if (!opts.quiet)
			print_stats(indexer.progress(), !opts.nocolor);

		std::this_thread::sleep_for(std::chrono::milliseconds(250));

		if (!opts.quiet) {
			std::cout << "\r";
			std::cout.flush();
		}
	}

	store.indexer().stop();

	if (!opts.quiet) {
		print_stats(store.indexer().progress(), !opts.nocolor);
		std::cout << std::endl;
	}

	return Ok();
}
