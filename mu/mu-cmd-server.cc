/*
** Copyright (C) 2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <string>
#include <algorithm>
#include <atomic>
#include <cstdio>

#include <unistd.h>

#include "mu-cmd.hh"
#include "mu-server.hh"

#include "utils/mu-utils.hh"
#include "utils/mu-command-handler.hh"
#include "utils/mu-readline.hh"

using namespace Mu;
static std::atomic<int> MuTerminate{0};
static bool              tty;

static void
sig_handler(int sig)
{
	MuTerminate = sig;
}

static void
install_sig_handler(void)
{
	static struct sigaction action;
	int                     i, sigs[] = {SIGINT, SIGHUP, SIGTERM, SIGPIPE};

	MuTerminate = 0;

	action.sa_handler = sig_handler;
	sigemptyset(&action.sa_mask);
	action.sa_flags = SA_RESETHAND;

	for (i = 0; i != G_N_ELEMENTS(sigs); ++i)
		if (sigaction(sigs[i], &action, NULL) != 0)
			g_critical("set sigaction for %d failed: %s",
				   sigs[i], g_strerror(errno));
}

/*
 * Markers for/after the length cookie that precedes the expression we write to
 * output. We use octal 376, 377 (ie, 0xfe, 0xff) as they will never occur in
 * utf8 */

#define COOKIE_PRE  "\376"
#define COOKIE_POST "\377"

static void
cookie(size_t n)
{
	const auto num{static_cast<unsigned>(n)};

	if (tty) // for testing.
		::printf("[%x]", num);
	else
		::printf(COOKIE_PRE "%x" COOKIE_POST, num);
}

static void
output_sexp_stdout(const Sexp& sexp, Server::OutputFlags flags)
{
	/* if requested, insert \n between list elements; note:
	 * is _not_ inherited by children */
	Sexp::Format fopts{};
	if (any_of(flags & Server::OutputFlags::SplitList))
		fopts |= Sexp::Format::SplitList;

	const auto str{sexp.to_string(fopts)};
	cookie(str.size() + 1);
	if (G_UNLIKELY(::puts(str.c_str()) < 0)) {
		g_critical("failed to write output '%s'", str.c_str());
		::raise(SIGTERM); /* terminate ourselves */
	}

	if (any_of(flags & Server::OutputFlags::Flush))
		std::fflush(stdout);
}

static void
report_error(const Mu::Error& err) noexcept
{
	output_sexp_stdout(Sexp(":error"_sym, Error::error_number(err.code()),
				":message"_sym, err.what()),
			   Server::OutputFlags::Flush);
}


Result<void>
Mu::mu_cmd_server(const Mu::Options& opts) try {

	auto store = Store::make(opts.runtime_path(RuntimePath::XapianDb),
				 Store::Options::Writable);
	if (!store)
		return Err(store.error());

	Server server{*store, output_sexp_stdout};
	g_message("created server with store @ %s; maildir @ %s; debug-mode %s;"
		  "readline: %s",
		  store->properties().database_path.c_str(),
		  store->properties().root_maildir.c_str(),
		  opts.debug ? "yes" : "no",
		  have_readline() ? "yes" : "no");

	tty = ::isatty(::fileno(stdout));
	const auto eval = std::string{opts.server.commands ? "(help :full t)" : opts.server.eval};
	if (!eval.empty()) {
		server.invoke(eval);
		return Ok();
	}

	// Note, the readline stuff is inactive unless on a tty.
	const auto histpath{opts.runtime_path(RuntimePath::Cache) + "/history"};
	setup_readline(histpath, 50);

	install_sig_handler();
	std::cout << ";; Welcome to the " << PACKAGE_STRING << " command-server"
		  << (opts.debug ? " (debug-mode)" : "") << '\n'
		  << ";; Use (help) to get a list of commands, (quit) to quit.\n";

	bool do_quit{};
	while (!MuTerminate && !do_quit) {
		std::fflush(stdout); // Needed for Windows, see issue #1827.
		const auto line{read_line(do_quit)};
		if (line.find_first_not_of(" \t") == std::string::npos)
			continue; // skip whitespace-only lines

		do_quit = server.invoke(line) ? false : true;
		save_line(line);
	}

	if (MuTerminate != 0)
		g_message ("shutting down due to signal %d", MuTerminate.load());

	shutdown_readline();

	return Ok();

} catch (const Error& er) {
	/* note: user-level error, "OK" for mu */
	report_error(er);
	g_warning("server caught exception: %s", er.what());
	return Ok();
} catch (...) {
	g_critical("server caught exception");
	return Err(Error::Code::Internal, "caught exception");
}
