/*
** Copyright (C) 2020-2025 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#if BUILD_SCM
#include "scm/mu-scm.hh"
#endif/*BUILD_SCM*/

#include "utils/mu-utils.hh"
#include "utils/mu-readline.hh"

using namespace Mu;

static std::atomic<int> MuTerminate{0};
static bool     tty;

static void
sig_handler(int sig)
{
	MuTerminate = sig;
}

static void
install_sig_handler()
{
	MuTerminate = 0;

	struct sigaction action{};
	action.sa_handler = sig_handler;
	sigemptyset(&action.sa_mask);
	action.sa_flags = SA_RESETHAND;

	for (auto sig: {SIGINT, SIGHUP, SIGTERM, SIGPIPE})
		if (sigaction(sig, &action, NULL) != 0)
			mu_critical("set sigaction for {} failed: {}",
				    sig, g_strerror(errno));
}

/*
 * djcb@djcbsoftware.nl
 *
 * Markers for/after the length cookie that precedes the expression we write to
 * output. We use octal 376, 377 (ie, 0xfe, 0xff) as they will never occur in
 * utf8
 */

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
output_stdout(const std::string& str, Server::OutputFlags flags)
{
	cookie(str.size() + 1);
	if (G_UNLIKELY(::puts(str.c_str()) < 0)) {
		mu_critical("failed to write output '{}'", str);
		::raise(SIGTERM); /* terminate ourselves */
	}
	if (any_of(flags & Server::OutputFlags::Flush))
		std::fflush(stdout);
}


static void
report_error(const Mu::Error& err) noexcept
{
	output_stdout(Sexp(":error"_sym, Error::error_number(err.code()),
			   ":message"_sym, err.what()).to_string(),
		      Server::OutputFlags::Flush);
}

static void
maybe_setup_readline(const Mu::Options& opts)
{
	tty = ::isatty(::fileno(stdout));

	// Note, the readline stuff is inactive unless on a tty.
	const auto histpath{opts.runtime_path(RuntimePath::Cache) + "/history"};
	setup_readline(histpath, 50);
}

// get the SCM socket path we're listening on, or empty if not listening
static std::string
maybe_listen_path(const Mu::Store& store, const Mu::Options& opts)
{
#ifdef BUILD_SCM
	if (!opts.scm.socket_path)
		return {};
	const auto socket_path{*opts.scm.socket_path};
	const auto res = Mu::Scm::run_repl(store, opts, socket_path);
	if (!res) {
		mu_warning("failed to start scm socket: {}", res.error().what());
		return {};
	} else
		return  socket_path;
#endif /*BUILD_SCM*/
	return {};
}

static bool
maybe_eval(Server& server, const Mu::Options& opts)
{
	const auto eval = std::string{opts.server.commands ?
		"(help :full t)" : opts.server.eval};

	if (!eval.empty()) {
		server.invoke(eval);
		return true;
	} else
		return false;
}

Result<void>
Mu::mu_cmd_server(const Mu::Options& opts) try {

	auto store = Store::make(opts.runtime_path(RuntimePath::XapianDb),
				 Store::Options::Writable);
	if (!store)
		return Err(store.error());

	// empty when we're not listening
	const auto socket_path = maybe_listen_path(*store, opts);

	Server::Options sopts{opts.server.allow_temp_file, socket_path};
	Server server{*store, sopts, output_stdout};

	if (maybe_eval(server, opts))
		return Ok();

	maybe_setup_readline(opts);
	install_sig_handler();

	mu_println(";; Welcome to the " PACKAGE_STRING " command-server{}",
		   opts.debug ? " (debug-mode)" : "");
	if (!socket_path.empty())
	mu_println(";; SCM socket listening on {}", socket_path);
	mu_println(";; Use (help) to get a list of commands, (quit) to quit.");

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
		mu_message ("shutting down due to signal {}", MuTerminate.load());

	shutdown_readline();
	return Ok();

} catch (const Error& er) { /* note: user-level error, "OK" for mu */
	report_error(er);
	mu_warning("server caught exception: {}", er.what());
	return Ok();
} catch (...) {
	mu_critical("server caught exception");
	return Err(Error::Code::Internal, "caught exception");
}
