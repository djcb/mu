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
#include <unistd.h>

#include "mu-runtime.h"
#include "mu-cmd.hh"
#include "mu-server.hh"


#include "utils/mu-utils.hh"
#include "utils/mu-command-parser.hh"
#include "utils/mu-readline.hh"

using namespace Mu;
static std::atomic<bool> MuTerminate{false};
static bool tty;

static void
install_sig_handler (void)
{
        struct sigaction action;
        int i, sigs[] = { SIGINT, SIGHUP, SIGTERM, SIGPIPE };

        MuTerminate = false;

        action.sa_handler = [](int sig){ MuTerminate = true; };
        sigemptyset(&action.sa_mask);
        action.sa_flags   = SA_RESETHAND;

        for (i = 0; i != G_N_ELEMENTS(sigs); ++i)
                if (sigaction (sigs[i], &action, NULL) != 0)
                        g_critical ("set sigaction for %d failed: %s",
                                    sigs[i], g_strerror (errno));;
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
        if (tty) // for testing.
                ::printf ("[%x]", n);
        else
                ::printf (COOKIE_PRE "%x" COOKIE_POST, n);
}

static void
output_sexp_stdout (Sexp&& sexp)
{
        const auto str{sexp.to_sexp_string()};
        cookie(str.size() + 1);
        if (G_UNLIKELY(::puts(str.c_str()) < 0)) {
                g_critical ("failed to write output '%s'", str.c_str());
                ::raise (SIGTERM); /* terminate ourselves */
        }
}

MuError
mu_cmd_server (const MuConfig *opts, GError **err) try {

        Store store{mu_runtime_path(MU_RUNTIME_PATH_XAPIANDB), false/*writable*/};
        Server server{store, output_sexp_stdout};

        g_message ("created server with store @ %s; maildir @ %s; debug-mode %s",
                   store.metadata().database_path.c_str(),
                   store.metadata().root_maildir.c_str(),
                   opts->debug ? "yes" : "no");

        tty = ::isatty(::fileno(stdout));

        const auto eval = std::string {
                opts->commands ? "(help :full t)" : opts->eval ? opts->eval : ""};
        if (!eval.empty()) {
                server.invoke(eval);
                return MU_OK;
        }

        // Note, the readline stuff is inactive unless on a tty.
        const auto histpath{std::string{mu_runtime_path(MU_RUNTIME_PATH_CACHE)} + "/history"};
        setup_readline(histpath, 50);

        install_sig_handler();
        std::cout << ";; Welcome to the "  << PACKAGE_STRING << " command-server\n"
                  << ";; Use (help) to get a list of commands, (quit) to quit.\n";

        bool do_quit{};
        while (!MuTerminate && !do_quit) {

                const auto line{read_line(do_quit)};
                if (line.find_first_not_of(" \t") == std::string::npos)
                        continue; // skip whitespace-only lines

                do_quit = server.invoke(line) ? false : true;
                save_line(line);
        }
        shutdown_readline();

        return MU_OK;

} catch (const Error& er) {
        g_critical ("server caught exception: %s", er.what());
        g_set_error(err, MU_ERROR_DOMAIN, MU_ERROR, "%s", er.what());
        return MU_ERROR;
} catch (...) {
        g_critical ("server caught exception");
        g_set_error(err, MU_ERROR_DOMAIN, MU_ERROR, "%s", "caught exception");
        return MU_ERROR;
}
