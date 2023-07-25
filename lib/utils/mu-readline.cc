/*
** Copyright (C) 2020-2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-utils.hh"
#include "mu-readline.hh"

#include <string>
#include <unistd.h>

#ifdef HAVE_LIBREADLINE
#if defined(HAVE_READLINE_READLINE_H)
#include <readline/readline.h>
#elif defined(HAVE_READLINE_H)
#include <readline.h>
#else  /* !defined(HAVE_READLINE_H) */
extern char* readline();
#endif /* !defined(HAVE_READLINE_H) */
char* cmdline = NULL;
#else  /* !defined(HAVE_READLINE_READLINE_H) */
/* no readline */
#endif /* HAVE_LIBREADLINE */

#ifdef HAVE_READLINE_HISTORY
#if defined(HAVE_READLINE_HISTORY_H)
#include <readline/history.h>
#elif defined(HAVE_HISTORY_H)
#include <history.h>
#else  /* !defined(HAVE_HISTORY_H) */
extern void  add_history();
extern int   write_history();
extern int   read_history();
#endif /* defined(HAVE_READLINE_HISTORY_H) */
/* no history */
#endif /* HAVE_READLINE_HISTORY */

#if defined(HAVE_LIBREADLINE) && defined(HAVE_READLINE_HISTORY)
#define HAVE_READLINE (1)
#else
#define HAVE_READLINE (0)
#endif

using namespace Mu;

static bool        is_a_tty{};
static std::string hist_path;
static size_t      max_lines{};

// LCOV_EXCL_START

bool
Mu::have_readline()
{
	return HAVE_READLINE != 0;
}

void
Mu::setup_readline(const std::string& histpath, size_t maxlines)
{
	is_a_tty  = !!::isatty(::fileno(stdout));
	hist_path = histpath;
	max_lines = maxlines;

#if HAVE_READLINE
	rl_bind_key('\t', rl_insert); // default (filenames) is not useful
	using_history();
	read_history(hist_path.c_str());

	if (max_lines > 0)
		stifle_history(max_lines);
#endif /*HAVE_READLINE*/
}

void
Mu::shutdown_readline()
{
#if HAVE_READLINE
	if (!is_a_tty)
		return;

	write_history(hist_path.c_str());
	if (max_lines > 0)
		history_truncate_file(hist_path.c_str(), max_lines);
#endif /*HAVE_READLINE*/
}

std::string
Mu::read_line(bool& do_quit)
{
#if HAVE_READLINE
	if (is_a_tty) {
		auto buf = readline(";; mu% ");
		if (!buf) {
			do_quit = true;
			return {};
		}
		std::string line{buf};
		::free(buf);
		return line;
	}
#endif /*HAVE_READLINE*/

	std::string line;
	mu_print(";; mu> ");
	if (!std::getline(std::cin, line))
		do_quit = true;

	return line;
}

void
Mu::save_line(const std::string& line)
{
#if HAVE_READLINE
	if (is_a_tty)
		add_history(line.c_str());
#endif /*HAVE_READLINE*/
}

// LCOV_EXCL_STOP
