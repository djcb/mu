/*
** Copyright (C) 2008-2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <config.h>
#include <functional>

#include <glib.h>
#include <glib-object.h>
#include <locale.h>

#include "mu-cmd.hh"
#include "mu-options.hh"
#include "utils/mu-utils.hh"
#include "utils/mu-logger.hh"

#include "mu-cmd.hh"

using namespace Mu;

static int
handle_result(const Result<void>& res, const Mu::Options& opts)
{
	if (res)
		return 0;

	using Color = MaybeAnsi::Color;
	MaybeAnsi col{!opts.nocolor};

	// show the error and some help, but not if it's only a softerror.
	if (!res.error().is_soft_error()) {
		std::cerr << col.fg(Color::Red) << "error" << col.reset() << ": "
			  << col.fg(Color::BrightYellow)
			  << res.error().what() << "\n";
	} else
		std::cerr <<  col.fg(Color::BrightBlue) << res.error().what() << '\n';

	std::cerr << col.fg(Color::Green);

	// perhaps give some useful hint on how to solve it.
	switch (res.error().code()) {
	case Error::Code::InvalidArgument:
		break;
	case Error::Code::StoreLock:
		std::cerr << "Perhaps mu is already running?\n";
		break;
	case Error::Code::SchemaMismatch:
		std::cerr << "Please (re)initialize mu with 'mu init' "
			  << "see mu-init(1) for details\n";
		break;
	default:
		break; /* nothing to do */
	}

	std::cerr << col.reset();

	return res.error().exit_code();
}

int
main(int argc, char* argv[])
{
	setlocale(LC_ALL, "");

	/*
	 * read command-line options
	 */
	const auto opts{Options::make(argc, argv)};
	if (!opts) {
		std::cerr << "error: " << opts.error().what() << "\n";
		return opts.error().exit_code();
	} else if (!opts->sub_command) {
		// nothing more to do.
		return 0;
	}

	/*
	 * set up logging
	 */
	Logger::Options lopts{Logger::Options::None};
	if (opts->log_stderr)
		lopts |= Logger::Options::StdOutErr;
	if (opts->debug)
		lopts |= Logger::Options::Debug;

	const auto logger = Logger::make(opts->runtime_path(RuntimePath::LogFile),
					 lopts);
	if (!logger) {
		std::cerr << "error:" << logger.error().what() << "\n";
		return logger.error().exit_code();
	}

	/*
	 * handle sub command
	 */
	return handle_result(mu_cmd_execute(*opts), *opts);
}
