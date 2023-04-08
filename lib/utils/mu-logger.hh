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

#ifndef MU_LOGGER_HH__
#define MU_LOGGER_HH__

#include <string>
#include <utils/mu-utils.hh>
#include <utils/mu-result.hh>

namespace Mu {

/**
 * RAII object for handling logging (through g_(debug|warning|...))
 *
 */
struct Logger {

	/**
	 * Logging options
	 *
	 */
	enum struct Options {
		None      = 0,      /**< Nothing specific */
		StdOutErr = 1 << 1, /**< Log to stdout/stderr */
		File      = 1 << 2, /**< Force logging to file, even if journal available */
		Debug     = 1 << 3, /**< Include debug-level logs */
	};

	/**
	 * Initialize the logging sub-system.
	 *
	 * Note that the path is only used if structured logging fails --
	 * practically, it goes to the file if there's no systemd/journald.
	 *
	 * if the environment variable MU_LOG_STDOUTERR is set,
	 * LogOptions::StdoutErr is implied.
	 *
	 * @param path path to the log file
	 * @param opts logging options
	 */
	static Result<Logger> make(const std::string& path, Options opts=Options::None);

	/**
	 * DTOR
	 *
	 */
	~Logger();

private:
	Logger(const std::string& path, Options opts);
};

MU_ENABLE_BITOPS(Logger::Options);

} // namespace Mu

#endif /* MU_LOGGER_HH__ */
