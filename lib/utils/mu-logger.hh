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

#ifndef MU_LOGGER_HH__
#define MU_LOGGER_HH__

#include <string>
#include "utils/mu-utils.hh"

namespace Mu {

/**
 * Logging options
 *
 */
enum struct LogOptions {
        None       = 0,         /**< Nothing specific */
        StdOutErr  = 1 << 1,    /**< Log to stdout/stderr */
        Debug      = 1 << 2,    /**< Include debug-level logs */
};

/**
 * Initialize the logging system. Note that the path is only used if structured
 * logging fails -- practically, it goes to the file if there's
 * systemd/journald.
 *
 * @param path path to the log file
 * @param opts logging options
 */
void log_init (const std::string& path, LogOptions opts);

/**
 * Uninitialize the logging system
 *
 */
void log_uninit();

/**
 * Change the logging options.
 *
 * @param opts options
 */
void log_set_options (LogOptions opts);

/**
 * Get the current log options
 *
 * @return the log options
 */
LogOptions log_get_options ();


} // namespace Mu
MU_ENABLE_BITOPS(Mu::LogOptions);

#endif /* MU_LOGGER_HH__ */
