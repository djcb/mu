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
#include <string>

namespace Mu {

/**
 * Setup readline when available and on tty.
 *
 * @param histpath path to the history file
 * @param max_lines maximum number of history to save
 */
void setup_readline(const std::string& histpath, size_t max_lines);

/**
 * Shutdown readline
 *
 */
void shutdown_readline();

/**
 * Read a command line
 *
 * @param do_quit recceives whether we should quit.
 *
 * @return the string read or empty
 */
std::string read_line(bool& do_quit);

/**
 * Save a line to history (or do nothing when readline is not active)
 *
 * @param line a line.
 */
void save_line(const std::string& line);


/**
 * Do we have the non-shim readline?
 *
 * @return true or failse
 */
bool have_readline();

} // namespace Mu
