/*
** Copyright (C) 2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef MU_UTILS_FORMAT_HH__
#define MU_UTILS_FORMAT_HH__

#include <string>
#include <cstdarg>

namespace Mu {

/**
 * Quote & escape a string for " and \
 *
 * @param str a string
 *
 * @return quoted string
 */
std::string quote(const std::string& str);

/**
 * Format a string, printf style
 *
 * @param frm format string
 * @param ... parameters
 *
 * @return a formatted string
 */
std::string format(const char* frm, ...) __attribute__((format(printf, 1, 2)));

/**
 * Format a string, printf style
 *
 * @param frm format string
 * @param ... parameters
 *
 * @return a formatted string
 */
std::string vformat(const char* frm, va_list args) __attribute__((format(printf, 1, 0)));


} // namepace Mu


#endif /* MU_UTILS_FORMAT_HH__ */
