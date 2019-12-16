/*
**  Copyright (C) 2017 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
**
**  This library is free software; you can redistribute it and/or
**  modify it under the terms of the GNU Lesser General Public License
**  as published by the Free Software Foundation; either version 2.1
**  of the License, or (at your option) any later version.
**
**  This library is distributed in the hope that it will be useful,
**  but WITHOUT ANY WARRANTY; without even the implied warranty of
**  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
**  Lesser General Public License for more details.
**
**  You should have received a copy of the GNU Lesser General Public
**  License along with this library; if not, write to the Free
**  Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
**  02110-1301, USA.
*/

#ifndef __MU_UTILS_HH__
#define __MU_UTILS_HH__

#include <string>
#include <vector>
#include <cstdarg>

namespace Mu {

/**
 * Flatten a string -- downcase and fold diacritics etc.
 *
 * @param str a string
 *
 * @return a flattened string
 */
std::string utf8_flatten (const char *str);
inline std::string utf8_flatten (const std::string& s) { return utf8_flatten(s.c_str()); }



/**
 * Replace all control characters with spaces, and remove leading and trailing space.
 *
 * @param dirty an unclean string
 *
 * @return a cleaned-up string.
 */
std::string utf8_clean (const std::string& dirty);


/**
 * Split a string in parts
 *
 * @param str a string
 * @param sepa the separator
 *
 * @return the parts.
 */
std::vector<std::string> split (const std::string& str,
				const std::string& sepa);

/**
 * Quote & escape a string
 *
 * @param str a string
 *
 * @return quoted string
 */
std::string quote (const std::string& str);

/**
 * Format a string, printf style
 *
 * @param frm format string
 * @param ... parameters
 *
 * @return a formatted string
 */
std::string format (const char *frm, ...) __attribute__((format(printf, 1, 2)));

/**
 * Format a string, printf style
 *
 * @param frm format string
 * @param ... parameters
 *
 * @return a formatted string
 */
std::string format (const char *frm, va_list args) __attribute__((format(printf, 1, 0)));



/**
 * Convert an ISO date to the corresponding time expressed as a string
 * with a 10-digit time_t
 *
 * @param date
 * @param first
 *
 * @return
 */
std::string date_to_time_t_string (const std::string& date, bool first);

/**
 * 64-bit incarnation of time_t expressed as a 10-digit string. Uses 64-bit for the time-value,
 *  regardless of the size of time_t.
 *
 * @param t some time value
 *
 * @return
 */
std::string date_to_time_t_string (int64_t t);



/**
 * Convert a size string to a size in bytes
 *
 * @param sizestr the size string
 * @param first
 *
 * @return the size expressed as a string with the decimal number of bytes
 */
std::string size_to_string (const std::string& sizestr, bool first);

/**
 * Convert a size into a size in bytes string
 *
 * @param size the size
 * @param first
 *
 * @return the size expressed as a string with the decimal number of bytes
 */
std::string size_to_string (int64_t size);

} // namespace Mu

#endif /* __MU_UTILS_HH__ */
