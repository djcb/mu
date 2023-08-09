/*
** Copyright (C) 2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef MU_OPTION__
#define MU_OPTION__

#include <tl/optional.hpp>
#include <stdexcept>
#include <string>

namespace Mu {

/// Either a value of type T, or None
template <typename T> using Option = tl::optional<T>;

template <typename T>
Option<T>
Some(T&& t)
{
	return std::move(t);
}
constexpr auto Nothing = tl::nullopt; // 'None' is already taken.

template<typename T> T
unwrap(Option<T>&& res)
{
	if (!!res)
		return std::move(res.value());
	else
		throw std::runtime_error("failure is not an option");
}


/**
 * Maybe create a string from a const char pointer.
 *
 * @param str a char pointer or NULL
 *
 * @return option with either the string or nothing if str was NULL.
 */
Option<std::string>
static inline to_string_opt(const char* str) {
	if (str)
		return std::string{str};
	else
		return Nothing;
}

/**
 * Like maybe_string that takes a const char*, but additionally,
 * g_free() the string.
 *
 * @param str char pointer or NULL (consumed)
 *
 * @return option with either the string or nothing if str was NULL.
 */
Option<std::string> to_string_opt_gchar(char*&& str);


} // namespace Mu
#endif /*MU_OPTION__*/
