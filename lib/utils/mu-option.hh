/*
 * Created on 2020-11-08 by Dirk-Jan C. Binnema <dbinnema@logitech.com>
 *
 * Copyright (c) 2020 Logitech, Inc.  All Rights Reserved
 * This program is a trade secret of LOGITECH, and it is not to be reproduced,
 * published, disclosed to others, copied, adapted, distributed or displayed
 * without the prior authorization of LOGITECH.
 *
 * Licensee agrees to attach or embed this notice on all copies of the program,
 * including partial copies or modified versions thereof.
 *
 */

#ifndef MU_OPTION__
#define MU_OPTION__

#include "thirdparty/optional.hpp"
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
constexpr auto Nothing = tl::nullopt; // 'None' is take already

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
