/*
** Copyright (C) 2019-2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef MU_RESULT_HH__
#define MU_RESULT_HH__

#include "thirdparty/expected.hpp"
#include "utils/mu-error.hh"

namespace Mu {
/**
 * A little Rust-envy...a Result is _either_ some value of type T, _or_ a Mu::Error
 */
template <typename T> using Result = tl::expected<T, Error>;

/**
 * Ok() is not typically strictly needed (unlike Err), but imitates Rust's Ok
 * and it helps the reader.
 *
 * @param t the value to return
 *
 * @return a success Result<T>
 */
template <typename T>
class Result<T>::expected
// note: "class", not "typename";
// https://stackoverflow.com/questions/46412754/class-name-injection-and-constructors
Ok(T&& t)
{
	return std::move(t);
}

/**
 * Implementation of Ok() for void results.
 *
 * @return a success Result<void>
 */
static inline Result<void>
Ok()
{
	return {};
}

/**
 * Return an error
 *
 * @param err the error
 *
 * @return error
 */
static inline tl::unexpected<Error>
Err(Error&& err)
{
	return tl::unexpected(std::move(err));
}

static inline tl::unexpected<Error>
Err(const Error& err)
{
	return tl::unexpected(err);
}

template<typename T>
static inline tl::unexpected<Error>
Err(const Result<T>& res)
{
	return res.error();
}



template <typename T>
static inline Result<void>
Ok(const T& t)
{
	if (t)
		return Ok();
	else
		return Err(t.error());
}


/*
 * convenience
 */

static inline tl::unexpected<Error>
Err(Error::Code errcode, std::string&& msg="")
{
	return Err(Error{errcode, std::move(msg)});
}

__attribute__((format(printf, 2, 0)))
static inline tl::unexpected<Error>
Err(Error::Code errcode, const char* frm, ...)
{
	va_list args;
	va_start(args, frm);
	auto str{vformat(frm, args)};
	va_end(args);

	return Err(errcode, std::move(str));
}

__attribute__((format(printf, 3, 0)))
static inline tl::unexpected<Error>
Err(Error::Code errcode, GError **err, const char* frm, ...)
{
	va_list args;
	va_start(args, frm);
	auto str{vformat(frm, args)};
	va_end(args);

	if (err && *err)
		str += format(" (%s)", (*err)->message ? (*err)->message : "");
	g_clear_error(err);

	return Err(errcode, std::move(str));
}



/**
 * Assert that some result has a value (for unit tests)
 *
 * @param R some result
 */
#define assert_valid_result(R) do {			\
	if(!R) {					\
		g_printerr("%s:%u: error-result: %s\n",	\
			   __FILE__, __LINE__,		\
			   (R).error().what());		\
		g_assert_true(!!R);			\
	}						\
} while(0)



}// namespace Mu

#endif /* MU_RESULT_HH__ */
