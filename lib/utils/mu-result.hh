/*
** Copyright (C) 2019-2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <utility>
#include <type_traits>

#include <tl/expected.hpp>
#include "utils/mu-error.hh"

namespace Mu {
/**
 * A little Rust-envy...a Result is _either_ some value of type T, _or_ a Mu::Error
 *
 * Note: nothing outside this header uses tl:: directly, and Result only uses
 * the std::expected subset of the API; so once mu requires C++23, this can
 * simply become std::expected (with tl::unexpected -> std::unexpected below).
 */
template <typename T> using Result = tl::expected<T, Error>;

/**
 * Ok() is not typically strictly needed (unlike Err), but imitates Rust's Ok
 * and it helps the reader.
 *
 * @param t the value to return; copied when passed an lvalue, moved when
 * passed an rvalue.
 *
 * @return a success Result<T>
 */
template <typename T> [[nodiscard]] Result<std::decay_t<T>>
Ok(T&& t)
{
	return std::forward<T>(t);
}

/**
 * Implementation of Ok() for void results.
 *
 * @return a success Result<void>
 */
[[nodiscard]] inline Result<void>
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
[[nodiscard]] inline tl::unexpected<Error>
Err(Error&& err)
{
	return tl::unexpected(std::move(err));
}

[[nodiscard]] inline tl::unexpected<Error>
Err(const Error& err)
{
	return tl::unexpected(err);
}

/**
 * Get the error from some error Result
 *
 * @param res a Result; must hold an error (checking that is up to the
 * caller)
 *
 * @return the error
 */
template<typename T>
[[nodiscard]] inline tl::unexpected<Error>
Err(const Result<T>& res)
{
	return tl::unexpected(res.error());
}

template<typename T>
[[nodiscard]] inline tl::unexpected<Error>
Err(Result<T>&& res)
{
	return tl::unexpected(std::move(res.error()));
}

/*
 * convenience
 */
template <typename ...T>
[[nodiscard]] tl::unexpected<Error>
Err(Error::Code code, fmt::format_string<T...> frm, T&&... args)
{
	return Err(Error{code, frm, std::forward<T>(args)...});
}

template <typename ...T>
[[nodiscard]] tl::unexpected<Error>
Err(Error::Code code, GError **err, fmt::format_string<T...> frm, T&&... args)
{
	return Err(Error{code, err, frm, std::forward<T>(args)...});
}

/**
 * Get the value from a Result, or throw its error
 *
 * @param res a Result
 *
 * @return the value (moved out of @p res)
 */
template<typename T> T
unwrap(Result<T>&& res)
{
	if (!!res)
		return std::move(res.value());
	else
		throw std::move(res.error());
}

/**
 * Assert that some result has a value (for unit tests)
 *
 * @param R some result
 */
#define assert_valid_result(R) do {				\
	auto&& res__ = R;					\
	if(!res__) {						\
		mu_printerrln("{}:{}: error-result: {}",	\
			   __FILE__, __LINE__,			\
			   (res__).error().what());		\
		g_assert_true(!!res__);				\
	}							\
} while(0)

}// namespace Mu

#endif /* MU_RESULT_HH__ */
