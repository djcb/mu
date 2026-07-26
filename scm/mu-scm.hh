/*
** Copyright (C) 2025 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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


#ifndef MU_SCM_HH
#define MU_SCM_HH

#include <string>
#include <string_view>
#include <type_traits>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wredundant-decls"
#include <libguile.h>
#pragma GCC diagnostic pop

#include "utils/mu-result.hh"
#include "mu/mu-options.hh"
#include "mu-store.hh"

/**
 * Namespace for the Scm (Guile) subsystem
 *
 */
namespace Mu::Scm {
	/**
	 * Start a Guile/SCM REPL
	 *
	 * Initialize the Scm sub-system, then start a REPL, based on the
	 * configuration.
	 *
	 * @param store a Store object
	 * @param opts options; opts.scm.script_path must be Nothing
	 * @param socket_path if non-empty, run in the background on the
	 * socket path (Unix domain socket); otherwise, run an
	 * interactive shell in blocking mode.
	 *
	 * @return Ok() or some error
	 */
	Result<void> run_repl(const Store& store, const Options& opts,
			      const std::string& socket_path = {});

	/**
	 * Run a Guile/SCM script
	 *
	 * Initialize the Scm sub-system, then start run a script,
	 * based on the configuration.
	 *
	 * @param store a Store object
	 * @param opts options
	 * @param a path to the script
	 *
	 * @return Ok() or some error
	 */
	Result<void> run_script(const Store& store, const Options& opts,
				const std::string& script_path);



	/**
	 * Evaluate a Guile/SCM expression
	 *
	 * Initialize the Scm sub-system, then start evaluate some expression
	 * based on the configuration.
	 *
	 * @param store a Store object
	 * @param opts options
	 * @param expr some expression to evaluate
	 *
	 * @return Ok() or some error
	 */
	Result<void> run_eval(const Mu::Store& store, const Mu::Options& opts,
			      const std::string& expr);

	/**
	 * Helpers
	 *
	 * @{*/

	// https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2022/p2593r0.html
	template<typename T> struct always_false : std::false_type {};

        template <typename T>
        concept IsCharArray =
            std::is_array_v<T> && std::is_same_v<std::remove_extent_t<T>, char>;
        /**< Is this is a char-array? */

        template <typename T>
        concept IsSignedIntegral = std::is_integral_v<T> && std::is_signed_v<T>;
	/**< Is this a signed integral type? */

        template <typename T>
        concept IsUnsignedIntegral =
            std::is_integral_v<T> && std::is_unsigned_v<T>;
	/**< Is this an unsigned integral type? */


	/**
	 * C++ Exception to capture an SCM exception
	 *
	 * An ScmError is a C++ exception that captures the information for a
	 * Scm exception to be thrown later at some convenient moment (_after_
	 * an orderly finishing of the block, so DTORs etc can run.)
	 */
	struct ScmError {
		enum struct Id { WrongType, WrongArg, Error };
		ScmError(Id id, const char *subr, int pos, SCM bad_val,
			 const char *expected):
			id{Id::WrongType}, subr{subr}, pos(pos), bad_val{bad_val},
			expected{expected} {}
		ScmError(const char *subr, const char *msg):
			id{Id::Error}, subr{subr}, msg{msg} {}

		/**
		 * This will do a "non-local exit", ie. does not return.
		 *
		 */
		[[noreturn]] void throw_scm() const {
			/* Enforce exhaustive switch (do _not_ add a default case) */
#pragma GCC diagnostic push
#pragma GCC diagnostic error   "-Wswitch"
			switch(id) {
			case Id::WrongType:
			case Id::WrongArg:
				scm_wrong_type_arg_msg(subr, pos, bad_val, expected);
				break;
			case Id::Error:
				scm_misc_error(subr, msg, SCM_BOOL_F);
				break;
#pragma GCC diagnostic ignored "-Wswitch-default"
#pragma GCC diagnostic pop
			}
			throw; // never reached, just to appease compiler.
		}

	private:
		const Id id;
		const char *subr{};
		int pos;
		SCM bad_val;
		const char *expected;
		const char *msg{};
	};


	/**
	 * Make SCM symbol from string-like value
	 *
	 * @param val some value
	 *
	 * @return an SCM symbol
	 */
	template<typename T>
	SCM make_symbol(const T& val){
		using Type = std::remove_cvref_t<T>;
		if constexpr (std::is_same_v<Type, std::string> ||
			      std::is_same_v<Type, std::string_view>)
			return scm_from_utf8_symboln(val.data(), val.size());
		else if constexpr (IsCharArray<T> || std::is_same_v<Type, const char*>)
			return scm_from_utf8_symbol(val);
		else {
			static_assert(always_false<Type>::value, "source type not supported");
			return SCM_UNSPECIFIED;
		}
	}

	/**
	 * Get some C++ value from an SCM object, generically.
	 *
	 * This throws ScmError in case of any errors.
	 *
	 * @param ARG some SCM object
	 * @param func function where this is used
	 * @param pos argument number (starting from 1)
	 * @param expected the expected type
	 *
	 * @return C++ value
	 */
	template<typename T>
	T from_scm(SCM ARG, const char *func, int pos) {
		const auto ensure = [&](bool pred, SCM ARG, const char *expected) {
			if (!pred)
				throw ScmError{ScmError::Id::WrongType, func, pos, ARG, expected};
		};
		// note: use the C predicates (scm_is_string etc.); the Scheme
		// predicates (scm_string_p etc.) return an SCM boolean, which
		// is truthy as a C++ bool even when it is #f.
		using Type = std::remove_cvref_t<T>;
		if constexpr (std::is_same_v<Type, SCM>) {
			return ARG;
		} else if constexpr (std::is_same_v<Type, std::string>) {
			ensure(scm_is_string(ARG), ARG, "string");
			size_t len{};
			auto str{scm_to_utf8_stringn(ARG, &len)};
			std::string res{str, len};
			::free(str);
			return res;
		} else if constexpr (std::is_same_v<Type, char>) {
			ensure(SCM_CHARP(ARG), ARG, "character");
			return static_cast<char>(SCM_CHAR(ARG));
		} else if constexpr (std::is_same_v<Type, bool>) {
			ensure(scm_is_bool(ARG), ARG, "bool");
			return scm_to_bool(ARG);
		} else if constexpr (IsSignedIntegral<Type>) {
			ensure(scm_is_signed_integer(ARG, std::numeric_limits<Type>::min(),
						     std::numeric_limits<Type>::max()),
			       ARG, "integer");
			return scm_to_int(ARG);
		} else if constexpr (IsUnsignedIntegral<Type>) {
			ensure(scm_is_unsigned_integer(ARG, std::numeric_limits<Type>::min(),
						       std::numeric_limits<Type>::max()),
			       ARG,  "unsigned");
			return scm_to_uint(ARG);
		} else  {
			static_assert(always_false<Type>::value, "target type not supported");
			return {};
		}
	}


	/**
	 * Like from_SCM, but if ARG is boolean false, return default value.
	 *
	 * @param ARG argument
	 * @param default_value default value
	 *
	 * @return value
	 */
	template<typename T>
	T from_scm_with_default(SCM ARG, const T default_value,  const char *func, int pos) {
		return (scm_is_bool(ARG) && scm_is_false(ARG)) ? default_value : from_scm<T>(ARG, func, pos);
	}


	/**
	 * Get some SCM from a C++ value, generically.
	 *
	 * @param val some C++ object
	 *
	 * @return an SCM
	 */
	template<typename T>
	SCM to_scm(const T& val) {
		using Type = std::remove_cvref_t<T>;
		if constexpr (std::is_same_v<Type, std::string> ||
			      std::is_same_v<Type, std::string_view>)
			return scm_from_utf8_stringn(val.data(), val.size());
		else if constexpr (IsCharArray<Type>|| std::is_same_v<Type, const char*>)
			return scm_from_utf8_string(val);
		else if constexpr (std::is_same_v<Type, std::vector<std::string>>) {
			SCM lst{SCM_EOL};
			for (auto it = val.end(); it-- != val.begin();)
				lst = scm_cons(to_scm(*it), lst);
			return lst;
		}
		else if constexpr (std::is_same_v<Type, bool>)
			return scm_from_bool(val);
		else if constexpr (std::is_same_v<Type, char>)
			return SCM_MAKE_CHAR(static_cast<unsigned char>(val));
		else if constexpr (std::is_same_v<Type, size_t>)
			return scm_from_size_t(val);
		else if constexpr (std::is_same_v<Type, int>)
			return scm_from_int(val);
		else if constexpr (std::is_same_v<Type, GLogLevelFlags>)
			return scm_from_int(static_cast<int>(val));
		else if constexpr (std::is_same_v<Type, int64_t>)
			return scm_from_int64(val);
		else if constexpr (std::is_same_v<Type, uint64_t>)
			return scm_from_uint64(val);
		else if constexpr (std::is_same_v<Type, SCM>)
			return val;
		else {
			static_assert(always_false<Type>::value,
				      "source type not supported");
			return SCM_UNSPECIFIED;
		}
	}

	// base case.
	static inline SCM alist_add(SCM alist) {
		return alist;
	}
	/**
	 * Add key-value pair to an alist
	 *
	 * This assumes that keys are unique ("acons")
	 *
	 * @param alist some alist
	 * @param key key
	 * @param val value
	 * @param keyvals... 0 or more key, value parmeters
	 *
	 * @return the updated alist
	 */
	template<typename Key, typename Value, typename... KeyVals>
	static inline SCM alist_add(SCM alist, const Key& key, const Value& val,
				    KeyVals&&... keyvals) {
		SCM res = scm_acons(to_scm(key), to_scm(val), alist);
		return alist_add(res, std::forward<KeyVals>(keyvals)...);
	}

	//template<scm_t_catch_body Func, scm_t_catch_handler Handler>
	static inline SCM try_scm(scm_t_catch_body func, scm_t_catch_handler handler) {
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-function-type"
		return scm_internal_catch(
			SCM_BOOL_F,
			func, SCM_BOOL_F,
			handler, SCM_BOOL_F);
#pragma GCC diagnostic pop
	}


	/**@}*/
}

#endif /*MU_SCM_HH*/
