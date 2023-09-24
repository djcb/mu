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

#ifndef MU_ERROR_HH__
#define MU_ERROR_HH__

#include <stdexcept>
#include <string>
#include <errno.h>
#include <cstdint>

#include "mu-utils.hh"
#include <glib.h>

#ifndef FMT_HEADER_ONLY
#define FMT_HEADER_ONLY
#endif

#include <fmt/format.h>
#include <fmt/core.h>

namespace Mu {

// calculate an error enum value.
constexpr uint32_t err_enum(uint8_t code, uint8_t rv, uint8_t cat) {
	return static_cast<uint32_t>(code|(rv << 16)|cat<<24);
}

struct Error final : public std::exception {

	// 16 lower bits are for the error code;the next 8 bits are for the return code; the upper
	// byte is for flags
	static constexpr uint8_t SoftError = 1;

	enum struct Code: uint32_t {
		Ok                      = err_enum(0,0,0),

		// used by mu4e.
		NoMatches               = err_enum(4,2,SoftError),
		SchemaMismatch          = err_enum(110,11,0),

		// other
		AccessDenied            = err_enum(100,1,0),
		AssertionFailure        = err_enum(101,1,0),
		Command                 = err_enum(102,1,0),
		Crypto                  = err_enum(103,1,0),
		File                    = err_enum(104,1,0),
		Index                   = err_enum(105,1,0),
		Internal                = err_enum(106,1,0),
		InvalidArgument         = err_enum(107,1,0),
		Message                 = err_enum(108,1,0),
		NotFound                = err_enum(109,1,0),
		Parsing                 = err_enum(111,1,0),
		Play                    = err_enum(112,1,0),
		Query                   = err_enum(113,1,0),
		Script                  = err_enum(115,1,0),
		ScriptNotFound          = err_enum(116,1,0),
		Store                   = err_enum(117,1,0),
		StoreLock               = err_enum(118,19,0),
		UnverifiedSignature     = err_enum(119,1,0),
		User                    = err_enum(120,1,0),
		Xapian                  = err_enum(121,1,0),

		CannotReinit           = err_enum(122,1,0),
	};

	/**
	 * Construct an error
	 *
	 * @param code the error-code
	 * @param args... libfmt-style format string and parameters
	 */
	template<typename...T>
	Error(Code code, fmt::format_string<T...> frm, T&&... args):
		code_{code},
		what_{fmt::format(frm, std::forward<T>(args)...)} {}

	/**
	 * Construct an error
	 *
	 * @param code the error-code
	 * @param gerr a GError (or {}); the error is _consumed_ by this function
	 * @param args... libfmt-style format string and parameters
	 */
	template<typename...T>
	Error(Code code, GError **gerr, fmt::format_string<T...> frm, T&&... args):
		code_{code},
		what_{fmt::format(frm, std::forward<T>(args)...) +
		fmt::format(": {}", (gerr && *gerr) ? (*gerr)->message :
			    "something went wrong")}
		{ g_clear_error(gerr); }

	/**
	 * Get the descriptive message for this error.
	 *
	 * @return
	 */
	virtual const char* what() const noexcept override { return what_.c_str(); }

	/**
	 * Get the error-code for this error
	 *
	 * @return the error-code
	 */
	Code code() const noexcept { return code_; }

	/**
	 * Get the error number (e.g. for reporting to mu4e) for some error.
	 *
	 * @param c error code
	 *
	 * @return the error number
	 */
	static constexpr uint32_t error_number(Code c) noexcept {
		return static_cast<uint32_t>(c) & 0xffff;
	}

	/**
	 * Is this is a 'soft error'?
	 *
	 * @return true or false
	 */
	constexpr bool is_soft_error() const {
		return !!((static_cast<uint32_t>(code_)>>24) & SoftError);
	}

	constexpr uint8_t exit_code() const {
		return ((static_cast<uint32_t>(code_) >> 16) & 0xff);
	}

	/**
	 * Fill a GError with the error information
	 *
	 * @param err GError** (or NULL)
	 */
	void fill_g_error(GError **err) const noexcept{
		g_set_error(err, error_quark(), static_cast<int>(code_),
			    "%s", what_.c_str());
	}

	/**
	 * Add an end-user hint
	 *
	 * @param args... libfmt-style format string and parameters
	 *
	 * @return the error
	 */
	template<typename...T>
	Error& add_hint(fmt::format_string<T...> frm, T&&... args) {
		hint_ = fmt::format(frm, std::forward<T>(args)...);
		return *this;
	}

	/**
	 * Get the hint
	 *
	 * @return the hint, empty for no hint.
	 */
	const std::string& hint() const { return hint_; }

private:
	static inline GQuark error_quark (void) {
		static GQuark error_domain = 0;
		if (G_UNLIKELY(error_domain == 0))
			error_domain = g_quark_from_static_string("mu-error-quark");
		return error_domain;
	}

	const Code		code_;
	const std::string	what_;
	std::string		hint_;
};

static inline auto
format_as(const Error& err) {
	return mu_format("<{} ({}:{})>",
			 err.what(),
			 Error::error_number(err.code()),
			 err.exit_code());
}

} // namespace Mu

#endif /* MU_ERROR_HH__ */
