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

#include "mu-utils-format.hh"
#include <glib.h>

namespace Mu {

// calculate an error enum value.
constexpr uint32_t err_enum(uint8_t code, uint8_t rv, uint8_t cat) {
	return static_cast<uint32_t>(code|(rv << 16)|cat<<24);
}

struct Error final : public std::exception {

	// 16 lower bits are for the error code the next 8 bits is for the return code
	// upper byte is for flags
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
	};

	/**
	 * Construct an error
	 *
	 * @param codearg error-code
	 * @param msgarg the error description
	 */
	Error(Code codearg, const std::string& msgarg) : code_{codearg}, what_{msgarg} {}
	Error(Code codearg, std::string&& msgarg) : code_{codearg}, what_{std::move(msgarg)} {}

	/**
	 * Build an error from an error-code and a format string
	 *
	 * @param code error-code
	 * @param frm format string
	 * @param ... format parameters
	 *
	 * @return an Error object
	 */
	__attribute__((format(printf, 3, 0))) Error(Code codearg, const char* frm, ...)
	    : code_{codearg}
	{
		va_list args;
		va_start(args, frm);
		what_ = vformat(frm, args);
		va_end(args);
	}

	Error(Error&& rhs)	= default;
	Error(const Error& rhs) = default;

	/**
	 * Build an error from a GError an error-code and a format string
	 *
	 * @param code error-code
	 * @param gerr a GError or {}, which is consumed
	 * @param frm format string
	 * @param ... format parameters
	 *
	 * @return an Error object
	 */
	__attribute__((format(printf, 4, 0)))
	Error(Code codearg, GError** err, const char* frm, ...)
	    : code_{codearg}
	{
		va_list args;
		va_start(args, frm);
		what_ = vformat(frm, args);
		va_end(args);

		if (err && *err)
			what_ += format(": %s", (*err)->message);
		else
			what_ += ": something went wrong";

		g_clear_error(err);
	}

	/**
	 * DTOR
	 *
	 */
	virtual ~Error() override = default;

	/**
	 * Get the descriptive message.
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
		return (static_cast<uint32_t>(code_) & SoftError) != 0;
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

private:
	static inline GQuark error_quark (void) {
		static GQuark error_domain = 0;
		if (G_UNLIKELY(error_domain == 0))
			error_domain = g_quark_from_static_string("mu-error-quark");
		return error_domain;
	}

	const Code  code_;
	std::string what_;
};

} // namespace Mu

#endif /* MU_ERROR_HH__ */
