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
#include <cstdint>

#include "mu-utils-format.hh"
#include <glib.h>

namespace Mu {

struct Error final : public std::exception {

	// 16 lower bits are for the error code the next 8 bits is for the return code
	// upper byte is for flags

	static constexpr uint32_t SoftError = 1 << 23;

#define ERROR_ENUM(RV,CAT) \
	static_cast<uint32_t>(__LINE__ | ((RV) << 15) | (CAT))

	enum struct Code: uint32_t {
		AccessDenied	    = ERROR_ENUM(1,0),
		AssertionFailure    = ERROR_ENUM(1,0),
		Command		    = ERROR_ENUM(1,0),
		ContactNotFound     = ERROR_ENUM(2,SoftError),
		Crypto		    = ERROR_ENUM(1,0),
		File		    = ERROR_ENUM(1,0),
		Index		    = ERROR_ENUM(1,0),
		Internal	    = ERROR_ENUM(1,0),
		InvalidArgument	    = ERROR_ENUM(1,0),
		Message		    = ERROR_ENUM(1,0),
		NoMatches	    = ERROR_ENUM(4,SoftError),
		NotFound	    = ERROR_ENUM(1,0),
		Parsing		    = ERROR_ENUM(1,0),
		Play		    = ERROR_ENUM(1,0),
		Query		    = ERROR_ENUM(1,0),
		SchemaMismatch	    = ERROR_ENUM(1,0),
		Script              = ERROR_ENUM(1,0),
		ScriptNotFound      = ERROR_ENUM(1,0),
		Store		    = ERROR_ENUM(1,0),
		StoreLock           = ERROR_ENUM(19,0),
		UnverifiedSignature = ERROR_ENUM(1,0),
		User		    = ERROR_ENUM(1,0),
		Xapian		    = ERROR_ENUM(1,0),
	};


	/**
	 * Construct an error
	 *
	 * @param codearg error-code
	 * #param msgarg the error diecription
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

	Error(Error&& rhs)      = default;
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
	 * Is this is a 'soft error'?
	 *
	 * @return true or false
	 */
	constexpr bool is_soft_error() const {
		return (static_cast<uint32_t>(code_) & SoftError) != 0;
	}

	constexpr uint8_t exit_code() const {
		return ((static_cast<uint32_t>(code_) >> 15) & 0xff);
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
