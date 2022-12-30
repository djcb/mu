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
#ifndef MU_REGEX_HH__
#define MU_REGEX_HH__

#include <glib.h>
#include <utils/mu-result.hh>

namespace Mu {
/**
 * RAII wrapper around a GRegex which in itself is a wrapper around PCRE. We use
 * PCRE rather than std::regex because it is much faster.
 */
struct Regex {
	/**
	 * Trivial constructor
	 *
	 * @return
	 */
	Regex() noexcept: rx_{}  {}


	/**
	 * Construct a new Regex object
	 *
	 * @param ptrn PRRE regular expression pattern
	 * @param cflags compile flags
	 * @param mflags match flags
	 *
	 * @return a Regex object or an error.
	 */
	static Result<Regex> make(const std::string& ptrn,
				  GRegexCompileFlags cflags = G_REGEX_DEFAULT,
				   GRegexMatchFlags mflags = G_REGEX_MATCH_DEFAULT) noexcept try {
		return Regex(ptrn.c_str(), cflags, mflags);
	} catch (const Error& err) {
		return Err(err);
	}

	/**
	 * DTOR
	 */
	~Regex() {
		if (rx_)
			g_regex_unref(rx_);
	}

	/**
	 * Cast to the the underlying GRegex*
	 *
	 * @return a GRegex*
	 */
	operator const GRegex*() const noexcept { return rx_; }

	/**
	 * Doe this object contain a valid GRegex*?
	 *
	 * @return true or false
	 */
	operator bool() const noexcept { return !!rx_; }


	// No need for a move CTOR, copying is cheap (GRegex is refcounted)

	/**
	 * Copy CTOR
	 *
	 * @param other some other Regex
	 */
	Regex(const Regex& other) noexcept { *this = other; }

	/**
	 * operator=
	 *
	 * @param other copy some other object to this one
	 *
	 * @return *this
	 */
	Regex& operator=(const Regex& other) noexcept {
		if (this != &other) {
			auto oldrx = rx_;
			rx_ = other.rx_ ? g_regex_ref(other.rx_): nullptr;
			if (oldrx)
				g_regex_unref(oldrx);
		}
		return *this;
	}

	/**
	 * Does this regexp match the given string? An unset Regex matches
	 * nothing.
	 *
	 * @param str
	 * @param mflags
	 *
	 * @return true or false
	 */
	bool matches(const std::string& str,
		     GRegexMatchFlags mflags=G_REGEX_MATCH_DEFAULT) const noexcept {
		return rx_ ? g_regex_match(rx_, str.c_str(), mflags, {}) : false;
	}

private:
	Regex(const char *ptrn, GRegexCompileFlags cflags, GRegexMatchFlags mflags) {
		GError *err{};
		rx_ = g_regex_new(ptrn, cflags, mflags, &err);
		if (!rx_)
			throw Err(Error::Code::InvalidArgument,
				  "invalid regexp: '%s'", ptrn);
	}
	GRegex *rx_{};
};

} // namespace Mu


#endif /* MU_REGEX_HH__ */
