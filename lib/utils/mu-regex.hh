/*
** Copyright (C) 2022-2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#
#include <utils/mu-result.hh>
#include <utils/mu-utils.hh>

namespace Mu {
/**
 * RAII wrapper around a GRegex which in itself is a wrapper around PCRE. We use
 * PCRE rather than std::regex because it is much faster.
 */
struct Regex {
#if !GLIB_CHECK_VERSION(2,74,0) /* backward compat */
#define G_REGEX_DEFAULT	      (static_cast<GRegexCompileFlags>(0))
#define G_REGEX_MATCH_DEFAULT (static_cast<GRegexMatchFlags>(0))
#endif
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
	 * Copy CTOR
	 *
	 * @param other some other Regex
	 */
	Regex(const Regex& other) noexcept: rx_{}  { *this = other; }

	/**
	 * Move CTOR
	 *
	 * @param other some other Regex
	 */
	Regex(Regex&& other) noexcept: rx_{} { *this = std::move(other); }


	/**
	 * DTOR
	 */
	~Regex() noexcept { g_clear_pointer(&rx_, g_regex_unref); }

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

	/**
	 * operator=
	 *
	 * @param other copy some other object to this one
	 *
	 * @return *this
	 */
	Regex& operator=(const Regex& other) noexcept {
		if (this != &other) {
			g_clear_pointer(&rx_, g_regex_unref);
			if (other.rx_)
				rx_ = g_regex_ref(other.rx_);
		}
		return *this;
	}

	/**
	 * operator=
	 *
	 * @param other move some other object to this one
	 *
	 * @return *this
	 */
	Regex& operator=(Regex&& other) noexcept {
		if (this != &other) {
			g_clear_pointer(&rx_, g_regex_unref);
			rx_ = other.rx_;
			other.rx_ = nullptr;
		}
		return *this;
	}

	/**
	 * Does this regexp match the given string? An unset Regex matches
	 * nothing.
	 *
	 * @param str string to test
	 * @param mflags match flags
	 *
	 * @return true or false
	 */
	bool matches(const std::string& str,
		     GRegexMatchFlags mflags=G_REGEX_MATCH_DEFAULT) const noexcept {
		if (!rx_)
			return false;
		else
			return g_regex_match(rx_, str.c_str(), mflags, nullptr);
		// strangely, valgrind reports some memory error related to
		// the str.c_str(). It *seems* like a false alarm.
	}

	/**
	 * Replace all occurrences of @this regexp in some string with a
	 * replacement string
	 *
	 * @param str some string
	 * @param repl replacement string
	 *
	 * @return string or error
	 */
	Result<std::string> replace(const std::string& str, const std::string& repl) const {
		GError *gerr{};

		if (!rx_)
			return Err(Error::Code::InvalidArgument, "missing regexp");
		else if (auto&& s{g_regex_replace(rx_, str.c_str(), str.length(), 0,
					     repl.c_str(), G_REGEX_MATCH_DEFAULT, &gerr)}; !s)
			return Err(Error::Code::InvalidArgument, &gerr, "error in Regex::replace");
		else
			return Ok(to_string_gchar(std::move(s)));
	}

	const GRegex* g_regex() const { return rx_; }

private:
	Regex(const char *ptrn, GRegexCompileFlags cflags, GRegexMatchFlags mflags) {
		GError *err{};
		if (rx_ = g_regex_new(ptrn, cflags, mflags, &err); !rx_)
			throw Error{Error::Code::InvalidArgument, &err,
				"invalid regexp: '{}'", ptrn};
	}

	GRegex *rx_{};
};

static inline std::string format_as(const Regex& rx) {
	if (auto&& grx{rx.g_regex()}; !grx)
		return "//";
	else
		return mu_format("/{}/", g_regex_get_pattern(grx));

}


} // namespace Mu


#endif /* MU_REGEX_HH__ */
