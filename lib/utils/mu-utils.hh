/*
**  Copyright (C) 2020-2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
**
**  This library is free software; you can redistribute it and/or
**  modify it under the terms of the GNU Lesser General Public License
**  as published by the Free Software Foundation; either version 2.1
**  of the License, or (at your option) any later version.
**
**  This library is distributed in the hope that it will be useful,
**  but WITHOUT ANY WARRANTY; without even the implied warranty of
**  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
**  Lesser General Public License for more details.
**
**  You should have received a copy of the GNU Lesser General Public
**  License along with this library; if not, write to the Free
**  Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
**  02110-1301, USA.
*/

#ifndef __MU_UTILS_HH__
#define __MU_UTILS_HH__

#include <string>
#include <string_view>
#include <sstream>
#include <vector>
#include <chrono>
#include <memory>
#include <cstdarg>
#include <glib.h>
#include <ostream>
#include <iostream>
#include <type_traits>
#include <algorithm>
#include <numeric>

#include "mu-utils-format.hh"
#include "mu-option.hh"

namespace Mu {

using StringVec = std::vector<std::string>;

/**
 * Flatten a string -- downcase and fold diacritics etc.
 *
 * @param str a string
 *
 * @return a flattened string
 */
std::string utf8_flatten(const char* str);
inline std::string
utf8_flatten(const std::string& s)
{
	return utf8_flatten(s.c_str());
}

/**
 * Replace all control characters with spaces, and remove leading and trailing space.
 *
 * @param dirty an unclean string
 *
 * @return a cleaned-up string.
 */
std::string utf8_clean(const std::string& dirty);

/**
 * Remove ctrl characters, replacing them with ' '; subsequent
 * ctrl characters are replaced by a single ' '
 *
 * @param str a string
 *
 * @return the string without control characters
 */
std::string remove_ctrl(const std::string& str);

/**
 * Split a string in parts. As a special case, splitting an empty string
 * yields an empty vector (not a vector with a single empty element)
 *
 * @param str a string
 * @param sepa the separator
 *
 * @return the parts.
 */
std::vector<std::string> split(const std::string& str, const std::string& sepa);

/**
 * Split a string in parts. As a special case, splitting an empty string
 * yields an empty vector (not a vector with a single empty element)
 *
 * @param str a string
 * @param sepa the separator
 *
 * @return the parts.
 */
std::vector<std::string> split(const std::string& str, char sepa);

/**
 * Join the strings in svec into a string, separated by sepa
 *
 * @param svec a string vector
 * @param sepa separator
 *
 * @return string
 */
std::string join(const std::vector<std::string>& svec, const std::string& sepa);
static inline std::string join(const std::vector<std::string>& svec, char sepa) {
	return join(svec, std::string(1, sepa));
}

/**
 * write a string (assumed to be in utf8-format) to a stream,
 * converted to the current locale
 *
 * @param str a string
 * @param stream a stream
 *
 * @return true if printing worked, false otherwise
 */
bool fputs_encoded (const std::string& str, FILE *stream);

/**
 * print a formatted string (assumed to be in utf8-format) to stdout,
 * converted to the current locale
 *
 * @param a standard printf() format string, followed by a parameter list
 *
 * @return true if printing worked, false otherwise
 */
bool print_encoded (const char *frm, ...) G_GNUC_PRINTF(1,2);

/**
 * Parse a date string to the corresponding time_t
 * *
 * @param date the date expressed a YYYYMMDDHHMMSS or any n... of the first
 * characters, using the local timezone.
 * @param first whether to fill out incomplete dates to the start or the end;
 * ie. either 1972 -> 197201010000 or 1972 -> 197212312359
 *
 * @return the corresponding time_t or Nothing if parsing failed.
 */
Option<int64_t> parse_date_time(const std::string& date, bool first);

/**
 * 64-bit incarnation of time_t expressed as a 10-digit string. Uses 64-bit for the time-value,
 *  regardless of the size of time_t.
 *
 * @param t some time value
 *
 * @return
 */
std::string date_to_time_t_string(int64_t t);

/**
 * Get a string for a given time_t and format
 * memory that must be freed after use.
 *
 * @param frm the format of the string (in strftime(3) format)
 * @param t the time as time_t
 * @param utc whether to display as UTC(if true) or local time
 *
 * @return a string representation of the time in UTF8-format, or empty in case
 * of error.
 */
std::string time_to_string(const char *frm, time_t t, bool utc = false) G_GNUC_CONST;


/**
 * Hack to avoid locale crashes
 *
 * @return true if setting locale worked; false otherwise
 */
bool locale_workaround();


/**
 * Is the given timezone available? For tests
 *
 * @param tz a timezone, such as Europe/Helsinki
 *
 * @return true or false
 */
bool timezone_available(const std::string& tz);


// https://stackoverflow.com/questions/19053351/how-do-i-use-a-custom-deleter-with-a-stdunique-ptr-member
template <auto fn>
struct deleter_from_fn {
	template <typename T>
	constexpr void operator()(T* arg) const {
		fn(arg);
	}
};
template <typename T, auto fn>
using deletable_unique_ptr = std::unique_ptr<T, deleter_from_fn<fn>>;



using Clock    = std::chrono::steady_clock;
using Duration = Clock::duration;

template <typename Unit>
constexpr int64_t
to_unit(Duration d)
{
	using namespace std::chrono;
	return duration_cast<Unit>(d).count();
}

constexpr int64_t
to_s(Duration d)
{
	return to_unit<std::chrono::seconds>(d);
}
constexpr int64_t
to_ms(Duration d)
{
	return to_unit<std::chrono::milliseconds>(d);
}
constexpr int64_t
to_us(Duration d)
{
	return to_unit<std::chrono::microseconds>(d);
}

struct StopWatch {
	using Clock = std::chrono::steady_clock;
	StopWatch(const std::string name) : start_{Clock::now()}, name_{name} {}
	~StopWatch()
	{
		const auto us{static_cast<double>(to_us(Clock::now() - start_))};
		if (us > 2000000)
			g_debug("sw: %s: finished after %0.1f s", name_.c_str(), us / 1000000);
		else if (us > 2000)
			g_debug("sw: %s: finished after %0.1f ms", name_.c_str(), us / 1000);
		else
			g_debug("sw: %s: finished after %g us", name_.c_str(), us);
	}

private:
	Clock::time_point start_;
	std::string       name_;
};

/**
 * Convert a size string to a size in bytes
 *
 * @param sizestr the size string
 * @param first
 *
 * @return the size or Nothing if parsing failed
 */
Option<int64_t> parse_size(const std::string& sizestr, bool first);

/**
 * Convert a size into a size in bytes string
 *
 * @param size the size
 * @param first
 *
 * @return the size expressed as a string with the decimal number of bytes
 */
std::string size_to_string(int64_t size);

/**
 * get a crude 'summary' of the string, ie. the first /n/ lines of the strings,
 * with all newlines removed, replaced by single spaces
 *
 * @param str the source string
 * @param max_lines the maximum number of lines to include in the summary
 *
 * @return a newly allocated string with the summary. use g_free to free it.
 */
std::string summarize(const std::string& str, size_t max_lines);

/**
 * Convert any ostreamable<< value to a string
 *
 * @param t the value
 *
 * @return a std::string
 */
template <typename T>
static inline std::string
to_string(const T& val)
{
	std::stringstream sstr;
	sstr << val;

	return sstr.str();
}

/**
 * Consume a gchar and return a std::string
 *
 * @param str a gchar* (consumed/freed)
 *
 * @return a std::string, empty if gchar was {}
 */
static inline std::string
to_string_gchar(gchar*&& str)
{
	std::string s(str?str:"");
	g_free(str);
	return s;
}


/*
 * Lexicals Number are lexicographically sortable string representations
 * of numbers. Start with 'g' + length of number in hex, followed by
 * the ascii for the hex represntation. So,
 *
 * 0  -> 'g0'
 * 1  -> 'g1'
 * 10 -> 'ga'
 * 16 -> 'h10'
 *
 * etc.
 */
std::string to_lexnum(int64_t val);
int64_t from_lexnum(const std::string& str);

/**
 * Like std::find_if, but using sequence instead of a range.
 *
 * @param seq some std::find_if compatible sequence
 * @param pred a predicate
 *
 * @return an iterator
 */
template<typename Sequence, typename UnaryPredicate>
typename Sequence::const_iterator seq_find_if(const Sequence& seq, UnaryPredicate pred) {
	return std::find_if(seq.cbegin(), seq.cend(), pred);
}

/**
 * Is pred(element) true for at least one element of sequence?
 *
 * @param seq sequence
 * @param pred a predicate
 *
 * @return true or false
 */
template<typename Sequence, typename UnaryPredicate>
bool seq_some(const Sequence& seq, UnaryPredicate pred) {
	return seq_find_if(seq, pred) != seq.cend();
}

/**
 * Create a sequence that has all element of seq for which pred is true
 *
 * @param seq sequence
 * @param pred false
 *
 * @return sequence
 */
template<typename Sequence, typename UnaryPredicate>
Sequence seq_filter(const Sequence& seq, UnaryPredicate pred) {
	Sequence res;
	std::copy_if(seq.begin(), seq.end(), std::back_inserter(res), pred);
	return res;
}

/**
 * Create a sequence that has all element of seq for which pred is false
 *
 * @param seq sequence
 * @param pred false
 *
 * @return sequence
 */
template<typename Sequence, typename UnaryPredicate>
Sequence seq_remove(const Sequence& seq, UnaryPredicate pred) {
	Sequence res;
	std::remove_copy_if(seq.begin(), seq.end(), std::back_inserter(res), pred);
	return res;
}

template<typename Sequence, typename Compare>
void seq_sort(Sequence& seq, Compare cmp) { std::sort(seq.begin(), seq.end(), cmp); }


/**
 * Like std::accumulate, but using a sequence instead of a range.
 *
 * @param seq some std::accumulate compatible sequence
 * @param init the initial value
 * @param op binary operation to calculate the next element
 *
 * @return the result value.
 */
template<typename Sequence, typename ResultType,  typename BinaryOp>
ResultType seq_fold(const Sequence& seq, ResultType init, BinaryOp op) {
	return std::accumulate(seq.cbegin(), seq.cend(), init, op);
}

template<typename Sequence, typename UnaryOp>
void seq_for_each(const Sequence& seq, UnaryOp op) {
	std::for_each(seq.cbegin(), seq.cend(), op);
}

/**
 * array of associated pair elements -- like an alist
 * but based on std::array and thus can be constexpr
 */
template<typename T1, typename T2, std::size_t N>
      using AssocPairs = std::array<std::pair<T1, T2>, N>;

/**
  * Get the first value of the pair where the second element is @param s.
  *
  * @param p AssocPairs
  * @param s some second pair value
  *
  * @return the matching first pair value, or Nothing if not found.
  */
template<typename P>
constexpr Option<typename P::value_type::first_type>
to_first(const P& p, typename P::value_type::second_type s)
{
	for (const auto& item: p)
		if (item.second == s)
			return item.first;
	return Nothing;
}

/**
  * Get the second value of the pair where the first element is @param f.
  *
  * @param p AssocPairs
  * @param f some first pair value
  *
  * @return the matching second pair value, or Nothing if not found.
  */
template<typename P>
constexpr Option<typename P::value_type::second_type>
to_second(const P& p, typename P::value_type::first_type f)
{
	for (const auto& item: p)
		if (item.first == f)
			return item.second;
	return Nothing;
}

/**
 * Convert string view in something printable with %.*s
 */
#define STR_V(sv__) static_cast<int>((sv__).size()), (sv__).data()

struct MaybeAnsi {
	explicit MaybeAnsi(bool use_color) : color_{use_color} {}

	enum struct Color {
		Black   = 30,
		Red     = 31,
		Green   = 32,
		Yellow  = 33,
		Blue    = 34,
		Magenta = 35,
		Cyan    = 36,
		White   = 37,

		BrightBlack   = 90,
		BrightRed     = 91,
		BrightGreen   = 92,
		BrightYellow  = 93,
		BrightBlue    = 94,
		BrightMagenta = 95,
		BrightCyan    = 96,
		BrightWhite   = 97,
	};

	std::string fg(Color c) const { return ansi(c, true); }
	std::string bg(Color c) const { return ansi(c, false); }

	std::string reset() const { return color_ ? "\x1b[0m" : ""; }

private:
	std::string ansi(Color c, bool fg = true) const
	{
		return color_ ? format("\x1b[%dm", static_cast<int>(c) + (fg ? 0 : 10)) : "";
	}

	const bool color_;
};

#define MU_COLOR_RED		"\x1b[31m"
#define MU_COLOR_GREEN		"\x1b[32m"
#define MU_COLOR_YELLOW		"\x1b[33m"
#define MU_COLOR_BLUE		"\x1b[34m"
#define MU_COLOR_MAGENTA	"\x1b[35m"
#define MU_COLOR_CYAN		"\x1b[36m"
#define MU_COLOR_DEFAULT	"\x1b[0m"


/// Allow using enum structs as bitflags
#define MU_TO_NUM(ET, ELM)  std::underlying_type_t<ET>(ELM)
#define MU_TO_ENUM(ET, NUM) static_cast<ET>(NUM)
#define MU_ENABLE_BITOPS(ET)                                                                    \
	constexpr ET operator&(ET e1, ET e2)                                                    \
	{                                                                                       \
		return MU_TO_ENUM(ET, MU_TO_NUM(ET, e1) & MU_TO_NUM(ET, e2));                   \
	}                                                                                       \
	constexpr ET operator|(ET e1, ET e2)                                                    \
	{                                                                                       \
		return MU_TO_ENUM(ET, MU_TO_NUM(ET, e1) | MU_TO_NUM(ET, e2));                   \
	}                                                                                       \
	constexpr ET      operator~(ET e) { return MU_TO_ENUM(ET, ~(MU_TO_NUM(ET, e))); }       \
	constexpr bool    any_of(ET e) { return MU_TO_NUM(ET, e) != 0; }                        \
	constexpr bool    none_of(ET e) { return MU_TO_NUM(ET, e) == 0; }                       \
	constexpr bool    one_of(ET e1, ET e2) { return (e1 & e2) == e2; }			\
	constexpr ET& operator&=(ET& e1, ET e2) { return e1 = e1 & e2; }                        \
	constexpr ET& operator|=(ET& e1, ET e2) { return e1 = e1 | e2; }                        \
	static_assert(1==1) // require a semicolon

} // namespace Mu

#endif /* __MU_UTILS_HH__ */
