/*
**  Copyright (C) 2017-2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE
#include <stdexcept>
#endif /*_XOPEN_SOURCE*/

#include <array>

#include <time.h>

#define GNU_SOURCE
#include <stdio.h>
#include <stdint.h>
#include <unistd.h>

#include <string.h>
#include <iostream>
#include <algorithm>
#include <numeric>
#include <functional>
#include <cinttypes>
#include <charconv>
#include <limits>

#include <glib.h>
#include <glib/gprintf.h>

#include "mu-utils.hh"
#include "mu-unbroken.hh"

#include "mu-error.hh"
#include "mu-option.hh"

using namespace Mu;

namespace {

static gunichar
unichar_tolower(gunichar uc)
{
	if (!g_unichar_isalpha(uc))
		return uc;

	if (g_unichar_get_script(uc) != G_UNICODE_SCRIPT_LATIN)
		return g_unichar_tolower(uc);

	switch (uc) {
	case 0x00e6:
	case 0x00c6: return 'e'; /* æ */
	case 0x00f8: return 'o'; /* ø */
	case 0x0110:
	case 0x0111:
		return 'd'; /* đ */
			    /* todo: many more */
	default: return g_unichar_tolower(uc);
	}
}

/**
 * gx_utf8_flatten:
 * @str: a UTF-8 string
 * @len: the length of @str, or -1 if it is %NULL-terminated
 *
 * Flatten some UTF-8 string; that is, downcase it and remove any diacritics.
 *
 * Returns: (transfer full): a flattened string, free with g_free().
 */
static char*
gx_utf8_flatten(const gchar* str, gssize len)
{
	GString* gstr;
	char *   norm, *cur;

	g_return_val_if_fail(str, NULL);

	norm = g_utf8_normalize(str, len, G_NORMALIZE_ALL);
	if (!norm)
		return NULL;

	gstr = g_string_sized_new(strlen(norm));

	for (cur = norm; cur && *cur; cur = g_utf8_next_char(cur)) {
		gunichar uc;

		uc = g_utf8_get_char(cur);
		if (g_unichar_combining_class(uc) != 0)
			continue;

		g_string_append_unichar(gstr, unichar_tolower(uc));
	}

	g_free(norm);

	return g_string_free(gstr, FALSE);
}

} // namespace

bool
Mu::contains_unbroken_script(const char *str)
{
	while (str && *str) {
		auto uc = g_utf8_get_char(str);
		if (is_unbroken_script(uc))
			return true;
		str = g_utf8_next_char(str);
	}

	return false;
}

std::string // gx_utf8_flatten
Mu::utf8_flatten(const char* str)
{
	if (!str)
		return {};

	if (contains_unbroken_script(str))
		return std::string{str};

	// the pure-ascii case
	if (g_str_is_ascii(str)) {
		auto    l = g_ascii_strdown(str, -1);
		std::string s{l};
		g_free(l);
		return s;
	}

	// seems we need the big guns
	char* flat = gx_utf8_flatten(str, -1);
	if (!flat)
		return {};

	std::string s{flat};
	g_free(flat);

	return s;
}


/* turn \0-terminated buf into ascii (which is a utf8 subset); convert
 *   any non-ascii into '.'
 */
static char*
asciify_in_place (char *buf)
{
	char *c;

	g_return_val_if_fail (buf, NULL);

	for (c = buf; c && *c; ++c) {
		if ((!isprint(*c) && !isspace (*c)) || !isascii(*c))
			*c = '.';
	}

	return buf;
}

static char*
utf8ify (const char *buf)
{
	char *utf8;

	g_return_val_if_fail (buf, NULL);

	utf8 = g_strdup (buf);

	if (!g_utf8_validate (buf, -1, NULL))
		asciify_in_place (utf8);

	return utf8;
}


std::string
Mu::utf8_clean(const std::string& dirty)
{
	g_autoptr(GString) gstr = g_string_sized_new(dirty.length());
	g_autofree char *cstr	= utf8ify(dirty.c_str());

	for (auto cur = cstr; cur && *cur; cur = g_utf8_next_char(cur)) {
		const gunichar uc = g_utf8_get_char(cur);
		if (g_unichar_iscntrl(uc))
			g_string_append_c(gstr, ' ');
		else
			g_string_append_unichar(gstr, uc);
	}

	return std::string{g_strstrip(gstr->str)};
}


std::string
Mu::utf8_wordbreak(const std::string& txt)
{
	g_autoptr(GString) gstr = g_string_sized_new(txt.length());

	bool spc{};
	for (auto cur = txt.c_str(); cur && *cur; cur = g_utf8_next_char(cur)) {
		const gunichar uc = g_utf8_get_char(cur);

		if (g_unichar_iscntrl(uc)) {
			g_string_append_c(gstr, ' ');
			continue;
		}
		// inspired by Xapian's termgenerator.

		switch(uc) {
		case '\'':
		case '&':
		case 0xb7:
		case 0x5f4:
		case 0x2019:
		case 0x201b:
		case 0x2027:
		case ',':
		case '.':
		case ';':
		case '+':
		case '#':
		case '-':
		case 0x037e: // GREEK QUESTION MARK
		case 0x0589: // ARMENIAN FULL STOP
		case 0x060D: // ARABIC DATE SEPARATOR
		case 0x07F8: // NKO COMMA
		case 0x2044: // FRACTION SLASH
		case 0xFE10: // PRESENTATION FORM FOR VERTICAL COMMA
		case 0xFE13: // PRESENTATION FORM FOR VERTICAL COLON
		case 0xFE14: // PRESENTATION FORM FOR VERTICAL SEMICOLON
			if (spc)
				break;
			spc = true;
			g_string_append_c(gstr, ' ');
			break;
		default:
			spc = false;
			g_string_append_unichar(gstr, uc);
			break;
		}
	}

	return std::string{g_strstrip(gstr->str)};
}


std::string
Mu::remove_ctrl(const std::string& str)
{
	char        prev{'\0'};
	std::string result;
	result.reserve(str.length());

	for (auto&& c : str) {
		if (::iscntrl(c) || c == ' ') {
			if (prev != ' ')
				result += prev = ' ';
		} else
			result += prev = c;
	}

	return result;
}

std::vector<std::string>
Mu::split(const std::string& str, const std::string& sepa)
{
	std::vector<std::string> vec;
	size_t b = 0, e = 0;

	/* special cases */
	if (str.empty())
		return vec;
	else if (sepa.empty()) {
		for (auto&& c: str)
			vec.emplace_back(1, c);
		return vec;
	}

	while (true) {
		if (e = str.find(sepa, b); e != std::string::npos) {
			vec.emplace_back(str.substr(b, e - b));
			b = e + sepa.length();
		} else {
			vec.emplace_back(str.substr(b));
			break;
		}
	}

	return vec;
}

std::vector<std::string>
Mu::split(const std::string& str, char sepa)
{
	std::vector<std::string> vec;
	size_t b = 0, e = 0;

	/* special case */
	if (str.empty())
		return vec;

	while (true) {
		if (e = str.find(sepa, b); e != std::string::npos) {
			vec.emplace_back(str.substr(b, e - b));
			b = e + sizeof(sepa);
		} else {
			vec.emplace_back(str.substr(b));
			break;
		}
	}

	return vec;
}

std::string
Mu::join(const std::vector<std::string>& svec, const std::string& sepa)
{
	if (svec.empty())
		return {};


	/* calculate the overall size beforehand, to avoid re-allocations. */
	size_t value_len =
		std::accumulate(svec.cbegin(), svec.cend(), 0,
				[](size_t size, const std::string& s) {
					return size + s.size();
				}) + (svec.size() - 1) * sepa.length();

	std::string value;
	value.reserve(value_len);

	std::accumulate(svec.cbegin(), svec.cend(), std::ref(value),
			[&](std::string& s1, const std::string& s2)->std::string& {
				if (s1.empty())
					s1 = s2;
				else {
					s1.append(sepa);
					s1.append(s2);
				}
				return s1;
			});

	return value;
}

std::string
Mu::quote(const std::string& str)
{
	std::string res{"\""};

	for (auto&& k : str) {
		switch (k) {
		case '"': res += "\\\""; break;
		case '\\': res += "\\\\"; break;
		default: res += k;
		}
	}

	return res + "\"";
}

static Option<::time_t>
delta_ymwdhMs(const std::string& expr)
{
	char* endptr;
	auto  num = strtol(expr.c_str(), &endptr, 10);
	if (num <= 0 || num > 9999 || !endptr || !*endptr)
		return Nothing;

	int years, months, weeks, days, hours, minutes, seconds;
	years = months = weeks = days = hours = minutes = seconds = 0;

	switch (endptr[0]) {
	case 's': seconds = num; break;
	case 'M': minutes = num; break;
	case 'h': hours = num; break;
	case 'd': days = num; break;
	case 'w': weeks = num; break;
	case 'm': months = num; break;
	case 'y': years = num; break;
	default:
		return Nothing;
	}

	GDateTime *then, *now = g_date_time_new_now_local();
	if (weeks != 0)
		then = g_date_time_add_weeks(now, -weeks);
	else
		then =
		    g_date_time_add_full(now, -years, -months, -days, -hours, -minutes, -seconds);

	auto  t = std::max<::time_t>(0, g_date_time_to_unix(then));

	g_date_time_unref(then);
	g_date_time_unref(now);

	return t;
}

static Option<::time_t>
special_date_time(const std::string& d, bool is_first)
{
	if (d == "now")
		return ::time({});

	if (d == "today") {
		GDateTime *dt, *midnight;
		dt = g_date_time_new_now_local();

		if (!is_first) {
			GDateTime* tmp = dt;
			dt             = g_date_time_add_days(dt, 1);
			g_date_time_unref(tmp);
		}

		midnight = g_date_time_add_full(dt,
						0,
						0,
						0,
						-g_date_time_get_hour(dt),
						-g_date_time_get_minute(dt),
						-g_date_time_get_second(dt));
		time_t t = MAX(0, (gint64)g_date_time_to_unix(midnight));
		g_date_time_unref(dt);
		g_date_time_unref(midnight);

		return t;
	}

	return Nothing;
}

// if a date has a month day greater than the number of days in that month,
// change it to a valid date point to the last second in that month
static void
fixup_month(struct tm* tbuf)
{
	decltype(tbuf->tm_mday) max_days;
	const auto		month = tbuf->tm_mon + 1;
	const auto		year  = tbuf->tm_year + 1900;

	switch (month) {
	case 2:
		if (year % 4 == 0 && (year % 100 != 0 || year % 400 == 0))
			max_days = 29;
		else
			max_days = 28;
		break;
	case 4:
	case 6:
	case 9:
	case 11:
		max_days = 30;
		break;
	default:
		max_days = 31;
		break;
	}

	if (tbuf->tm_mday > max_days) {
		tbuf->tm_mday = max_days;
		tbuf->tm_hour = 23;
		tbuf->tm_min  = 59;
		tbuf->tm_sec  = 59;
	}
 }


Option<::time_t>
Mu::parse_date_time(const std::string& dstr, bool is_first, bool utc)
{
	struct tm tbuf{};
	GDateTime *dtime{};
	gint64 t;

	/* one-sided dates */
	if (dstr.empty())
		return is_first ? time_t_min : time_t_max;
	else if (dstr == "today" || dstr == "now")
		return special_date_time(dstr, is_first);
	else if (dstr.find_first_of("ymdwhMs") != std::string::npos)
		return delta_ymwdhMs(dstr);

	constexpr char UserDateMin[] = "19700101000000";
	constexpr char UserDateMax[] = "29991231235959";

	std::string date(is_first ? UserDateMin : UserDateMax);
	std::copy_if(dstr.begin(), dstr.end(), date.begin(), [](auto c) { return isdigit(c); });

	if (!::strptime(date.c_str(), "%Y%m%d%H%M%S", &tbuf) &&
	    !::strptime(date.c_str(), "%Y%m%d%H%M", &tbuf) &&
	    !::strptime(date.c_str(), "%Y%m%d%H", &tbuf) &&
	    !::strptime(date.c_str(), "%Y%m%d", &tbuf) &&
	    !::strptime(date.c_str(), "%Y%m", &tbuf) &&
	    !::strptime(date.c_str(), "%Y", &tbuf))
		return Nothing;

	fixup_month(&tbuf);
	dtime = utc ?
		g_date_time_new_utc(tbuf.tm_year + 1900,
				      tbuf.tm_mon + 1,
				      tbuf.tm_mday,
				      tbuf.tm_hour,
				      tbuf.tm_min,
				      tbuf.tm_sec) :
		g_date_time_new_local(tbuf.tm_year + 1900,
				      tbuf.tm_mon + 1,
				      tbuf.tm_mday,
				      tbuf.tm_hour,
				      tbuf.tm_min,
				      tbuf.tm_sec);

	t = g_date_time_to_unix(dtime);
	g_date_time_unref(dtime);

	return to_time_t(t);
}


Option<int64_t>
Mu::parse_size(const std::string& val, bool is_first)
{
	int64_t		size{-1};
	std::string	str;
	GRegex*		rx;
	GMatchInfo*	minfo;

	/* one-sided ranges */
	if (val.empty())
		return is_first ? 0 : std::numeric_limits<int64_t>::max();

	rx = g_regex_new("^(\\d+)(b|k|kb|m|mb|g|gb)?$",
			 G_REGEX_CASELESS, (GRegexMatchFlags)0, NULL);
	minfo = NULL;
	if (g_regex_match(rx, val.c_str(), (GRegexMatchFlags)0, &minfo)) {

		char*  s;
		s    = g_match_info_fetch(minfo, 1);
		size = atoll(s);
		g_free(s);

		s = g_match_info_fetch(minfo, 2);
		switch (s ? g_ascii_tolower(s[0]) : 0) {
		case 'k': size *= 1024; break;
		case 'm': size *= (1024 * 1024); break;
		case 'g': size *= (1024 * 1024 * 1024); break;
		default: break;
		}

		g_free(s);
	}

	g_regex_unref(rx);
	g_match_info_unref(minfo);

	if (size < 0)
		return Nothing;
	else
		return size;

}

std::string
Mu::to_lexnum(int64_t val)
{
	char buf[18]; /* 1 byte prefix + hex + \0 */
	buf[0] = 'f' + ::snprintf(buf + 1, sizeof(buf) - 1, "%" PRIx64, val);
	return buf;
}

int64_t
Mu::from_lexnum(const std::string& str)
{
	int64_t val{};
	std::from_chars(str.c_str() + 1, str.c_str() + str.size(), val, 16);

	return val;
}

bool
Mu::locale_workaround() try
{
	// quite horrible... but some systems break otherwise with
	//  https://github.com/djcb/mu/issues/2252

	try {
		std::locale::global(std::locale(""));
	} catch (const std::runtime_error& re) {
		g_setenv("LC_ALL", "C", 1);
		std::locale::global(std::locale(""));
	}

	return true;

} catch (...) {
	return false;
}

bool
Mu::timezone_available(const std::string& tz)
{
	const auto old_tz = g_getenv("TZ");

	g_setenv("TZ", tz.c_str(), TRUE);

	auto tzone = g_time_zone_new_local ();
	bool have_tz = g_strcmp0(g_time_zone_get_identifier(tzone), tz.c_str()) == 0;
	g_time_zone_unref (tzone);

	if (old_tz)
		g_setenv("TZ", old_tz, TRUE);
	else
		g_unsetenv("TZ");

	return have_tz;
}

std::string
Mu::summarize(const std::string& str, size_t max_lines)
{
	size_t nl_seen;
	unsigned i,j;
	gboolean last_was_blank;

	if (str.empty())
		return {};

	/* len for summary <= original len */
	char *summary = g_new (gchar, str.length() + 1);

	/* copy the string up to max_lines lines, replace CR/LF/tab with
	 * single space */
	for (i = j = 0, nl_seen = 0, last_was_blank = TRUE;
	     nl_seen < max_lines && i < str.length(); ++i) {

		if (str[i] == '\n' || str[i] == '\r' ||
		    str[i] == '\t' || str[i] == ' ' ) {

			if (str[i] == '\n')
				++nl_seen;

			/* no double-blanks or blank at end of str */
			if (!last_was_blank && str[i+1] != '\0')
				summary[j++] = ' ';

			last_was_blank = TRUE;
		} else {

			summary[j++] = str[i];
			last_was_blank = FALSE;
		}
	}

	summary[j] = '\0';

	return to_string_gchar(std::move(summary)/*consumes*/);
}




static bool
locale_is_utf8 (void)
{
	const gchar *dummy;
	static int is_utf8 = -1;
	if (G_UNLIKELY(is_utf8 == -1))
		is_utf8 = g_get_charset(&dummy) ? 1 : 0;

	return !!is_utf8;
}

bool
Mu::fputs_encoded (const std::string& str, FILE *stream)
{
	g_return_val_if_fail (stream, false);

	/* g_get_charset return TRUE when the locale is UTF8 */
	if (locale_is_utf8())
		return ::fputs (str.c_str(), stream) == EOF ? false: true;

	 /* charset is _not_ utf8, so we need to convert it */
	char *conv{};
	if (g_utf8_validate (str.c_str(), -1, NULL))
		conv = g_locale_from_utf8 (str.c_str(), -1, {}, {}, {});

	/* conversion failed; this happens because is some cases GMime may gives
	 * us non-UTF-8 strings from e.g. wrongly encoded message-subjects; if
	 * so, we escape the string */
	conv = conv ? conv : g_strescape (str.c_str(), "\n\t");
	int rv   = conv ? ::fputs (conv, stream) : EOF;
	g_free (conv);

	return (rv == EOF) ? false: true;
}
