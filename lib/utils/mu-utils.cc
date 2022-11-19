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
#include <regex>

#include <glib.h>
#include <glib/gprintf.h>

#include "mu-utils.hh"
#include "mu-utils-format.hh"
#include "mu-util.h"
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

std::string // gx_utf8_flatten
Mu::utf8_flatten(const char* str)
{
	if (!str)
		return {};

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

std::vector<std::string>
Mu::split(const std::string& str, const std::regex& sepa_rx)
{
	std::sregex_token_iterator it(str.begin(), str.end(), sepa_rx, -1);
	std::sregex_token_iterator end;

	return {it, end};
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

std::string
Mu::format(const char* frm, ...)
{
	va_list args;

	va_start(args, frm);
	auto str = vformat(frm, args);
	va_end(args);

	return str;
}

std::string
Mu::vformat(const char* frm, va_list args)
{
	char*      s{};
	const auto res = g_vasprintf(&s, frm, args);
	if (res == -1) {
		std::cerr << "string format failed" << std::endl;
		return {};
	}

	std::string str{s};
	g_free(s);

	return str;
}

std::string
Mu::time_to_string(const std::string& frm_, time_t t, bool utc)
{
	/* Temporary hack... https://github.com/djcb/mu/issues/2230 */
	const auto frm =
		g_utf8_validate(frm_.c_str(), frm_.length(), {}) ?
		frm_ : "%c";

	GDateTime* dt = std::invoke([&] {
		if (utc)
			return g_date_time_new_from_unix_utc(t);
		else
			return g_date_time_new_from_unix_local(t);
	});

	if (!dt) {
		g_warning("time_t out of range: <%" G_GUINT64_FORMAT ">",
			  static_cast<guint64>(t));
		return {};
	}

	auto datestr{to_string_opt_gchar(g_date_time_format(dt, frm.c_str()))};
	g_date_time_unref(dt);
	if (!datestr)
		g_warning("failed to format time with format '%s'", frm.c_str());

	return datestr.value_or("");
}

static Option<int64_t>
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

	auto  t = std::max<int64_t>(0, g_date_time_to_unix(then));

	g_date_time_unref(then);
	g_date_time_unref(now);

	return t;
}

static Option<int64_t>
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


Option<int64_t>
Mu::parse_date_time(const std::string& dstr, bool is_first)
{
	struct tm tbuf{};
	GDateTime *dtime{};
	int64_t t;

	/* one-sided dates */
	if (dstr.empty())
		return is_first ? 0 : G_MAXINT64;
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
	dtime = g_date_time_new_local(tbuf.tm_year + 1900,
				      tbuf.tm_mon + 1,
				      tbuf.tm_mday,
				      tbuf.tm_hour,
				      tbuf.tm_min,
				      tbuf.tm_sec);
	t = g_date_time_to_unix(dtime);
	g_date_time_unref(dtime);

	return std::max<int64_t>(t, 0);
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


std::string
Mu::canonicalize_filename(const std::string& path, const std::string& relative_to)
{
	auto str{to_string_opt_gchar(
		g_canonicalize_filename(
			path.c_str(),
			relative_to.empty() ? nullptr : relative_to.c_str())).value()};

	// remove trailing '/'... is this needed?
	if (str[str.length()-1] == G_DIR_SEPARATOR)
		str.erase(str.length() - 1);

	return str;
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
Mu::runtime_path(Mu::RuntimePath path, const std::string& muhome)
{
	auto [mu_cache, mu_config] =
		std::invoke([&]()->std::pair<std::string, std::string> {

			static std::string mu{"/mu"};
			if (muhome.empty())
				return { g_get_user_cache_dir() + mu,
					 g_get_user_config_dir() + mu };
			else
				return { muhome, muhome };
	});

	switch (path) {
	case Mu::RuntimePath::Cache:
		return mu_cache;
	case Mu::RuntimePath::XapianDb:
		return mu_cache + "/xapian";
	case Mu::RuntimePath::LogFile:
		return mu_cache + "/mu.log";
	case Mu::RuntimePath::Bookmarks:
		return mu_config + "/bookmarks";
	case Mu::RuntimePath::Config:
		return mu_config;
	case Mu::RuntimePath::Scripts:
		return mu_config + "/scripts";
	default:
		throw std::logic_error("unknown path");
	}
}
