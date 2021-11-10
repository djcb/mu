/*
**  Copyright (C) 2017-2021 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#define _XOPEN_SOURCE
#include <time.h>

#define GNU_SOURCE
#include <stdio.h>
#include <stdint.h>

#include <string.h>
#include <iostream>
#include <algorithm>

#include <glib.h>
#include <glib/gprintf.h>

#include "mu-utils.hh"
#include "mu-util.h"

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
		auto        l = g_ascii_strdown(str, -1);
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

std::string
Mu::utf8_clean(const std::string& dirty)
{
	GString* gstr = g_string_sized_new(dirty.length());

	for (auto cur = dirty.c_str(); cur && *cur; cur = g_utf8_next_char(cur)) {
		const gunichar uc = g_utf8_get_char(cur);
		if (g_unichar_iscntrl(uc))
			g_string_append_c(gstr, ' ');
		else
			g_string_append_unichar(gstr, uc);
	}

	std::string clean(gstr->str, gstr->len);
	g_string_free(gstr, TRUE);

	clean.erase(0, clean.find_first_not_of(" "));
	clean.erase(clean.find_last_not_of(" ") + 1); // remove trailing space

	return clean;
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
	char**                   parts = g_strsplit(str.c_str(), sepa.c_str(), -1);
	std::vector<std::string> vec;
	for (auto part = parts; part && *part; ++part)
		vec.push_back(*part);

	g_strfreev(parts);

	return vec;
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

constexpr const auto InternalDateFormat = "%010" G_GINT64_FORMAT;
constexpr const char InternalDateMin[]  = "0000000000";
constexpr const char InternalDateMax[]  = "9999999999";
static_assert(sizeof(InternalDateMin) == 10 + 1, "invalid");
static_assert(sizeof(InternalDateMax) == 10 + 1, "invalid");

static std::string
date_boundary(bool is_first)
{
	return is_first ? InternalDateMin : InternalDateMax;
}

std::string
Mu::date_to_time_t_string(int64_t t)
{
	char buf[sizeof(InternalDateMax)];
	g_snprintf(buf, sizeof(buf), InternalDateFormat, t);

	return buf;
}

std::string
Mu::time_to_string(const std::string& frm, time_t t, bool utc)
{
	GDateTime* dt = [&] {
		if (utc)
			return g_date_time_new_from_unix_utc(t);
		else
			return g_date_time_new_from_unix_local(t);
	}();

	char* str = g_date_time_format(dt, frm.c_str());
	g_date_time_unref(dt);
	if (!str) {
		g_warning("failed to format time");
		return {};
	}

	/* ensure it's utf8 */
	char* utf8_str = g_locale_to_utf8(str, -1, NULL, NULL, NULL);
	g_free(str);
	if (!utf8_str) {
		g_warning("failed to convert date to utf8");
		return {};
	}

	std::string res{utf8_str};
	g_free(utf8_str);

	return res;
}

static std::string
delta_ymwdhMs(const std::string& expr)
{
	char* endptr;
	auto  num = strtol(expr.c_str(), &endptr, 10);
	if (num <= 0 || num > 9999 || !endptr || !*endptr)
		return date_boundary(true);

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
	default: return date_boundary(true);
	}

	GDateTime *then, *now = g_date_time_new_now_local();
	if (weeks != 0)
		then = g_date_time_add_weeks(now, -weeks);
	else
		then =
		    g_date_time_add_full(now, -years, -months, -days, -hours, -minutes, -seconds);

	time_t t = MAX(0, (gint64)g_date_time_to_unix(then));

	g_date_time_unref(then);
	g_date_time_unref(now);

	return date_to_time_t_string(t);
}

static std::string
special_date(const std::string& d, bool is_first)
{
	if (d == "now")
		return date_to_time_t_string(time(NULL));

	else if (d == "today") {
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
		return date_to_time_t_string((time_t)t);

	} else
		return date_boundary(is_first);
}

// if a date has a month day greater than the number of days in that month,
// change it to a valid date point to the last second in that month
static void
fixup_month(struct tm* tbuf)
{
	decltype(tbuf->tm_mday) max_days;
	const auto              month = tbuf->tm_mon + 1;
	const auto              year  = tbuf->tm_year + 1900;

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
	case 11: max_days = 30; break;
	default: max_days = 31; break;
	}

	if (tbuf->tm_mday > max_days) {
		tbuf->tm_mday = max_days;
		tbuf->tm_hour = 23;
		tbuf->tm_min  = 59;
		tbuf->tm_sec  = 59;
	}
}

std::string
Mu::date_to_time_t_string(const std::string& dstr, bool is_first)
{
	gint64     t;
	struct tm  tbuf;
	GDateTime* dtime;

	/* one-sided dates */
	if (dstr.empty())
		return date_boundary(is_first);
	else if (dstr == "today" || dstr == "now")
		return special_date(dstr, is_first);

	else if (dstr.find_first_of("ymdwhMs") != std::string::npos)
		return delta_ymwdhMs(dstr);

	constexpr char UserDateMin[] = "19700101000000";
	constexpr char UserDateMax[] = "29991231235959";

	std::string date(is_first ? UserDateMin : UserDateMax);
	std::copy_if(dstr.begin(), dstr.end(), date.begin(), [](auto c) { return isdigit(c); });

	memset(&tbuf, 0, sizeof tbuf);
	if (!strptime(date.c_str(), "%Y%m%d%H%M%S", &tbuf) &&
	    !strptime(date.c_str(), "%Y%m%d%H%M", &tbuf) &&
	    !strptime(date.c_str(), "%Y%m%d", &tbuf) && !strptime(date.c_str(), "%Y%m", &tbuf) &&
	    !strptime(date.c_str(), "%Y", &tbuf))
		return date_boundary(is_first);

	fixup_month(&tbuf);

	dtime = g_date_time_new_local(tbuf.tm_year + 1900,
	                              tbuf.tm_mon + 1,
	                              tbuf.tm_mday,
	                              tbuf.tm_hour,
	                              tbuf.tm_min,
	                              tbuf.tm_sec);
	if (!dtime) {
		g_warning("invalid %s date '%s'", is_first ? "lower" : "upper", date.c_str());
		return date_boundary(is_first);
	}

	t = g_date_time_to_unix(dtime);
	g_date_time_unref(dtime);

	if (t < 0 || t > 9999999999)
		return date_boundary(is_first);
	else
		return date_to_time_t_string(t);
}

constexpr const auto SizeFormat = "%010" G_GINT64_FORMAT;

constexpr const char SizeMin[] = "0000000000";
constexpr const char SizeMax[] = "9999999999";
static_assert(sizeof(SizeMin) == 10 + 1, "invalid");
static_assert(sizeof(SizeMax) == 10 + 1, "invalid");

static std::string
size_boundary(bool is_first)
{
	return is_first ? SizeMin : SizeMax;
}

std::string
Mu::size_to_string(int64_t size)
{
	char buf[sizeof(SizeMax)];
	g_snprintf(buf, sizeof(buf), SizeFormat, size);

	return buf;
}

std::string
Mu::size_to_string(const std::string& val, bool is_first)
{
	std::string str;
	GRegex*     rx;
	GMatchInfo* minfo;

	/* one-sided ranges */
	if (val.empty())
		return size_boundary(is_first);

	rx = g_regex_new("(\\d+)(b|k|kb|m|mb|g|gb)?", G_REGEX_CASELESS, (GRegexMatchFlags)0, NULL);
	minfo = NULL;
	if (g_regex_match(rx, val.c_str(), (GRegexMatchFlags)0, &minfo)) {
		gint64 size;
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
		str = size_to_string(size);
	} else
		str = size_boundary(is_first);

	g_regex_unref(rx);
	g_match_info_unref(minfo);

	return str;
}

std::string
Mu::canonicalize_filename(const std::string& path, const std::string& relative_to)
{
	char* fname =
	    g_canonicalize_filename(path.c_str(), relative_to.empty() ? NULL : relative_to.c_str());

	std::string rv{fname};
	g_free(fname);

	return rv;
}

void
Mu::assert_equal(const std::string& s1, const std::string& s2)
{
	g_assert_cmpstr(s1.c_str(), ==, s2.c_str());
}

void
Mu::assert_equal(const Mu::StringVec& v1, const Mu::StringVec& v2)
{
	g_assert_cmpuint(v1.size(), ==, v2.size());

	for (auto i = 0U; i != v1.size(); ++i)
		assert_equal(v1[i], v2[i]);
}

void
Mu::allow_warnings()
{
	g_test_log_set_fatal_handler(
	    [](const char*, GLogLevelFlags, const char*, gpointer) { return FALSE; },
	    {});
}
