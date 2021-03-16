/*
** Copyright (C) 2017 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <vector>
#include <glib.h>

#include <iostream>
#include <sstream>
#include <functional>

#include "mu-utils.hh"

using namespace Mu;

struct Case {
	const std::string	expr;
	bool			is_first{};
	const std::string	expected;
};
using CaseVec = std::vector<Case>;
using ProcFunc = std::function<std::string(std::string, bool)>;


static void
test_cases(const CaseVec& cases, ProcFunc proc)
{
	for (const auto& casus : cases ) {

		const auto res = proc(casus.expr, casus.is_first);
		if (g_test_verbose()) {
			std::cout << "\n";
			std::cout << casus.expr << ' ' << casus.is_first << std::endl;
			std::cout << "exp: '" << casus.expected << "'" << std::endl;
			std::cout << "got: '" << res << "'" << std::endl;
		}

		g_assert_true (casus.expected == res);
	}
}

static void
test_date_basic ()
{
	g_setenv ("TZ", "Europe/Helsinki", TRUE);

	CaseVec cases = {
		{ "2015-09-18T09:10:23", true,  "1442556623" },
		{ "1972-12-14T09:10:23", true,	"0093165023" },
		{ "1854-11-18T17:10:23", true,	"0000000000" },

		{ "2000-02-31T09:10:23", true,  "0951861599" },
		{ "2000-02-29T23:59:59", true,  "0951861599" },

		{ "2016",		true,	"1451599200" },
		{ "2016",		false,  "1483221599" },

		{ "fnorb",		 true,	"0000000000" },
		{ "fnorb",		 false, "9999999999" },
		{ "",			 false, "9999999999" },
		{ "",			 true,	"0000000000" }
	};

	test_cases (cases, [](auto s, auto f){ return date_to_time_t_string(s,f); });
}

static void
test_date_ymwdhMs (void)
{
	struct {
		std::string	expr;
		long		diff;
		int		tolerance;
	} tests[] = {
		{ "3h", 3 * 60 * 60, 1 },
		{ "21d", 21 * 24 * 60 * 60, 3600 + 1 },
		{ "2w", 2 * 7 * 24 * 60 * 60, 3600 + 1 },

		{ "2y", 2 * 365 * 24 * 60 * 60, 24 * 3600 + 1 },
		{ "3m", 3 * 30 * 24 * 60 * 60, 3 * 24 * 3600 + 1 }
	};

	for (auto i = 0; i != G_N_ELEMENTS(tests); ++i) {
		const auto diff = time(NULL) -
			strtol(Mu::date_to_time_t_string(tests[i].expr, true).c_str(),
			       NULL, 10);
		if (g_test_verbose())
			std::cerr << tests[i].expr << ' '
				  << diff << ' '
				  << tests[i].diff << std::endl;

		g_assert_true (tests[i].diff - diff <= tests[i].tolerance);
	}

	g_assert_true (strtol(Mu::date_to_time_t_string("-1y", true).c_str(),
			      NULL, 10) == 0);
}

static void
test_size ()
{
	CaseVec cases = {
		{ "456", true,  "0000000456" },
		{ "",    false, "9999999999" },
		{ "",    true,  "0000000000" },
	};

	test_cases (cases, [](auto s, auto f){ return size_to_string(s,f); });
}


static void
test_flatten ()
{
	CaseVec cases = {
		{ "Менделе́ев", true,  "менделеев" },
		{ "",    false, "" },
		{ "Ångström",    true,  "angstrom" },
	};

	test_cases (cases, [](auto s, auto f){ return utf8_flatten(s); });
}

static void
test_remove_ctrl ()
{
	CaseVec cases = {
		{ "Foo\n\nbar", true,  "Foo bar" },
		{ "",    false, "" },
		{ "   ",    false, " " },
		{ "Hello   World   ",    false, "Hello World " },
		{ "Ångström", false,  "Ångström" },
	};

	test_cases (cases, [](auto s, auto f){ return remove_ctrl(s); });
}



static void
test_clean ()
{
	CaseVec cases = {
		{ "\t a\t\nb ", true,  "a  b" },
		{ "",    false, "" },
		{ "Ångström",    true,  "Ångström" },
	};

	test_cases (cases, [](auto s, auto f){ return utf8_clean(s); });
}


static void
test_format ()
{
	g_assert_true (format ("hello %s, %u", "world", 123) ==
		       "hello world, 123");
}


enum struct Bits { None = 0, Bit1 = 1 << 0, Bit2 = 1 << 1 };
MU_ENABLE_BITOPS(Bits);

static void
test_define_bitmap()
{
	g_assert_cmpuint((guint)Bits::None,==,(guint)0);
	g_assert_cmpuint((guint)Bits::Bit1,==,(guint)1);
	g_assert_cmpuint((guint)Bits::Bit2,==,(guint)2);

	g_assert_cmpuint((guint)(Bits::Bit1|Bits::Bit2),==,(guint)3);
	g_assert_cmpuint((guint)(Bits::Bit1&Bits::Bit2),==,(guint)0);

	g_assert_cmpuint((guint)(Bits::Bit1&(~Bits::Bit2)),==,(guint)1);

	{
		Bits b{Bits::Bit1};
		b|=Bits::Bit2;
		g_assert_cmpuint((guint)b,==,(guint)3);
	}

	{
		Bits b{Bits::Bit1};
		b&=Bits::Bit1;
		g_assert_cmpuint((guint)b,==,(guint)1);
	}


}



int
main (int argc, char *argv[])
{
	g_test_init (&argc, &argv, NULL);

	g_test_add_func ("/utils/date-basic",  test_date_basic);
	g_test_add_func ("/utils/date-ymwdhMs",  test_date_ymwdhMs);
	g_test_add_func ("/utils/size",  test_size);
	g_test_add_func ("/utils/flatten",  test_flatten);
	g_test_add_func ("/utils/remove-ctrl",  test_remove_ctrl);
	g_test_add_func ("/utils/clean",  test_clean);
	g_test_add_func ("/utils/format",  test_format);
	g_test_add_func ("/utils/define-bitmap",  test_define_bitmap);

	return g_test_run ();
}
