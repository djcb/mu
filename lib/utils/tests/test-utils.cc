/*
** Copyright (C) 2017-2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <array>

#include "mu-utils.hh"
#include "mu-test-utils.hh"
#include "mu-error.hh"

using namespace Mu;


struct Case {
	const std::string expr;
	bool              is_first{};
	const std::string expected;
};
using CaseVec  = std::vector<Case>;
using ProcFunc = std::function<std::string(std::string, bool)>;

static void
test_cases(const CaseVec& cases, ProcFunc proc)
{
	for (const auto& casus : cases) {
		const auto res = proc(casus.expr, casus.is_first);
		if (g_test_verbose()) {
			std::cout << "\n";
			std::cout << casus.expr << ' ' << casus.is_first << std::endl;
			std::cout << "exp: '" << casus.expected << "'" << std::endl;
			std::cout << "got: '" << res << "'" << std::endl;
		}

		g_assert_true(casus.expected == res);
	}
}

static void
test_date_basic()
{
	const auto hki = "Europe/Helsinki";

	// ensure we have the needed TZ or skip the test.
	if (!timezone_available(hki)) {
		g_test_skip("timezone Europe/Helsinki not available");
		return;
	}

	g_setenv("TZ", hki, TRUE);
	constexpr std::array<std::tuple<const char*, bool/*is_first*/, int64_t>, 13> cases = {{
			{"2015-09-18T09:10:23", true, 1442556623},
			{"1972-12-14T09:10:23", true, 93165023},
			{"1854-11-18T17:10:23", true, 0},

			{"2000-02-31T09:10:23", true, 951861599},
			{"2000-02-29T23:59:59", true, 951861599},

			{"20220602", true, 1654117200},
			{"20220605", false, 1654462799},

			{"202206", true, 1654030800},
			{"202206", false, 1656622799},

			{"2016", true, 1451599200},
			{"2016", false, 1483221599},

			// {"fnorb", true,  -1},
			// {"fnorb", false, -1},
			{"", false, G_MAXINT64},
			{"", true, 0}
		}};

	for (auto& test: cases) {
		if (g_test_verbose())
			g_debug("checking %s", std::get<0>(test));
		g_assert_cmpuint(parse_date_time(std::get<0>(test),
						 std::get<1>(test)).value_or(-1),==,
				 std::get<2>(test));
	}
}

static void
test_date_ymwdhMs(void)
{
	struct testcase {
		std::string	expr;
		int64_t		diff;
		int		tolerance;
	};

	std::array<testcase, 7> cases = {{
		{"7s", 7, 1},
		{"3M", 3 * 60, 1},
		{"3h", 3 * 60 * 60, 1},
		{"21d", 21 * 24 * 60 * 60, 3600 + 1},
		{"2w", 2 * 7 * 24 * 60 * 60, 3600 + 1},
		{"2y", 2 * 365 * 24 * 60 * 60, 24 * 3600 + 1},
		{"3m", 3 * 30 * 24 * 60 * 60, 3 * 24 * 3600 + 1}
		}};

	for (auto&& tcase: cases) {
		const auto date = parse_date_time(tcase.expr, true);
		g_assert_true(date);
		const auto diff = ::time({}) - *date;
		if (g_test_verbose())
			std::cerr << tcase.expr << ' ' << diff << ' ' << tcase.diff << '\n';

		g_assert_true(tcase.diff - diff <= tcase.tolerance);
	}

	// note: perhaps it'd be nice if we'd detect this error;
	// currently we're being rather tolerant
	// g_assert_false(!!parse_date_time("25q", false));
}

static void
test_parse_size()
{
	constexpr std::array<std::tuple<const char*, bool, int64_t>, 6> cases = {{
			{ "456", false, 456 },
			{ "", false, G_MAXINT64 },
			{ "", true, 0 },
			{ "2K", false, 2048 },
			{ "2M", true, 2097152 },
			{ "5G", true, 5368709120 }
		}};
	for(auto&& test: cases) {
		g_assert_cmpint(parse_size(std::get<0>(test), std::get<1>(test))
				.value_or(-1), ==, std::get<2>(test));
	}

	g_assert_false(!!parse_size("-1", true));
	g_assert_false(!!parse_size("scoobydoobydoo", false));
}

static void
test_flatten()
{
	CaseVec cases = {
	    {"Менделе́ев", true, "менделеев"},
	    {"", false, ""},
	    {"Ångström", true, "angstrom"},
	};

	test_cases(cases, [](auto s, auto f) { return utf8_flatten(s); });
}

static void
test_remove_ctrl()
{
	CaseVec cases = {
	    {"Foo\n\nbar", true, "Foo bar"},
	    {"", false, ""},
	    {"   ", false, " "},
	    {"Hello   World   ", false, "Hello World "},
	    {"Ångström", false, "Ångström"},
	};

	test_cases(cases, [](auto s, auto f) { return remove_ctrl(s); });
}

static void
test_clean()
{
	CaseVec cases = {
	    {"\t a\t\nb ", true, "a  b"},
	    {"", false, ""},
	    {"Ångström", true, "Ångström"},
	};

	test_cases(cases, [](auto s, auto f) { return utf8_clean(s); });
}

static void
test_format()
{
	g_assert_true(format("hello %s", "world") == "hello world");
	g_assert_true(format("hello %s, %u", "world", 123) == "hello world, 123");
}

static void
test_split()
{
	using svec = std::vector<std::string>;
	auto assert_equal_svec=[](const svec& sv1, const svec& sv2) {
		g_assert_cmpuint(sv1.size(),==,sv2.size());
		for (auto i = 0U; i != sv1.size(); ++i)
			g_assert_cmpstr(sv1[i].c_str(),==,sv2[i].c_str());
	};

	// string sepa
	assert_equal_svec(split("axbxc", "x"), {"a", "b", "c"});
	assert_equal_svec(split("axbxcx", "x"), {"a", "b", "c", ""});
	assert_equal_svec(split("", "boo"), {});
	assert_equal_svec(split("ayybyyc", "yy"), {"a", "b", "c"});
	assert_equal_svec(split("abc", ""), {"a", "b", "c"});
	assert_equal_svec(split("", "boo"), {});

	// char sepa
	assert_equal_svec(split("axbxc", 'x'), {"a", "b", "c"});
	assert_equal_svec(split("axbxcx", 'x'), {"a", "b", "c", ""});

	// rx sexp
	assert_equal_svec(split("axbyc", std::regex("[xy]")), {"a", "b", "c"});
}

static void
test_join()
{
	assert_equal(join({"a", "b", "c"}, "x"), "axbxc");
	assert_equal(join({"a", "b", "c"}, ""), "abc");
	assert_equal(join({},"foo"), "");
	assert_equal(join({"d", "e", "f"}, "foo"), "dfooefoof");
}


enum struct Bits { None = 0, Bit1 = 1 << 0, Bit2 = 1 << 1 };
MU_ENABLE_BITOPS(Bits);

static void
test_define_bitmap()
{
	g_assert_cmpuint((guint)Bits::None, ==, (guint)0);
	g_assert_cmpuint((guint)Bits::Bit1, ==, (guint)1);
	g_assert_cmpuint((guint)Bits::Bit2, ==, (guint)2);

	g_assert_cmpuint((guint)(Bits::Bit1 | Bits::Bit2), ==, (guint)3);
	g_assert_cmpuint((guint)(Bits::Bit1 & Bits::Bit2), ==, (guint)0);

	g_assert_cmpuint((guint)(Bits::Bit1 & (~Bits::Bit2)), ==, (guint)1);

	{
		Bits b{Bits::Bit1};
		b |= Bits::Bit2;
		g_assert_cmpuint((guint)b, ==, (guint)3);
	}

	{
		Bits b{Bits::Bit1};
		b &= Bits::Bit1;
		g_assert_cmpuint((guint)b, ==, (guint)1);
	}
}

static void
test_to_from_lexnum()
{
	assert_equal(to_lexnum(0), "g0");
	assert_equal(to_lexnum(100), "h64");
	assert_equal(to_lexnum(12345), "j3039");

	g_assert_cmpuint(from_lexnum(to_lexnum(0)), ==, 0);
	g_assert_cmpuint(from_lexnum(to_lexnum(7777)), ==, 7777);
	g_assert_cmpuint(from_lexnum(to_lexnum(9876543)), ==, 9876543);
}

static void
test_locale_workaround()
{
	g_assert_true(locale_workaround());

	g_setenv("LC_ALL", "BOO", 1);

	g_assert_true(locale_workaround());
}

static void
test_error()
{
	GError *err;
	err = g_error_new(MU_ERROR_DOMAIN, 77, "Hello, %s", "world");
	Error ex{Error::Code::Crypto, &err, "boo"};
	g_assert_cmpstr(ex.what(), ==, "boo: Hello, world");

	ex.fill_g_error(&err);
	g_assert_cmpuint(err->code, ==, static_cast<unsigned>(Error::Code::Crypto));
	g_clear_error(&err);
}

enum struct TestEnum { A, B, C };
constexpr AssocPairs<TestEnum, std::string_view, 3>
test_epairs = {{
	{TestEnum::A, "a"},
	{TestEnum::B, "b"},
	{TestEnum::C, "c"},
}};

static constexpr Option<std::string_view>
to_name(TestEnum te)
{
	return to_second(test_epairs, te);
}

static constexpr Option<TestEnum>
to_type(std::string_view name)
{
	return to_first(test_epairs, name);

}

static void
test_enum_pairs(void)
{
	assert_equal(to_name(TestEnum::A).value(), "a");
	g_assert_true(to_type("c").value() ==  TestEnum::C);
}

int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	g_test_add_func("/utils/date-basic", test_date_basic);
	g_test_add_func("/utils/date-ymwdhMs", test_date_ymwdhMs);
	g_test_add_func("/utils/parse-size", test_parse_size);
	g_test_add_func("/utils/flatten", test_flatten);
	g_test_add_func("/utils/remove-ctrl", test_remove_ctrl);
	g_test_add_func("/utils/clean", test_clean);
	g_test_add_func("/utils/format", test_format);
	g_test_add_func("/utils/split", test_split);
	g_test_add_func("/utils/join", test_join);
	g_test_add_func("/utils/define-bitmap", test_define_bitmap);
	g_test_add_func("/utils/to-from-lexnum", test_to_from_lexnum);
	g_test_add_func("/utils/locale-workaround", test_locale_workaround);
	g_test_add_func("/utils/error", test_error);
	g_test_add_func("/utils/enum-pairs", test_enum_pairs);

	return g_test_run();
}
