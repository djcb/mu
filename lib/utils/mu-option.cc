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

#include "mu-option.hh"
#include <glib.h>

using namespace Mu;

Mu::Option<std::string>
Mu::to_string_opt_gchar(gchar*&& str)
{
	auto res = to_string_opt(str);
	g_free(str);

	return res;
}

#if BUILD_TESTS
#include "mu-test-utils.hh"

static Option<int>
get_opt_int(bool b)
{
	if (b)
		return Some(123);
	else
		return Nothing;
}

static void
test_option()
{
	{
		const auto oi{get_opt_int(true)};
		g_assert_true(!!oi);
		g_assert_cmpint(oi.value(), ==, 123);
	}

	{
		const auto oi{get_opt_int(false)};
		g_assert_false(!!oi);
		g_assert_false(oi.has_value());
		g_assert_cmpint(oi.value_or(456), ==, 456);
	}
}

static void
test_unwrap()
{
	{
		auto&& oi{get_opt_int(true)};
		g_assert_cmpint(unwrap(std::move(oi)), ==, 123);
	}

	auto ex{0};
	try {
		auto&& oi{get_opt_int(false)};
		unwrap(std::move(oi));
	} catch(...) {
		ex = 1;
	}

	g_assert_cmpuint(ex, ==, 1);
}

static void
test_opt_gchar()
{
	auto o1{to_string_opt_gchar(g_strdup("boo!"))};
	auto o2{to_string_opt_gchar(nullptr)};

	g_assert_false(!!o2);
	g_assert_true(o1.value() == "boo!");
}



int
main(int argc, char* argv[])
{
	g_test_init(&argc, &argv, NULL);

	g_test_add_func("/option/option", test_option);
	g_test_add_func("/option/unwrap", test_unwrap);
	g_test_add_func("/option/opt-gchar", test_opt_gchar);

	return g_test_run();
}

#endif /*BUILD_TESTS*/
