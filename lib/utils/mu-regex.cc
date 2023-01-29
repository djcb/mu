/*
** Copyright (C) 2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-regex.hh"
#include <iostream>

using namespace Mu;

// LCOV_EXCL_STOP

#if BUILD_TESTS
#include "mu-test-utils.hh"

// No need for extensive regex test, we just rely on GRegex.

static void
test_regex_match()
{
	auto rx = Regex::make("a.*b.c");
	assert_valid_result(rx);

	g_assert_true(rx->matches("axxxxxbqc"));
	g_assert_false(rx->matches("axxxxxbqqc"));
}

static void
test_regex_replace()
{
	auto rx = Regex::make("f.o");
	assert_valid_result(rx);

	assert_equal(rx->replace("foobar", "cuux"), "cuuxbar");
}

int
main(int argc, char* argv[])
try {
        mu_test_init(&argc, &argv);

        g_test_add_func("/regex/match", test_regex_match);
        g_test_add_func("/regex/replace", test_regex_replace);
        return g_test_run();

} catch (const std::runtime_error& re) {
        std::cerr << re.what() << "\n";
        return 1;
}

#endif /*BUILD_TESTS*/
