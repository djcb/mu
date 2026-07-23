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

#if BUILD_TESTS
#include "mu-test-utils.hh"

// No need for extensive regex test, we just rely on GRegex.

static void
test_regex_match()
{
	auto rx = Regex::make("a.*b.c");
	assert_valid_result(rx);

	assert_equal(mu_format("{}", *rx), "/a.*b.c/");

	g_assert_true(rx->matches("axxxxxbqc"));
	g_assert_false(rx->matches("axxxxxbqqc"));

	{ // unset matches nothing.
		Regex rx2;
		g_assert_false(rx2.matches(""));
	}
}


static void
test_regex_match2()
{
	Regex rx;
	{
		std::string foo = "h.llo";
		rx = unwrap(Regex::make(foo.c_str()));
	}

	std::string hei = "hei";

	g_assert_true(rx.matches("hallo"));
	g_assert_false(rx.matches(hei));
}


static void
test_regex_match_groups()
{
	{
		auto rx = Regex::make("^(\\d+)-(\\w+)$");
		assert_valid_result(rx);

		const auto groups{rx->match_groups("123-abc")};
		g_assert_true(!!groups);
		g_assert_cmpuint(groups->size(), ==, 3);
		assert_equal(groups->at(0), "123-abc");
		assert_equal(groups->at(1), "123");
		assert_equal(groups->at(2), "abc");

		// no match -> Nothing
		g_assert_false(!!rx->match_groups("nope"));
	}

	{ // always capture-count + 1 elements; a trailing group that
	  // did not participate is an empty string
		auto rx = Regex::make("^(\\d+)(k|m|g)?$", G_REGEX_CASELESS);
		assert_valid_result(rx);

		const auto groups{rx->match_groups("512")};
		g_assert_true(!!groups);
		g_assert_cmpuint(groups->size(), ==, 3);
		assert_equal(groups->at(1), "512");
		assert_equal(groups->at(2), "");

		const auto groups2{rx->match_groups("512K")};
		g_assert_true(!!groups2);
		g_assert_cmpuint(groups2->size(), ==, 3);
		assert_equal(groups2->at(2), "K");
	}

	{ // a non-participating group in the middle
		auto rx = Regex::make("^(?:(a)|(b))(c)$");
		assert_valid_result(rx);

		const auto groups{rx->match_groups("bc")};
		g_assert_true(!!groups);
		g_assert_cmpuint(groups->size(), ==, 4);
		assert_equal(groups->at(1), "");
		assert_equal(groups->at(2), "b");
		assert_equal(groups->at(3), "c");
	}

	{ // no capture groups at all -> just the full match
		auto rx = Regex::make("b.c");
		assert_valid_result(rx);

		const auto groups{rx->match_groups("abxcd")};
		g_assert_true(!!groups);
		g_assert_cmpuint(groups->size(), ==, 1);
		assert_equal(groups->at(0), "bxc");
	}

	{ // unset rx matches nothing
		Regex rx;
		g_assert_false(!!rx.match_groups("foo"));
	}
}

static void
test_regex_replace()
{
	{
		auto rx = Regex::make("f.o");
		assert_valid_result(rx);
		assert_equal(rx->replace("foobar", "cuux").value_or("error"), "cuuxbar");
	}

	{
		auto rx = Regex::make("f.o", G_REGEX_MULTILINE);
		assert_valid_result(rx);
		assert_equal(rx->replace("foobar\nfoobar", "cuux").value_or("error"),
			     "cuuxbar\ncuuxbar");
	}
}


static void
test_regex_fail()
{
	allow_warnings();

	{ // unset rx can't replace / error.
		Regex rx;
		assert_equal(mu_format("{}", rx), "//");
		g_assert_false(!!rx.replace("foo", "bar"));
	}

	{
		auto rx = Regex::make("(");
		g_assert_false(!!rx);

	}

}

int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	g_test_add_func("/regex/match", test_regex_match);
	g_test_add_func("/regex/match2", test_regex_match2);
	g_test_add_func("/regex/match-groups", test_regex_match_groups);
	g_test_add_func("/regex/replace", test_regex_replace);
	g_test_add_func("/regex/fail", test_regex_fail);

	return g_test_run();
}

#endif /*BUILD_TESTS*/
