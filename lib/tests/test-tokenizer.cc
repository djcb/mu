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

#include "mu-tokenizer.hh"

struct Case {
	const char*      str;
	const Mu::Tokens tokens;
};

using CaseVec = std::vector<Case>;

using namespace Mu;
using TT = Token::Type;

static void
test_cases(const CaseVec& cases)
{
	for (const auto& casus : cases) {
		const auto tokens = tokenize(casus.str);

		g_assert_cmpuint((guint)tokens.size(), ==, (guint)casus.tokens.size());
		for (size_t u = 0; u != tokens.size(); ++u) {
			if (g_test_verbose()) {
				std::cerr << "case " << u << " " << casus.str << std::endl;
				std::cerr << "exp: '" << casus.tokens[u] << "'" << std::endl;
				std::cerr << "got: '" << tokens[u] << "'" << std::endl;
			}
			g_assert_true(tokens[u] == casus.tokens[u]);
		}
	}
}

static void
test_basic()
{
	CaseVec cases = {
	    {"", {}},

	    {"foo", Tokens{Token{3, TT::Data, "foo"}}},

	    {"foo bar cuux",
	     Tokens{Token{3, TT::Data, "foo"},
	            Token{7, TT::Data, "bar"},
	            Token{12, TT::Data, "cuux"}}},

	    {"\"foo bar\"", Tokens{Token{9, TT::Data, "foo bar"}}},

	    // ie. ignore missing closing '"'
	    {"\"foo bar", Tokens{Token{8, TT::Data, "foo bar"}}},

	};

	test_cases(cases);
}

static void
test_specials()
{
	CaseVec cases = {
	    {")*(",
	     Tokens{Token{1, TT::Close, ")"}, Token{2, TT::Data, "*"}, Token{3, TT::Open, "("}}},
	    {"\")*(\"", Tokens{Token{5, TT::Data, ")*("}}},
	};

	test_cases(cases);
}

static void
test_ops()
{
	CaseVec cases = {{"foo and bar oR cuux XoR fnorb",
	                  Tokens{Token{3, TT::Data, "foo"},
	                         Token{7, TT::And, "and"},
	                         Token{11, TT::Data, "bar"},
	                         Token{14, TT::Or, "oR"},
	                         Token{19, TT::Data, "cuux"},
	                         Token{23, TT::Xor, "XoR"},
	                         Token{29, TT::Data, "fnorb"}}},
	                 {"NOT (aap or mies)",
	                  Tokens{Token{3, TT::Not, "NOT"},
	                         Token{5, TT::Open, "("},
	                         Token{8, TT::Data, "aap"},
	                         Token{11, TT::Or, "or"},
	                         Token{16, TT::Data, "mies"},
	                         Token{17, TT::Close, ")"}}}};

	test_cases(cases);
}

static void
test_escape()
{
	CaseVec cases = {{"foo\"bar\"", Tokens{Token{8, TT::Data, "foobar"}}},
	                 {"\"fnorb\"", Tokens{Token{7, TT::Data, "fnorb"}}},
	                 {"\\\"fnorb\\\"", Tokens{Token{9, TT::Data, "fnorb"}}},
	                 {"foo\\\"bar\\\"", Tokens{Token{10, TT::Data, "foobar"}}}};

	test_cases(cases);
}

static void
test_to_string()
{
	std::stringstream ss;
	for (auto&& t : tokenize("foo and bar xor not cuux or fnorb"))
		ss << t << ' ';

	g_assert_true(ss.str() == "3: <data> [foo] 7: <and> [and] 11: <data> [bar] "
	                          "15: <xor> [xor] 19: <not> [not] 24: <data> [cuux] "
	                          "27: <or> [or] 33: <data> [fnorb] ");
}

int
main(int argc, char* argv[])
{
	g_test_init(&argc, &argv, NULL);

	g_test_add_func("/tokens/basic", test_basic);
	g_test_add_func("/tokens/specials", test_specials);
	g_test_add_func("/tokens/ops", test_ops);
	g_test_add_func("/tokens/escape", test_escape);
	g_test_add_func("/tokens/to-string", test_to_string);

	return g_test_run();
}
