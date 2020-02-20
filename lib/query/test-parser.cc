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

#include "mu-parser.hh"
using namespace Mu;

struct Case {
	const std::string expr;
	const std::string expected;
	WarningVec warnings;
};

using CaseVec = std::vector<Case>;

static void
test_cases(const CaseVec& cases)
{
	for (const auto& casus : cases ) {

		WarningVec warnings;
		const auto tree = parse (casus.expr, warnings);

		std::stringstream ss;
		ss << tree;

		if (g_test_verbose()) {
			std::cout << "\n";
			std::cout << casus.expr << std::endl;
			std::cout << "exp:" << casus.expected << std::endl;
			std::cout << "got:" << ss.str() << std::endl;
		}
		g_assert_true (casus.expected == ss.str());

		// g_assert_cmpuint (casus.warnings.size(), ==, warnings.size());
		// for (auto i = 0; i != (int)casus.warnings.size(); ++i) {
		// 	std::cout << "exp:" << casus.warnings[i] << std::endl;
		// 	std::cout << "got:" << warnings[i] << std::endl;
		// 	g_assert_true (casus.warnings[i] == warnings[i]);
		// }
	}
}

static void
test_basic ()
{
	CaseVec cases = {
		//{ "", R"#((atom :value ""))#"},
		{ "foo",  R"#((value "" "foo"))#", },
		{ "foo       or         bar",
		  R"#((or(value "" "foo")(value "" "bar")))#" },
		{ "foo and bar",
		  R"#((and(value "" "foo")(value "" "bar")))#"},
	};

	test_cases (cases);
}

static void
test_complex ()
{
	CaseVec cases = {
		{ "foo and bar or cuux",
		  R"#((or(and(value "" "foo")(value "" "bar")))#" +
		  std::string(R"#((value "" "cuux")))#") },

		{ "a and not b",
		  R"#((and(value "" "a")(not(value "" "b"))))#"
		},
		{ "a and b and c",
		  R"#((and(value "" "a")(and(value "" "b")(value "" "c"))))#"
		},
		{ "(a or b) and c",
		  R"#((and(or(value "" "a")(value "" "b"))(value "" "c")))#"
		},
		{ "a b", // implicit and
		  R"#((and(value "" "a")(value "" "b")))#"
		},
		{ "a not b", // implicit and not
		  R"#((and(value "" "a")(not(value "" "b"))))#"
		},
		{ "not b", // implicit and not
		  R"#((not(value "" "b")))#"
		}
	};

	test_cases (cases);
}


static void
test_range ()
{
	CaseVec cases = {
		{ "range:a..b", // implicit and
		  R"#((range "range" "a" "b"))#"
		},
	};

	test_cases (cases);
}


static void
test_flatten ()
{
	CaseVec cases = {
		{ " Mötørhęåđ", R"#((value "" "motorhead"))#" }
	};

	test_cases (cases);
}

int
main (int argc, char *argv[])
{
	g_test_init (&argc, &argv, NULL);

	g_test_add_func ("/parser/basic",    test_basic);
	g_test_add_func ("/parser/complex",  test_complex);
	g_test_add_func ("/parser/range",    test_range);
	g_test_add_func ("/parser/flatten",  test_flatten);

	return g_test_run ();
}
