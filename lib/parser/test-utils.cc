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

#include "parser.hh"
using namespace Mux;

struct Case {
	const std::string	expr;
	bool			is_first;
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
			std::cout << "exp:" << casus.expected << std::endl;
			std::cout << "got:" << res << std::endl;
		}

		g_assert_true (casus.expected == res);
	}
}

static void
test_date ()
{
	g_setenv ("TZ", "Europe/Helsinki", TRUE);

	CaseVec cases = {
		{ "2015-09-18T09:10:23", true,  "001442556623" },
		{ "1972-12-14T09:10:23", true,  "000093165023" },
		{ "1854-11-18T17:10:23", true,  "000000000000" },
		{ "fnorb",               true,  "000000000000" },
		{ "fnorb",               false, "999999999999" },
		{ "",                    false, "999999999999" },
		{ "",                    true,  "000000000000" }
	};

	test_cases (cases, [](auto s, auto f){ return date_to_time_t_string(s,f); });
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


int
main (int argc, char *argv[])
{
	g_test_init (&argc, &argv, NULL);

	g_test_add_func ("/utils/process-date",  test_date);
	g_test_add_func ("/utils/process-size",  test_size);

	return g_test_run ();
}
