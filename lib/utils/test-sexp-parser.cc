/*
** Copyright (C) 2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-command-parser.hh"
#include "mu-utils.hh"

using namespace Mu;

static bool
check_parse (const std::string& expr, const std::string& expected)
{
        try {
                const auto parsed{to_string(Sexp::parse(expr))};
                g_assert_cmpstr(parsed.c_str(), ==, expected.c_str());
                return true;

        } catch (const Error& err) {
                g_warning ("caught exception parsing '%s': %s", expr.c_str(), err.what());
                return false;
        }
}

static void
test_parser()
{
        check_parse(R"(:foo-123)", "<symbol>{:foo-123}");
        check_parse(R"("foo")",    "<string>{foo}");
        check_parse(R"(12345)",    "<integer>{12345}");
        check_parse(R"(-12345)",   "<integer>{-12345}");
        check_parse(R"((123 bar "cuux"))",    "<list>(<integer>{123}<symbol>{bar}<string>{cuux})");

        check_parse(R"("\"")", "<string>{\"}");
        check_parse(R"("\\")", "<string>{\\}");
}

int
main (int argc, char *argv[]) try
{
        g_test_init (&argc, &argv, NULL);

        if (argc == 2) {
                std::cout << Sexp::parse(argv[1]) << '\n';
                return 0;
        }

        g_test_add_func ("/utils/command-parser/parse", test_parser);

	return g_test_run ();


} catch (const std::runtime_error& re) {
        std::cerr << re.what() << "\n";
        return 1;
}
