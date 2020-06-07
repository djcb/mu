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
using namespace Sexp;

static bool
check_parse (const std::string& expr, const std::string& expected)
{
        try {
                const auto parsed{to_string(Node::make(expr))};
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
        check_parse(":foo-123",             ":foo-123");
        check_parse("foo",                  "foo");
        check_parse(R"(12345)",             "12345");
        check_parse(R"(-12345)",            "-12345");
        check_parse(R"((123 bar "cuux"))",  "(123 bar \"cuux\")");

        check_parse(R"("foo\"bar\"cuux")",  "\"foo\\\"bar\\\"cuux\"");

        check_parse(R"("foo
bar")",  "\"foo\\nbar\"");
}

static void
test_builder()
{
        const auto nstr{Node::make_string("foo")};
        g_assert_true(nstr.value() == "foo");
        g_assert_true(nstr.type()  == Node::Type::String);
        assert_equal(nstr.to_string(), "\"foo\"");

        const auto nnum{Node::make_number(123)};
        g_assert_true(nnum.value() == "123");
        g_assert_true(nnum.type()  == Node::Type::Number);
        assert_equal(nnum.to_string(), "123");

        const auto nsym{Node::make_symbol("blub")};
        g_assert_true(nsym.value() == "blub");
        g_assert_true(nsym.type()  == Node::Type::Symbol);
        assert_equal(nsym.to_string(), "blub");

        Node::Seq seq;
        seq.add(Node::make_string("foo"));
        seq.add(Node::make_number(123));
        seq.add(Node::make_symbol("blub"));

        const auto nlst = Node::make_list(std::move(seq));
        g_assert_true(nlst.elements().size() == 3);
        g_assert_true(nlst.type() == Node::Type::List);
        g_assert_true(nlst.elements().at(1).value() == "123");

        assert_equal(nlst.to_string(),"(\"foo\" 123 blub)");
}

static void
test_props()
{
        Node::Seq seq;

        seq.add_prop(":foo", "bär");
        seq.add_prop(":cuux", 123);
        seq.add_prop(":flub", Node::make_symbol("fnord"));

        Node::Seq seq2;
        seq2.add(Node::make_string("foo"));
        seq2.add(Node::make_number(123));
        seq2.add(Node::make_symbol("blub"));

        seq.add_prop(":boo", std::move(seq2));

        Node expr = Node::make_list(std::move(seq));
        assert_equal(expr.to_string(),
                     "(:foo \"b\\303\\244r\" :cuux 123 :flub fnord :boo (\"foo\" 123 blub))");
}

int
main (int argc, char *argv[]) try
{
        g_test_init (&argc, &argv, NULL);

        if (argc == 2) {
                std::cout << Sexp::Node::make(argv[1]) << '\n';
                return 0;
        }

        g_test_add_func ("/utils/sexp/parser",  test_parser);
        g_test_add_func ("/utils/sexp/builder", test_builder);
        g_test_add_func ("/utils/sexp/props",   test_props);

	return g_test_run ();


} catch (const std::runtime_error& re) {
        std::cerr << re.what() << "\n";
        return 1;
}
