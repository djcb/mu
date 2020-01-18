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

#include "mu-command-parser.hh"
#include "mu-utils.hh"

using namespace Mu;

static void
test_param_getters()
{
        const auto node { Sexp::parse(R"((foo :bar 123 :cuux "456" :boo nil :bah true))")};

        std::cout << node << "\n";

        g_assert_cmpint(Command::get_int_or(node.children,"bar"), ==, 123);
        assert_equal(Command::get_string_or(node.children, "bra", "bla"), "bla");
        assert_equal(Command::get_string_or(node.children, "cuux"), "456");

        g_assert_true(Command::get_bool_or(node.children,"boo") == false);
        g_assert_true(Command::get_bool_or(node.children,"bah") == true);
}


static bool
call (const Command::CommandMap& cmap, const std::string& sexp) try
{
        const auto node{Sexp::parse(sexp)};
        g_message ("invoking %s", to_string(node).c_str());

        invoke (cmap, node);

        return true;

} catch (const Error& err) {
        g_warning ("%s", err.what());
        return false;
}

static void
test_command()
{
        using namespace Command;

        CommandMap cmap;

        cmap.emplace("my-command",
                     CommandInfo{
                             ArgMap{ {"param1", ArgInfo{Sexp::Type::String, true, "some string" }},
                                     {"param2", ArgInfo{Sexp::Type::Integer, false, "some integer"}}},
                            "My command,",
                            {}});

        //std::cout << cmap << "\n";

        g_assert_true(call(cmap, "(my-command :param1 \"hello\")"));
        g_assert_true(call(cmap, "(my-command :param1 \"hello\" :param2 123)"));
        g_assert_true(call(cmap, "(my-command :param1 \"hello\" :param2 123 :param3 xxx)"));
}

static void
test_command2()
{
        using namespace Command;

        CommandMap cmap;
        cmap.emplace("bla",
                   CommandInfo{
                           ArgMap{
                                   {"foo",  ArgInfo{Sexp::Type::Integer, false, "foo"}},
                                   {"bar",  ArgInfo{Sexp::Type::String, false,  "bar"}},
                           },"yeah",
                           [&](const auto& params){}});


        g_assert_true (call(cmap, "(bla :foo nil :bla nil)"));
}


static void
test_command_fail()
{
        using namespace Command;

        allow_warnings();

        CommandMap cmap;

        cmap.emplace("my-command",
                     CommandInfo{
                             ArgMap{ {"param1", ArgInfo{Sexp::Type::String, true, "some string" }},
                                     {"param2", ArgInfo{Sexp::Type::Integer, false, "some integer"}}},
                            "My command,",
                            {}});

        g_assert_false (call(cmap, "(my-command)"));
        g_assert_false (call(cmap, "(my-command2)"));
        g_assert_false(call(cmap, "(my-command :param1 123 :param2 123)"));
        g_assert_false(call(cmap, "(my-command :param1 \"hello\" :param2 \"123\")"));

}


int
main (int argc, char *argv[]) try
{
        g_test_init (&argc, &argv, NULL);

        g_test_add_func ("/utils/command-parser/param-getters", test_param_getters);
        g_test_add_func ("/utils/command-parser/command", test_command);
        g_test_add_func ("/utils/command-parser/command2", test_command2);
        g_test_add_func ("/utils/command-parser/command-fail", test_command_fail);

	return g_test_run ();


} catch (const std::runtime_error& re) {
        std::cerr << re.what() << "\n";
        return 1;
}
