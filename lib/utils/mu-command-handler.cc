/*
** Copyright (C) 2020-2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-command-handler.hh"
#include "mu-error.hh"
#include "mu-utils.hh"

#include <iostream>
#include <algorithm>

using namespace Mu;

Option<std::vector<std::string>>
Command::string_vec_arg(const std::string& name) const
{
	auto&& val{arg_val(name, Sexp::Type::List)};
	if (!val)
		return Nothing;

	std::vector<std::string> vec;
	for (const auto& item : val->list()) {
		if (!item.stringp()) {
			// mu_warning("command: non-string in string-list for {}: {}",
			//	  name, to_string());
			return Nothing;
		} else
			vec.emplace_back(item.string());
	}

	return vec;
}

static Result<void>
validate(const CommandHandler::CommandInfoMap& cmap,
	 const CommandHandler::CommandInfo& cmd_info,
	 const Command& cmd)
{
	// all required parameters must be present
	for (auto&& arg : cmd_info.args) {

		const auto& argname{arg.first};
		const auto& arginfo{arg.second};

		// calls use keyword-parameters, e.g.
		//
		//    (my-function :bar 1 :cuux "fnorb")
		//
		//  so, we're looking for the odd-numbered parameters.
		const auto param_it = cmd.find_arg(argname);
		const auto&& param_val = std::next(param_it);
		// it's an error when a required parameter is missing.
		if (param_it == cmd.cend()) {
			if (arginfo.required)
				return Err(Error::Code::Command,
					   "missing required parameter {} in command '{}'",
					   argname, cmd.to_string());
			continue; // not required
		}

		// the types must match, but the 'nil' symbol is acceptable as "no value"
		if (param_val->type() != arginfo.type && !(param_val->nilp()))
			return Err(Error::Code::Command,
				   "parameter {} expects type {}, but got {} in command '{}'",
				   argname, to_string(arginfo.type),
				   to_string(param_val->type()), cmd.to_string());
	}

	// all parameters must be known
	for (auto it = cmd.cbegin() + 1; it != cmd.cend() && it + 1 != cmd.cend(); it += 2) {
		const auto& cmdargname{it->symbol()};
		if (std::none_of(cmd_info.args.cbegin(), cmd_info.args.cend(),
				 [&](auto&& arg) { return cmdargname == arg.first; }))
			return Err(Error::Code::Command,
				   "unknown parameter '{} 'in command '{}'",
				   cmdargname.name.c_str(), cmd.to_string().c_str());
	}

	return Ok();

}

Result<void>
CommandHandler::invoke(const Command& cmd, bool do_validate) const
{
	const auto cmit{cmap_.find(cmd.name())};
	if (cmit == cmap_.cend())
		return Err(Error::Code::Command,
			   "unknown command '{}'", cmd.to_string().c_str());

	const auto& cmd_info{cmit->second};
	if (do_validate) {
		if (auto&& res = validate(cmap_, cmd_info, cmd); !res)
			return Err(res.error());
	}

	if (cmd_info.handler)
		cmd_info.handler(cmd);

	return Ok();
}


// LCOV_EXCL_START
#ifdef BUILD_TESTS

#include "mu-test-utils.hh"


static void
test_args()
{
	const auto cmd = Command::make_parse(R"((foo :bar 123 :cuux "456" :boo nil :bah true))");
	assert_valid_result(cmd);

	assert_equal(cmd->name(), "foo");
	g_assert_true(cmd->find_arg(":bar") != cmd->cend());
	g_assert_true(cmd->find_arg(":bxr") == cmd->cend());

	g_assert_cmpint(cmd->number_arg(":bar").value_or(-1), ==, 123);
	g_assert_cmpint(cmd->number_arg(":bor").value_or(-1), ==, -1);

	assert_equal(cmd->string_arg(":cuux").value_or(""), "456");
	assert_equal(cmd->string_arg(":caax").value_or(""), ""); // not present
	assert_equal(cmd->string_arg(":bar").value_or("abc"), "abc"); // wrong type

	g_assert_false(cmd->boolean_arg(":boo"));
	g_assert_true(cmd->boolean_arg(":bah"));
}

using	CommandInfoMap = CommandHandler::CommandInfoMap;
using	ArgMap	       = CommandHandler::ArgMap;
using	ArgInfo	       = CommandHandler::ArgInfo;
using	CommandInfo    = CommandHandler::CommandInfo;

static Result<void>
call(const CommandInfoMap& cmap, const std::string& str) try {

	if (const auto cmd{Command::make_parse(str)}; !cmd)
		return Err(Error::Code::Internal, "invalid s-expression '{}'", str);
	else
		return CommandHandler(cmap).invoke(*cmd);

} catch (const Error& err) {
	return Err(Error{err});
}

static void
test_command()
{
	allow_warnings();

	CommandInfoMap ci_map;
	ci_map.emplace(
	    "my-command",
	    CommandInfo{ArgMap{{":param1", ArgInfo{Sexp::Type::String, true, "some string"}},
			       {":param2", ArgInfo{Sexp::Type::Number, false, "some integer"}}},
			"My command,",
			{}});
	ci_map.emplace(
		"another-command",
		CommandInfo{
			ArgMap{
				{":queries", ArgInfo{Sexp::Type::List, false,
					 "queries for which to get read/unread numbers"}},
				{":symbol", ArgInfo{Sexp::Type::Symbol, true,
					 "some boring symbol"}},
				{":bool", ArgInfo{Sexp::Type::Symbol, true,
					 "some even more boring boolean symbol"}},
				{":symbol2", ArgInfo{Sexp::Type::Symbol, false,
					 "some even more boring symbol"}},
				{":bool2", ArgInfo{Sexp::Type::Symbol, false,
					 "some boring boolean symbol"}},
			},
			"get unread/totals information for a list of queries",
			[&](const auto& params) {
				const auto queries{params.string_vec_arg(":queries")
					.value_or(std::vector<std::string>{})};
				g_assert_cmpuint(queries.size(),==,3);
				g_assert_true(params.bool_arg(":bool").value_or(false) == true);
				assert_equal(params.symbol_arg(":symbol").value_or("boo"), "sym");

				g_assert_false(!!params.bool_arg(":bool2"));
				g_assert_false(!!params.bool_arg(":symbol2"));

			}});

	CommandHandler handler(std::move(ci_map));
	const auto cmap{handler.info_map()};

	assert_valid_result(call(cmap, "(my-command :param1 \"hello\")"));
	assert_valid_result(call(cmap, "(my-command :param1 \"hello\" :param2 123)"));
	g_assert_false(!!call(cmap, "(my-command :param1 \"hello\" :param2 123 :param3 xxx)"));
	assert_valid_result(call(cmap, "(another-command :queries (\"foo\" \"bar\" \"cuux\") "
				 ":symbol sym :bool true)"));
}

static void
test_command2()
{
	allow_warnings();

	CommandInfoMap cmap;
	cmap.emplace("bla",
		     CommandInfo{ArgMap{
				     {":foo", ArgInfo{Sexp::Type::Number, false, "foo"}},
				     {":bar", ArgInfo{Sexp::Type::String, false, "bar"}},
			     }, "yeah",
			     [&](const auto& params) {}});

	g_assert_true(call(cmap, "(bla :foo nil)"));
	g_assert_false(call(cmap, "(bla :foo nil :bla nil)"));
}

static void
test_command_fail()
{
	allow_warnings();

	CommandInfoMap cmap;

	cmap.emplace(
	    "my-command",
	    CommandInfo{ArgMap{{":param1", ArgInfo{Sexp::Type::String, true, "some string"}},
			       {":param2", ArgInfo{Sexp::Type::Number, false, "some integer"}}},
			"My command,",
			{}});

	g_assert_false(call(cmap, "(my-command)"));
	g_assert_false(call(cmap, "(my-command2)"));
	g_assert_false(call(cmap, "(my-command :param1 123 :param2 123)"));
	g_assert_false(call(cmap, "(my-command :param1 \"hello\" :param2 \"123\")"));

	g_assert_false(call(cmap, "(my-command"));

	g_assert_false(!!Command::make_parse(R"((foo :bar 123 :cuux "456" :boo nil :bah))"));
}


int
main(int argc, char* argv[]) try {

	mu_test_init(&argc, &argv);

	g_test_add_func("/utils/command-parser/args", test_args);
	g_test_add_func("/utils/command-parser/command", test_command);
	g_test_add_func("/utils/command-parser/command2", test_command2);
	g_test_add_func("/utils/command-parser/command-fail", test_command_fail);

	return g_test_run();

} catch (const std::runtime_error& re) {
	std::cerr << re.what() << "\n";
	return 1;
}

#endif /*BUILD_TESTS*/
// LCOV_EXCL_STOP
