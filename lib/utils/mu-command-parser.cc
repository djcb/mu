/*
** Copyright (C) 2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-command-parser.hh"
#include "mu-utils.hh"

#include <iostream>
#include <algorithm>

using namespace Mu;
using namespace Command;
using namespace Sexp;

static Mu::Error
command_error(const std::string& msg)
{
        return Mu::Error(Error::Code::Command,  msg);
}


void
Command::invoke(const Command::CommandMap& cmap, const Node& call)
{
        if (call.type != Type::List || call.children.empty() ||
            call.children[0].type != Type::Symbol)
                throw command_error("call must be a list starting with a symbol");

        const auto& params{call.children};
        const auto cmd_it = cmap.find(params[0].value);
        if (cmd_it == cmap.end())
                throw command_error("unknown command '" + params[0].value + "'");

        const auto& cinfo{cmd_it->second};

        // all required parameters must be present
        for (auto&& arg: cinfo.args) {
                const auto& argname{arg.first};
                const auto& arginfo{arg.second};

                // calls used keyword-parameters, e.g.
                //    (my-function :bar 1 :cuux "fnorb")
                // so, we're looking for the odd-numbered parameters.
                const auto param_it = [&]() {
                        for (size_t i = 1; i < params.size(); i += 2)
                                if (params[i].type == Type::Symbol &&
                                    params[i].value == ':' + argname)
                                        return params.begin() + i + 1;

                        return params.end();

                }();

                // it's an error when a required parameter is missing.
                if (param_it == params.end()) {
                        if (arginfo.required)
                                throw command_error("missing required parameter '" + argname + "'");
                        continue; // not required
                }

                // the types must match, but the 'nil' symbol is acceptable as
                // "no value"
                if (param_it->type != arginfo.type &&
                    !(param_it->type == Type::Symbol && param_it->value == "nil"))
                        throw command_error("parameter '" + argname + "' expects type " +
                                            to_string(arginfo.type) +
                                            " but got " + to_string(param_it->type));
        }

        // all passed parameters must be known
        for (size_t i = 1; i < params.size(); i += 2) {
                if (std::none_of(cinfo.args.begin(), cinfo.args.end(),
                                 [&](auto&& arg) {return params[i].value == ":" + arg.first;}))
                        throw command_error("unknown parameter '" + params[i].value + "'");
        }

        if (cinfo.handler)
                cinfo.handler(params);
}

static Parameters::const_iterator
find_param_node (const Parameters& params, const std::string& argname)
{
        for (size_t i = 1; i < params.size(); i += 2) {
                if (i + 1 != params.size() &&
                    params[i].type == Type::Symbol &&
                    params[i].value == ':' + argname)
                        return params.begin() + i + 1;
        }

        return params.end();
}

constexpr auto Nil = "nil";

static bool
is_nil(const Node& node)
{
        return node.type == Type::Symbol && node.value == Nil;
}

const std::string&
Command::get_string_or (const Parameters& params, const std::string& argname,
                        const std::string& alt)
{
        const auto it = find_param_node (params, argname);
        if (it == params.end() || is_nil(*it))
                return alt;
        else if (it->type != Type::String)
                throw Error(Error::Code::InvalidArgument, "expected <string> but got %s (value: '%s')",
                            to_string(it->type).c_str(),
                            it->value.c_str());

        return it->value;
}

const std::string&
Command::get_symbol_or (const Parameters& params, const std::string& argname,
                        const std::string& alt)
{
        const auto it = find_param_node (params, argname);
        if (it == params.end() || is_nil(*it))
                return alt;
        else if (it->type != Type::Symbol)
                throw Error(Error::Code::InvalidArgument, "expected <symbol> but got %s (value: '%s')",
                            to_string(it->type).c_str(),
                            it->value.c_str());

        return it->value;
}


int
Command::get_int_or (const Parameters& params, const std::string& argname,
                     int alt)
{
        const auto it = find_param_node (params, argname);
        if (it == params.end() || is_nil(*it))
                return alt;
        else if (it->type != Type::Integer)
                throw Error(Error::Code::InvalidArgument, "expected <integer> but got %s",
                            to_string(it->type).c_str());
        else
                return ::atoi(it->value.c_str());
}

bool
Command::get_bool_or (const Parameters& params, const std::string& argname,
                      bool alt)
{
        const auto it = find_param_node (params, argname);
        if (it == params.end())
                return alt;
        else if (it->type != Type::Symbol)
                throw Error(Error::Code::InvalidArgument, "expected <symbol> but got %s",
                            to_string(it->type).c_str());
        else
                return it->value != Nil;
}

std::vector<std::string>
Command::get_string_vec  (const Parameters& params, const std::string& argname)
{
        const auto it = find_param_node (params, argname);
        if (it == params.end() || is_nil(*it))
                return {};
        else if (it->type != Type::List)
                throw Error(Error::Code::InvalidArgument, "expected <list> but got %s",
                            to_string(it->type).c_str());

        std::vector<std::string> vec;
        for (const auto& n: it->children) {
                if (n.type != Type::String)
                        throw Error(Error::Code::InvalidArgument,
                                    "expected string element but got %s",
                                    to_string(n.type).c_str());
                vec.emplace_back (n.value);
        }

        return vec;
}
