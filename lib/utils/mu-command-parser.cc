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
#include "mu-error.hh"
#include "mu-utils.hh"

#include <iostream>
#include <algorithm>

using namespace Mu;
using namespace Command;

void
Command::invoke(const Command::CommandMap& cmap, const Sexp& call)
{
        if (!call.is_call()) {
                throw Mu::Error{Error::Code::Command,
                                "expected call-sexpr but got %s",
                                call.to_string().c_str()};
        }

        const auto& params{call.list()};
        const auto cmd_it = cmap.find(params.at(0).value());
        if (cmd_it == cmap.end())
                throw Mu::Error{Error::Code::Command,
                                "unknown command in call %s",
                                call.to_string().c_str()};

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
                                if (params.at(i).is_symbol() && params.at(i).value() == argname)
                                        return params.begin() + i + 1;

                        return params.end();

                }();

                // it's an error when a required parameter is missing.
                if (param_it == params.end()) {
                        if (arginfo.required)
                                throw Mu::Error{Error::Code::Command,
                                                "missing required parameter %s in call %s",
                                                argname.c_str(), call.to_string().c_str()};
                        continue; // not required
                }

                // the types must match, but the 'nil' symbol is acceptable as
                // "no value"
                if (param_it->type() != arginfo.type && !(param_it->is_nil()))
                        throw Mu::Error{Error::Code::Command,
                                        "parameter %s expects type %s, but got %s in call %s",
                                        argname.c_str(), to_string(arginfo.type).c_str(),
                                        to_string(param_it->type()).c_str(),
                                        call.to_string().c_str()};
        }

        // all passed parameters must be known
        for (size_t i = 1; i < params.size(); i += 2) {
                if (std::none_of(cinfo.args.begin(), cinfo.args.end(),
                                 [&](auto&& arg) {return params.at(i).value() == arg.first;}))
                        throw Mu::Error{Error::Code::Command,
                                        "unknown parameter %s in call %s",
                                        params.at(i).value().c_str(), call.to_string().c_str()};
        }

        if (cinfo.handler)
                cinfo.handler(params);
}

static auto
find_param_node (const Parameters& params, const std::string& argname)
{
        if (params.empty())
                throw Error(Error::Code::InvalidArgument,
                            "params must not be empty");

        if (argname.empty() || argname.at(0) != ':')
                throw Error(Error::Code::InvalidArgument,
                            "property key must start with ':' but got '%s')",
                            argname.c_str());

        for (size_t i = 1; i < params.size(); i += 2) {
                if (i + 1 != params.size() &&
                    params.at(i).is_symbol() && params.at(i).value() == argname)
                        return params.begin() + i + 1;
        }

        return params.end();
}


static Error
wrong_type (Sexp::Type expected, Sexp::Type got)
{
        return Error(Error::Code::InvalidArgument,
                     "expected <%s> but got <%s>",
                     to_string(expected).c_str(),
                     to_string(got).c_str());
}


const std::string&
Command::get_string_or (const Parameters& params, const std::string& argname,
                        const std::string& alt)
{
        const auto it = find_param_node (params, argname);
        if (it == params.end() || it->is_nil())
                return alt;
        else if (!it->is_string())
                throw wrong_type(Sexp::Type::String, it->type());
        else
                return it->value();
}

const std::string&
Command::get_symbol_or (const Parameters& params, const std::string& argname,
                        const std::string& alt)
{
        const auto it = find_param_node (params, argname);
        if (it == params.end() || it->is_nil())
                return alt;
        else if (!it->is_symbol())
                throw wrong_type(Sexp::Type::Symbol, it->type());
        else
                return it->value();
}


int
Command::get_int_or (const Parameters& params, const std::string& argname,
                     int alt)
{
        const auto it = find_param_node (params, argname);
        if (it == params.end() || it->is_nil())
                return alt;
        else if (!it->is_number())
                throw wrong_type(Sexp::Type::Number, it->type());
        else
                return ::atoi(it->value().c_str());
}

bool
Command::get_bool_or (const Parameters& params, const std::string& argname,
                      bool alt)
{
        const auto it = find_param_node (params, argname);
        if (it == params.end())
                return alt;
        else if (!it->is_symbol())
                throw wrong_type(Sexp::Type::Symbol, it->type());
        else
                return it->is_nil() ? false : true;
}

std::vector<std::string>
Command::get_string_vec  (const Parameters& params, const std::string& argname)
{
        const auto it = find_param_node (params, argname);
        if (it == params.end() || it->is_nil())
                return {};
        else if (!it->is_list())
                throw wrong_type(Sexp::Type::List, it->type());

        std::vector<std::string> vec;
        for (const auto& n: it->list()) {
                if (!n.is_string())
                        throw wrong_type(Sexp::Type::String, n.type());
                vec.emplace_back (n.value());
        }

        return vec;
}
