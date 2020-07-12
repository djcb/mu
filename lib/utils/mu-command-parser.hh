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
#ifndef MU_COMMAND_PARSER_HH__
#define MU_COMMAND_PARSER_HH__

#include <vector>
#include <string>
#include <ostream>
#include <stdexcept>
#include <unordered_map>
#include <functional>
#include <algorithm>

#include "utils/mu-error.hh"
#include "utils/mu-sexp.hh"


namespace Mu {
namespace Command {

///
/// Commands are s-expressions with the follow properties:

/// 1) a command is a list with a command-name as its first argument
/// 2) the rest of the parameters are pairs of colon-prefixed symbol and a value of some
///    type (ie. 'keyword arguments')
/// 3) each command is described by its CommandInfo structure, which defines the type
/// 4) calls to the command must include all required parameters
/// 5) all parameters must be of the specified type; however the symbol 'nil' is allowed
///    for specify a non-required parameter to be absent; this is for convenience on the
///    call side.


/// Information about a function argument
struct ArgInfo {
        ArgInfo (Sexp::Type typearg, bool requiredarg, std::string&& docarg):
                type{typearg}, required{requiredarg},docstring{std::move(docarg)}
                {}
        const Sexp::Type type; /**< Sexp::Type of the argument */
        const bool             required; /**< Is this argument required? */
        const std::string      docstring; /**< Documentation */
};

/// The arguments for a function, which maps their names to the information.
using ArgMap     = std::unordered_map<std::string, ArgInfo>;
// The parameters to a Handler.
using Parameters = Sexp::Seq;

int                get_int_or    (const Parameters& parms, const std::string& argname, int alt=0);
bool               get_bool_or   (const Parameters& parms, const std::string& argname, bool alt=false);
const std::string& get_string_or (const Parameters& parms, const std::string& argname, const std::string& alt="");
const std::string& get_symbol_or (const Parameters& parms, const std::string& argname, const std::string& alt="nil");


std::vector<std::string> get_string_vec  (const Parameters& params, const std::string& argname);


// A handler function
using Handler    = std::function<void(const Parameters&)>;

/// Information about some command
struct CommandInfo {
        CommandInfo(ArgMap&& argmaparg, std::string&& docarg, Handler&& handlerarg):
                args{std::move(argmaparg)}, docstring{std::move(docarg)}, handler{std::move(handlerarg)}
                {}
        const ArgMap      args;
        const std::string docstring;
        const Handler     handler;

        /**
         * Get a sorted list of argument names, for display. Required args come
         * first, then alphabetical.
         *
         * @return vec with the sorted names.
         */
        std::vector<std::string> sorted_argnames() const { // sort args -- by required, then alphabetical.
                std::vector<std::string> names;
                for (auto&& arg: args)
                        names.emplace_back(arg.first);
                std::sort(names.begin(), names.end(), [&](const auto& name1, const auto& name2) {
                        const auto& arg1{args.find(name1)->second};
                        const auto& arg2{args.find(name2)->second};
                        if (arg1.required != arg2.required)
                                return arg1.required;
                        else
                                return name1 < name2;
                });
                return names;
        }
};
/// All commands, mapping their name to information about them.
using CommandMap = std::unordered_map<std::string, CommandInfo>;

/**
 * Validate that the call (a Sexp) specifies a valid call, then invoke it.
 *
 * A call uses keyword arguments, e.g. something like:
 *    (foo :bar 1 :cuux "fnorb")
 *
 * On error, throw Error.
 *
 * @param cmap map of commands
 * @param call node describing a call.
 */
void invoke(const Command::CommandMap& cmap, const Sexp& call);


static inline std::ostream&
operator<<(std::ostream& os, const Command::ArgInfo& info)
{
        os << info.type
           << " (" << ( info.required ? "required" : "optional" ) << ")";

        return os;
}

static inline std::ostream&
operator<<(std::ostream& os, const Command::CommandInfo& info)
{
        for (auto&& arg: info.args)
                os << "  " << arg.first << " " << arg.second << '\n'
                   << "    " << arg.second.docstring << "\n";

        return os;
}

static inline std::ostream&
operator<<(std::ostream& os, const Command::CommandMap& map)
{
        for (auto&& c: map)
                os << c.first << '\n' << c.second;

        return os;
}

} // namespace Command
} // namespace Mu


#endif /* MU_COMMAND_PARSER_HH__ */
