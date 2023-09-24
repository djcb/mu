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
#ifndef MU_COMMAND_HANDLER_HH__
#define MU_COMMAND_HANDLER_HH__

#include <vector>
#include <string>
#include <ostream>
#include <stdexcept>
#include <unordered_map>
#include <functional>
#include <algorithm>

#include "utils/mu-error.hh"
#include "utils/mu-sexp.hh"
#include "utils/mu-option.hh"

namespace Mu {

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

struct Command: public Sexp {

	static Result<Command> make(Sexp&& sexp) try {
		return Ok(Command{std::move(sexp)});
	} catch (const Error& e) {
		return Err(e);
	}

	static Result<Command> make_parse(const std::string& cmdstr) try {
		if (auto&& sexp{Sexp::parse(cmdstr)}; !sexp)
			return Err(sexp.error());
		else
			return Ok(Command(std::move(*sexp)));
	} catch (const Error& e) {
		return Err(e);
	}

	/**
	 * Get name of the command (first element) in a command exp
	 *
	 * @return name
	 */
	const std::string& name() const {
		return cbegin()->symbol().name;
	}

	/**
	 * Find the argument with the given name.
	 *
	 * @param arg name
	 *
	 * @return iterator point at the argument, or cend
	 */
	const_iterator find_arg(const std::string& arg) const {
		return find_prop(arg, cbegin() + 1, cend());
	}

	/**
	 * Get a string argument
	 *
	 * @param name of the argument
	 *
	 * @return ref to string, or Nothing if not found
	 */
	Option<const std::string&> string_arg(const std::string& name) const {
		if (auto&& val{arg_val(name, Sexp::Type::String)}; !val)
			return Nothing;
		else
			return val->string();
	}

	/**
	 * Get a string-vec argument
	 *
	 * @param name of the argument
	 *
	 * @return ref to string-vec, or Nothing if not found or some error.
	 */
	Option<std::vector<std::string>> string_vec_arg(const std::string& name) const;

	/**
	 * Get a symbol argument
	 *
	 * @param name of the argument
	 *
	 * @return ref to symbol name, or Nothing if not found
	 */
	Option<const std::string&> symbol_arg(const std::string& name) const {
		if (auto&& val{arg_val(name, Sexp::Type::Symbol)}; !val)
			return Nothing;
		else
			return val->symbol().name;
	}

	/**
	 * Get a number argument
	 *
	 * @param name of the argument
	 *
	 * @return number or Nothing if not found
	 */
	Option<int> number_arg(const std::string& name) const {
		if (auto&& val{arg_val(name, Sexp::Type::Number)}; !val)
			return Nothing;
		else
			return static_cast<int>(val->number());
	}

	/*
	 * helpers
	 */

	/**
	 * Get a boolean argument
	 *
	 * @param name of the argument
	 *
	 * @return true if there's a non-nil symbol value for the given
	 * name; false otherwise.
	 */
	Option<bool> bool_arg(const std::string& name) const {
		if (auto&& symb{symbol_arg(name)}; !symb)
			return Nothing;
		else
			return symb.value() == "nil" ? false : true;
	}

	/**
	 * Treat any argument as a boolean
	 *
	 * @param name name of the argument
	 *
	 * @return false if the the argument is absent or the symbol false;
	 * otherwise true.
	 */
	bool boolean_arg(const std::string& name) const {
		auto&& it{find_arg(name)};
		return (it == cend() || std::next(it)->nilp()) ? false : true;
	}

private:
	explicit Command(Sexp&& s){
		*this = std::move(static_cast<Command&&>(s));
		if (!listp() || empty() || !cbegin()->symbolp() ||
		    !plistp(cbegin() + 1, cend()))
			throw Error(Error::Code::Command,
				    "expected command, got '{}'", to_string());
	}


	Option<const Sexp&> arg_val(const std::string& name, Sexp::Type type) const {
		if (auto&& it{find_arg(name)}; it == cend()) {
			//std::cerr << "--> %s name found " << name << '\n';
			return Nothing;
		} else if (auto&& val{it + 1}; val->type() != type) {
			//std::cerr << "--> type " << Sexp::type_name(it->type()) << '\n';
			return Nothing;
		} else
			return *val;
	}
};

struct CommandHandler {

	/// Information about a function argument
	struct ArgInfo {
		ArgInfo(Sexp::Type typearg, bool requiredarg, std::string&& docarg)
			: type{typearg}, required{requiredarg}, docstring{std::move(docarg)} {}
		const Sexp::Type  type;      /**< Sexp::Type of the argument */
		const bool        required;  /**< Is this argument required? */
		const std::string docstring; /**< Documentation */
	};

	/// The arguments for a function, which maps their names to the information.
	using ArgMap = std::unordered_map<std::string, ArgInfo>;

	// A handler function
	using Handler = std::function<void(const Command&)>;

	/// Information about some command
	struct CommandInfo {
		CommandInfo(ArgMap&& argmaparg, std::string&& docarg, Handler&& handlerarg)
			: args{std::move(argmaparg)}, docstring{std::move(docarg)},
			  handler{std::move(handlerarg)} {}
		const ArgMap      args;
		const std::string docstring;
		const Handler     handler;

		/**
		 * Get a sorted list of argument names, for display. Required args come
		 * first, then alphabetical.
		 *
		 * @return vec with the sorted names.
		 */ /* LCOV_EXCL_START */
		std::vector<std::string> sorted_argnames() const {
			// sort args -- by required, then alphabetical.
			std::vector<std::string> names;
			for (auto&& arg : args)
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
		/* LCOV_EXCL_STOP */

	};

	/// All commands, mapping their name to information about them.
	using CommandInfoMap = std::unordered_map<std::string, CommandInfo>;

	CommandHandler(const CommandInfoMap& cmap): cmap_{cmap} {}
	CommandHandler(CommandInfoMap&& cmap):      cmap_{std::move(cmap)} {}

	const CommandInfoMap& info_map() const { return cmap_; }

	/**
	 * Invoke some command
	 *
	 * A command uses keyword arguments, e.g. something like: (foo :bar 1
	 *    :cuux "fnorb")
	 *
	 * @param cmd a Sexp describing a command call
	 * @param validate whether to validate before invoking. Useful during
	 * development.
	 *
	 * Return Ok() or some Error
	 */
	Result<void> invoke(const Command& cmd, bool validate=true) const;

private:
	const CommandInfoMap cmap_;
};

/* LCOV_EXCL_START */
static inline std::ostream&
operator<<(std::ostream& os, const CommandHandler::ArgInfo& info)
{
	os << info.type << " (" << (info.required ? "required" : "optional") << ")";

	return os;
}
/* LCOV_EXCL_STOP */

static inline std::ostream&
operator<<(std::ostream& os, const CommandHandler::CommandInfo& info)
{
	for (auto&& arg : info.args)
		os << "  " << arg.first << " " << arg.second << '\n'
		   << "    " << arg.second.docstring << "\n";

	return os;
}

static inline std::ostream&
operator<<(std::ostream& os, const CommandHandler::CommandInfoMap& map)
{
	for (auto&& c : map)
		os << c.first << '\n' << c.second;

	return os;
}

} // namespace Mu

#endif /* MU_COMMAND_HANDLER_HH__ */
