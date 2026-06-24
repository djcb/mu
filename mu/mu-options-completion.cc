/*
** Copyright (C) 2026 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <config.h>

#include <algorithm>
#include <ranges>
#include <string>
#include <vector>

#include <utils/mu-utils.hh>
#include "mu-options-completion.hh"

#include "CLI/CLI.hpp"

using namespace Mu;

// common

namespace {

/**
 * Allowed values for options (add_choice_option)
 */
struct ChoiceInfo {
	const CLI::Option	 *opt;	  /**< the option */
	std::vector<std::string> values;  /**< allowed values, sorted */
};
std::vector<ChoiceInfo> choice_infos;

/** The kind of value an option or positional argument takes */
enum struct ValueKind { Flag, Choice, File, Dir, Other };

// The type-name of opt without suffices; e.g. "<dir>:expand muhome path" -> "<dir>".
std::string
type_name_base(const CLI::Option& opt)
{
	auto tname{opt.get_type_name()};
	if (const auto pos = tname.find(':'); pos != std::string::npos)
		tname.erase(pos);
	return tname;
}

// Determine ValueKind for opt
ValueKind
value_kind(const CLI::Option& opt)
{
	// CLI11 flags are options that expect no value
	if (!opt.get_positional() && opt.get_expected_min() == 0)
		return ValueKind::Flag;

	if (std::ranges::any_of(choice_infos, [&](auto&& ci) {
		return ci.opt == &opt; }))
		return ValueKind::Choice;

	const auto tname{type_name_base(opt)};
	if (tname == "<dir>" || tname == "<maildir>")
		return ValueKind::Dir;
	if (tname == "<file>" || tname == "<path>" || tname == "<message-path>" ||
	    tname == "<files>")
		return ValueKind::File;

	// some file-ish positionals lack a type-name; use their name.
	if (opt.get_positional()) {
		const auto& pname{opt.get_single_name()};
		if (pname == "files" || pname == "input" || pname == "output")
			return ValueKind::File;
	}

	return ValueKind::Other;
}

// Allowed values
std::vector<std::string>
choice_values(const CLI::Option& opt)
{
	std::vector<std::string> vals;
	for (const auto& ci: choice_infos) {
		if (ci.opt != &opt)
			continue;
		for (const auto& val: ci.values)
			for (auto&& part: split(val, '|'))
				vals.emplace_back(std::move(part));
		std::ranges::sort(vals);
	}
	return vals;
}

// All names for opt
std::vector<std::string>
option_names(const CLI::Option& opt)
{
	std::vector<std::string> names;
	for (const auto& sname: opt.get_snames())
		names.emplace_back("-" + sname);
	for (const auto& lname: opt.get_lnames())
		names.emplace_back("--" + lname);
	return names;
}

// Description of opt
std::string
option_blurb(const CLI::Option& opt)
{
	auto desc{opt.get_description()};
	if (const auto pos = desc.find("; one of {"); pos != std::string::npos)
		desc.erase(pos);
	return desc;
}

// Sanitize name - -> _
std::string
func_name(std::string name)
{
	std::ranges::replace(name, '-', '_');
	return name;
}

// flag opts
std::vector<const CLI::Option*>
command_options(const CLI::App& cmd)
{
	return cmd.get_options([](const CLI::Option *opt) {
		return !opt->get_group().empty() && opt->nonpositional();
	});
}

// arg opts
std::vector<const CLI::Option*>
command_positionals(const CLI::App& cmd)
{
	return cmd.get_options([](const CLI::Option *opt) {
		return !opt->get_group().empty() && opt->get_positional();
	});
}

// subcommands
std::vector<const CLI::App*>
command_subs(const CLI::App& cmd)
{
	return cmd.get_subcommands([](const CLI::App *sub) {
		return !sub->get_group().empty() &&
			sub->get_group() != "Scripts"; // skip
	});
}

// names of subs
std::vector<std::string>
sub_names(const std::vector<const CLI::App*>& subs)
{
	auto names{subs | std::views::transform([](auto&& sub) {
		return sub->get_name(); })};
	return {names.begin(), names.end()};
}

//
// zsh
//

// Escape string for zsh
std::string
zsh_escape(const std::string& s)
{
	std::string res;
	res.reserve(s.size());
	for (auto c: s) {
		switch (c) {
		case '\'': res += "'\\''"; break;
		case '[':  res += "\\[";   break;
		case ']':  res += "\\]";   break;
		case ':':  res += "\\:";   break;
		default:   res += c;       break;
		}
	}
	return res;
}

// zsh completion-action for opt
std::string
zsh_action(const CLI::Option& opt)
{
	switch (value_kind(opt)) {
	case ValueKind::Choice:	return "(" + join(choice_values(opt), " ") + ")";
	case ValueKind::Dir:	return "_files -/";
	case ValueKind::File:	return "_files";
	default:		return "";
	}
}

// zsh _arguments spec
std::string
zsh_optspec(const CLI::Option& opt)
{
	const auto names{option_names(opt)};
	const auto takes_value{opt.get_expected_min() > 0};

	// "[description]", plus ":message:action" for value-taking options
	const auto desc{option_blurb(opt)};
	auto rest{desc.empty() ? std::string{} : "[" + zsh_escape(desc) + "]"};
	if (takes_value) {
		auto label{type_name_base(opt)};
		if (label.size() > 2 && label.front() == '<' && label.back() == '>')
			label = label.substr(1, label.size() - 2);
		else
			label = "value";
		rest += ":" + zsh_escape(label) + ":" + zsh_action(opt);
	}

	// repeatable options get a '*' prefix; multi-name options an exclusion
	// list, so e.g. '--format' is not offered after '-o'.
	std::string prefix;
	if (takes_value && opt.get_expected_max() > 1)
		prefix = "'*'";
	else if (names.size() > 1)
		prefix = "'(" + join(names, " ") + ")'";

	// value-taking options get their value attached: '--foo=' / '-f+'
	const auto attach = [&](const std::string& name) {
		if (!takes_value)
			return name;
		return name + (name.starts_with("--") ? "=" : "+");
	};

	if (names.size() == 1)
		return prefix + "'" + attach(names.front()) + rest + "'";

	std::vector<std::string> attached;
	attached.reserve(names.size());
	for (const auto& name: names)
		attached.emplace_back(attach(name));

	return prefix + "{" + join(attached, ",") + "}'" + rest + "'";
}

// zsh _arguments spec for positional argument (1-based)
std::string
zsh_positional_spec(const CLI::Option& opt, size_t index)
{
	const auto rest{zsh_escape(opt.get_single_name()) + ":" +
		zsh_action(opt)};

	if (opt.get_expected_max() > 1)
		return "'*:" + rest + "'";
	else
		return mu_format("'{}{}{}'", index,
				 opt.get_required() ? ":" : "::", rest);
}

// Emit zsh completion functions
void
emit_zsh_cmd(std::string& out, const CLI::App& cmd, const std::string& fname)
{
	const auto subs{command_subs(cmd)};

	for (auto&& sub: subs)
		emit_zsh_cmd(out, *sub, fname + "_" + func_name(sub->get_name()));

	std::vector<std::string> specs;
	for (auto&& opt: command_options(cmd))
		specs.emplace_back(zsh_optspec(*opt));

	if (subs.empty()) {
		size_t index{};
		for (auto&& pos: command_positionals(cmd))
			specs.emplace_back(zsh_positional_spec(*pos, ++index));

		out += fname + "() {\n  _arguments -S \\\n    " +
			join(specs, " \\\n    ") + "\n}\n\n";
		return;
	}

	// with subcommands: a helper listing them, plus a state-machine
	// dispatching to the per-subcommand functions.
	out += fname + "_commands() {\n  local -a commands\n  commands=(\n";
	for (auto&& sub: subs)
		out += "    '" + sub->get_name() + ":" +
			zsh_escape(sub->get_description()) + "'\n";
	out += "  )\n  _describe -t commands 'command' commands\n}\n\n";

	specs.emplace_back("'1: :" + fname + "_commands'");
	specs.emplace_back("'*::arg:->args'");

	auto ctx{func_name(fname).substr(1)}; // "_mu_labels" -> "mu-labels"
	std::ranges::replace(ctx, '_', '-');

	out += fname + "() {\n"
		"  local curcontext=\"$curcontext\" state line ret=1\n"
		"  _arguments -S -C \\\n    " + join(specs, " \\\n    ") +
		" && ret=0\n\n"
		"  case $state in\n"
		"    (args)\n"
		"      curcontext=\"${curcontext%:*:*}:" + ctx + "-${words[1]}:\"\n"
		"      case ${words[1]} in\n";
	for (auto&& sub: subs)
		out += "        (" + sub->get_name() + ") " + fname + "_" +
			func_name(sub->get_name()) + " && ret=0;;\n";
	out += "      esac\n"
		"      ;;\n"
		"  esac\n\n"
		"  return ret\n"
		"}\n\n";
}

// Generate zsh completion script for app */
std::string
zsh_completions(const CLI::App& app)
{
	std::string out;
	out += "#compdef mu\n\n"
		"# Generated by 'mu completions zsh' (mu " PACKAGE_VERSION "); "
		"do not edit.\n"
		"#\n"
		"# Install by placing this file, named '_mu', in a directory\n"
		"# in your $fpath.\n\n";

	emit_zsh_cmd(out, app, "_mu");
	out += "_mu \"$@\"\n";

	return out;
}

//
// Bash
//

// Bash snippet that hands off to a subcommand's function, if a
// subcommand word is found.
std::string
bash_sub_dispatch(const std::vector<const CLI::App*>& subs, const std::string& fname)
{
	if (subs.empty())
		return {};

	std::string res{
		"  local i=$1 sub=\n"
		"  while (( i < COMP_CWORD )); do\n"
		"    case \"${COMP_WORDS[i]}\" in\n"
		"      =|-*) ((i++));;\n"
		"      *) sub=\"${COMP_WORDS[i]}\"; ((i++)); break;;\n"
		"    esac\n"
		"  done\n"
		"  case \"$sub\" in\n"};
	for (auto&& sub: subs)
		res += "    " + sub->get_name() + ") " + fname + "_" +
			func_name(sub->get_name()) + " \"$i\"; return;;\n";
	res += "  esac\n";

	return res;
}

// Bash snippet that completes the values of value-taking options (based on
// the previous word).
std::string
bash_prev_cases(const CLI::App& cmd)
{
	std::string cases;
	for (auto&& opt: command_options(cmd)) {
		const auto kind{value_kind(*opt)};
		if (kind == ValueKind::Flag)
			continue;
		const auto pat{join(option_names(*opt), "|")};
		switch (kind) {
		case ValueKind::Choice:
			cases += "    " + pat + ") COMPREPLY=($(compgen -W \"" +
				join(choice_values(*opt), " ") +
				"\" -- \"$cur\")); return;;\n";
			break;
		case ValueKind::Dir:
			cases += "    " + pat +
				") COMPREPLY=($(compgen -d -- \"$cur\")); return;;\n";
			break;
		case ValueKind::File:
			cases += "    " + pat +
				") COMPREPLY=($(compgen -f -- \"$cur\")); return;;\n";
			break;
		default: // takes a value we cannot complete
			cases += "    " + pat + ") return;;\n";
			break;
		}
	}

	if (cases.empty())
		return {};

	return "  case \"$prev\" in\n" + cases + "  esac\n";
}

// Bash snippet that completes option names, subcommand names or positional
// arguments (based on the current word).
std::string
bash_cur_cases(const CLI::App& cmd, const std::vector<const CLI::App*>& subs)
{
	std::vector<std::string> optnames;
	for (auto&& opt: command_options(cmd))
		for (auto&& name: option_names(*opt))
			optnames.emplace_back(name);

	std::string res{"  case \"$cur\" in\n"
		"    -*) COMPREPLY=($(compgen -W \"" + join(optnames, " ") +
		"\" -- \"$cur\"));;\n"};

	std::string pos_action;
	if (!subs.empty())
		pos_action = "COMPREPLY=($(compgen -W \"" +
			join(sub_names(subs), " ") + "\" -- \"$cur\"))";
	else for (auto&& pos: command_positionals(cmd)) {
		if (const auto kind{value_kind(*pos)}; kind == ValueKind::File) {
			pos_action = "COMPREPLY=($(compgen -f -- \"$cur\"))";
			break;
		} else if (kind == ValueKind::Dir) {
			pos_action = "COMPREPLY=($(compgen -d -- \"$cur\"))";
			break;
		}
	}
	if (!pos_action.empty())
		res += "    *) " + pos_action + ";;\n";

	return res + "  esac\n";
}

// Emit bash completion function(s) for cmd
void
emit_bash_cmd(std::string& out, const CLI::App& cmd, const std::string& fname)
{
	const auto subs{command_subs(cmd)};

	for (auto&& sub: subs)
		emit_bash_cmd(out, *sub, fname + "_" + func_name(sub->get_name()));

	out += fname + "() {\n" +
		bash_sub_dispatch(subs, fname) +
		bash_prev_cases(cmd) +
		bash_cur_cases(cmd, subs) +
		"}\n\n";
}

/** Generate the full bash completion script for app */
std::string
bash_completions(const CLI::App& app)
{
	std::string out;
	out += "# bash completion for mu; generated by 'mu completions bash' "
		"(mu " PACKAGE_VERSION "); do not edit.\n"
		"#\n"
		"# Install by sourcing this file from your .bashrc, or by\n"
		"# placing it, named 'mu', in the bash-completion completions\n"
		"# directory.\n\n";

	emit_bash_cmd(out, app, "_mu_cmd");

	out += "_mu() {\n"
		"  local cur prev\n"
		"  COMPREPLY=()\n"
		"  cur=\"${COMP_WORDS[COMP_CWORD]}\"\n"
		"  prev=\"${COMP_WORDS[COMP_CWORD-1]}\"\n"
		"  # handle '--opt=value' (bash splits on '=')\n"
		"  [[ \"$cur\" == \"=\" ]] && cur=\"\"\n"
		"  [[ \"$prev\" == \"=\" && $COMP_CWORD -ge 2 ]] && "
		"prev=\"${COMP_WORDS[COMP_CWORD-2]}\"\n\n"
		"  _mu_cmd 1\n"
		"}\n\n"
		"complete -F _mu mu\n";

	return out;
}

//
// fish
//

// Escape for fish
std::string
fish_escape(const std::string& s)
{
	std::string res;

	res.reserve(s.size());
	for (auto c: s) {
		if (c == '\'' || c == '\\')
			res += '\\';
		res += c;
	}

	return res;
}

// Fish 'complete' lines for cmd's options; COMPLETE is the common prefix.
std::string
fish_option_lines(const CLI::App& cmd, const std::string& complete)
{
	std::string res;
	for (auto&& opt: command_options(cmd)) {
		auto line{complete};
		for (const auto& sname: opt->get_snames())
			line += " -s " + sname;
		for (const auto& lname: opt->get_lnames())
			line += " -l " + lname;

		switch (value_kind(*opt)) {
		case ValueKind::Flag:
			break;
		case ValueKind::Choice:
			line += " -x -a '" + join(choice_values(*opt), " ") + "'";
			break;
		case ValueKind::Dir:
			line += " -x -a \"(__fish_complete_directories)\"";
			break;
		case ValueKind::File:
			line += " -r"; // requires a value; files allowed
			break;
		default:
			line += " -x"; // requires a value we cannot complete
			break;
		}

		if (const auto desc{option_blurb(*opt)}; !desc.empty())
			line += " -d '" + fish_escape(desc) + "'";

		res += line + "\n";
	}

	return res;
}

// Fish 'complete' line re-enabling file/dir completion for cmd's file-ish
// positionals (suppressed by the global 'complete -c mu -f'), if any.
std::string
fish_positional_lines(const CLI::App& cmd, const std::string& complete)
{
	for (auto&& pos: command_positionals(cmd)) {
		if (const auto kind{value_kind(*pos)}; kind == ValueKind::File)
			return complete + " -F\n";
		else if (kind == ValueKind::Dir)
			return complete + " -f -a "
				"\"(__fish_complete_directories)\"\n";
	}

	return {};
}

// Fish 'complete' lines for cmd's subcommand names; COND is the condition
// for cmd itself.
std::string
fish_subcommand_lines(const std::vector<const CLI::App*>& subs,
		      const std::string& cond)
{
	const auto nosub_cond{cond.empty() ?
		std::string{"__fish_use_subcommand"} :
		cond + "; and not __fish_seen_subcommand_from " +
		join(sub_names(subs), " ")};

	std::string res;
	for (auto&& sub: subs)
		res += "complete -c mu -n \"" + nosub_cond + "\" -f -a " +
			sub->get_name() + " -d '" +
			fish_escape(sub->get_description()) + "'\n";

	return res;
}

// Emit fish 'complete' invocations
void
emit_fish_cmd(std::string& out, const CLI::App& cmd, const std::string& cond)
{
	const auto complete{cond.empty() ?
		std::string{"complete -c mu"} :
		"complete -c mu -n \"" + cond + "\""};

	// this command's options
	out += fish_option_lines(cmd, complete);

	const auto subs{command_subs(cmd)};
	if (subs.empty()) {
		out += fish_positional_lines(cmd, complete);
		return;
	}

	// subcommand names...
	out += fish_subcommand_lines(subs, cond);

	// ... and their own completions
	for (auto&& sub: subs) {
		const auto subcond{(cond.empty() ? "" : cond + "; and ") +
			"__fish_seen_subcommand_from " + sub->get_name()};
		out += "\n# mu " + sub->get_name() + "\n";
		emit_fish_cmd(out, *sub, subcond);
	}
}

// Generate fish completion script for app
std::string
fish_completions(const CLI::App& app)
{
	std::string out;
	out += "# fish completion for mu; generated by 'mu completions fish' "
		"(mu " PACKAGE_VERSION "); do not edit.\n";

	emit_fish_cmd(out, app, "");

	return out;
}

} // anonymous namespace

void
Mu::Completion::register_choices(const CLI::Option *opt,
				 std::vector<std::string> values)
{
	choice_infos.push_back({opt, std::move(values)});
}

void
Mu::Completion::clear_choices()
{
	choice_infos.clear();
}

std::string
Mu::Completion::completion_script(const CLI::App& app, const std::string& shell)
{
	if (shell == "zsh")
		return zsh_completions(app);
	else if (shell == "fish")
		return fish_completions(app);
	else
		return bash_completions(app);
}
