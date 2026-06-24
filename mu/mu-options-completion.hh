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

#ifndef MU_OPTIONS_COMPLETION_HH__
#define MU_OPTIONS_COMPLETION_HH__

#include <string>
#include <vector>

namespace CLI {
class App;
class Option;
}

/*
 * Shell-completion generation ("mu completions <shell>"); walk the
 * fully-populated CLI::App tree and generate a completion script for the
 * given shell. This covers all non-hidden subcommands, including the nested
 * ones (labels); Guile scripts are excluded.
 */
namespace Mu::Completion {

/**
 * Register the allowed values for a choice-option (see add_choice_option),
 * so completion scripts can offer them.
 *
 * @param opt the option
 * @param values the allowed values, sorted; "name|shortcut" values are
 * split into their parts
 */
void register_choices(const CLI::Option *opt, std::vector<std::string> values);

/**
 * Clear all registered choice-options; needed when re-creating the CLI::App
 * (the registered options point into the old one).
 */
void clear_choices();

/**
 * Generate a shell-completion script for @p shell by walking @p app.
 *
 * @param app the fully-populated CLI app
 * @param shell "bash", "fish" or "zsh"
 *
 * @return the completion script
 */
std::string completion_script(const CLI::App& app, const std::string& shell);

} // namespace Mu::Completion

#endif /* MU_OPTIONS_COMPLETION_HH__ */
