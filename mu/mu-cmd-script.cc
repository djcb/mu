/*
** Copyright (C) 2012-2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "config.h"

#include "mu-cmd.hh"
#include "mu-script.hh"
#include "utils/mu-utils.hh"

using namespace Mu;

Result<void>
Mu::mu_cmd_script(const Options& opts)
{
	ScriptPaths paths = { MU_SCRIPTS_DIR };
	const auto&& scriptinfos{script_infos(paths)};
	auto script_it = Mu::seq_find_if(scriptinfos, [&](auto&& item) {
		return item.name == opts.script.name;
	});

	if (script_it == scriptinfos.cend())
		return Err(Error::Code::InvalidArgument,
			   "cannot find script '{}'", opts.script.name);

	std::vector<std::string> params{opts.script.params};
	if (!opts.muhome.empty()) {
		params.emplace_back("--muhome");
		params.emplace_back(opts.muhome);
	}

	// won't return unless there's an error.
	return run_script(script_it->path, opts.script.params);
}
