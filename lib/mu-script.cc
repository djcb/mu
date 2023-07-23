/*
** Copyright (C) 2022-2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-script.hh"
#include "mu/mu-options.hh"
#include "utils/mu-utils.hh"
#include "utils/mu-option.hh"

#include <fstream>
#include <iostream>

#ifdef BUILD_GUILE
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wredundant-decls"
#include <libguile.h>
#pragma GCC diagnostic pop
#endif /*BUILD_GUILE*/

using namespace Mu;

static std::string
get_name(const std::string& path)
{
	auto pos = path.find_last_of("/");
	if (pos == std::string::npos)
		return path;

	auto name = path.substr(pos + 1);

	pos = name.find_last_of(".");
	if (pos == std::string::npos)
		return name;

	return name.substr(0, pos);
}


static Mu::Option<Mu::ScriptInfo>
get_info(std::string&& path, const std::string& prefix)
{
	std::ifstream file{path};
	if (!file.is_open()) {
		mu_warning ("failed to open {}", path);
		return Nothing;
	}

	Mu::ScriptInfo info{};
	info.path = path;
	info.name = get_name(path);

	std::string line;
	while (std::getline(file, line)) {

		if (line.find(prefix) != 0)
			continue;

		line = line.substr(prefix.length());

		if (info.oneline.empty())
			info.oneline = line;
		else
			info.description += line;
	}

	// std::cerr << "ONELINE: " << info.oneline << '\n';
	// std::cerr << "DESCR  : " << info.description << '\n';

	return info;
}



static void
script_infos_in_dir(const std::string& scriptdir, Mu::ScriptInfos& infos)
{
	DIR *dir = opendir(scriptdir.c_str());
	if (!dir) {
		mu_debug("failed to open '{}': {}", scriptdir,
			g_strerror(errno));
		return;
	}

	const std::string ext{".scm"};

	struct dirent *dentry;
	while ((dentry = readdir(dir))) {

		if (!g_str_has_suffix(dentry->d_name, ext.c_str()))
			continue;

		auto&& info = get_info(scriptdir + "/" + dentry->d_name, ";; INFO: ");
		if (!info)
			continue;

		infos.emplace_back(std::move(*info));
	}

	closedir(dir); /* ignore error checking... */
}


Mu::ScriptInfos
Mu::script_infos(const Mu::ScriptPaths& paths)
{
	/* create a list of names, paths */
	ScriptInfos infos;
	for (auto&& dir: paths) {
		script_infos_in_dir(dir, infos);
	}

	std::sort(infos.begin(), infos.end(), [](auto&& i1, auto&& i2) {
		return i1.name < i2.name;
	});

	return infos;
}

Result<void>
Mu::run_script(const std::string& path,
	       const std::vector<std::string>& args)
{
#ifndef BUILD_GUILE
	return Err(Error::Code::Script,
		   "guile script support is not available");
#else
	std::string mainargs;
	for (auto&& arg: args)
		mainargs += mu_format("{}\"{}\"", mainargs.empty() ? "" : " ", arg);
	auto expr = mu_format("(main '(\"{}\" {}))", get_name(path), mainargs);

	std::vector<const char*> argv = {
		GUILE_BINARY,
		"-l", path.c_str(),
		"-c", expr.c_str(),
	};

	/* does not return */
	scm_boot_guile(argv.size(), const_cast<char**>(argv.data()),
		       [](void *closure, int argc, char **argv) {
			       scm_shell(argc, argv);
		       }, NULL);

	return Ok();
#endif /*BUILD_GUILE*/
}
