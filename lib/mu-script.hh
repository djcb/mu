/*
** Copyright (C) 2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#ifndef MU_SCRIPT_HH__
#define MU_SCRIPT_HH__

#include <string>
#include <vector>

#include <utils/mu-result.hh>

namespace Mu {

/**
 * Information about a script.
 * 
 */
struct ScriptInfo {
	std::string name;	 /**< Name of script */
	std::string path;	 /**< Full path to script */
	std::string oneline;     /**< One-line description */
	std::string description; /**< More help */
};

/// Sequence of script infos.
using ScriptInfos = std::vector<ScriptInfo>;

/** 
 * Get information about the available scripts
 * 
 * @return infos
 */
using ScriptPaths = std::vector<std::string>;
ScriptInfos script_infos(const ScriptPaths& paths);


/** 
 * Run some specific script
 * 
 * @param path full path to the scripts
 * @param args argument vector to pass to the script
 * 
 * @return Ok() or some error; however, note that this does not return after succesfully
 * starting a script.
 */
Result<void> run_script(const std::string& path, const std::vector<std::string>& args);

} // namepace Mu

#endif /* MU_SCRIPT_HH__ */
