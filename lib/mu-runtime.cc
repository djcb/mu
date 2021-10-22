/*
** Copyright (C) 2019-2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-runtime.hh"
#include "utils/mu-util.h"
#include "utils/mu-logger.hh"

#include <locale.h> /* for setlocale() */

#include <string>
#include <unordered_map>
static std::unordered_map<MuRuntimePath, std::string> RuntimePaths;

constexpr auto PartsDir  = "parts";
constexpr auto LogDir    = "log";
constexpr auto XapianDir = "xapian";
constexpr auto MuName    = "mu";
constexpr auto Bookmarks = "bookmarks";

static const std::string Sepa{G_DIR_SEPARATOR_S};

static void
init_paths_xdg()
{
	RuntimePaths.emplace(MU_RUNTIME_PATH_XAPIANDB,
	                     g_get_user_cache_dir() + Sepa + MuName + Sepa + XapianDir);
	RuntimePaths.emplace(MU_RUNTIME_PATH_CACHE, g_get_user_cache_dir() + Sepa + MuName);
	RuntimePaths.emplace(MU_RUNTIME_PATH_MIMECACHE,
	                     g_get_user_cache_dir() + Sepa + MuName + Sepa + PartsDir);
	RuntimePaths.emplace(MU_RUNTIME_PATH_LOGDIR, g_get_user_cache_dir() + Sepa + MuName);
	RuntimePaths.emplace(MU_RUNTIME_PATH_BOOKMARKS, g_get_user_config_dir() + Sepa + MuName);
}

static void
init_paths_muhome(const char* muhome)
{
	RuntimePaths.emplace(MU_RUNTIME_PATH_XAPIANDB, muhome + Sepa + XapianDir);
	RuntimePaths.emplace(MU_RUNTIME_PATH_CACHE, muhome);
	RuntimePaths.emplace(MU_RUNTIME_PATH_MIMECACHE, muhome + Sepa + PartsDir);
	RuntimePaths.emplace(MU_RUNTIME_PATH_LOGDIR, muhome + Sepa + LogDir);
	RuntimePaths.emplace(MU_RUNTIME_PATH_BOOKMARKS, muhome + Sepa + Bookmarks);
}

gboolean
mu_runtime_init(const char* muhome, const char* name, gboolean debug)
{
	g_return_val_if_fail(RuntimePaths.empty(), FALSE);
	g_return_val_if_fail(name, FALSE);

	setlocale(LC_ALL, "");

	if (muhome)
		init_paths_muhome(muhome);
	else
		init_paths_xdg();

	for (const auto& d : RuntimePaths) {
		char* dir;
		if (d.first == MU_RUNTIME_PATH_BOOKMARKS) // special case
			dir = g_path_get_dirname(d.second.c_str());
		else
			dir = g_strdup(d.second.c_str());

		auto ok = mu_util_create_dir_maybe(dir, 0700, TRUE);
		if (!ok) {
			g_critical("failed to create %s", dir);
			g_free(dir);
			mu_runtime_uninit();
			return FALSE;
		}
		g_free(dir);
	}

	const auto log_path = RuntimePaths[MU_RUNTIME_PATH_LOGDIR] + Sepa + name + ".log";

	using namespace Mu;
	LogOptions opts{LogOptions::None};
	if (debug)
		opts |= (LogOptions::Debug | LogOptions::None);

	Mu::log_init(log_path, opts);

	return TRUE;
}

void
mu_runtime_uninit(void)
{
	RuntimePaths.clear();
	Mu::log_uninit();
}

const char*
mu_runtime_path(MuRuntimePath path)
{
	const auto it = RuntimePaths.find(path);
	if (it == RuntimePaths.end())
		return NULL;
	else
		return it->second.c_str();
}
