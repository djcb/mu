/*
** Copyright (C) 2008-2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <glib.h>
#include <glib/gstdio.h>

#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include <langinfo.h>
#include <locale.h>

#include "utils/mu-utils.hh"
#include "utils/mu-test-utils.hh"
#include "utils/mu-utils-file.hh"
#include "utils/mu-error.hh"

using namespace Mu;

/* LCOV_EXCL_START*/
bool
Mu::mu_test_mu_hacker()
{
	return !!g_getenv("MU_HACKER");
}
/* LCOV_EXCL_STOP*/


const char*
Mu::set_tz(const char* tz)
{
	static const char* oldtz;

	oldtz = getenv("TZ");
	if (tz)
		setenv("TZ", tz, 1);
	else
		unsetenv("TZ");

	tzset();
	return oldtz;
}

bool
Mu::set_en_us_utf8_locale()
{
	setenv("LC_ALL", "en_US.UTF-8", 1);

	if (auto str = setlocale(LC_ALL, "en_US.UTF-8"); !str)
		return false;

	if (strcmp(nl_langinfo(CODESET), "UTF-8") != 0)
		return false;

	return true;
}

static void
black_hole(void)
{
	return; /* do nothing */
}

void
Mu::mu_test_init(int *argc, char ***argv)
{
	TempDir temp_dir;

	g_unsetenv("XAPIAN_CJK_NGRAM");
	g_setenv("MU_TEST", "yes", TRUE);
	g_setenv("XDG_CACHE_HOME", temp_dir.path().c_str(), TRUE);

	setlocale(LC_ALL, "");

	g_test_init(argc, argv, NULL);

	g_test_bug_base("https://github.com/djcb/mu/issues/");

	if (!g_test_verbose())
		g_log_set_handler(
			NULL,
			(GLogLevelFlags)(G_LOG_LEVEL_MASK |
					 G_LOG_FLAG_FATAL | G_LOG_FLAG_RECURSION),
			(GLogFunc)black_hole, NULL);
}

void
Mu::allow_warnings()
{
	g_test_log_set_fatal_handler(
	    [](const char*, GLogLevelFlags, const char*, gpointer) { return FALSE; },
	    {});
}

Mu::TempDir::TempDir(bool autodelete): autodelete_{autodelete} {

	if (auto res{make_temp_dir()}; !res)
		throw res.error();
	else
		path_ = std::move(*res);

	mu_debug("created '{}'", path_);
}

Mu::TempDir::~TempDir()
{
	if (::access(path_.c_str(), F_OK) != 0)
		return; /* nothing to do */

	if (!autodelete_) {
		mu_debug("_not_ deleting {}", path_);
		return;
	}

	if (auto&& res{run_command0({RM_PROGRAM, "-fr", path_})}; !res) {
		/* LCOV_EXCL_START*/
		mu_warning("error removing {}: {}", path_, format_as(res.error()));
		/* LCOV_EXCL_STOP*/
	} else
		mu_debug("removed '{}'", path_);
}
