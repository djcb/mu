/*
** Copyright (C) 2008-2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "utils/mu-test-utils.hh"
#include "utils/mu-error.hh"


using namespace Mu;

char*
Mu::test_mu_common_get_random_tmpdir()
{
	char* dir;
	int   res;

	dir = g_strdup_printf("%s%cmu-test-%d%ctest-%x",
			      g_get_tmp_dir(),
			      G_DIR_SEPARATOR,
			      getuid(),
			      G_DIR_SEPARATOR,
			      (int)random() * getpid() * (int)time(NULL));

	res = g_mkdir_with_parents(dir, 0700);
	g_assert(res != -1);

	return dir;
}

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
	setlocale(LC_ALL, "en_US.UTF-8");

	if (strcmp(nl_langinfo(CODESET), "UTF-8") != 0) {
		g_print("Note: Unit tests require the en_US.utf8 locale. "
			"Ignoring test cases.\n");
		return FALSE;
	}

	return TRUE;
}

static void
black_hole(void)
{
	return; /* do nothing */
}

void
Mu::mu_test_init(int *argc, char ***argv)
{
	const auto tmpdir{test_random_tmpdir()};

	g_setenv("MU_TEST", "yes", TRUE);
	g_setenv("XDG_CACHE_HOME", tmpdir.c_str(), TRUE);

	g_test_init(argc, argv, NULL);

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



Mu::TempDir::TempDir(bool autodelete): autodelete_{autodelete}
{
	GError *err{};
	gchar *tmpdir = g_dir_make_tmp("mu-tmp-XXXXXX", &err);
	if (!tmpdir)
		throw Mu::Error(Error::Code::File, &err,
				"failed to create temporary directory");

	path_ = tmpdir;
	g_free(tmpdir);

	g_debug("created '%s'", path_.c_str());
}

Mu::TempDir::~TempDir()
{
	if (::access(path_.c_str(), F_OK) != 0)
		return; /* nothing to do */

	if (!autodelete_) {
		g_debug("_not_ deleting %s", path_.c_str());
		return;
	}

	/* ugly */
	GError *err{};
	const auto cmd{format("/bin/rm -rf '%s'", path_.c_str())};
	if (!g_spawn_command_line_sync(cmd.c_str(), NULL, NULL, NULL, &err)) {
		g_warning("error: %s\n", err ? err->message : "?");
		g_clear_error(&err);
	} else
		g_debug("removed '%s'", path_.c_str());
}
