/*
** Copyright (C) 2008-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#if HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include <glib.h>
#include <glib/gstdio.h>

#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include <langinfo.h>
#include <locale.h>

#include "test-mu-common.h"

char*
test_mu_common_get_random_tmpdir (void)
{
	char*dir;
	int  res;

	dir = g_strdup_printf (
		"%s%cmu-test-%d%ctest-%x",
		g_get_tmp_dir(),
		G_DIR_SEPARATOR,
		getuid(),
		G_DIR_SEPARATOR,
		(int)random()*getpid()*(int)time(NULL));

	res = g_mkdir_with_parents (dir, 0700);
	g_assert (res != -1);

	return dir;
}


const char*
set_tz (const char* tz)
{
	static const char* oldtz;

	oldtz = getenv ("TZ");
	if (tz)
		setenv ("TZ", tz, 1);
	else
		unsetenv ("TZ");

	tzset ();
	return oldtz;
}


gboolean
set_en_us_utf8_locale (void)
{
	setenv ("LC_ALL", "en_US.utf8", 1);
	setlocale (LC_ALL, "en_US.utf8");

	if (strcmp (nl_langinfo(CODESET), "UTF-8") != 0) {
		g_print ("Note: Unit tests require the en_US.utf8 locale. "
			 "Ignoring test cases.\n");
		return FALSE;
	}

	return TRUE;
}


void
black_hole (void)
{
	return; /* do nothing */
}
