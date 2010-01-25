/* 
** Copyright (C) 2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include <glib.h>
#include <stdlib.h>

#include "src/mu-util.h"

static void
test_mu_util_dir_expand (void)
{
	gchar *got, *expected;
	
	got = mu_util_dir_expand ("~/Desktop");
	expected = g_strdup_printf ("%s%cDesktop",
				    getenv("HOME"), G_DIR_SEPARATOR);

	g_assert_cmpstr (got,==,expected);
	g_free (got);
	g_free (expected);
}


static void
shutup (void) {}

int
main (int argc, char *argv[])
{
	g_test_init (&argc, &argv, NULL);

	g_test_add_func ("/mu-util/mu-util-dir-expand-01",
			 test_mu_util_dir_expand);

	g_log_set_handler (NULL,
			   G_LOG_LEVEL_DEBUG|G_LOG_LEVEL_MESSAGE|G_LOG_LEVEL_INFO,
			   (GLogFunc)shutup, NULL);

	return g_test_run ();
}
