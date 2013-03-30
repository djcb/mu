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
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#include <locale.h>

#include "test-mu-common.h"
#include "src/mu-runtime.h"

static void
test_mu_runtime_init (void)
{
	gchar* tmpdir;

	tmpdir = test_mu_common_get_random_tmpdir();
	g_assert (tmpdir);
	g_assert (mu_runtime_init (tmpdir, "test-mu-runtime") == TRUE);	
	mu_runtime_uninit ();

	g_assert (mu_runtime_init (tmpdir, "test-mu-runtime") == TRUE);	
	mu_runtime_uninit ();

	g_free (tmpdir);	
}


static void
test_mu_runtime_data (void)
{
	gchar *homedir, *xdir, *bmfile;

	homedir = test_mu_common_get_random_tmpdir();
	g_assert (homedir);

	xdir = g_strdup_printf ("%s%c%s", homedir, 
				   G_DIR_SEPARATOR, "xapian" );
	 
	bmfile = g_strdup_printf ("%s%c%s", homedir, 
				   G_DIR_SEPARATOR, "bookmarks");

	g_assert (mu_runtime_init (homedir, "test-mu-runtime") == TRUE);	
	
	g_assert_cmpstr (homedir, ==, mu_runtime_path (MU_RUNTIME_PATH_MUHOME));
	g_assert_cmpstr (xdir, ==, mu_runtime_path (MU_RUNTIME_PATH_XAPIANDB));
	g_assert_cmpstr (bmfile, ==, mu_runtime_path (MU_RUNTIME_PATH_BOOKMARKS));

	mu_runtime_uninit ();

	g_free (homedir);
	g_free (xdir);
	g_free (bmfile);	
}



int
main (int argc, char *argv[])
{
	g_test_init (&argc, &argv, NULL);

	/* mu_runtime_init/uninit */
	g_test_add_func ("/mu-runtime/mu-runtime-init",
			 test_mu_runtime_init);
	g_test_add_func ("/mu-runtime/mu-runtime-data",
			 test_mu_runtime_data);

	g_log_set_handler (NULL,
			   G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL| G_LOG_FLAG_RECURSION,
			   (GLogFunc)black_hole, NULL);
	
	return g_test_run ();
}


