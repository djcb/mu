/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

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
#include "mu-date.h"

static void
test_mu_date_interpret_begin(void)
{
	time_t now;
	now = time(NULL);

	g_assert_cmpstr(mu_date_interpret_s("now", TRUE), ==, mu_date_str_s("%Y%m%d%H%M%S", now));

	g_assert_cmpstr(mu_date_interpret_s("today", TRUE), ==, mu_date_str_s("%Y%m%d000000", now));
}

static void
test_mu_date_interpret_end(void)
{
	time_t now;
	now = time(NULL);

	g_assert_cmpstr(mu_date_interpret_s("now", FALSE), ==, mu_date_str_s("%Y%m%d%H%M%S", now));

	g_assert_cmpstr(mu_date_interpret_s("today", FALSE),
	                ==,
	                mu_date_str_s("%Y%m%d235959", now));
}

int
main(int argc, char* argv[])
{
	g_test_init(&argc, &argv, NULL);

	g_test_add_func("/mu-str/mu_date_parse_hdwmy", test_mu_date_parse_hdwmy);
	g_test_add_func("/mu-str/mu_date_complete_begin", test_mu_date_complete_begin);
	g_test_add_func("/mu-str/mu_date_complete_end", test_mu_date_complete_end);

	g_test_add_func("/mu-str/mu_date_interpret_begin", test_mu_date_interpret_begin);
	g_test_add_func("/mu-str/mu_date_interpret_end", test_mu_date_interpret_end);

	g_log_set_handler(NULL,
	                  G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL | G_LOG_FLAG_RECURSION,
	                  (GLogFunc)black_hole,
	                  NULL);

	return g_test_run();
}
