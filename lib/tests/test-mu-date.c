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
test_mu_date_str (void)
{
	struct tm *tmbuf;
	char buf[64];
	gchar *tmp;
	time_t some_time;

	some_time = 1234567890;
	tmbuf = localtime (&some_time);
	strftime (buf, 64, "%x", tmbuf);

	/*  $ date -ud@1234567890; Fri Feb 13 23:31:30 UTC 2009 */
	g_assert_cmpstr (mu_date_str_s ("%x", some_time), ==, buf);

	/* date -ud@987654321 Thu Apr 19 04:25:21 UTC 2001 */
	some_time = 987654321;
	tmbuf = localtime (&some_time);
	strftime (buf, 64, "%c", tmbuf);
	tmp = mu_date_str ("%c", some_time);

	g_assert_cmpstr (tmp, ==, buf);
	g_free (tmp);
}



static void
test_mu_date_parse_hdwmy (void)
{
	time_t diff;

	diff = time(NULL) - mu_date_parse_hdwmy ("3h");
	g_assert (diff > 0);
	g_assert_cmpuint (3 * 60 * 60 - diff, <=, 1);

	diff = time(NULL) - mu_date_parse_hdwmy ("5y");
	g_assert (diff > 0);
	g_assert_cmpuint (5 * 365 * 24 * 60 * 60 - diff, <=, 1);

	diff = time(NULL) - mu_date_parse_hdwmy ("3m");
	g_assert (diff > 0);
	g_assert_cmpuint (3 * 30 * 24 * 60 * 60 - diff, <=, 1);

	diff = time(NULL) - mu_date_parse_hdwmy ("21d");
	g_assert (diff > 0);
	g_assert_cmpuint (21 * 24 * 60 * 60 - diff, <=, 1);

	diff = time(NULL) - mu_date_parse_hdwmy ("2w");
	g_assert (diff > 0);
	g_assert_cmpuint (2 * 7 * 24 * 60 * 60 - diff, <=, 1);


	g_assert_cmpint (mu_date_parse_hdwmy("-1y"),==, (time_t)-1);
}


static void
test_mu_date_complete_begin (void)
{
	g_assert_cmpstr (mu_date_complete_s("2000", TRUE), ==,
			 "20000101000000");
	g_assert_cmpstr (mu_date_complete_s("2", TRUE), ==,
			 "20000101000000");
	g_assert_cmpstr (mu_date_complete_s ("", TRUE), ==,
			 "00000101000000");
	g_assert_cmpstr (mu_date_complete_s ("201007", TRUE), ==,
			 "20100701000000");
	g_assert_cmpstr (mu_date_complete_s ("19721214", TRUE), ==,
			 "19721214000000");
	g_assert_cmpstr (mu_date_complete_s ("19721214234512", TRUE), ==,
			 "19721214234512");

	g_assert_cmpstr (mu_date_complete_s ("2010-07", TRUE), ==,
			 "20100701000000");
	g_assert_cmpstr (mu_date_complete_s ("1972/12/14", TRUE), ==,
			 "19721214000000");
	g_assert_cmpstr (mu_date_complete_s ("1972-12-14 23:45:12", TRUE), ==,
			 "19721214234512");

}


static void
test_mu_date_complete_end (void)
{
	g_assert_cmpstr (mu_date_complete_s ("2000", FALSE), ==,
			 "20001231235959");
	g_assert_cmpstr (mu_date_complete_s ("2", FALSE), ==,
			 "29991231235959");
	g_assert_cmpstr (mu_date_complete_s ("", FALSE), ==,
			 "99991231235959");
	g_assert_cmpstr (mu_date_complete_s ("201007", FALSE), ==,
			 "20100731235959");
	g_assert_cmpstr (mu_date_complete_s ("19721214", FALSE), ==,
			 "19721214235959");
	g_assert_cmpstr (mu_date_complete_s ("19721214234512", FALSE), ==,
			 "19721214234512");

	g_assert_cmpstr (mu_date_complete_s ("2010-07", FALSE), ==,
			 "20100731235959");
	g_assert_cmpstr (mu_date_complete_s ("1972.12.14", FALSE), ==,
			 "19721214235959");
	g_assert_cmpstr (mu_date_complete_s ("1972.12.14 23:45/12", FALSE), ==,
			 "19721214234512");
}




static void
test_mu_date_interpret_begin (void)
{
	time_t now;
	now = time (NULL);

	g_assert_cmpstr (mu_date_interpret_s ("now", TRUE) , ==,
			 mu_date_str_s("%Y%m%d%H%M%S", now));

	g_assert_cmpstr (mu_date_interpret_s ("today", TRUE) , ==,
			 mu_date_str_s("%Y%m%d000000", now));
}

static void
test_mu_date_interpret_end (void)
{
	time_t now;
	now = time (NULL);

	g_assert_cmpstr (mu_date_interpret_s ("now", FALSE) , ==,
			 mu_date_str_s("%Y%m%d%H%M%S", now));

	g_assert_cmpstr (mu_date_interpret_s ("today", FALSE) , ==,
			 mu_date_str_s("%Y%m%d235959", now));
}





int
main (int argc, char *argv[])
{
	g_test_init (&argc, &argv, NULL);

	/* mu_str_date */
	g_test_add_func ("/mu-str/mu-date-str",
			 test_mu_date_str);

	g_test_add_func ("/mu-str/mu_date_parse_hdwmy",
			 test_mu_date_parse_hdwmy);
	g_test_add_func ("/mu-str/mu_date_complete_begin",
			 test_mu_date_complete_begin);
	g_test_add_func ("/mu-str/mu_date_complete_end",
			 test_mu_date_complete_end);

	g_test_add_func ("/mu-str/mu_date_interpret_begin",
			 test_mu_date_interpret_begin);
	g_test_add_func ("/mu-str/mu_date_interpret_end",
			 test_mu_date_interpret_end);


	g_log_set_handler (NULL,
			   G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL| G_LOG_FLAG_RECURSION,
			   (GLogFunc)black_hole, NULL);

	return g_test_run ();
}
