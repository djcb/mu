/* 
** Copyright (C) 2008-2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include "src/mu-msg-str.h"

static void
test_mu_msg_str_date_01 (void)
{
	struct tm *tmbuf;
	char buf[64];
	gchar *tmp;
	time_t some_time;

	some_time = 1234567890;
	tmbuf = localtime (&some_time);
	strftime (buf, 64, "%x", tmbuf);

	/*  $ date -ud@1234567890; Fri Feb 13 23:31:30 UTC 2009 */
	g_assert_cmpstr (mu_msg_str_date_s ("%x", some_time), ==, buf);

	/* date -ud@987654321 Thu Apr 19 04:25:21 UTC 2001 */
	some_time = 987654321;
	tmbuf = localtime (&some_time);
	strftime (buf, 64, "%c", tmbuf);
	tmp = mu_msg_str_date ("%c", some_time);
	
	g_assert_cmpstr (tmp, ==, buf);
	g_free (tmp);
		
}


static void
test_mu_msg_str_size_01 (void)
{
	struct lconv *lc;
	char *tmp2;
	
	lc = localeconv();

	tmp2 = g_strdup_printf ("0%s0 kB", lc->decimal_point);
	g_assert_cmpstr (mu_msg_str_size_s (0),           ==,  tmp2);
	g_free (tmp2);

	tmp2 = g_strdup_printf ("100%s0 kB", lc->decimal_point);
	g_assert_cmpstr (mu_msg_str_size_s (100000),      ==,  tmp2);
	g_free (tmp2);

	tmp2 = g_strdup_printf ("1%s1 MB", lc->decimal_point);
	g_assert_cmpstr (mu_msg_str_size_s (1100*1000), ==,  tmp2);
	g_free (tmp2);
}

	

static void
test_mu_msg_str_size_02 (void)
{
	struct lconv *lc;
	char *tmp1, *tmp2;
	
	lc = localeconv();
	
	tmp2 = g_strdup_printf ("1%s0 MB", lc->decimal_point);
	tmp1 = mu_msg_str_size (999999); 
	g_assert_cmpstr (tmp1, !=, tmp2);

	g_free (tmp1);
	g_free (tmp2);
}



static void
test_mu_msg_str_prio_01 (void)
{
	g_assert_cmpstr (mu_msg_str_prio(MU_MSG_PRIO_LOW), ==, "low");
	g_assert_cmpstr (mu_msg_str_prio(MU_MSG_PRIO_NORMAL), ==, "normal");
	g_assert_cmpstr (mu_msg_str_prio(MU_MSG_PRIO_HIGH), ==, "high");
}


static gboolean
ignore_error (const char* log_domain, GLogLevelFlags log_level, const gchar* msg,
	      gpointer user_data)
{
	return FALSE; /* don't abort */
}


static void
test_mu_msg_str_prio_02 (void)
{
	/* this must fail */
	g_test_log_set_fatal_handler ((GTestLogFatalFunc)ignore_error, NULL);

	g_assert_cmpstr (mu_msg_str_prio(666), ==, NULL);
}

int
main (int argc, char *argv[])
{
	g_test_init (&argc, &argv, NULL);

	/* mu_msg_str_date */
	g_test_add_func ("/mu-msg-str/mu-msg-str-date",
			 test_mu_msg_str_date_01);

	/* mu_msg_str_size */
	g_test_add_func ("/mu-msg-str/mu-msg-str-size-01",
			 test_mu_msg_str_size_01);
	g_test_add_func ("/mu-msg-str/mu-msg-str-size-02",
			 test_mu_msg_str_size_02);

	/* mu_msg_str_prio */
	g_test_add_func ("/mu-msg-str/mu-msg-str-prio-01",
			 test_mu_msg_str_prio_01);
	g_test_add_func ("/mu-msg-str/mu-msg-str-prio-02",
			 test_mu_msg_str_prio_02);

	/* FIXME: add tests for mu_msg_str_flags; but note the
	 * function simply calls mu_msg_field_str */
		
	g_log_set_handler (NULL,
			   G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL| G_LOG_FLAG_RECURSION,
			   (GLogFunc)black_hole, NULL);
	
	return g_test_run ();
}
