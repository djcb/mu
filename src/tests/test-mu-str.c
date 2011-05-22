/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

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
#include "src/mu-str.h"
#include "src/mu-msg-prio.h"

static void
test_mu_str_date_01 (void)
{
	struct tm *tmbuf;
	char buf[64];
	gchar *tmp;
	time_t some_time;
		
	some_time = 1234567890;
	tmbuf = localtime (&some_time);
	strftime (buf, 64, "%x", tmbuf);

	/*  $ date -ud@1234567890; Fri Feb 13 23:31:30 UTC 2009 */
	g_assert_cmpstr (mu_str_date_s ("%x", some_time), ==, buf);

	/* date -ud@987654321 Thu Apr 19 04:25:21 UTC 2001 */
	some_time = 987654321;
	tmbuf = localtime (&some_time);
	strftime (buf, 64, "%c", tmbuf);
	tmp = mu_str_date ("%c", some_time);
	
	g_assert_cmpstr (tmp, ==, buf);
	g_free (tmp);
		
}


static void
test_mu_str_size_01 (void)
{
	struct lconv *lc;
	char *tmp2;
	
	lc = localeconv();

	tmp2 = g_strdup_printf ("0%s0 kB", lc->decimal_point);
	g_assert_cmpstr (mu_str_size_s (0),           ==,  tmp2);
	g_free (tmp2);

	tmp2 = g_strdup_printf ("100%s0 kB", lc->decimal_point);
	g_assert_cmpstr (mu_str_size_s (100000),      ==,  tmp2);
	g_free (tmp2);

	tmp2 = g_strdup_printf ("1%s1 MB", lc->decimal_point);
	g_assert_cmpstr (mu_str_size_s (1100*1000), ==,  tmp2);
	g_free (tmp2);
}

	

static void
test_mu_str_size_02 (void)
{
	struct lconv *lc;
	char *tmp1, *tmp2;
	
	lc = localeconv();
	
	tmp2 = g_strdup_printf ("1%s0 MB", lc->decimal_point);
	tmp1 = mu_str_size (999999); 
	g_assert_cmpstr (tmp1, !=, tmp2);

	g_free (tmp1);
	g_free (tmp2);
}



static void
test_mu_str_prio_01 (void)
{
	g_assert_cmpstr(mu_msg_prio_name(MU_MSG_PRIO_LOW), ==, "low");
	g_assert_cmpstr(mu_msg_prio_name(MU_MSG_PRIO_NORMAL), ==, "normal");
	g_assert_cmpstr(mu_msg_prio_name(MU_MSG_PRIO_HIGH), ==, "high");
}


static gboolean
ignore_error (const char* log_domain, GLogLevelFlags log_level,
	      const gchar* msg, gpointer user_data)
{
	return FALSE; /* don't abort */
}


static void
test_mu_str_prio_02 (void)
{
	/* this must fail */
	g_test_log_set_fatal_handler ((GTestLogFatalFunc)ignore_error, NULL);
	g_assert_cmpstr (mu_msg_prio_name(666), ==, NULL);
}
	


static void
test_mu_str_normalize_01 (void)
{
	int			i;
	struct {
		const char*	word;
		const char*	norm;
	} words [] = {
		{ "dantès", "dantes"}, 
		{ "foo", "foo" },
		{ "Föö", "foo" },
		{ "číslo", "cislo" },
		{ "hÆvý mëÐal ümláõt", "haevy medal umlaot"}
	};

	
	for (i = 0; i != G_N_ELEMENTS(words); ++i) {
		gchar *str;
		str = mu_str_normalize (words[i].word, TRUE);
		g_assert_cmpstr (str, ==, words[i].norm);
		g_free (str);
	}
}


static void
test_mu_str_normalize_02 (void)
{
	int			i;
	struct {
		const char*	word;
		const char*	norm;
	} words [] = {
		{ "DantèS", "DanteS"}, 
		{ "foo", "foo" },
		{ "Föö", "Foo" },
		{ "číslO", "cislO" },
		{ "hÆvý mëÐal ümláõt", "hAevy meDal umlaot"}
	};

	
	for (i = 0; i != G_N_ELEMENTS(words); ++i) {
		gchar *str;
		str = mu_str_normalize (words[i].word, FALSE);
		g_assert_cmpstr (str, ==, words[i].norm);
		g_free (str);
	}
}


static void
test_mu_str_ascii_xapian_escape (void)
{
	int			i;
	struct {
		const char*	word;
		const char*	esc;
	} words [] = {
		{ "aap@noot.mies", "aap_noot_mies"}, 
		{ "Foo..Bar", "foo..bar" },
		{ "Foo.Bar", "foo_bar" },
		{ "Foo. Bar", "foo. bar" },
		{ "subject:test@foo", "subject:test_foo" },
		{ "xxx:test@bar", "xxx_test_bar" },
	};
		
	for (i = 0; i != G_N_ELEMENTS(words); ++i) {
		gchar *a = g_strdup (words[i].word);
		mu_str_ascii_xapian_escape_in_place (a);
		g_assert_cmpstr (a, ==, words[i].esc);
		g_free (a);
	}
}


static void
test_mu_str_display_contact (void)
{
	int			i;
	struct {
		const char*	word;
		const char*	disp;
	} words [] = {
		{ "\"Foo Bar\" <aap@noot.mies>", "Foo Bar"}, 
		{ "Foo Bar <aap@noot.mies>", "Foo Bar" },
		{ "<aap@noot.mies>", "aap@noot.mies" },
		{ "foo@bar.nl", "foo@bar.nl" }
	};
		
	for (i = 0; i != G_N_ELEMENTS(words); ++i) 
		g_assert_cmpstr (mu_str_display_contact_s (words[i].word), ==, 
				 words[i].disp);
}



static void
test_mu_str_date_parse_hdwmy (void)
{
	time_t diff;

	diff = time(NULL) - mu_str_date_parse_hdwmy ("3h");
	g_assert (diff > 0);
	g_assert_cmpuint (3 * 60 * 60 - diff, <=, 1);

	diff = time(NULL) - mu_str_date_parse_hdwmy ("5y");
	g_assert (diff > 0);
	g_assert_cmpuint (5 * 365 * 24 * 60 * 60 - diff, <=, 1);
	
	diff = time(NULL) - mu_str_date_parse_hdwmy ("3m");
	g_assert (diff > 0);
	g_assert_cmpuint (3 * 30 * 24 * 60 * 60 - diff, <=, 1);

	diff = time(NULL) - mu_str_date_parse_hdwmy ("21d");
	g_assert (diff > 0);
	g_assert_cmpuint (21 * 24 * 60 * 60 - diff, <=, 1);
	
	diff = time(NULL) - mu_str_date_parse_hdwmy ("2w");
	g_assert (diff > 0);
	g_assert_cmpuint (2 * 7 * 24 * 60 * 60 - diff, <=, 1);
	
	
	g_assert_cmpint (mu_str_date_parse_hdwmy("-1y"),==, (time_t)-1);  
}


static void
test_mu_str_guess_first_name (void)
{
	int i;
		
	struct {
		char *src, *exp;
	} tests[] = {
		{ "Richard M. Stallman", "Richard M." },
		{ "John Rambo", "John" },
		{ "Ivanhoe", "Ivanhoe" },
		{ "", "" }
	};
		
	for (i = 0; i != G_N_ELEMENTS(tests); ++i) {
		gchar *s;

		s  = mu_str_guess_first_name (tests[i].src);
		g_assert_cmpstr (s, ==, tests[i].exp);
		g_free (s);
	}
}


static void
test_mu_str_guess_last_name (void)
{
	int i;
		
	struct {
		char *src, *exp;
	} tests[] = {
		{ "Richard M. Stallman", "Stallman" },
		{ "John Rambo", "Rambo" },
		{ "Ivanhoe", "" },
		{ "", "" }
	};
				
	for (i = 0; i != G_N_ELEMENTS(tests); ++i) {
		gchar *s;

		s  = mu_str_guess_last_name (tests[i].src);
		g_assert_cmpstr (s, ==, tests[i].exp);
		g_free (s);
	}
}



static void
test_mu_str_guess_nick (void)
{
	int i;
		
	struct {
		char *src, *exp;
	} tests[] = {
		{ "Richard M. Stallman", "RichardMS" },
		{ "John Rambo", "JohnR" },
		{ "Ivanhoe", "Ivanhoe" },
		{ "", "" }
	};
				
	for (i = 0; i != G_N_ELEMENTS(tests); ++i) {
		gchar *s;

		s  = mu_str_guess_nick (tests[i].src);
		g_assert_cmpstr (s, ==, tests[i].exp);
		g_free (s);
	}
}






int
main (int argc, char *argv[])
{
	g_test_init (&argc, &argv, NULL);

	/* mu_str_date */
	g_test_add_func ("/mu-str/mu-str-date",
			 test_mu_str_date_01);

	/* mu_str_size */
	g_test_add_func ("/mu-str/mu-str-size-01",
			 test_mu_str_size_01);
	g_test_add_func ("/mu-str/mu-str-size-02",
			 test_mu_str_size_02);

	/* mu_str_prio */
	g_test_add_func ("/mu-str/mu-str-prio-01",
			 test_mu_str_prio_01);
	g_test_add_func ("/mu-str/mu-str-prio-02",
			 test_mu_str_prio_02);

	/* mu_str_normalize */
	g_test_add_func ("/mu-str/mu-str-normalize-01",
			 test_mu_str_normalize_01);
	g_test_add_func ("/mu-str/mu-str-normalize-02",
			 test_mu_str_normalize_02);

	g_test_add_func ("/mu-str/mu-str-ascii-xapian-escape",
			 test_mu_str_ascii_xapian_escape);

	g_test_add_func ("/mu-str/mu-str-display_contact",
			 test_mu_str_display_contact);			 
	

	g_test_add_func ("/mu-str/mu-str_date_parse_hdwmy",
			 test_mu_str_date_parse_hdwmy);

	g_test_add_func ("/mu-str/mu-str_guess_first_name",
			 test_mu_str_guess_first_name);
	g_test_add_func ("/mu-str/mu-str_guess_last_name",
			 test_mu_str_guess_last_name);
	g_test_add_func ("/mu-str/mu-str_guess_nick",
			 test_mu_str_guess_nick);
	
	/* FIXME: add tests for mu_str_flags; but note the
	 * function simply calls mu_msg_field_str */
		
	g_log_set_handler (NULL,
			   G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL| G_LOG_FLAG_RECURSION,
			   (GLogFunc)black_hole, NULL);
	
	return g_test_run ();
}
