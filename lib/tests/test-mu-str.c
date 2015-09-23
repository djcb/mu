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
#include "mu-str.h"
#include "mu-msg-prio.h"




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
test_mu_str_flatten (void)
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
		{ "hÆvý mëÐal ümláõt", "hævy_meðal_umlaot"}
	};


	for (i = 0; i != G_N_ELEMENTS(words); ++i) {
		gchar *str;
		str = mu_str_process_term (words[i].word);
		g_assert_cmpstr (str, ==, words[i].norm);
		g_free (str);
	}
}

static void
test_parse_arglist (void)
{
	const char *args;
	GHashTable *hash;
	GError *err;

	args = "cmd:find query:\"maildir:\\\"/sent items\\\"\" maxnum:500";

	err  = NULL;
	hash = mu_str_parse_arglist (args, &err);
	g_assert_no_error (err);
	g_assert (hash);

	g_assert_cmpstr (g_hash_table_lookup (hash, "cmd"), ==,
			 "find");
	g_assert_cmpstr (g_hash_table_lookup (hash, "query"), ==,
			 "maildir:\"/sent items\"");
	g_assert_cmpstr (g_hash_table_lookup (hash, "maxnum"), ==,
			 "500");

	g_hash_table_destroy (hash);
}







static void
test_mu_str_esc_to_list (void)
{
	int			i;
	struct {
		const char*  str;
		const char* strs[3];
	} strings [] = {
		{ "maildir:foo",
		  {"maildir:foo", NULL, NULL}},
		{ "maildir:sent items",
		  {"maildir:sent", "items", NULL}},
		{ "\"maildir:sent items\"",
		  {"maildir:sent items", NULL, NULL}},
	};

	for (i = 0; i != G_N_ELEMENTS(strings); ++i) {
		GSList *lst, *cur;
		unsigned u;
		lst = mu_str_esc_to_list (strings[i].str);
		for (cur = lst, u = 0; cur; cur = g_slist_next(cur), ++u)
			g_assert_cmpstr ((const char*)cur->data,==,
					 strings[i].strs[u]);
		mu_str_free_list (lst);
	}
}

static void
test_mu_str_process_query_term (void)
{
	int			i;
	struct {
		const char*	word;
		const char*	esc;
	} words [] = {
		{ "aap@noot.mies", "aap_noot_mies" },
		{ "Foo..Bar", "foo__bar" },
		{ "Foo.Bar", "foo_bar" },
		{ "Foo Bar", "foo_bar" },
		{ "\\foo", "_foo" },
		{ "subject:test@foo", "subject:test_foo" },
		{ "xxx:test@bar", "xxx:test_bar" },
		{ "aa$bb$cc", "aa_bb_cc" },
		{ "date:2010..2012", "date:2010..2012"},
		{ "d:2010..2012", "d:2010..2012"},
		{ "size:10..20", "size:10..20"},
		{ "x:2010..2012", "x:2010__2012"},
		{ "q:2010..2012", "q:2010__2012"},
		{ "subject:2010..2012", "subject:2010__2012"},
		{ "(maildir:foo)", "(maildir:foo)"},
		{ "Тесла, Никола", "тесла__никола"},
		{ "Masha@Аркона.ru", "masha_аркона_ru" },
		{ "foo:ελληνικά", "foo:ελληνικα" },
		{ "日本語!!", "日本語__" },
		{ "￡", "_" }
	};

	for (i = 0; i != G_N_ELEMENTS(words); ++i) {
		gchar *s;
		s = mu_str_process_query_term (words[i].word);
		if (g_test_verbose())
			g_print ("expected: '%s' <=> got: '%s'\n",
				 words[i].esc, s);
		g_assert_cmpstr (s, ==, words[i].esc);
		g_free (s);
	}
}


static void
test_mu_str_process_term (void)
{
	int			i;
	struct {
		const char*	word;
		const char*	esc;
	} words [] = {
		{ "aap@noot.mies", "aap_noot_mies" },
		{ "A&B", "a_b" },
		{ "Foo..Bar", "foo__bar" },
		{ "Foo.Bar", "foo_bar" },
		{ "Foo Bar", "foo_bar" },
		{ "\\foo", "_foo" },
		{ "subject:test@foo", "subject_test_foo" },
		{ "xxx:test@bar", "xxx_test_bar" },
		{ "aa$bb$cc", "aa_bb_cc" },
		{ "date:2010..2012", "date_2010__2012"},
		{ "subject:2010..2012", "subject_2010__2012"},
		{ "(maildir:foo)", "_maildir_foo_"},
		{ "Тесла, Никола", "тесла__никола"},
		{ "Masha@Аркона.ru", "masha_аркона_ru" },
		{ "foo:ελληνικά", "foo_ελληνικα" },
		{ "日本語!!", "日本語__" },
		{ "￡", "_" },
		/* invalid utf8 */
		{ "Hello\xC3\x2EWorld", "hello__world" }
	};

	for (i = 0; i != G_N_ELEMENTS(words); ++i) {
		gchar *s;
		s = mu_str_process_term (words[i].word);
		if (g_test_verbose())
			g_print ("expected: '%s' <=> got: '%s'\n",
				 words[i].esc, s);
		g_assert_cmpstr (s, ==, words[i].esc);
		g_free (s);
	}
}




static void
test_mu_str_process_text (void)
{
	int			i;
	struct {
		const char*	word;
		const char*	esc;
	} words [] = {
		{ "aap@noot.mies", "aap@noot.mies" },
		{ "A&B", "a&b" },
		{ "Foo..Bar", "foo..bar" },
		{ "Foo.Bar", "foo.bar" },
		{ "Foo Bar", "foo bar" },
		{ "\\foo", "\\foo" },
		{ "subject:test@foo", "subject:test@foo" },
		{ "xxx:test@bar", "xxx:test@bar" },
		{ "aa$bb$cc", "aa$bb$cc" },
		{ "date:2010..2012", "date:2010..2012"},
		{ "subject:2010..2012", "subject:2010..2012"},
		{ "(maildir:foo)", "(maildir:foo)"},
		{ "Тесла, Никола", "тесла, никола"},
		{ "Masha@Аркона.ru", "masha@аркона.ru" },
		{ "foo:ελληνικά", "foo:ελληνικα" },
		{ "日本語!!", "日本語!!" }
	};

	for (i = 0; i != G_N_ELEMENTS(words); ++i) {
		gchar *s;
		s = mu_str_process_text (words[i].word);
		if (g_test_verbose())
			g_print ("expected: '%s' <=> got: '%s'\n",
				 words[i].esc, s);
		g_assert_cmpstr (s, ==, words[i].esc);
		g_free (s);
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
assert_cmplst (GSList *lst, const char *items[])
{
	int i;

	if (!lst)
		g_assert (!items);

	for (i = 0; lst; lst = g_slist_next(lst), ++i)
		g_assert_cmpstr ((char*)lst->data,==,items[i]);

	g_assert (items[i] == NULL);
}


static GSList*
create_list (const char *items[])
{
	GSList *lst;

	lst = NULL;
	while (items && *items) {
		lst = g_slist_prepend (lst, g_strdup(*items));
		++items;
	}

	return g_slist_reverse (lst);

}

static void
test_mu_str_from_list (void)
{
	{
		const char *strs[] = {"aap", "noot", "mies", NULL};
		GSList *lst = create_list (strs);
		gchar  *str = mu_str_from_list (lst, ',');
		g_assert_cmpstr ("aap,noot,mies", ==, str);
		mu_str_free_list (lst);
		g_free (str);
	}

	{
		const char *strs[] = {"aap", "no,ot", "mies", NULL};
		GSList *lst = create_list (strs);
		gchar  *str = mu_str_from_list (lst, ',');
		g_assert_cmpstr ("aap,no,ot,mies", ==, str);
		mu_str_free_list (lst);
		g_free (str);
	}

	{
		const char *strs[] = {NULL};
		GSList *lst = create_list (strs);
		gchar  *str = mu_str_from_list (lst,'@');
		g_assert_cmpstr (NULL, ==, str);
		mu_str_free_list (lst);
		g_free (str);
	}


}


static void
test_mu_str_to_list (void)
{
	{
		const char *items[]= {"foo", "bar ", "cuux", NULL};
		GSList *lst = mu_str_to_list ("foo@bar @cuux",'@', FALSE);
		assert_cmplst (lst, items);
		mu_str_free_list (lst);
	}

	{
		GSList *lst = mu_str_to_list (NULL,'x',FALSE);
		g_assert (lst == NULL);
		mu_str_free_list (lst);
	}
}

static void
test_mu_str_to_list_strip (void)
{
	const char *items[]= {"foo", "bar", "cuux", NULL};
	GSList *lst = mu_str_to_list ("foo@bar @cuux",'@', TRUE);
	assert_cmplst (lst, items);
		mu_str_free_list (lst);
}


static void
test_mu_str_subject_normalize (void)
{
	int i;

	struct {
		char *src, *exp;
	} tests[] = {
		{ "test123", "test123" },
		{ "Re:test123", "test123" },
		{ "Re: Fwd: test123", "test123" },
		{ "Re[3]: Fwd: test123", "test123" },
		{ "operation: mindcrime", "operation: mindcrime" }, /*...*/
		{ "", "" }
	};

	for (i = 0; i != G_N_ELEMENTS(tests); ++i)
		g_assert_cmpstr (mu_str_subject_normalize (tests[i].src), ==,
				 tests[i].exp);
}



static void
test_mu_term_fixups (void)
{
	unsigned u;
	struct {
		const gchar *expr, *expected;
	} testcases [] = {
		{ "date:19700101", "date:19700101..19700101" },
		{ "date:19700101..19700101", "date:19700101..19700101" },
		{ "(date:20121107))", "(date:20121107..20121107))" },
		{ "maildir:/somepath", "maildir:/somepath" },
		{ "([maildir:/somepath]", "([maildir:/somepath]" },
		/* add more */
		{ "({", "({" },
		{ "({abc", "({abc" },
		{ "abc)}", "abc)}" },
		{ "", "" }
	};

 	for (u = 0; u != G_N_ELEMENTS(testcases); ++u) {
		gchar *prep;
		prep = mu_str_xapian_fixup_terms (testcases[u].expr);
		g_assert_cmpstr (prep, ==, testcases[u].expected);
		g_free (prep);
	}
}





static void
test_mu_str_replace (void)
{
	unsigned u;
	struct {
		const char*  str;
		const char* sub;
		const char *repl;
		const char *exp;
	} strings [] = {
		{ "hello", "ll", "xx", "hexxo" },
		{ "hello", "hello", "hi", "hi" },
		{ "hello", "foo", "bar", "hello" }
	};

	for (u = 0; u != G_N_ELEMENTS(strings); ++u) {
		char *res;
		res = mu_str_replace (strings[u].str,
				      strings[u].sub,
				      strings[u].repl);
		g_assert_cmpstr (res,==,strings[u].exp);
		g_free (res);
	}
}



int
main (int argc, char *argv[])
{
	setlocale (LC_ALL, "");

	g_test_init (&argc, &argv, NULL);

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

	g_test_add_func ("/mu-str/mu-str-flatten",
			 test_mu_str_flatten);

	g_test_add_func ("/mu-str/process-query-term",
			 test_mu_str_process_query_term);
	g_test_add_func ("/mu-str/process-term",
			 test_mu_str_process_term);
	g_test_add_func ("/mu-str/process-text",
			 test_mu_str_process_text);


	g_test_add_func ("/mu-str/mu-str-display_contact",
			 test_mu_str_display_contact);

	g_test_add_func ("/mu-str/mu-str-from-list",
			 test_mu_str_from_list);
	g_test_add_func ("/mu-str/mu-str-to-list",
			 test_mu_str_to_list);
	g_test_add_func ("/mu-str/mu-str-to-list-strip",
			 test_mu_str_to_list_strip);

	g_test_add_func ("/mu-str/mu-str-parse-arglist",
			 test_parse_arglist);

	g_test_add_func ("/mu-str/mu-str-replace",
			 test_mu_str_replace);

	g_test_add_func ("/mu-str/mu-str-esc-to-list",
			 test_mu_str_esc_to_list);

	g_test_add_func ("/mu-str/mu_str_subject_normalize",
			 test_mu_str_subject_normalize);

	/* mu_str_xapian_fixup_terms */
	g_test_add_func ("/mu-str/mu_term_fixups",
			 test_mu_term_fixups);


	/* FIXME: add tests for mu_str_flags; but note the
	 * function simply calls mu_msg_field_str */

	g_log_set_handler (NULL,
			   G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL| G_LOG_FLAG_RECURSION,
			   (GLogFunc)black_hole, NULL);

	return g_test_run ();
}
