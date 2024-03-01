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

#include <unordered_set>
#include <string>

#include <glib.h>
#include <glib/gstdio.h>

#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <locale.h>

#include "utils/mu-test-utils.hh"
#include "mu-query.hh"
#include "utils/mu-result.hh"
#include "utils/mu-utils.hh"
#include "utils/mu-utils-file.hh"
#include "mu-store.hh"

using namespace Mu;

static std::string DB_PATH1;
static std::string DB_PATH2;

static std::string
make_database(const std::string& dbdir, const std::string& testdir)
{
	/* use the env var rather than `--muhome` */
	g_setenv("MUHOME", dbdir.c_str(), 1);
	const auto cmdline{mu_format(
			"/bin/sh -c '"
			"{} --quiet init --maildir={} ; "
			"{} --quiet index'",
			MU_PROGRAM, testdir, MU_PROGRAM)};

	if (g_test_verbose())
		mu_printerrln("\n{}", cmdline);

	g_assert(g_spawn_command_line_sync(cmdline.c_str(), NULL, NULL, NULL, NULL));
	auto xpath = join_paths(dbdir, "xapian");
	/* ensure MUHOME worked */
	g_assert_cmpuint(::access(xpath.c_str(), F_OK), ==, 0);

	return xpath;
}

static void
assert_no_dups(const QueryResults& qres)
{
	std::unordered_set<std::string> msgid_set, path_set;

	for (auto&& mi : qres) {
		g_assert_true(msgid_set.find(mi.message_id().value()) == msgid_set.end());
		g_assert_true(path_set.find(mi.path().value()) == path_set.end());

		path_set.emplace(*mi.path());
		msgid_set.emplace(*mi.message_id());

		g_assert_false(msgid_set.find(mi.message_id().value()) == msgid_set.end());
		g_assert_false(path_set.find(mi.path().value()) == path_set.end());
	}
}

/* note: this also *moves the iter* */
static size_t
run_and_count_matches(const std::string& xpath,
		      const std::string& expr,
		      Mu::QueryFlags     flags = Mu::QueryFlags::None)
{
	auto store{Store::make(xpath)};
	assert_valid_result(store);

	// if (g_test_verbose()) {
	//	std::cout << "==> mquery: " << store.parse_query(expr, false) << "\n";
	//	std::cout << "==> xquery: " << store.parse_query(expr, true) << "\n";
	// }

	Mu::allow_warnings();

	auto qres{store->run_query(expr, {}, flags)};
	g_assert_true(!!qres);
	assert_no_dups(*qres);

	if (g_test_verbose())
		mu_println("'{}' => {}\n", expr, qres->size());

	return qres->size();
}

typedef struct {
	const char* query;
	size_t      count; /* expected number of matches */
} QResults;

static void
test_mu_query_01(void)
{
	int      i;
	QResults queries[] = {
	    {"basic", 3},
	    {"question", 5},
	    {"thanks", 2},
	    {"html", 4},
	    {"subject:exception", 1},
	    {"exception", 1},
	    {"subject:A&B", 1},
	    {"A&B", 1},
	    {"subject:elisp", 1},
	    {"html AND contains", 1},
	    {"html and contains", 1},
	    {"from:pepernoot", 0},
	    {"foo:pepernoot", 0},
	    {"funky", 1},
	    {"fünkÿ", 1},
	    { "", 19 },
	    {"msgid:abcd$efgh@example.com", 1},
	    {"i:abcd$efgh@example.com", 1},
#ifdef HAVE_CLD2
{	    "lang:en",  14},
#endif /*HAVE_CLD2*/
	};

	for (i = 0; i != G_N_ELEMENTS(queries); ++i)
		g_assert_cmpuint(run_and_count_matches(DB_PATH1, queries[i].query),
				 ==, queries[i].count);
}

static void
test_mu_query_02(void)
{
	const char* q;
	q = "i:f7ccd24b0808061357t453f5962w8b61f9a453b684d0@mail.gmail.com";
	g_assert_cmpuint(run_and_count_matches(DB_PATH1, q), ==, 1);
}

static void
test_mu_query_03(void)
{
	int      i;
	QResults queries[] = {{"ploughed", 1},
			      {"i:3BE9E6535E3029448670913581E7A1A20D852173@"
			       "emss35m06.us.lmco.com",
			       1},
			      {"i:!&!AAAAAAAAAYAAAAAAAAAOH1+8mkk+lLn7Gg5fke7"
			       "FbCgAAAEAAAAJ7eBDgcactKhXL6r8cEnJ8BAAAAAA==@"
			       "example.com",
			       1},

			      /* subsets of the words in the subject should match */
			      {"s:gcc include search order", 1},
			      {"s:gcc include search", 1},
			      {"s:search order", 1},
			      {"s:include", 1},

			      {"s:lisp", 1},
			      {"s:LISP", 1},

			      // { "s:\"Re: Learning LISP; Scheme vs elisp.\"", 1},
			      // { "subject:Re: Learning LISP; Scheme vs elisp.", 1},
			      // { "subject:\"Re: Learning LISP; Scheme vs elisp.\"", 1},
			      {"to:help-gnu-emacs@gnu.org", 4},
			      //{"t:help-gnu-emacs", 4},
			      {"flag:flagged", 1}};

	for (i = 0; i != G_N_ELEMENTS(queries); ++i)
		g_assert_cmpuint(run_and_count_matches(DB_PATH1, queries[i].query),
				 ==, queries[i].count);
}

static void
test_mu_query_04(void)
{
	int i;

	QResults queries[] = {
	    {"frodo@example.com", 1},
	    {"f:frodo@example.com", 1},
	    {"f:Frodo Baggins", 1},
	    {"bilbo@anotherexample.com", 1},
	    {"t:bilbo@anotherexample.com", 1},
	    {"t:bilbo", 1},
	    {"f:bilbo", 0},
	    {"baggins", 1},
	    {"prio:h", 1},
	    {"prio:high", 1},
	    {"prio:normal", 11},
	    {"prio:l", 7},
	    {"not prio:l", 12},
	};

	for (i = 0; i != G_N_ELEMENTS(queries); ++i)
		g_assert_cmpuint(run_and_count_matches(DB_PATH1, queries[i].query),
				 ==, queries[i].count);
}

static void
test_mu_query_logic(void)
{
	int      i;
	QResults queries[] = {{"subject:gcc", 1},
			      {"subject:lisp", 1},
			      {"subject:gcc OR subject:lisp", 2},
			      {"subject:gcc or subject:lisp", 2},
			      {"subject:gcc AND subject:lisp", 0},
			      {"subject:gcc OR (subject:scheme AND subject:elisp)", 2},
			      {"(subject:gcc OR subject:scheme) AND subject:elisp", 1}};

	for (i = 0; i != G_N_ELEMENTS(queries); ++i)
		g_assert_cmpuint(run_and_count_matches(DB_PATH1, queries[i].query),
				 ==, queries[i].count);
}

static void
test_mu_query_accented_chars_01(void)
{
	auto store = Store::make(DB_PATH1);
	assert_valid_result(store);

	auto qres{store->run_query("fünkÿ")};
	g_assert_true(!!qres);
	g_assert_false(qres->empty());

	const auto msg{qres->begin().message()};
	if (!msg) {
		mu_warning("error getting message");
		g_assert_not_reached();
	}

	assert_equal(msg->subject(), "Greetings from Lothlórien");
}

static void
test_mu_query_accented_chars_02(void)
{
	int i;

	QResults queries[] = {{"f:mü", 1},
		{ "s:motörhead", 1},
		{"t:Helmut", 1},
		{"t:Kröger", 1},
		{"s:MotorHeäD", 1},
	};

	for (i = 0; i != G_N_ELEMENTS(queries); ++i) {
		auto count = run_and_count_matches(DB_PATH1, queries[i].query);
		if (count != queries[i].count)
			mu_warning("query '{}'; expected {} but got {}",
				  queries[i].query, queries[i].count, count);
		g_assert_cmpuint(run_and_count_matches(DB_PATH1, queries[i].query),
				 ==, queries[i].count);
	}
}

static void
test_mu_query_accented_chars_fraiche(void)
{
	int i;

	QResults queries[] = {{"crème fraîche", 1},
			      {"creme fraiche", 1},
			      {"fraîche crème", 1},
			      {"будланула", 1},
			      {"БУДЛАНУЛА", 1},
			      {"CRÈME FRAÎCHE", 1},
			      {"CREME FRAICHE", 1}};

	for (i = 0; i != G_N_ELEMENTS(queries); ++i) {
		if (g_test_verbose())
			mu_println("{}", queries[i].query);

		g_assert_cmpuint(run_and_count_matches(DB_PATH2, queries[i].query),
				 ==, queries[i].count);
	}
}

static void
test_mu_query_wildcards(void)
{
	int i;

	QResults queries[] = {
	    {"f:mü", 1},
	    {"s:mo*", 1},
	    {"t:Helm*", 1},
	    {"queensryche", 1},
	    {"Queen*", 1},
	};

	for (i = 0; i != G_N_ELEMENTS(queries); ++i)
		g_assert_cmpuint(run_and_count_matches(DB_PATH1, queries[i].query),
				 ==, queries[i].count);
}

static void
test_mu_query_dates_helsinki(void)
{
	const auto hki = "Europe/Helsinki";
	if (!timezone_available(hki)) {
		g_test_skip("timezone not available");
		return;
	}

	int         i;
	const char* old_tz;

	QResults queries[] = {{"date:20080731..20080804", 5},
			      {"date:20080731..20080804 s:gcc", 1},
			      {"date:200808110803..now", 7},
			      {"date:200808110803..today", 7},
			      {"date:200808110801..now", 7}};

	old_tz = set_tz(hki);
	TempDir tdir;
	const auto xpath{make_database(tdir.path(), MU_TESTMAILDIR)};
	g_assert_false(xpath.empty());

	for (i = 0; i != G_N_ELEMENTS(queries); ++i)
		g_assert_cmpuint(run_and_count_matches(xpath, queries[i].query),
				 ==, queries[i].count);

	set_tz(old_tz);
}

static void
test_mu_query_dates_sydney(void)
{
	const auto syd = "Australia/Sydney";
	if (!timezone_available(syd)) {
		g_test_skip("timezone not available");
		return;
	}

	int         i;
	const char* old_tz;
	QResults    queries[] = {{"date:20080731..20080804", 5},
			      {"date:20080731..20080804 s:gcc", 1},
			      {"date:200808110803..now", 7},
			      {"date:200808110803..today", 7},
			      {"date:200808110801..now", 7}};
	old_tz = set_tz(syd);

	TempDir tdir;
	const auto xpath{make_database(tdir.path(), MU_TESTMAILDIR)};
	g_assert_false(xpath.empty());

	for (i = 0; i != G_N_ELEMENTS(queries); ++i)
		g_assert_cmpuint(run_and_count_matches(xpath, queries[i].query),
				 ==, queries[i].count);
	set_tz(old_tz);
}

static void
test_mu_query_dates_la(void)
{
	const auto la = "America/Los_Angeles";
	if (!timezone_available(la)) {
		g_test_skip("timezone not available");
		return;
	}

	int         i;
	const char* old_tz;

	QResults queries[] = {{"date:20080731..20080804", 5},
			      {"date:2008-07-31..2008-08-04", 5},
			      {"date:20080804..20080731", 5},
			      {"date:20080731..20080804 s:gcc", 1},
			      {"date:200808110803..now", 6},
			      {"date:200808110803..today", 6},
			      {"date:200808110801..now", 6}};
	old_tz = set_tz(la);

	TempDir tdir;
	const auto xpath{make_database(tdir.path(), MU_TESTMAILDIR)};
	g_assert_false(xpath.empty());

	for (i = 0; i != G_N_ELEMENTS(queries); ++i) {
		/* g_print ("%s\n", queries[i].query); */
		g_assert_cmpuint(run_and_count_matches(xpath, queries[i].query),
				 ==, queries[i].count);
	}

	set_tz(old_tz);
}

static void
test_mu_query_sizes(void)
{
	int      i;
	QResults queries[] = {
	    {"size:0b..2m", 19},
	    {"size:3b..2m", 19},
	    {"size:2k..4k", 4},

	    {"size:0b..2m", 19},
	    {"size:2m..0b", 19},
	};

	for (i = 0; i != G_N_ELEMENTS(queries); ++i)
		g_assert_cmpuint(run_and_count_matches(DB_PATH1, queries[i].query),
				 ==, queries[i].count);
}

static void
test_mu_query_attach(void)
{
	int      i;
	QResults queries[] = {{"j:sittingbull.jpg", 1}, {"file:custer", 0}, {"file:custer.jpg", 1}};

	for (i = 0; i != G_N_ELEMENTS(queries); ++i) {
		if (g_test_verbose())
			mu_println("query: {}", queries[i].query);
		g_assert_cmpuint(run_and_count_matches(DB_PATH2, queries[i].query),
				 ==, queries[i].count);
	}
}

static void
test_mu_query_msgid(void)
{
	int      i;
	QResults queries[] = {
	    {"i:CAHSaMxZ9rk5ASjqsbXizjTQuSk583=M6TORHz"
	     "=bfogtmbGGs5A@mail.gmail.com",
	     1},
	    {"msgid:CAHSaMxZ9rk5ASjqsbXizjTQuSk583=M6TORHz="
	     "bfogtmbGGs5A@mail.gmail.com",
	     1},

	};

	for (i = 0; i != G_N_ELEMENTS(queries); ++i) {
		if (g_test_verbose())
			mu_println("query: {}", queries[i].query);
		g_assert_cmpuint(run_and_count_matches(DB_PATH2, queries[i].query),
				 ==, queries[i].count);
	}
}

static void
test_mu_query_tags(void)
{
	int      i;
	QResults queries[] = {
	    {"x:paradise", 1},
	    {"tag:lost", 1},
	    {"tag:lost tag:paradise", 1},
	    {"tag:lost tag:horizon", 0},
	    {"tag:lost OR tag:horizon", 1},
	    {"tag:queensryche", 1},
	    {"tag:Queensrÿche", 1},
	    {"x:paradise,lost", 0},
	    {"x:paradise AND x:lost", 1},
	    {"x:\\\\backslash", 1},
	};

	for (i = 0; i != G_N_ELEMENTS(queries); ++i)
		g_assert_cmpuint(run_and_count_matches(DB_PATH2, queries[i].query),
				 ==, queries[i].count);
}

static void
test_mu_query_wom_bat(void)
{
	int      i;
	QResults queries[] = {
	    {"maildir:/wom_bat", 3},
	    //{ "\"maildir:/wom bat\"", 3},
	    // as expected, no longer works with new parser
	};

	for (i = 0; i != G_N_ELEMENTS(queries); ++i)
		g_assert_cmpuint(run_and_count_matches(DB_PATH2, queries[i].query),
				 ==, queries[i].count);
}

static void
test_mu_query_signed_encrypted(void)
{
	int      i;
	QResults queries[] = {
	    {"flag:encrypted", 2},
	    {"flag:signed", 2},
	};

	for (i = 0; i != G_N_ELEMENTS(queries); ++i)
		g_assert_cmpuint(run_and_count_matches(DB_PATH1, queries[i].query),
				 ==,
				 queries[i].count);
}

static void
test_mu_query_multi_to_cc(void)
{
	int      i;
	QResults queries[] = {
	    {"to:a@example.com", 1},
	    {"cc:d@example.com", 1},
	    {"to:b@example.com", 1},
	    {"cc:e@example.com", 1},
	    {"cc:e@example.com AND cc:d@example.com", 1},
	};

	for (i = 0; i != G_N_ELEMENTS(queries); ++i)
		g_assert_cmpuint(run_and_count_matches(DB_PATH1, queries[i].query),
				 ==, queries[i].count);
}

static void
test_mu_query_tags_02(void)
{
	int      i;
	QResults queries[] = {
	    {"x:paradise", 1},
	    {"tag:@NextActions", 1},
	    {"x:queensrÿche", 1},
	    {"tag:lost OR tag:operation*", 2},
	};

	for (i = 0; i != G_N_ELEMENTS(queries); ++i) {
		g_assert_cmpuint(run_and_count_matches(DB_PATH2, queries[i].query),
				 ==,  queries[i].count);
	}
}

/* Tests for https://github.com/djcb/mu/issues/380

   On certain platforms, something goes wrong during compilation and the
   --related option doesn't work.
*/
static void
test_mu_query_threads_compilation_error(void)
{
	TempDir tdir;
	const auto xpath = make_database(tdir.path(), MU_TESTMAILDIR);

	g_assert_cmpuint(run_and_count_matches(xpath, "msgid:uwsireh25.fsf@one.dot.net"), ==, 1);

	g_assert_cmpuint(run_and_count_matches(xpath,
					       "msgid:uwsireh25.fsf@one.dot.net",
					       QueryFlags::IncludeRelated), ==, 3);
}

int
main(int argc, char* argv[])
{
	TempDir td1;
	TempDir td2;

	mu_test_init(&argc, &argv);
	DB_PATH1 = make_database(td1.path(), MU_TESTMAILDIR);
	g_assert_false(DB_PATH1.empty());

	DB_PATH2 = make_database(td2.path(), MU_TESTMAILDIR2);
	g_assert_false(DB_PATH2.empty());

	g_test_add_func("/mu-query/test-mu-query-01", test_mu_query_01);
	g_test_add_func("/mu-query/test-mu-query-02", test_mu_query_02);
	g_test_add_func("/mu-query/test-mu-query-03", test_mu_query_03);
	g_test_add_func("/mu-query/test-mu-query-04", test_mu_query_04);

	g_test_add_func("/mu-query/test-mu-query-signed-encrypted", test_mu_query_signed_encrypted);
	g_test_add_func("/mu-query/test-mu-query-multi-to-cc", test_mu_query_multi_to_cc);
	g_test_add_func("/mu-query/test-mu-query-logic", test_mu_query_logic);

	g_test_add_func("/mu-query/test-mu-query-accented-chars-1",
			test_mu_query_accented_chars_01);
	g_test_add_func("/mu-query/test-mu-query-accented-chars-2",
			test_mu_query_accented_chars_02);
	g_test_add_func("/mu-query/test-mu-query-accented-chars-fraiche",
			test_mu_query_accented_chars_fraiche);

	g_test_add_func("/mu-query/test-mu-query-msgid", test_mu_query_msgid);

	g_test_add_func("/mu-query/test-mu-query-wom-bat", test_mu_query_wom_bat);

	g_test_add_func("/mu-query/test-mu-query-wildcards", test_mu_query_wildcards);
	g_test_add_func("/mu-query/test-mu-query-sizes", test_mu_query_sizes);

	g_test_add_func("/mu-query/test-mu-query-dates-helsinki", test_mu_query_dates_helsinki);
	g_test_add_func("/mu-query/test-mu-query-dates-sydney", test_mu_query_dates_sydney);
	g_test_add_func("/mu-query/test-mu-query-dates-la", test_mu_query_dates_la);

	g_test_add_func("/mu-query/test-mu-query-attach", test_mu_query_attach);
	g_test_add_func("/mu-query/test-mu-query-tags", test_mu_query_tags);
	g_test_add_func("/mu-query/test-mu-query-tags_02", test_mu_query_tags_02);

	g_test_add_func("/mu-query/test-mu-query-threads-compilation-error",
			test_mu_query_threads_compilation_error);

	return g_test_run();
}
