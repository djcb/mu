/*
** Copyright (C) 2008-2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <glib/gstdio.h>

#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include "test-mu-common.hh"
#include "mu-query.hh"
#include "utils/mu-str.h"
#include "utils/mu-utils.hh"

using namespace Mu;

struct tinfo {
        std::string threadpath;
        std::string msgid;
        std::string subject;
};

static void
assert_tinfo_equal (const tinfo& expected, const tinfo& actual)
{
        assert_equal (expected.threadpath, actual.threadpath);
        assert_equal (expected.subject, actual.subject);
        assert_equal (expected.msgid, actual.msgid);
}

static void
tinfo_init_from_item (tinfo& item, QueryResults::const_iterator& it)
{
        const auto& qmatch{it.query_match()};

        item.threadpath = qmatch.thread_path;
        item.subject    = qmatch.sort_key;
        item.msgid      = *it.message_id();

        if (g_test_verbose())
                g_print ("%s %s %s\n",
                         item.threadpath, item.subject, item.msgid);

}

static void
foreach_assert_tinfo_equal (QueryResults& qres,
                            const tinfo items[], guint n_items)
{
        size_t u{};
        for (auto&& item: qres) {
                tinfo ti{};
                tinfo_init_from_item (ti, item);
                assert_tinfo_equal (items[u], ti);
                ++u;
        }
        g_assert_cmpuint(u, ==, n_items);
}

static std::string
make_database (const std::string& testdir)
{
        char *tmpdir{test_mu_common_get_random_tmpdir()};
        const auto cmdline{
                format("/bin/sh -c '"
                       "%s init  --muhome=%s --maildir=%s --quiet ; "
                       "%s index --muhome=%s  --quiet'",
                       MU_PROGRAM,  tmpdir, testdir.c_str(),
                       MU_PROGRAM,  tmpdir)};


        if (g_test_verbose())
                g_printerr ("\n%s\n", cmdline.c_str());

        g_assert (g_spawn_command_line_sync (cmdline.c_str(), NULL, NULL,
                                             NULL, NULL));
        auto xpath= g_strdup_printf ("%s%c%s", tmpdir, G_DIR_SEPARATOR, "xapian");
        g_free (tmpdir);

        std::string dbpath{xpath};
        g_free(xpath);

        return dbpath;
}



/* note: this also *moves the iter* */
static QueryResults
run_and_get_results_full (const std::string& xpath, const std::string& expr,
                          MuMsgFieldId sort_field,
                          Mu::QueryFlags flags=Mu::QueryFlags::None)
{
        Mu::Store store{xpath};
        Mu::Query q{store};

        const auto myflags{flags | Mu::QueryFlags::Threading};
        auto res{q.run(expr, sort_field, myflags)};
        g_assert(!!res);

        return *res;
}

static QueryResults
run_and_get_results (const std::string& xpath, const char *query)
{
        return run_and_get_results_full (xpath, query, MU_MSG_FIELD_ID_SUBJECT);
}

static void
test_mu_threads_01 (void)
{
        const tinfo items [] = {
                {"0",   "root0@msg.id",  "root0"},
                {"0:0", "child0.0@msg.id", "Re: child 0.0"},
                {"0:1",   "child0.1@msg.id", "Re: child 0.1"},
                {"0:1:0", "child0.1.0@msg.id", "Re: child 0.1.0"},
                {"1",   "root1@msg.id", "root1"},
                {"2",   "root2@msg.id", "root2"},
                /* next one's been promoted 2.0.0 => 2.0 */
                {"2:0", "child2.0.0@msg.id", "Re: child 2.0.0"},
                /* next one's been promoted 3.0.0.0.0 => 3 */
                {"3", "child3.0.0.0.0@msg.id", "Re: child 3.0.0.0"},

                /* two children of absent root 4.0 */
                {"4:0", "child4.0@msg.id", "Re: child 4.0"},
                {"4:1", "child4.1@msg.id", "Re: child 4.1"}
        };

        const auto xpath{make_database(MU_TESTMAILDIR3)};
        g_assert (!xpath.empty());

        auto qres{run_and_get_results (xpath, "abc")};
        foreach_assert_tinfo_equal (qres, items, G_N_ELEMENTS (items));
}

static void
test_mu_threads_rogue (void)
{
        tinfo items1 [] = {
                {"0",     "cycle0@msg.id",     "cycle0"},
                {"0:0",   "cycle0.0@msg.id",   "cycle0.0"},
                {"0:0:0", "cycle0.0.0@msg.id", "cycle0.0.0"},
                {"0:1",   "rogue0@msg.id",     "rogue0"},
        };

        tinfo items2 [] = {
                {"0",     "cycle0.0@msg.id",   "cycle0.0"},
                {"0:0",   "cycle0@msg.id",     "cycle0"},
                {"0:0:0", "rogue0@msg.id",     "rogue0" },
                {"0:1",   "cycle0.0.0@msg.id", "cycle0.0.0"}
        };

        const auto xpath{make_database (MU_TESTMAILDIR3)};
        auto res{run_and_get_results (xpath, "def")};

        /* due to the random order in files can be indexed, there are two possible ways
         * for the threads to be built-up; both are okay */
        auto& items =  (res.begin().message_id() ==  "cycle0@msg.id") ? items1 : items2;

        foreach_assert_tinfo_equal (res, items, G_N_ELEMENTS (items1));
}

static QueryResults
query_testdir (const std::string& query, MuMsgFieldId sort_field,  gboolean descending)
{
        const auto flags{descending ? QueryFlags::Descending : QueryFlags::None};
        const auto xpath{make_database(MU_TESTMAILDIR3)};
        g_assert_false (xpath.empty());

        auto qres{run_and_get_results_full (xpath, query, sort_field, flags)};
        g_assert_false (qres.empty());

        return qres;
}

static void
check_sort_by_subject (const std::string& query,
                       const tinfo expected[],
                       guint n_expected, gboolean descending)
{

        auto qres{query_testdir (query, MU_MSG_FIELD_ID_SUBJECT, descending)};
        foreach_assert_tinfo_equal (qres, expected, n_expected);
}

static void
check_sort_by_subject_asc (const char *query, const tinfo expected[],
                           guint n_expected)
{
        check_sort_by_subject (query, expected, n_expected, FALSE);
}

static void
check_sort_by_subject_desc (const char *query, const tinfo expected[],
                            guint n_expected)
{
        check_sort_by_subject (query, expected, n_expected, TRUE);
}

static void
test_mu_threads_sort_1st_child_promotes_thread (void)
{
        const char *query = "maildir:/sort/1st-child-promotes-thread";

        const tinfo expected_asc [] = {
                { "0", "A@msg.id", "A"},
                { "1", "C@msg.id", "C"},
                { "2", "B@msg.id", "B"},
                { "2:0", "D@msg.id", "D"},
        };
        const tinfo expected_desc [] = {
                { "0", "B@msg.id", "B"},
                { "0:0", "D@msg.id", "D"},
                { "1", "C@msg.id", "C"},
                { "2", "A@msg.id", "A"},
        };

        check_sort_by_subject_asc (query, expected_asc,
                                   G_N_ELEMENTS (expected_asc));
        check_sort_by_subject_desc (query, expected_desc,
                                    G_N_ELEMENTS (expected_desc));
}

static void
test_mu_threads_sort_2nd_child_promotes_thread (void)
{
        const char *query = "maildir:/sort/2nd-child-promotes-thread";

        const tinfo expected_asc [] = {
                { "0", "A@msg.id", "A"},
                { "1", "D@msg.id", "D"},
                { "2", "B@msg.id", "B"},
                { "2:0", "C@msg.id", "C"},
                { "2:1", "E@msg.id", "E"},
        };
        const tinfo expected_desc [] = {
                { "0", "B@msg.id", "B"},
                { "0:0", "C@msg.id", "C"},
                { "0:1", "E@msg.id", "E"},
                { "1", "D@msg.id", "D"},
                { "2", "A@msg.id", "A"},
        };

        check_sort_by_subject_asc (query, expected_asc,
                                   G_N_ELEMENTS (expected_asc));
        check_sort_by_subject_desc (query, expected_desc,
                                    G_N_ELEMENTS (expected_desc));
}

static void
test_mu_threads_sort_orphan_promotes_thread (void)
{
        const char *query = "maildir:/sort/2nd-child-promotes-thread NOT B";

        /* B lost, C & E orphaned but not promoted */
        const tinfo expected_asc [] = {
                { "0", "A@msg.id", "A"},
                { "1", "D@msg.id", "D"},
                { "2:0", "C@msg.id", "C"},
                { "2:1", "E@msg.id", "E"},
        };
        const tinfo expected_desc [] = {
                { "0:0", "C@msg.id", "C"},
                { "0:1", "E@msg.id", "E"},
                { "1", "D@msg.id", "D"},
                { "2", "A@msg.id", "A"},
        };

        check_sort_by_subject_asc (query, expected_asc,
                                   G_N_ELEMENTS (expected_asc));
        check_sort_by_subject_desc (query, expected_desc,
                                    G_N_ELEMENTS (expected_desc));
}

/* Won't normally happen when sorting by date. */
static void
test_mu_threads_sort_child_does_not_promote_thread (void)
{
        const char *query = "maildir:/sort/child-does-not-promote-thread";

        const tinfo expected_asc [] = {
                { "0", "Y@msg.id", "Y"},
                { "0:0", "A@msg.id", "A"},
                { "1", "X@msg.id", "X"},
                { "2", "Z@msg.id", "Z"},
        };
        const tinfo expected_desc [] = {
                { "0", "Z@msg.id", "Z"},
                { "1", "X@msg.id", "X"},
                { "2", "Y@msg.id", "Y"},
                { "2:0", "A@msg.id", "A"},
        };

        check_sort_by_subject_asc (query, expected_asc,
                                   G_N_ELEMENTS (expected_asc));
        check_sort_by_subject_desc (query, expected_desc,
                                    G_N_ELEMENTS (expected_desc));
}

static void
test_mu_threads_sort_grandchild_promotes_thread (void)
{
        const char *query = "maildir:/sort/grandchild-promotes-thread";

        const tinfo expected_asc [] = {
                { "0", "A@msg.id", "A"},
                { "1", "D@msg.id", "D"},
                { "2", "B@msg.id", "B"},
                { "2:0", "C@msg.id", "C"},
                { "2:0:0", "E@msg.id", "E"},
        };
        const tinfo expected_desc [] = {
                { "0", "B@msg.id", "B"},
                { "0:0", "C@msg.id", "C"},
                { "0:0:0", "E@msg.id", "E"},
                { "1", "D@msg.id", "D"},
                { "2", "A@msg.id", "A"},
        };

        check_sort_by_subject_asc (query, expected_asc,
                                   G_N_ELEMENTS (expected_asc));
        check_sort_by_subject_desc (query, expected_desc,
                                    G_N_ELEMENTS (expected_desc));
}

static void
test_mu_threads_sort_granchild_promotes_only_subthread (void)
{
        const char *query = "maildir:/sort/grandchild-promotes-only-subthread";

        const tinfo expected_asc [] = {
                { "0", "A@msg.id", "A"},
                { "1", "B@msg.id", "B"},
                { "1:0", "C@msg.id", "C"},
                { "1:1", "D@msg.id", "D"},
                { "1:1:0", "F@msg.id", "F"},
                { "1:2", "E@msg.id", "E"},
                { "2", "G@msg.id", "G"},
        };
        const tinfo expected_desc [] = {
                { "0", "G@msg.id", "G"},
                { "1", "B@msg.id", "B"},
                { "1:0", "C@msg.id", "C"},
                { "1:1", "D@msg.id", "D"},
                { "1:1:0", "F@msg.id", "F"},
                { "1:2", "E@msg.id", "E"},
                { "2", "A@msg.id", "A"},
        };

        check_sort_by_subject_asc (query, expected_asc,
                                   G_N_ELEMENTS (expected_asc));
        check_sort_by_subject_desc (query, expected_desc,
                                    G_N_ELEMENTS (expected_desc));
}
int
main (int argc, char *argv[])
{
        int rv;

        g_test_init (&argc, &argv, NULL);

        g_test_add_func ("/mu-query/test-mu-threads-01", test_mu_threads_01);
        g_test_add_func ("/mu-query/test-mu-threads-rogue", test_mu_threads_rogue);

        g_test_add_func ("/mu-query/test-mu-threads-sort-1st-child-promotes-thread",
                         test_mu_threads_sort_1st_child_promotes_thread);
        g_test_add_func ("/mu-query/test-mu-threads-sort-2nd-child-promotes-thread",
                         test_mu_threads_sort_2nd_child_promotes_thread);
        g_test_add_func ("/mu-query/test-mu-threads-orphan-promotes-thread",
                         test_mu_threads_sort_orphan_promotes_thread);
        g_test_add_func ("/mu-query/test-mu-threads-sort-child-does-not-promote-thread",
                         test_mu_threads_sort_child_does_not_promote_thread);
        g_test_add_func ("/mu-query/test-mu-threads-sort-grandchild-promotes-thread",
                         test_mu_threads_sort_grandchild_promotes_thread);
        g_test_add_func ("/mu-query/test-mu-threads-sort-grandchild-promotes-only-subthread",
                         test_mu_threads_sort_granchild_promotes_only_subthread);

        g_log_set_handler (NULL,
                           (GLogLevelFlags)(G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL|
                                           G_LOG_FLAG_RECURSION),
                           (GLogFunc)black_hole, NULL);

        rv = g_test_run ();

        return rv;
}
