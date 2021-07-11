/*
** Copyright (C) 2008-2021 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 3 of the License, or
** (at your option) any later version.
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
#include <mu-query.hh>

#include <stdexcept>
#include <string>
#include <cctype>
#include <cstring>
#include <sstream>
#include <cmath>

#include <stdlib.h>
#include <xapian.h>
#include <glib/gstdio.h>

#include "mu-msg-fields.h"
#include "mu-query-results.hh"
#include "mu-query-match-deciders.hh"
#include "mu-query-threads.hh"
#include <mu-xapian.hh>

using namespace Mu;

struct Query::Private {
        Private(const Store& store): store_{store},
                                     parser_{store_} {}
        // New
        //bool calculate_threads (Xapian::Enquire& enq, size maxnum);

        Xapian::Enquire make_enquire (const std::string& expr,
                                      MuMsgFieldId sortfieldid, QueryFlags qflags) const;
        Xapian::Enquire make_related_enquire (const StringSet& thread_ids,
                                              MuMsgFieldId sortfieldid, QueryFlags qflags) const;

        Option<QueryResults> run_threaded (QueryResults&& qres, Xapian::Enquire& enq,
                                           QueryFlags qflags) const;
        Option<QueryResults> run_singular (const std::string& expr, MuMsgFieldId sortfieldid,
                                           QueryFlags qflags, size_t maxnum) const;
        Option<QueryResults> run_related (const std::string& expr, MuMsgFieldId sortfieldid,
                                          QueryFlags qflags, size_t maxnum) const;
        Option<QueryResults> run (const std::string& expr, MuMsgFieldId sortfieldid,
                                  QueryFlags qflags, size_t maxnum) const;

        const Store& store_;
        const Parser parser_;
};

Query::Query(const Store& store):
        priv_{std::make_unique<Private>(store)}
{}

Query::Query(Query&& other) = default;

Query::~Query() = default;


static Xapian::Enquire&
maybe_sort (Xapian::Enquire& enq, MuMsgFieldId sortfieldid, QueryFlags qflags)
{
        if (sortfieldid != MU_MSG_FIELD_ID_NONE)
                enq.set_sort_by_value(static_cast<Xapian::valueno>(sortfieldid),
                                      any_of(qflags & QueryFlags::Descending));
        return enq;
}

Xapian::Enquire
Query::Private::make_enquire (const std::string& expr,
                              MuMsgFieldId sortfieldid, QueryFlags qflags) const
{
        Xapian::Enquire enq{store_.database()};

        if (expr.empty() || expr == R"("")")
                enq.set_query(Xapian::Query::MatchAll);
        else {
                WarningVec warns;
                const auto tree{parser_.parse(expr, warns)};
                for (auto&& w: warns)
                        g_warning ("query warning: %s", to_string(w).c_str());
                enq.set_query(xapian_query(tree));
                g_debug ("qtree: %s", to_string(tree).c_str());
        }

        return maybe_sort (enq, sortfieldid, qflags);
}

Xapian::Enquire
Query::Private::make_related_enquire (const StringSet& thread_ids,
                                      MuMsgFieldId sortfieldid, QueryFlags qflags)  const
{
        Xapian::Enquire enq{store_.database()};
        static std::string pfx (1, mu_msg_field_xapian_prefix(MU_MSG_FIELD_ID_THREAD_ID));

        std::vector<Xapian::Query> qvec;
        for (auto&& t: thread_ids)
                qvec.emplace_back(pfx + t);
        Xapian::Query qr{Xapian::Query::OP_OR, qvec.begin(), qvec.end()};
        enq.set_query(qr);

        return maybe_sort (enq, sortfieldid, qflags);

}

struct ThreadKeyMaker: public Xapian::KeyMaker {
        ThreadKeyMaker (const QueryMatches& matches): match_info_(matches) {}
        std::string operator()(const Xapian::Document& doc) const override {
                const auto it{match_info_.find(doc.get_docid())};
                return (it == match_info_.end()) ? "" :  it->second.thread_path;
        }
        const QueryMatches& match_info_;
};

Option<QueryResults>
Query::Private::run_threaded (QueryResults&& qres, Xapian::Enquire& enq,
                              QueryFlags qflags) const
{
        const auto descending{any_of(qflags & QueryFlags::Descending)};

        calculate_threads(qres, descending);

        ThreadKeyMaker key_maker{qres.query_matches()};
        enq.set_sort_by_key(&key_maker, descending);

        DeciderInfo minfo;
        minfo.matches = qres.query_matches();
        auto mset{enq.get_mset(0, store_.size(), {},
                               make_thread_decider(qflags, minfo).get())};
        mset.fetch();

        return QueryResults{mset, std::move(qres.query_matches())};
}


Option<QueryResults>
Query::Private::run_singular (const std::string& expr, MuMsgFieldId sortfieldid,
                              QueryFlags qflags, size_t maxnum) const
{
        // i.e. a query _without_ related messages, but still possibly
        // with threading.
        //
        // In the threading case, the sortfield-id is ignored, we always sort by
        // date (since threading the threading results are always by date.)

        const auto singular_qflags{qflags | QueryFlags::Leader};
        const auto threading{any_of(qflags & QueryFlags::Threading)};

        DeciderInfo minfo{};
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored   "-Wextra"
        auto enq{make_enquire(expr, threading ? MU_MSG_FIELD_ID_DATE : sortfieldid, qflags)};
        #pragma GCC diagnostic ignored "-Wswitch-default"
#pragma GCC diagnostic pop
        auto mset{enq.get_mset(0, maxnum, {}, make_leader_decider(singular_qflags, minfo).get())};
        mset.fetch();

        auto qres{QueryResults{mset, std::move(minfo.matches)}};

        return threading ? run_threaded(std::move(qres), enq, qflags) : qres;
}

static Option<std::string>
opt_string(const Xapian::Document& doc, MuMsgFieldId id) noexcept try {
        auto&& val{doc.get_value(id)};
        return val.empty() ? Nothing : Some(val);
} MU_XAPIAN_CATCH_BLOCK_RETURN (Nothing);

Option<QueryResults>
Query::Private::run_related (const std::string& expr, MuMsgFieldId sortfieldid,
                             QueryFlags qflags, size_t maxnum) const
{
        // i.e. a query _with_ related messages and possibly with threading.
        //
        // In the threading case, the sortfield-id is ignored, we always sort by
        // date (since threading the threading results are always by date.);
        // moreover, in either threaded or non-threaded case, we sort the first
        // ("leader") query by date, i.e, we prefer the newest or oldest
        // (descending) messages.
        const auto leader_qflags{QueryFlags::Leader | qflags};
        const auto threading{any_of(qflags & QueryFlags::Threading)};

        // Run our first, "leader" query
        DeciderInfo minfo{};
        auto enq{make_enquire(expr, MU_MSG_FIELD_ID_DATE, leader_qflags)};
        const auto mset{enq.get_mset(0, maxnum, {},
                                     make_leader_decider(leader_qflags, minfo).get())};

        // Gather the thread-ids we found
        mset.fetch();
        for (auto it = mset.begin(); it != mset.end(); ++it) {
                auto thread_id{opt_string(it.get_document(), MU_MSG_FIELD_ID_THREAD_ID)};
                if (thread_id)
                        minfo.thread_ids.emplace(std::move(*thread_id));
        }

        // Now, determine the "related query".
        //
        // In the threaded-case, we search among _all_ messages, since complete
        // threads are preferred; no need to sort in that case since the search
        // is unlimited and the sorting happens during threading.
        auto r_enq{make_related_enquire(minfo.thread_ids,
                                        threading ? MU_MSG_FIELD_ID_NONE : sortfieldid, qflags)};
        const auto r_mset{r_enq.get_mset(0, threading ? store_.size() : maxnum,
                                         {}, make_related_decider(qflags, minfo).get())};
        auto qres{QueryResults{r_mset, std::move(minfo.matches)}};
        return threading ? run_threaded(std::move(qres), r_enq, qflags) : qres;
}


Option<QueryResults>
Query::Private::run (const std::string& expr, MuMsgFieldId sortfieldid,
                     QueryFlags qflags, size_t maxnum) const
{
        const auto eff_maxnum{maxnum == 0 ? store_.size() : maxnum};
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored   "-Wextra"
        const auto eff_sortfield{sortfieldid == MU_MSG_FIELD_ID_NONE ?
                MU_MSG_FIELD_ID_DATE : sortfieldid };
#pragma GCC diagnostic pop
        if (any_of(qflags & QueryFlags::IncludeRelated))
                return run_related (expr, eff_sortfield, qflags, eff_maxnum);
        else
                return run_singular(expr, eff_sortfield, qflags, eff_maxnum);
}


Option<QueryResults>
Query::run (const std::string& expr, MuMsgFieldId sortfieldid,
           QueryFlags qflags, size_t maxnum) const try
{
        // some flags are for internal use only.
        g_return_val_if_fail (none_of(qflags & QueryFlags::Leader), Nothing);

        StopWatch sw{format("ran query '%s'; related: %s; threads: %s; max-size: %zu",
                            expr.c_str(),
                            any_of(qflags & QueryFlags::IncludeRelated) ? "yes" : "no",
                            any_of(qflags & QueryFlags::Threading) ? "yes" : "no",
                            maxnum)};

        return priv_->run(expr, sortfieldid, qflags, maxnum);

} catch (...) {
        return Nothing;
}


size_t
Query::count (const std::string& expr) const try
{
        const auto enq{priv_->make_enquire(expr, MU_MSG_FIELD_ID_NONE, {})};
        auto mset{enq.get_mset(0, priv_->store_.size())};
        mset.fetch();

        return mset.size();

}MU_XAPIAN_CATCH_BLOCK_RETURN (0);



std::string
Query::parse (const std::string& expr, bool xapian) const
{
        WarningVec warns;
        const auto tree{priv_->parser_.parse(expr, warns)};
        for (auto&& w: warns)
                g_warning ("query warning: %s", to_string(w).c_str());

        if (xapian)
                return xapian_query(tree).get_description();
        else
                return to_string(tree);
}
