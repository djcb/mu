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

#include "mu-query-results.hh"
#include "mu-query-match-deciders.hh"
#include "mu-query-threads.hh"
#include <mu-xapian.hh>
#include "utils/mu-xapian-utils.hh"

using namespace Mu;

struct Query::Private {
	Private(const Store& store) : store_{store}, parser_{store_} {}
	// New
	// bool calculate_threads (Xapian::Enquire& enq, size maxnum);

	Xapian::Enquire make_enquire(const std::string& expr, Field::Id sortfield_id,
				     QueryFlags qflags) const;
	Xapian::Enquire make_related_enquire(const StringSet& thread_ids,
					     Field::Id sortfield_id,
					     QueryFlags qflags) const;

	Option<QueryResults> run_threaded(QueryResults&& qres, Xapian::Enquire& enq,
					  QueryFlags qflags, size_t max_size) const;
	Option<QueryResults> run_singular(const std::string&       expr,
					  Field::Id sortfield_id,
					  QueryFlags qflags, size_t maxnum) const;
	Option<QueryResults> run_related(const std::string&       expr,
					 Field::Id sortfield_id,
					 QueryFlags qflags, size_t maxnum) const;

	Option<QueryResults> run(const std::string& expr,
				 Field::Id sortfield_id, QueryFlags qflags,
				 size_t maxnum) const;

	size_t store_size() const { return store_.database().get_doccount(); }

	const Store& store_;
	const Parser parser_;
};

Query::Query(const Store& store) : priv_{std::make_unique<Private>(store)} {}

Query::Query(Query&& other) = default;

Query::~Query() = default;

static Xapian::Enquire&
sort_enquire(Xapian::Enquire& enq, Field::Id sortfield_id, QueryFlags qflags)
{
	const auto value_no{field_from_id(sortfield_id).value_no()};
	enq.set_sort_by_value(value_no, any_of(qflags & QueryFlags::Descending));

	return enq;
}

Xapian::Enquire
Query::Private::make_enquire(const std::string& expr,
			     Field::Id		sortfield_id,
			     QueryFlags         qflags) const
{
	Xapian::Enquire enq{store_.database()};

	if (expr.empty() || expr == R"("")")
		enq.set_query(Xapian::Query::MatchAll);
	else {
		WarningVec warns;
		const auto tree{parser_.parse(expr, warns)};
		for (auto&& w : warns)
			g_warning("query warning: %s", to_string(w).c_str());
		enq.set_query(xapian_query(tree));
		g_debug("qtree: %s", to_string(tree).c_str());
	}

	sort_enquire(enq, sortfield_id, qflags);

	return enq;
}

Xapian::Enquire
Query::Private::make_related_enquire(const StringSet& thread_ids,
				     Field::Id sortfield_id,
				     QueryFlags qflags) const
{
	Xapian::Enquire            enq{store_.database()};
	std::vector<Xapian::Query> qvec;
	for (auto&& t : thread_ids) {
		qvec.emplace_back(field_from_id(Field::Id::ThreadId).xapian_term(t));
                // The first email in a thread might not have a References
                // header, or might have References header with bogus value
                // (since it is the first email in the thread). In such case,
                // the Message-ID header can be used to lookup the this first
                // email.
                qvec.emplace_back(field_from_id(Field::Id::MessageId).xapian_term(t));
        }

	Xapian::Query qr{Xapian::Query::OP_OR, qvec.begin(), qvec.end()};
	enq.set_query(qr);

	sort_enquire(enq, sortfield_id, qflags);

	return enq;
}

struct ThreadKeyMaker : public Xapian::KeyMaker {
	ThreadKeyMaker(const QueryMatches& matches) : match_info_(matches) {}
	std::string operator()(const Xapian::Document& doc) const override
	{
		const auto it{match_info_.find(doc.get_docid())};
		return (it == match_info_.end()) ? "" : it->second.thread_path;
	}
	const QueryMatches& match_info_;
};

Option<QueryResults>
Query::Private::run_threaded(QueryResults&& qres, Xapian::Enquire& enq, QueryFlags qflags,
			     size_t maxnum) const
{
	const auto descending{any_of(qflags & QueryFlags::Descending)};

	calculate_threads(qres, descending);

	ThreadKeyMaker key_maker{qres.query_matches()};
	enq.set_sort_by_key(&key_maker, descending);

	DeciderInfo minfo;
	minfo.matches = qres.query_matches();
	auto mset{enq.get_mset(0, maxnum, {}, make_thread_decider(qflags, minfo).get())};
	mset.fetch();

	return QueryResults{mset, std::move(qres.query_matches())};
}

Option<QueryResults>
Query::Private::run_singular(const std::string& expr,
			     Field::Id sortfield_id,
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
#pragma GCC diagnostic ignored "-Wextra"
	auto enq{make_enquire(expr, threading ? Field::Id::Date : sortfield_id, qflags)};
#pragma GCC diagnostic ignored "-Wswitch-default"
#pragma GCC diagnostic pop
	auto mset{enq.get_mset(0, maxnum, {},
			       make_leader_decider(singular_qflags, minfo).get())};
	mset.fetch();

	auto qres{QueryResults{mset, std::move(minfo.matches)}};

	return threading ? run_threaded(std::move(qres), enq, qflags, maxnum) : qres;
}

static Option<std::string>
opt_string(const Xapian::Document& doc, Field::Id id) noexcept
{
	const auto  value_no{field_from_id(id).value_no()};
	std::string val =
	    xapian_try([&] { return doc.get_value(value_no); }, std::string{""});
	if (val.empty())
		return Nothing;
	else
		return Some(std::move(val));
}

Option<QueryResults>
Query::Private::run_related(const std::string& expr,
			    Field::Id sortfield_id,
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
	auto        enq{make_enquire(expr, Field::Id::Date, leader_qflags)};
	const auto  mset{
	    enq.get_mset(0, maxnum, {}, make_leader_decider(leader_qflags, minfo).get())};

	// Gather the thread-ids we found
	mset.fetch();
	for (auto it = mset.begin(); it != mset.end(); ++it) {
		auto thread_id{opt_string(it.get_document(), Field::Id::ThreadId)};
		if (thread_id)
			minfo.thread_ids.emplace(std::move(*thread_id));
	}

	// Now, determine the "related query".
	//
	// In the threaded-case, we search among _all_ messages, since complete
	// threads are preferred; no need to sort in that case since the search
	// is unlimited and the sorting happens during threading.
	auto r_enq = std::invoke([&]{
		if (threading)
			return make_related_enquire(minfo.thread_ids, Field::Id::Date,
						    qflags );
		else
			return make_related_enquire(minfo.thread_ids, sortfield_id, qflags);
	});

	const auto r_mset{r_enq.get_mset(0, threading ? store_size() : maxnum, {},
					 make_related_decider(qflags, minfo).get())};
	auto       qres{QueryResults{r_mset, std::move(minfo.matches)}};
	return threading ? run_threaded(std::move(qres), r_enq, qflags, maxnum) : qres;
}

Option<QueryResults>
Query::Private::run(const std::string&  expr, Field::Id sortfield_id, QueryFlags qflags,
		    size_t maxnum) const
{
	const auto eff_maxnum{maxnum == 0 ? store_size() : maxnum};

	if (any_of(qflags & QueryFlags::IncludeRelated))
		return run_related(expr, sortfield_id, qflags, eff_maxnum);
	else
		return run_singular(expr, sortfield_id, qflags, eff_maxnum);
}

Result<QueryResults>
Query::run(const std::string& expr, Field::Id sortfield_id,
	   QueryFlags qflags, size_t maxnum) const
{
	// some flags are for internal use only.
	g_return_val_if_fail(none_of(qflags & QueryFlags::Leader),
			     Err(Error::Code::InvalidArgument, "cannot pass Leader flag"));

	StopWatch sw{format(
	    "ran query '%s'; related: %s; threads: %s; max-size: %zu", expr.c_str(),
	    any_of(qflags & QueryFlags::IncludeRelated) ? "yes" : "no",
	    any_of(qflags & QueryFlags::Threading) ? "yes" : "no", maxnum)};

	return xapian_try_result([&]{
		if (auto&& res = priv_->run(expr, sortfield_id, qflags, maxnum); res)
			return Result<QueryResults>(Ok(std::move(res.value())));
		else
			return Result<QueryResults>(Err(Error::Code::Query,
							"failed to run query"));
	});

}

size_t
Query::count(const std::string& expr) const
{
	return xapian_try(
	    [&] {
		    const auto enq{priv_->make_enquire(expr, {}, {})};
		    auto       mset{enq.get_mset(0, priv_->store_size())};
		    mset.fetch();
		    return mset.size();
	    },
	    0);
}

/* LCOV_EXCL_START*/
std::string
Query::parse(const std::string& expr, bool xapian) const
{
	WarningVec warns;
	const auto tree{priv_->parser_.parse(expr, warns)};
	for (auto&& w : warns)
		g_warning("query warning: %s", to_string(w).c_str());

	if (xapian)
		return xapian_query(tree).get_description();
	else
		return to_string(tree);
}
/* LCOV_EXCL_STOP*/
