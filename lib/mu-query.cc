/*
** Copyright (C) 2008-2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <stdlib.h>
#include <xapian.h>
#include <glib/gstdio.h>

#include "mu-msg-fields.h"

#include "mu-msg-iter.h"

#include "utils/mu-str.h"
#include "utils/mu-date.h"
#include <utils/mu-utils.hh>

#include <mu-xapian.hh>

using namespace Mu;

struct Query::Private {
        Private(const Store& store): store_{store},
                                     parser_{store_} {}

        Xapian::Query   make_query (const std::string& expr, GError **err) const;
        Xapian::Enquire make_enquire (const std::string& expr, MuMsgFieldId sortfieldid,
                                      bool descending, GError **err) const;
        GHashTable* find_thread_ids (MuMsgIter *iter, GHashTable **orig_set) const;

        Xapian::Query make_related_query (MuMsgIter *iter, GHashTable **orig_set) const;

        void find_related_messages (MuMsgIter **iter, int maxnum,
                                    MuMsgFieldId sortfieldid, Query::Flags flags,
                                    Xapian::Query orig_query) const;

        const Store& store_;
        const Parser parser_;
};


static constexpr MuMsgIterFlags
msg_iter_flags (Query::Flags flags)
{
	MuMsgIterFlags iflags{MU_MSG_ITER_FLAG_NONE};

	if (any_of(flags & Query::Flags::Descending))
		iflags |= MU_MSG_ITER_FLAG_DESCENDING;
	if (any_of(flags & Query::Flags::SkipUnreadable))
		iflags |= MU_MSG_ITER_FLAG_SKIP_UNREADABLE;
	if (any_of(flags & Query::Flags::SkipDups))
		iflags |= MU_MSG_ITER_FLAG_SKIP_DUPS;
	if (any_of(flags & Query::Flags::Threading))
		iflags |= MU_MSG_ITER_FLAG_THREADS;

	return iflags;
}

Xapian::Query
Query::Private::make_query (const std::string& expr, GError **err) const try {

	Mu::WarningVec warns;
        const auto tree{parser_.parse(expr, warns)};
	for (auto&& w: warns)
                g_warning ("query warning: %s", to_string(w).c_str());

	return Mu::xapian_query (tree);

} catch (...) {
	mu_util_g_set_error (err, MU_ERROR_XAPIAN_QUERY,
			     "parse error in query");
	throw;
}


Xapian::Enquire
Query::Private::make_enquire (const std::string& expr, MuMsgFieldId sortfieldid,
                              bool descending, GError **err) const
{
	Xapian::Enquire enq{store_.database()};

        try {
		if (!expr.empty() && expr != R"("")")
                        enq.set_query(make_query (expr, err));
		else/* empty or "" means "matchall" */
			enq.set_query(Xapian::Query::MatchAll);
	} catch (...) {
		mu_util_g_set_error (err, MU_ERROR_XAPIAN_QUERY, "parse error in query");
		throw;
	}

	enq.set_cutoff(0,0);

        return enq;
}

/*
 * record all thread-ids for the messages; also 'orig_set' receives all
 * original matches (a map msgid-->docid), so we can make sure the
 * originals are not seen as 'duplicates' later (when skipping
 * duplicates).  We want to favor the originals over the related
 * messages, when skipping duplicates.
 */
GHashTable*
Query::Private::find_thread_ids (MuMsgIter *iter, GHashTable **orig_set) const
{
	GHashTable *ids;

	ids	  = g_hash_table_new_full (g_str_hash, g_str_equal,
					   (GDestroyNotify)g_free, NULL);
	*orig_set = g_hash_table_new_full (g_str_hash, g_str_equal,
					   (GDestroyNotify)g_free, NULL);

	while (!mu_msg_iter_is_done (iter)) {
		char		*thread_id, *msgid;
		unsigned	 docid;
		/* record the thread id for the message */
		if ((thread_id = mu_msg_iter_get_thread_id (iter)))
			g_hash_table_insert (ids, thread_id,
					     GSIZE_TO_POINTER(TRUE));
		/* record the original set */
		docid = mu_msg_iter_get_docid(iter);
		if (docid != 0 && (msgid = mu_msg_iter_get_msgid (iter)))
			g_hash_table_insert (*orig_set, msgid,
					     GSIZE_TO_POINTER(docid));

		if (!mu_msg_iter_next (iter))
			break;
	}

	return ids;
}


Xapian::Query
Query::Private::make_related_query (MuMsgIter *iter, GHashTable **orig_set) const
{
	GHashTable *hash;
	GList *id_list, *cur;
	std::vector<Xapian::Query> qvec;
	static std::string pfx (1, mu_msg_field_xapian_prefix
				(MU_MSG_FIELD_ID_THREAD_ID));

	/* orig_set receives the hash msgid->docid of the set of
	 * original matches */
	hash = find_thread_ids (iter, orig_set);
	/* id_list now gets a list of all thread-ids seen in the query
	 * results; either in the Message-Id field or in
	 * References. */
	id_list = g_hash_table_get_keys (hash);

	// now, we create a vector with queries for each of the
	// thread-ids, which we combine below. This is /much/ faster
	// than creating the query as 'query = Query (OR, query)'...
	for (cur = id_list; cur; cur = g_list_next(cur))
		qvec.push_back (Xapian::Query((std::string
					       (pfx + (char*)cur->data))));

	g_hash_table_destroy (hash);
	g_list_free (id_list);

	return Xapian::Query (Xapian::Query::OP_OR, qvec.begin(), qvec.end());
}


void
Query::Private::find_related_messages (MuMsgIter **iter, int maxnum,
                                       MuMsgFieldId sortfieldid, Query::Flags flags,
                                       Xapian::Query orig_query) const
{
	GHashTable *orig_set;
	Xapian::Enquire enq{store_.database()};
	MuMsgIter *rel_iter;
	const bool inc_related{any_of(flags & Query::Flags::IncludeRelated)};

	orig_set = NULL;
	Xapian::Query new_query{make_related_query (*iter, &orig_set)};
	/* If related message are not desired, filter out messages which would not
	   have matched the original query.
	 */
	if (!inc_related)
	    new_query = Xapian::Query (Xapian::Query::OP_AND, orig_query, new_query);
	enq.set_query(new_query);
	enq.set_cutoff(0,0);

	rel_iter= mu_msg_iter_new (
		reinterpret_cast<XapianEnquire*>(&enq),
		maxnum,
		sortfieldid,
		msg_iter_flags (flags),
		NULL);

	mu_msg_iter_destroy (*iter);

	// set the preferred set for the iterator (ie., the set of
	// messages not considered to be duplicates) to be the
	// original matches -- the matches without considering
	// 'related'
	mu_msg_iter_set_preferred (rel_iter, orig_set);
	g_hash_table_destroy (orig_set);

	*iter = rel_iter;
}

Query::Query(const Store& store):
        priv_{std::make_unique<Private>(store)}
{}

Query::Query(Query&& other) = default;

Query::~Query() = default;


MuMsgIter*
Query::run (const std::string& expr, MuMsgFieldId sortfieldid, Query::Flags flags,
            size_t maxnum, GError **err) const
{
	g_return_val_if_fail (mu_msg_field_id_is_valid (sortfieldid)	||
			      sortfieldid    == MU_MSG_FIELD_ID_NONE,
			      NULL);
	try {
		MuMsgIter *iter;
		const bool threads     = any_of(flags & Flags::Threading);
		const bool inc_related = any_of(flags & Flags::IncludeRelated);
		const bool descending  = any_of(flags & Flags::Descending);
		Xapian::Enquire enq (priv_->make_enquire(expr, sortfieldid, descending, err));

		/* when we're doing a 'include-related query', wea're actually
		 * doing /two/ queries; one to get the initial matches, and
		 * based on that one to get all messages in threads in those
		 * matches.
		 */

		/* get the 'real' maxnum if it was specified as < 0 */
		maxnum = maxnum == 0 ? priv_->store_.size(): maxnum;
		/* Calculating threads involves two queries, so do the calculation only in
		 * the second query instead of in both.
		 */
                Query::Flags first_flags{};
		if (threads)
			first_flags = flags & ~Flags::Threading;
		else
			first_flags = flags;
		/* Perform the initial query, returning up to max num results.
		 */
		iter  = mu_msg_iter_new (
			reinterpret_cast<XapianEnquire*>(&enq),
			maxnum,
			sortfieldid,
			msg_iter_flags (first_flags),
			err);
		/* If we want threads or related messages, find related messages using a
		 * second query based on the message ids / refs of the first query's result.
		 * Do this even if we don't want to include related messages in the final
		 * result so we can apply the threading algorithm to the related message set
		 * of a maxnum-sized result instead of the unbounded result of the first
		 * query. If threads are desired but related message are not, we will remove
		 * the undesired related messages later.
		 */
		if(threads||inc_related)
			priv_->find_related_messages (&iter, maxnum, sortfieldid, flags,
                                                      enq.get_query());

                return iter;

	} MU_XAPIAN_CATCH_BLOCK_G_ERROR_RETURN (err, MU_ERROR_XAPIAN, 0);
}


size_t
Query::count (const std::string& expr) const try
{
        const auto enq{priv_->make_enquire(expr, MU_MSG_FIELD_ID_NONE, false, nullptr)};
        auto mset{enq.get_mset(0, priv_->store_.size())};
        mset.fetch();

        return mset.size();

}MU_XAPIAN_CATCH_BLOCK_RETURN (0);



std::string
Query::parse(const std::string& expr, bool xapian) const try
{
        if (xapian) {
                GError *err{};
                const auto descr{priv_->make_query(expr, &err).get_description()};
                if (err) {
                        g_warning ("query error: %s", err->message);
                        g_clear_error(&err);
                }
                return descr;
        } else {
		Mu::WarningVec warns;
		const auto tree = priv_->parser_.parse (expr, warns);
                for (auto&& w: warns)
                        g_warning ("query error: %s", to_string(w).c_str());

                return to_string(tree);

	}

} MU_XAPIAN_CATCH_BLOCK_RETURN("");
