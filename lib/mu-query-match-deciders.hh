/*
** Copyright (C) 2021-2024 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef MU_QUERY_MATCH_DECIDERS_HH__
#define MU_QUERY_MATCH_DECIDERS_HH__

#include <unordered_set>
#include <unordered_map>
#include <memory>

#include "mu-xapian-db.hh"

#include "mu-query-results.hh"

namespace Mu {
using StringSet = std::unordered_set<std::string>;

struct DeciderInfo {
	QueryMatches matches;
	StringSet    thread_ids;
	StringSet    message_ids;
};

/**
 * Make a "leader" decider, that is, a MatchDecider for either a singular or the
 * first query in the leader/related pair of queries. Gather information for
 * threading, and the subsequent "related" query.
 *
 * @param qflags     query flags
 * @param match_info receives information about the matches.
 *
 * @return a unique_ptr to a match decider.
 */
std::unique_ptr<Xapian::MatchDecider> make_leader_decider(QueryFlags qflags, DeciderInfo& info);

/**
 * Make a "related" decider, that is, a MatchDecider for the second query
 * in the leader/related pair of queries.
 *
 * @param qflags     query flags
 * @param match_info receives information about the matches.
 *
 * @return a unique_ptr to a match decider.
 */
std::unique_ptr<Xapian::MatchDecider> make_related_decider(QueryFlags qflags, DeciderInfo& info);

/**
 * Make a "thread" decider, that is, a MatchDecider that removes all but the
 * document excepts for the ones found during initial/related searches.
 *
 * @param qflags     query flags
 * @param match_info receives information about the matches.
 *
 * @return a unique_ptr to a match decider.
 */
std::unique_ptr<Xapian::MatchDecider> make_thread_decider(QueryFlags qflags, DeciderInfo& info);

} // namespace Mu

#endif /* MU_QUERY_MATCH_DECIDERS_HH__ */
