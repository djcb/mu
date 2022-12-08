/*
** Copyright (C) 2020-2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-query-match-deciders.hh"

#include "mu-query-results.hh"
#include "utils/mu-option.hh"

using namespace Mu;


// We use a MatchDecider to gather information about the matches, and decide
// whether to include them in the results.
//
// Note that to include the "related" messages, we need _two_ queries; the first
// one to get the initial matches (called the Leader-Query) and a Related-Query,
// to get the Leader matches + all messages that have a thread-id seen in the
// Leader matches.
//
// We use the MatchDecider to gather information and use it for both queries.

struct MatchDecider : public Xapian::MatchDecider {
	MatchDecider(QueryFlags qflags, DeciderInfo& info) : qflags_{qflags}, decider_info_{info} {}
	/**
	 * Update the match structure with unreadable/duplicate flags
	 *
	 * @param doc a Xapian document.
	 *
	 * @return a new QueryMatch object
	 */
	QueryMatch make_query_match(const Xapian::Document& doc) const
	{
		QueryMatch qm{};

		auto msgid{opt_string(doc, Field::Id::MessageId)
			       .value_or(*opt_string(doc, Field::Id::Path))};
		if (!decider_info_.message_ids.emplace(std::move(msgid)).second)
			qm.flags |= QueryMatch::Flags::Duplicate;

		const auto path{opt_string(doc, Field::Id::Path)};
		if (!path || ::access(path->c_str(), R_OK) != 0)
			qm.flags |= QueryMatch::Flags::Unreadable;

		return qm;
	}

	/**
	 * Should this message be included in the results?
	 *
	 * @param qm a query match
	 *
	 * @return true or false
	 */
	bool should_include(const QueryMatch& qm) const
	{
		if (any_of(qflags_ & QueryFlags::SkipDuplicates) &&
		    any_of(qm.flags & QueryMatch::Flags::Duplicate))
			return false;

		if (any_of(qflags_ & QueryFlags::SkipUnreadable) &&
		    any_of(qm.flags & QueryMatch::Flags::Unreadable))
			return false;

		return true;
	}
	/**
	 * Gather thread ids from this match.
	 *
	 * @param doc the document (message)
	 *
	 */
	void gather_thread_ids(const Xapian::Document& doc) const
	{
		auto thread_id{opt_string(doc, Field::Id::ThreadId)};
		if (thread_id)
			decider_info_.thread_ids.emplace(std::move(*thread_id));
	}

protected:
	const QueryFlags qflags_;
	DeciderInfo&     decider_info_;

private:
	Option<std::string> opt_string(const Xapian::Document& doc, Field::Id id) const noexcept {
		const auto value_no{field_from_id(id).value_no()};
		std::string val = xapian_try([&] { return doc.get_value(value_no); }, std::string{""});
		if (val.empty())
			return Nothing;
		else
			return Some(std::move(val));
	}
};

struct MatchDeciderLeader final : public MatchDecider {
	MatchDeciderLeader(QueryFlags qflags, DeciderInfo& info) : MatchDecider(qflags, info) {}
	/**
	 * operator()
	 *
	 * This receives the documents considered during a Xapian query, and
	 * is to return either true (keep) or false (ignore)
	 *
	 * We use this to potentiallly avoid certain messages (documents):
	 * - with QueryFlags::SkipUnreadable this will return false for message
	 *   that are not readable in the file-system
	 * - with QueryFlags::SkipDuplicates this will return false for messages
	 *   whose message-id was seen before.
	 *
	 * Even if we do not skip these messages entirely, we remember whether
	 * they were unreadable/duplicate (in the QueryMatch::Flags), so we can
	 * quickly find that info when doing the second 'related' query.
	 *
	 * The "leader" query. Matches here get the Leader flag unless they are
	 * duplicates / unreadable. We check the duplicate/readable status
	 * regardless of whether SkipDuplicates/SkipUnreadable was passed
	 * (to gather that information); however those flags
	 * affect our true/false verdict.
	 *
	 * @param doc xapian document
	 *
	 * @return true or false
	 */
	bool operator()(const Xapian::Document& doc) const override {
		// by definition, we haven't seen the docid before,
		// so no need to search
		auto it = decider_info_.matches.emplace(doc.get_docid(), make_query_match(doc));
		it.first->second.flags |= QueryMatch::Flags::Leader;

		return should_include(it.first->second);
	}
};

std::unique_ptr<Xapian::MatchDecider>
Mu::make_leader_decider(QueryFlags qflags, DeciderInfo& info)
{
	return std::make_unique<MatchDeciderLeader>(qflags, info);
}

struct MatchDeciderRelated final : public MatchDecider {
	MatchDeciderRelated(QueryFlags qflags, DeciderInfo& info) : MatchDecider(qflags, info) {}
	/**
	 * operator()
	 *
	 * This receives the documents considered during a Xapian query, and
	 * is to return either true (keep) or false (ignore)
	 *
	 * We use this to potentially avoid certain messages (documents):
	 * - with QueryFlags::SkipUnreadable this will return false for message
	 *   that are not readable in the file-system
	 * - with QueryFlags::SkipDuplicates this will return false for messages
	 *   whose message-id was seen before.
	 *
	 * Unlike in the "leader" decider (scroll up), we don't need to remember
	 * messages we won't include.
	 *
	 * @param doc xapian document
	 *
	 * @return true or false
	 */
	bool operator()(const Xapian::Document& doc) const override {
		// we may have seen this match in the "Leader" query.
		const auto it = decider_info_.matches.find(doc.get_docid());
		if (it != decider_info_.matches.end())
			return should_include(it->second);

		auto qm{make_query_match(doc)};
		if (should_include(qm)) {
			qm.flags |= QueryMatch::Flags::Related;
			decider_info_.matches.emplace(doc.get_docid(), std::move(qm));
			return true;
		} else
			return false; // nope.
	}
};

std::unique_ptr<Xapian::MatchDecider>
Mu::make_related_decider(QueryFlags qflags, DeciderInfo& info)
{
	return std::make_unique<MatchDeciderRelated>(qflags, info);
}

struct MatchDeciderThread final : public MatchDecider {
	MatchDeciderThread(QueryFlags qflags, DeciderInfo& info) : MatchDecider{qflags, info} {}
	/**
	 * operator()
	 *
	 * This receives the documents considered during a Xapian query, and
	 * is to return either true (keep) or false (ignore)
	 *
	 * Only include documents that earlier checks have decided to include.
	 *
	 * @param doc xapian document
	 *
	 * @return true or false
	 */
	bool operator()(const Xapian::Document& doc) const override {
		// we may have seen this match in the "Leader" query,
		// or in the second (unbuounded) related query;
		const auto it{decider_info_.matches.find(doc.get_docid())};
		return it != decider_info_.matches.end() && !it->second.thread_path.empty();
	}
};

std::unique_ptr<Xapian::MatchDecider>
Mu::make_thread_decider(QueryFlags qflags, DeciderInfo& info)
{
	return std::make_unique<MatchDeciderThread>(qflags, info);
}
