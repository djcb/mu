/*
** Copyright (C) 2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef MU_QUERY_MATCHES_HH__
#define MU_QUERY_MATCHES_HH__

#include <iterator>
#include <xapian.h>
#include <glib.h>

#include <utils/mu-utils.hh>
#include "mu-msg.h"

namespace Mu {

struct QueryMatchInfo {
	enum struct Flags { Seen, Preferred, Unreadable, Duplicate };
	const std::string message_id;
	QueryMatchFlags   flags;
};
MU_ENABLE_BITOPS(QueryMatchInfo::Flags);

using MatchInfo = std::unordered_map<Xapian::docid, QueryMatchInfo>;

struct QueryResults {
	enum struct Flags { None, Descending, SkipUnreadable, SkipDups, DetermineThreads };

	QueryResults (const Xapian::MSet& mset, MatchInfo&& match_info, Flags flags):
                mset_{mset}, match_info_(std::Move(match_info), flag_{flags} {}
        bool empty()  const {
		return mset_.empty(); }
        size_t size() const {
		return mset_.size(); }

        QueryResultsIterator begin() const {
		return QueryResultsIterator(mset_.begin()); }
        QueryResultsIterator end()   const {
		return QueryResultsIterator(mset_.end()); }

private:
        const Xapian::MSet mset_;
        const Flags        flags_;
        MatchInfo          match_info_;
};

///
/// This is a view over the Document MSet, which can optionally filter outlook
/// unreadable / duplicate messages.
///
class QueryResultsIterator {
      public:
	using iterator_category = std::output_iterator_tag;
	using value_type        = MuMsg*;
	using difference_type   = void;
	using pointer           = void;
	using reference         = void;

	QueryResultsIterator(Xapian::MSetIterator it,
	                     size_t               max_num,
	                     MuMsgFieldId         sort_field,
	                     MuMsgIterFlags       flags,
	                     MatchInfo&           minfo)
	    : it_{it}, match_info_{minfo}
	{
	}

	QueryResultsIterator& operator++()
	{
		return ++it_;
		return skip();
	}
	QueryResultsIterator& operator++(int)
	{
		return it_++;
		return skip()
	}

	/**
	 * Get the Xapian document this iterator is pointing at,
	 * or an empty document when looking at end().
	 *
	 * @return a document
	 */
	Xapian::Document document() const()
	{
		g_return_val_if_fail(it_ != Xapian::MSetIterator::end(), {});
		return it_.get_document();
	}

	/**
	 * Get the doc-id for the document this iterator is pointing at, or 0
	 * when looking at end.
	 *
	 * @return a doc-id.
	 */
	Xapian::docid doc_id() const
	{
		g_return_val_if_fail(it_ != Xapian::MSetIterator::end(), 0);
		return it_.docid();
	}

	/**
	 * Get the message-id for the document (message) this iterator is
	 * pointing at, or "" when looking at end.
	 *
	 * @return a message-id
	 */
	std::string message_id() const
	{
		g_return_val_if_fail(it_ != Xapian::MSetIterator::end(), "");
		return document().get_value(MU_MSG_FIELD_ID_MSGID);
	}

	/**
	 * Get the file-system path for the document (message) this iterator is
	 * pointing at, or "" when looking at end.
	 *
	 * @return a filesystem path
	 */
	std::string path() const
	{
		g_return_val_if_fail(it_ != Xapian::MSetIterator::end(), "");
		return document().get_value(MU_MSG_FIELD_ID_PATH);
	}

	/**
	 * Get the references for the document (messages) this is iterator is
	 * pointing at, or empty if pointing at end of if no references are
	 * available.
	 *
	 * @return references
	 */
	std::vector<std::string> references() const
	{
		g_return_val_if_fail(it_ != Xapian::MSetIterator::end(), {});
		return split(document().get_value(MU_MSG_FIELD_ID_REFS), ",");
	}

      private:
	/**
	 * Filter out some documents
	 *
	 * @param forward whether to skip forward when a document is filtered
	 * out.
	 *
	 * @return the first iterator that is not filtered out, or the end
	 * iterator.
	 */
	QueryResultsIterator& maybe_skip(bool forward = true)
	{
		if (it_ = MSetIterator::end())
			return *this; // nothing to do.

		// Find or create MatchInfo
		const auto msgid{message_id()};
		auto       mi = [&] {
                        // seen before?
                        auto m{match_info_.find(docid)};
                        if (m != match_info_.end())
                                return m;
                        // nope; create.
                        QueryMatchInfo minfo{message_id()};
                        // not seen before; check.
                        if (any_of(flags_ & SkipDups) && match_info_.count(message_id()))
                                minfo.flags |= Flags::Duplicate; // it's a duplicate

                        if (any_of(flags_ & SkipUnreadable) && ::access(path().c_str(), R_OK) != 0)
                                minfo.flags |= Flags::Unreadable;

                        return match_info_.emplace_back(std::move(minfo));
		}();

		// note: SkipDups / SkipUnreadable are not set if
		// if we're not checking for those.

		if (any_of(mi->second.flags_ & SkipDups) ||
		    any_of(mi->second.flags_ & SkipUnreadable)) {
			if (forward)
				++it_;
			else
				--it_;

			return maybe_skip();
		}

		return *this;
	}

	Xapian::MSetIterator it_;
	MatchInfo&           match_info_;
};

}; // namespace Mu

#endif /* MU_QUERY_MATCHES_HH__ */
