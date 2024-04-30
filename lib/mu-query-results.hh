/*
** Copyright (C) 2022-2024 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef MU_QUERY_RESULTS_HH__
#define MU_QUERY_RESULTS_HH__

#include <algorithm>
#include <limits>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <limits>
#include <ostream>
#include <cmath>
#include <memory>

#include <unistd.h>
#include <fcntl.h>
#include <glib.h>

#include <mu-xapian-db.hh>
#include <utils/mu-utils.hh>
#include <utils/mu-option.hh>

#include <message/mu-message.hh>

namespace Mu {

/**
 * This implements a QueryResults structure, which capture the results of a
 * Xapian query, and a QueryResultsIterator, which gives C++-compliant iterator
 * to go over the results. and finally QueryThreader (in query-threader.cc) which
 * calculates the threads, using the JWZ algorithm.
 */

/// Flags that influence now matches are presented (or skipped)
enum struct QueryFlags {
	None           = 0,      /**< no flags */
	Descending     = 1 << 0, /**< sort z->a */
	SkipUnreadable = 1 << 1, /**< skip unreadable msgs */
	SkipDuplicates = 1 << 2, /**< skip duplicate msgs */
	IncludeRelated = 1 << 3, /**< include related msgs */
	Threading      = 1 << 4, /**< calculate threading info */
	// internal
	Leader = 1 << 5, /**< This is the leader query (for internal use
			  * only)*/
};
MU_ENABLE_BITOPS(QueryFlags);

/// Stores all the essential information for sorting the results.
struct QueryMatch {
	/// Flags for a match (message) found
	enum struct Flags {
		None       = 0,      /**< No Flags */
		Leader     = 1 << 0, /**< Mark direct matches as leader */
		Related    = 1 << 1, /**< A related message */
		Unreadable = 1 << 2, /**< No readable file */
		Duplicate  = 1 << 3, /**< Message-id seen before */

		Root     = 1 << 10, /**< Is this the thread-root? */
		First    = 1 << 11, /**< Is this the first message in a thread? */
		Last     = 1 << 12, /**< Is this the last message in a thread? */
		Orphan   = 1 << 13, /**< Is this message without a parent? */
		HasChild = 1 << 14, /**< Does this message have a child? */

		ThreadSubject = 1 << 20, /**< Message holds subject for (sub)thread */
	};

	Flags       flags{Flags::None}; /**< Flags */
	std::string date_key;           /**< The date-key (for sorting all sub-root levels) */
	// the thread subject is the subject of the first message in a thread,
	// and any message that has a different subject compared to its predecessor
	// (ignoring prefixes such as Re:)
	//
	// otherwise, it is empty.
	std::string subject;        /**< subject for this message */
	size_t      thread_level{}; /**< The thread level */
	std::string thread_path;    /**< The hex-numerial path in the thread, ie. '00:01:0a' */
	std::string thread_date;    /**< date of newest message in thread */

	bool operator<(const QueryMatch& rhs) const { return date_key < rhs.date_key; }

	bool has_flag(Flags flag) const;
};

MU_ENABLE_BITOPS(QueryMatch::Flags);

inline bool
QueryMatch::has_flag(QueryMatch::Flags flag) const
{
	return any_of(flags & flag);
}

/* LCOV_EXCL_START */
static inline std::ostream&
operator<<(std::ostream& os, QueryMatch::Flags mflags)
{
	if (mflags == QueryMatch::Flags::None) {
		os << "<none>";
		return os;
	}

	if (any_of(mflags & QueryMatch::Flags::Leader))
		os << "leader ";
	if (any_of(mflags & QueryMatch::Flags::Unreadable))
		os << "unreadable ";
	if (any_of(mflags & QueryMatch::Flags::Duplicate))
		os << "dup ";

	if (any_of(mflags & QueryMatch::Flags::Root))
		os << "root ";
	if (any_of(mflags & QueryMatch::Flags::Related))
		os << "related ";
	if (any_of(mflags & QueryMatch::Flags::First))
		os << "first ";
	if (any_of(mflags & QueryMatch::Flags::Last))
		os << "last ";
	if (any_of(mflags & QueryMatch::Flags::Orphan))
		os << "orphan ";
	if (any_of(mflags & QueryMatch::Flags::HasChild))
		os << "has-child ";

	return os;
}

inline std::ostream&
operator<<(std::ostream& os, const QueryMatch& qmatch)
{
	os << "qm:[" << qmatch.thread_path << "]: " // " (" << qmatch.thread_level << "): "
	   << "> date:<" << qmatch.date_key << "> "
	   << "flags:{" << qmatch.flags << "}";

	return os;
}
/* LCOV_EXCL_STOP*/

using QueryMatches = std::unordered_map<Xapian::docid, QueryMatch>;


///
/// This is a view over the Xapian::MSet, which can optionally filter unreadable
/// / duplicate messages.
///
/// Note, we internally skip unreadable/duplicate messages (when asked too); those
/// skipped ones do _not_ count towards the max_size
///
class QueryResultsIterator {
public:
	using iterator_category = std::output_iterator_tag;
	using value_type        = Message;
	using difference_type   = void;
	using pointer           = void;
	using reference         = void;

	QueryResultsIterator(Xapian::MSetIterator mset_it, QueryMatches& query_matches)
		: mset_it_{mset_it}, query_matches_{query_matches} {
	}

	/**
	 * Increment the iterator (we don't support post-increment)
	 *
	 * @return an updated iterator, or end() if we were already at end()
	 */
	QueryResultsIterator& operator++() {
		++mset_it_;
		mdoc_ = Nothing;
		return *this;
	}

	/**
	 * (Non)Equivalence operators
	 *
	 * @param rhs some other iterator
	 *
	 * @return true or false
	 */
	bool operator==(const QueryResultsIterator& rhs) const { return mset_it_ == rhs.mset_it_; }
	bool operator!=(const QueryResultsIterator& rhs) const { return mset_it_ != rhs.mset_it_; }

	QueryResultsIterator&       operator*() { return *this; }
	const QueryResultsIterator& operator*() const { return *this; }


	/**
	 * Get the Xapian::Document this iterator is pointing at,
	 * or an empty document when looking at end().
	 *
	 * @return a document
	 */
	Option<Xapian::Document> document() const {
		return xapian_try([this]()->Option<Xapian::Document> {
			auto doc{mset_it_.get_document()};
			if (doc.get_docid() == 0)
				return Nothing;
			else
				return Some(std::move(doc));
			}, Nothing);
	}


	/**
	 * get the corresponding Message for this iter, if any
	 *
	 * @return a Message or Nothing
	 */
	Option<Message> message() const {
		if (auto&& xdoc{document()}; !xdoc)
			return Nothing;
		else if (auto&& doc{Message::make_from_document(std::move(xdoc.value()))};
			 !doc)
			return Nothing;
		else
			return Some(std::move(doc.value()));
	}

	/**
	 * Get the doc-id for the document this iterator is pointing at, or 0
	 * when looking at end.
	 *
	 * @return a doc-id.
	 */
	Xapian::docid doc_id() const { return *mset_it_; }

	/**
	 * Get the message-id for the document (message) this iterator is
	 * pointing at, or not when not available
	 *
	 * @return a message-id
	 */
	Option<std::string> message_id() const noexcept {
		return opt_string(Field::Id::MessageId);
	}

	/**
	 * Get the thread-id for the document (message) this iterator is
	 * pointing at, or Nothing.
	 *
	 * @return a message-id
	 */
	Option<std::string> thread_id() const noexcept {
		return opt_string(Field::Id::ThreadId);
	}

	/**
	 * Get the file-system path for the document (message) this iterator is
	 * pointing at, or Nothing.
	 *
	 * @return a filesystem path
	 */
	Option<std::string> path() const noexcept {
		return opt_string(Field::Id::Path);
	}

	/**
	 * Get the a sortable date str for the document (message) the iterator
	 * is pointing at. pointing at, or Nothing. This (encoded) string
	 * has the same sort-order as the corresponding date.
	 *
	 * @return a filesystem path
	 */
	Option<std::string> date_str() const noexcept {
		return opt_string(Field::Id::Date);
	}

	/**
	 * Get the subject for the document (message) this iterator is pointing
	 * at.
	 *
	 * @return the subject
	 */
	Option<std::string> subject() const noexcept {
		return opt_string(Field::Id::Subject);
	}

	/**
	 * Get the references for the document (messages) this is iterator is
	 * pointing at, or empty if pointing at end of if no references are
	 * available.
	 *
	 * @return references
	 */
	std::vector<std::string> references() const noexcept {
		return mu_document().string_vec_value(Field::Id::References);
	}

	/**
	 * Get some value from the document, or Nothing if empty.
	 *
	 * @param id a message field id
	 *
	 * @return the value
	 */
	Option<std::string> opt_string(Field::Id id) const noexcept {
		if (auto&& val{mu_document().string_value(id)}; val.empty())
			return Nothing;
		else
			return Some(std::move(val));
	}

	/**
	 * Get the Query match info for this message.
	 *
	 * @return the match info.
	 */
	QueryMatch& query_match() {
		g_assert(query_matches_.find(doc_id()) != query_matches_.end());
		return query_matches_.find(doc_id())->second;
	}
	const QueryMatch& query_match() const {
		g_assert(query_matches_.find(doc_id()) != query_matches_.end());
		return query_matches_.find(doc_id())->second;
	}

private:
	/**
	 * Get a (cached) reference for the Mu::Document corresponding
	 * to the current iter.
	 *
	 * @return cached mu document,
	 */
	const Mu::Document& mu_document() const {
		if (!mdoc_) {
			if (auto xdoc = document(); !xdoc)
				std::runtime_error("iter without document");
			else
				mdoc_ = Mu::Document{xdoc.value()};
		}
		return mdoc_.value();
	}

	mutable Option<Mu::Document>    mdoc_; // cache.
	Xapian::MSetIterator		mset_it_;
	QueryMatches&			query_matches_;
};


static inline auto
format_as(const QueryResultsIterator& it)
{
	return it.path().value_or("<no path>");
}

constexpr auto MaxQueryResultsSize = std::numeric_limits<size_t>::max();

class QueryResults {
public:
	/// Helper types
	using iterator       = QueryResultsIterator;
	using const_iterator = const iterator;

	/**
	 * Construct a QueryResults object
	 *
	 * @param mset an Xapian::MSet with matches
	 */
	QueryResults(const Xapian::MSet& mset, QueryMatches&& query_matches)
	    : mset_{mset}, query_matches_{std::move(query_matches)}
	{
	}
	/**
	 * Is this QueryResults object empty (ie., no matches)?
	 *
	 * @return true are false
	 */
	bool empty() const { return mset_.empty(); }

	/**
	 * Get the number of matches in this QueryResult
	 *
	 * @return number of matches
	 */
	size_t size() const { return mset_.size(); }

	/**
	 * Get the begin iterator to the results.
	 *
	 * @return iterator
	 */
	const iterator begin() const { return QueryResultsIterator(mset_.begin(), query_matches_); }

	/**
	 * Get the end iterator to the results.
	 *
	 * @return iterator
	 */
	const_iterator end() const { return QueryResultsIterator(mset_.end(), query_matches_); }

	/**
	 * Get the query-matches for these QueryResults. The non-const
	 * version can be use to _steal_ the query results, by moving
	 * them.
	 *
	 * @return query-matches
	 */
	const QueryMatches& query_matches() const { return query_matches_; }
	QueryMatches&       query_matches() { return query_matches_; }

private:
	const Xapian::MSet   mset_;
	mutable QueryMatches query_matches_;
};

} // namespace Mu

#endif /* MU_QUERY_RESULTS_HH__ */
