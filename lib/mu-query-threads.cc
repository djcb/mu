/*
** Copyright (C) 2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-query-threads.hh"
#include <message/mu-message.hh>

#include <set>
#include <unordered_set>
#include <list>
#include <cassert>
#include <cstring>
#include <iostream>
#include <iomanip>

#include <utils/mu-option.hh>

using namespace Mu;

struct Container {
	using Containers = std::vector<Container*>;

	Container() = default;
	Container(Option<QueryMatch&> msg) : query_match{msg} {}
	Container(const Container&) = delete;
	Container(Container&&)      = default;

	void add_child(Container& new_child)
	{
		new_child.parent = this;
		children.emplace_back(&new_child);
	}
	void remove_child(Container& child)
	{
		children.erase(find_child(child));
		assert(!has_child(child));
	}

	Containers::iterator find_child(Container& child)
	{
		return std::find_if(children.begin(), children.end(), [&](auto&& c) {
			return c == &child;
		});
	}
	Containers::const_iterator find_child(Container& child) const
	{
		return std::find_if(children.begin(), children.end(), [&](auto&& c) {
			return c == &child;
		});
	}
	bool has_child(Container& child) const { return find_child(child) != children.cend(); }

	bool is_reachable(Container* other) const
	{
		auto up{ur_parent()};
		return up && up == other->ur_parent();
	}
	template <typename Func> void for_each_child(Func&& func)
	{
		auto it{children.rbegin()};
		while (it != children.rend()) {
			auto next = std::next(it);
			func(*it);
			it = next;
		}
	}
	// During sorting, this is the cached value for the (recursive) date-key
	// of this container -- ie.. either the one from the first of its
	// children, or from its query-match, if it has no children.
	//
	// Note that the sub-root-levels of threads are always sorted by date,
	// in ascending order, regardless of whatever sorting was specified for
	// the root-level.

	std::string thread_date_key;

	Option<QueryMatch&> query_match;
	bool                is_nuked{};
	Container*          parent{};
	Containers          children;

	using ContainerVec = std::vector<Container*>;

      private:
	const Container* ur_parent() const
	{
		assert(this->parent != this);
		return parent ? parent->ur_parent() : this;
	}
};

using Containers   = Container::Containers;
using ContainerVec = Container::ContainerVec;

/* LCOV_EXCL_START */
static std::ostream&
operator<<(std::ostream& os, const Container& container)
{
	os << "container: " << std::right << std::setw(10) << &container
	   << ": parent: " << std::right << std::setw(10) << container.parent << " ["
	   << container.thread_date_key << "]"
	   << "\n  children: ";

	for (auto&& c : container.children)
		os << std::right << std::setw(10) << c << " ";

	os << (container.is_nuked ? " nuked" : "");

	if (container.query_match)
		os << "\n  " << container.query_match.value();

	return os;
}
/* LCOV_EXCL_STOP */

using IdTable  = std::unordered_map<std::string, Container>;
using DupTable = std::multimap<std::string, Container>;

static void
handle_duplicates(IdTable& id_table, DupTable& dup_table)
{
	size_t n{};

	for (auto&& dup : dup_table) {
		const auto msgid{dup.first};
		auto       it = id_table.find(msgid);
		if (it == id_table.end())
			continue;

		// add duplicates as fake children
		char buf[32];
		::snprintf(buf, sizeof(buf), "dup-%zu", ++n);
		it->second.add_child(id_table.emplace(buf, std::move(dup.second)).first->second);
	}
}

template <typename QueryResultsType>
static IdTable
determine_id_table(QueryResultsType& qres)
{
	// 1. For each query_match
	IdTable  id_table;
	DupTable dups;
	for (auto&& mi : qres) {
		const auto msgid{mi.message_id().value_or(*mi.path())};
		// Step 0 (non-JWZ): filter out dups, handle those at the end
		if (mi.query_match().has_flag(QueryMatch::Flags::Duplicate)) {
			dups.emplace(msgid, mi.query_match());
			continue;
		}
		// 1.A If id_table contains an empty Container for this ID:
		// Store this query_match (query_match) in the Container's query_match (value) slot.
		// Else:
		//   Create a new Container object holding this query_match (query-match);
		//  Index the Container by Query_Match-ID
		auto  c_it      = id_table.find(msgid);
		auto& container = [&]() -> Container& {
			if (c_it != id_table.end()) {
				if (!c_it->second.query_match) // hmm, dup?
					c_it->second.query_match = mi.query_match();
				return c_it->second;
			} else {
				// Else:
				// Create a new Container object holding this query_match
				// (query-match); Index the Container by Query_Match-ID
				return id_table.emplace(msgid, mi.query_match()).first->second;
			}
		}();

		// We sort by date (ascending), *except* for the root; we don't
		// know what query_matchs will be at the root level yet, so remember
		// both. Moreover, even when sorting the top-level in descending
		// order, still sort the thread levels below that in ascending
		// order.
		container.thread_date_key = container.query_match->date_key =
		    mi.date_str().value_or("");
		// initial guess for the thread-date; might be updated
		// later.

		// remember the subject, we use it to determine the (sub)thread subject
		container.query_match->subject = mi.subject().value_or("");

		// 1.B
		// For each element in the query_match's References field:
		Container* parent_ref_container{};
		for (const auto& ref : mi.references()) {
			//   grand_<n>-parent -> grand_<n-1>-parent -> ... -> parent.

			// Find a Container object for the given Query_Match-ID; If it exists, use
			// it; otherwise make one with a null Query_Match.
			auto ref_container = [&]() -> Container* {
				auto ref_it = id_table.find(ref);
				if (ref_it == id_table.end())
					ref_it = id_table.emplace(ref, Nothing).first;
				return &ref_it->second;
			}();

			// Link the References field's Containers together in the order implied
			// by the References header.
			// * If they are already linked, don't change the existing links.
			//
			// * Do not add a link if adding that link would introduce a loop: that is,
			//   before asserting A->B, search down the children of B to see if A is
			//   reachable, and also search down the children of A to see if B is
			//   reachable. If either is already reachable as a child of the other,
			//   don't add the link.
			if (parent_ref_container && !ref_container->parent) {
				if (!parent_ref_container->is_reachable(ref_container))
					parent_ref_container->add_child(*ref_container);
				// else
				//         g_message ("%u: reachable %s -> %s", __LINE__,
				//         msgid.c_str(), ref.c_str());
			}

			parent_ref_container = ref_container;
		}

		// Add the query_match to the chain.
		if (parent_ref_container && !container.parent) {
			if (!parent_ref_container->is_reachable(&container))
				parent_ref_container->add_child(container);
			// else
			//         g_message ("%u: reachable %s -> parent", __LINE__,
			//         msgid.c_str());
		}
	}

	// non-JWZ: add duplicate messages.
	handle_duplicates(id_table, dups);

	return id_table;
}

/// Recursively walk all containers under the root set.
/// For each container:
///
///     If it is an empty container with no children, nuke it.
///
///     Note: Normally such containers won't occur, but they can show up when two
///     query_matchs have References lines that disagree. For example, assuming A and
///     B are query_matchs, and 1, 2, and 3 are references for query_matchs we haven't
///     seen:
///
///         A has references: 1, 2, 3
///         B has references: 1, 3
///
///     There is ambiguity as to whether 3 is a child of 1 or of 2. So,
///     depending on the processing order, we might end up with either
///
///           -- 1
///              |-- 2
///                  \-- 3
///                      |-- A
///                      \-- B
///
///     or
///
///           -- 1
///              |-- 2            <--- non root childless container!
///              \-- 3
///                  |-- A
///                  \-- B
///
///     If the Container has no Query_Match, but does have children, remove this
///     container but promote its children to this level (that is, splice them in
///     to the current child list.)
///
///     Do not promote the children if doing so would promote them to the root
///     set -- unless there is only one child, in which case, do.

static void
prune(Container* child)
{
	Container* container{child->parent};

	for (auto& grandchild : child->children) {
		grandchild->parent = container;
		if (container)
			container->children.emplace_back(grandchild);
	}

	child->children.clear();
	child->is_nuked = true;

	if (container)
		container->remove_child(*child);
}

static bool
prune_empty_containers(Container& container)
{
	Containers to_prune;

	container.for_each_child([&](auto& child) {
		if (prune_empty_containers(*child))
			to_prune.emplace_back(child);
	});

	for (auto& child : to_prune)
		prune(child);

	// Never nuke these.
	if (container.query_match)
		return false;

	// If it is an empty container with no children, nuke it.
	//
	// If the Container is empty, but does have children, remove this
	// container but promote its children to this level (that is, splice them in
	// to the current child list.)
	//
	// Do not promote the children if doing so would promote them to the root
	// set -- unless there is only one child, in which case, do.
	// const auto rootset_child{!container.parent->parent};
	if (container.parent || container.children.size() <= 1)
		return true; // splice/nuke it.

	return false;
}

static void
prune_empty_containers(IdTable& id_table)
{
	for (auto&& item : id_table) {
		auto& child(item.second);
		if (child.parent)
			continue; // not a root child.

		if (prune_empty_containers(item.second))
			prune(&child);
	}
}

//
// Sorting.
//

/// Register some information about a match (i.e., message) that we can use for
/// subsequent queries.
using ThreadPath = std::vector<unsigned>;
inline std::string
to_string(const ThreadPath& tpath, size_t digits)
{
	std::string str;
	str.reserve(tpath.size() * digits);

	bool first{true};
	for (auto&& segm : tpath) {
		str += mu_format("{}{:0{}x}", first ? "" : ":", segm, digits);
		first = false;
	}

	return str;
}

static bool // compare subjects, ignore anything before the last ':<space>*'
subject_matches(const std::string& sub1, const std::string& sub2)
{
	auto search_str = [](const std::string& s) -> const char* {
		const auto pos = s.find_last_of(':');
		if (pos == std::string::npos)
			return s.c_str();
		else {
			const auto pos2 = s.find_first_not_of(' ', pos + 1);
			return s.c_str() + (pos2 == std::string::npos ? pos : pos2);
		}
	};

	return g_strcmp0(search_str(sub1), search_str(sub2)) == 0;
}

static bool
update_container(Container&         container,
		 bool               descending,
		 ThreadPath&        tpath,
		 size_t             seg_size,
		 const std::string& prev_subject = "")
{
	if (!container.children.empty()) {
		Container* first = container.children.front();
		if (first->query_match)
			first->query_match->flags |= QueryMatch::Flags::First;
		Container* last = container.children.back();
		if (last->query_match)
			last->query_match->flags |= QueryMatch::Flags::Last;
	}

	if (!container.query_match)
		return false; // nothing else to do.

	auto& qmatch(*container.query_match);
	if (!container.parent)
		qmatch.flags |= QueryMatch::Flags::Root;
	else if (!container.parent->query_match)
		qmatch.flags |= QueryMatch::Flags::Orphan;

	if (!container.children.empty())
		qmatch.flags |= QueryMatch::Flags::HasChild;

	if (qmatch.has_flag(QueryMatch::Flags::Root) || prev_subject.empty() ||
	    !subject_matches(prev_subject, qmatch.subject))
		qmatch.flags |= QueryMatch::Flags::ThreadSubject;

	if (descending && container.parent) {
		// trick xapian by giving it "inverse" sorting key so our
		// ascending-date sorted threads stay in that order
		tpath.back() = ((1U << (4 * seg_size)) - 1) - tpath.back();
	}

	qmatch.thread_path  = to_string(tpath, seg_size);
	qmatch.thread_level = tpath.size() - 1;

	// ensure thread root comes before its children
	if (descending)
		qmatch.thread_path += ":z";

	return true;
}

static void
update_containers(Containers&  children,
		  bool         descending,
		  ThreadPath&  tpath,
		  size_t       seg_size,
		  std::string& prev_subject)
{
	size_t idx{0};

	for (auto&& c : children) {
		tpath.emplace_back(idx++);
		if (c->query_match) {
			update_container(*c, descending, tpath, seg_size, prev_subject);
			prev_subject = c->query_match->subject;
		}
		update_containers(c->children, descending, tpath, seg_size, prev_subject);
		tpath.pop_back();
	}
}

static void
update_containers(ContainerVec& root_vec, bool descending, size_t n)
{
	ThreadPath tpath;
	tpath.reserve(n);

	const auto seg_size = static_cast<size_t>(std::ceil(std::log2(n) / 4.0));
	/*note: 4 == std::log2(16)*/

	size_t idx{0};
	for (auto&& c : root_vec) {
		tpath.emplace_back(idx++);
		std::string prev_subject;
		if (update_container(*c, descending, tpath, seg_size))
			prev_subject = c->query_match->subject;
		update_containers(c->children, descending, tpath, seg_size, prev_subject);
		tpath.pop_back();
	}
}

static void
sort_container(Container& container)
{
	// 1. childless container.
	if (container.children.empty())
		return; // no children;  nothing to sort.

	// 2. container with children.
	// recurse, depth-first: sort the children
	for (auto& child : container.children)
		sort_container(*child);

	// now sort this level.
	std::sort(container.children.begin(), container.children.end(), [&](auto&& c1, auto&& c2) {
		return c1->thread_date_key < c2->thread_date_key;
	});

	// and 'bubble up' the date of the *newest* message with a date. We
	// reasonably assume that it's later than its parent.
	const auto& newest_date = container.children.back()->thread_date_key;
	if (!newest_date.empty())
		container.thread_date_key = newest_date;
}

static void
sort_siblings(IdTable& id_table, bool descending)
{
	if (id_table.empty())
		return;

	// unsorted vec of root containers. We can
	// only sort these _after_ sorting the children.
	ContainerVec root_vec;
	for (auto&& item : id_table) {
		if (!item.second.parent && !item.second.is_nuked)
			root_vec.emplace_back(&item.second);
	}

	// now sort all threads _under_ the root set (by date/ascending)
	for (auto&& c : root_vec)
		sort_container(*c);

	// and then sort the root set.
	//
	// The difference with the sub-root containers is that at the top-level,
	// we can sort either in ascending or descending order, while on the
	// subroot level it's always in ascending order.
	//
	// Note that unless we're testing, _xapian_ will handle
	// the ascending/descending of the top level.
	std::sort(root_vec.begin(), root_vec.end(), [&](auto&& c1, auto&& c2) {
#ifdef BUILD_TESTS
		if (descending)
			return c2->thread_date_key < c1->thread_date_key;
		else
#endif /*BUILD_TESTS*/
			return c1->thread_date_key < c2->thread_date_key;
	});

	// now all is sorted... final step is to determine thread paths and
	// other flags.
	update_containers(root_vec, descending, id_table.size());
}

/* LCOV_EXCL_START */
static std::ostream&
operator<<(std::ostream& os, const IdTable& id_table)
{
	os << "------------------------------------------------\n";
	for (auto&& item : id_table) {
		os << item.first << " => " << item.second << "\n";
	}
	os << "------------------------------------------------\n";

	std::set<std::string> ids;
	for (auto&& item : id_table) {
		if (item.second.query_match)
			ids.emplace(item.second.query_match->thread_path);
	}

	for (auto&& id : ids) {
		auto it = std::find_if(id_table.begin(), id_table.end(), [&](auto&& item) {
			return item.second.query_match &&
			       item.second.query_match->thread_path == id;
		});
		assert(it != id_table.end());
		os << it->first << ": " << it->second << '\n';
	}
	return os;
}
/* LCOV_EXCL_STOP */

template <typename Results>
static void
calculate_threads_real(Results& qres, bool descending)
{
	// Step 1: build the id_table
	auto id_table{determine_id_table(qres)};

	if (g_test_verbose())
		std::cout << "*** id-table(1):\n" << id_table << "\n";

	// // Step 2: get the root set
	// // Step 3: discard id_table
	// Nope: id-table owns the containers.
	// Step 4: prune empty containers
	prune_empty_containers(id_table);

	// Step 5: group root-set by subject.
	// Not implemented.

	// Step 6: we're done threading

	// Step 7: sort siblings. The segment-size is the number of hex-digits
	// in the thread-path string (so we can lexically compare them.)
	sort_siblings(id_table, descending);

	// Step 7a:. update querymatches
	for (auto&& item : id_table) {
		Container& c{item.second};
		if (c.query_match)
			c.query_match->thread_date = c.thread_date_key;
	}
	// if (g_test_verbose())
	//         std::cout << "*** id-table(2):\n" << id_table << "\n";
}

void
Mu::calculate_threads(Mu::QueryResults& qres, bool descending)
{
	calculate_threads_real(qres, descending);
}

#ifdef BUILD_TESTS

struct MockQueryResult {
	MockQueryResult(const std::string&              message_id_arg,
			const std::string&              date_arg,
			const std::vector<std::string>& refs_arg = {})
	    : message_id_{message_id_arg}, date_{date_arg}, refs_{refs_arg}
	{
	}
	MockQueryResult(const std::string&              message_id_arg,
			const std::vector<std::string>& refs_arg = {})
	    : MockQueryResult(message_id_arg, "", refs_arg)
	{
	}
	Option<std::string>             message_id() const { return message_id_; }
	Option<std::string>             path() const { return path_; }
	Option<std::string>             date_str() const { return date_; }
	Option<std::string>             subject() const { return subject_; }
	QueryMatch&                     query_match() { return query_match_; }
	const QueryMatch&               query_match() const { return query_match_; }
	const std::vector<std::string>& references() const { return refs_; }

	std::string              path_;
	std::string              message_id_;
	QueryMatch               query_match_{};
	std::string              date_;
	std::string              subject_;
	std::vector<std::string> refs_;
};

using MockQueryResults = std::vector<MockQueryResult>;

G_GNUC_UNUSED static std::ostream&
operator<<(std::ostream& os, const MockQueryResults& qrs)
{
	for (auto&& mi : qrs)
		os << mi.query_match().thread_path << " :: " << mi.message_id().value_or("<none>")
		   << std::endl;

	return os;
}

static void
calculate_threads(MockQueryResults& qres, bool descending)
{
	calculate_threads_real(qres, descending);
}

using Expected = std::vector<std::pair<std::string, std::string>>;

static void
assert_thread_paths(const MockQueryResults& qrs, const Expected& expected)
{
	for (auto&& exp : expected) {
		auto it = std::find_if(qrs.begin(), qrs.end(), [&](auto&& qr) {
			return qr.message_id().value_or("") == exp.first ||
			       qr.path().value_or("") == exp.first;
		});
		g_assert_true(it != qrs.end());
		mu_debug("thread-path ({}@{}): expected: '{}'; got '{}'",
			 it->message_id().value_or("<none>"),
			 it->path().value_or("<none>"),
			 exp.second, it->query_match().thread_path);
		g_assert_cmpstr(exp.second.c_str(), ==, it->query_match().thread_path.c_str());
	}
}

static void
test_sort_ascending()
{
	auto results = MockQueryResults{MockQueryResult{"m1", "1", {"m2"}},
					MockQueryResult{"m2", "2", {"m3"}},
					MockQueryResult{"m3", "3", {}},
					MockQueryResult{"m4", "4", {}}};

	calculate_threads(results, false);

	assert_thread_paths(results, {{"m1", "0:0:0"}, {"m2", "0:0"}, {"m3", "0"}, {"m4", "1"}});
}

static void
test_sort_descending()
{
	auto results = MockQueryResults{MockQueryResult{"m1", "1", {"m2"}},
					MockQueryResult{"m2", "2", {"m3"}},
					MockQueryResult{"m3", "3", {}},
					MockQueryResult{"m4", "4", {}}};

	calculate_threads(results, true);

	assert_thread_paths(results,
			    {{"m1", "1:f:f:z"}, {"m2", "1:f:z"}, {"m3", "1:z"}, {"m4", "0:z"}});
}

static void
test_id_table_inconsistent()
{
	auto results = MockQueryResults{
	    MockQueryResult{"m1", "1", {"m2"}}, // 1->2
	    MockQueryResult{"m2", "2", {"m1"}}, // 2->1
	    MockQueryResult{"m3", "3", {"m3"}}, // self ref
	    MockQueryResult{"m4", "4", {"m3", "m5"}},
	    MockQueryResult{"m5", "5", {"m4", "m4"}}, // dup parent
	};

	calculate_threads(results, false);
	assert_thread_paths(results,
			    {
				{"m2", "0"},
				{"m1", "0:0"},
				{"m3", "1"},
				{"m5", "1:0"},
				{"m4", "1:0:0"},
			    });
}

static void
test_dups_dup_last()
{
	MockQueryResult r1{"m1", "1", {}};
	r1.query_match().flags |= QueryMatch::Flags::Leader;
	r1.path_ = "/path1";

	MockQueryResult r1_dup{"m1", "1", {}};
	r1_dup.query_match().flags |= QueryMatch::Flags::Duplicate;
	r1_dup.path_ = "/path2";

	auto results = MockQueryResults{r1, r1_dup};

	calculate_threads(results, false);

	assert_thread_paths(results,
			    {
				{"/path1", "0"},
				{"/path2", "0:0"},
			    });
}

static void
test_dups_dup_first()
{
	// now dup becomes the leader; this will _demote_
	// r1.

	MockQueryResult r1_dup{"m1", "1", {}};
	r1_dup.query_match().flags |= QueryMatch::Flags::Duplicate;
	r1_dup.path_ = "/path1";

	MockQueryResult r1{"m1", "1", {}};
	r1.query_match().flags |= QueryMatch::Flags::Leader;
	r1.path_ = "/path2";

	auto results = MockQueryResults{r1_dup, r1};

	calculate_threads(results, false);

	assert_thread_paths(results, {
			{"/path2", "0"},
			{"/path1", "0:0"},
		});
}

static void
test_dups_dup_multi()
{
	// now dup becomes the leader; this will _demote_
	// r1.

	MockQueryResult r1_dup1{"m1", "1", {}};
	r1_dup1.query_match().flags |= QueryMatch::Flags::Duplicate;
	r1_dup1.path_ = "/path1";

	MockQueryResult r1_dup2{"m1", "1", {}};
	r1_dup2.query_match().flags |= QueryMatch::Flags::Duplicate;
	r1_dup2.path_ = "/path2";

	MockQueryResult r1{"m1", "1", {}};
	r1.query_match().flags |= QueryMatch::Flags::Leader;
	r1.path_ = "/path3";

	auto results = MockQueryResults{r1_dup1, r1_dup2, r1};
	calculate_threads(results, false);

	assert_thread_paths(results, {
			{"/path3", "0"},
			{"/path1", "0:0"},
			{"/path2", "0:1"},
		});
}




static void
test_do_not_prune_root_empty_with_children()
{
	// m7 should not be nuked
	auto results = MockQueryResults{
	    MockQueryResult{"x1", "1", {"m7"}},
	    MockQueryResult{"x2", "2", {"m7"}},
	};

	calculate_threads(results, false);

	assert_thread_paths(results,
			    {
				{"x1", "0:0"},
				{"x2", "0:1"},
			    });
}

static void
test_prune_root_empty_with_child()
{
	// m7 should be nuked
	auto results = MockQueryResults{
	    MockQueryResult{"m1", "1", {"m7"}},
	};

	calculate_threads(results, false);

	assert_thread_paths(results,
			    {
				{"m1", "0"},
			    });
}

static void
test_prune_empty_with_children()
{
	// m6 should be nuked
	auto results = MockQueryResults{
	    MockQueryResult{"m1", "1", {"m7", "m6"}},
	    MockQueryResult{"m2", "2", {"m7", "m6"}},
	};

	calculate_threads(results, false);

	assert_thread_paths(results,
			    {
				{"m1", "0:0"},
				{"m2", "0:1"},
			    });
}

static void
test_thread_info_ascending()
{
	auto results = MockQueryResults{
	    MockQueryResult{"m1", "5", {}},
	    MockQueryResult{"m2", "1", {}},
	    MockQueryResult{"m3", "3", {"m2"}},
	    MockQueryResult{"m4", "2", {"m2"}},
	    // orphan siblings
	    MockQueryResult{"m10", "6", {"m9"}},
	    MockQueryResult{"m11", "7", {"m9"}},
	};
	calculate_threads(results, false);

	assert_thread_paths(results,
			    {
				{"m2", "0"},   // 2
				{"m4", "0:0"}, //   2
				{"m3", "0:1"}, //   3
				{"m1", "1"},   // 5

				{"m10", "2:0"}, //   6
				{"m11", "2:1"}, //   7
			    });

	g_assert_true(results[0].query_match().has_flag(QueryMatch::Flags::Root));
	g_assert_true(results[1].query_match().has_flag(QueryMatch::Flags::Root |
							QueryMatch::Flags::HasChild));
	g_assert_true(results[2].query_match().has_flag(QueryMatch::Flags::Last));
	g_assert_true(results[3].query_match().has_flag(QueryMatch::Flags::First));
	g_assert_true(results[4].query_match().has_flag(QueryMatch::Flags::Orphan |
							QueryMatch::Flags::First));
	g_assert_true(
	    results[5].query_match().has_flag(QueryMatch::Flags::Orphan | QueryMatch::Flags::Last));
}

static void
test_thread_info_descending()
{
	auto results = MockQueryResults{
	    MockQueryResult{"m1", "5", {}},
	    MockQueryResult{"m2", "1", {}},
	    MockQueryResult{"m3", "3", {"m2"}},
	    MockQueryResult{"m4", "2", {"m2"}},
	    // orphan siblings
	    MockQueryResult{"m10", "6", {"m9"}},
	    MockQueryResult{"m11", "7", {"m9"}},
	};
	calculate_threads(results, true /*descending*/);

	assert_thread_paths(results,
			    {
				{"m1", "1:z"},   // 5
				{"m2", "2:z"},   // 2
				{"m4", "2:f:z"}, //   2
				{"m3", "2:e:z"}, //   3

				{"m10", "0:f:z"}, //   6
				{"m11", "0:e:z"}, //   7
			    });
	g_assert_true(results[0].query_match().has_flag(QueryMatch::Flags::Root));
	g_assert_true(results[1].query_match().has_flag(QueryMatch::Flags::Root |
							QueryMatch::Flags::HasChild));
	g_assert_true(results[2].query_match().has_flag(QueryMatch::Flags::Last));
	g_assert_true(results[3].query_match().has_flag(QueryMatch::Flags::First));

	g_assert_true(
	    results[4].query_match().has_flag(QueryMatch::Flags::Orphan | QueryMatch::Flags::Last));
	g_assert_true(results[5].query_match().has_flag(QueryMatch::Flags::Orphan |
							QueryMatch::Flags::First));
}

int
main(int argc, char* argv[])
try {
	g_test_init(&argc, &argv, NULL);

	g_test_add_func("/threader/sort/ascending", test_sort_ascending);
	g_test_add_func("/threader/sort/decending", test_sort_descending);

	g_test_add_func("/threader/id-table-inconsistent", test_id_table_inconsistent);
	g_test_add_func("/threader/dups/dup-last", test_dups_dup_last);
	g_test_add_func("/threader/dups/dup-first", test_dups_dup_first);
	g_test_add_func("/threader/dups/dup-multi", test_dups_dup_multi);

	g_test_add_func("/threader/prune/do-not-prune-root-empty-with-children",
			test_do_not_prune_root_empty_with_children);
	g_test_add_func("/threader/prune/prune-root-empty-with-child",
			test_prune_root_empty_with_child);
	g_test_add_func("/threader/prune/prune-empty-with-children",
			test_prune_empty_with_children);

	g_test_add_func("/threader/thread-info/ascending", test_thread_info_ascending);
	g_test_add_func("/threader/thread-info/descending", test_thread_info_descending);

	return g_test_run();
} catch (const std::runtime_error& re) {
	std::cerr << re.what() << "\n";
	return 1;
} catch (...) {
	std::cerr << "caught exception\n";
	return 1;
}

#endif /*BUILD_TESTS*/
