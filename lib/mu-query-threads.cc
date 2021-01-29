/*
** Copyright (C) 2021 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include "mu-msg-fields.h"

#include <set>
#include <unordered_set>
#include <cassert>
#include <cstring>
#include <iostream>
#include <iomanip>

#include <utils/mu-option.hh>

using namespace Mu;

struct Container {
        using children_type  = std::unordered_set<Container*>;
        Container() = default;
        Container(Option<QueryMatch&> msg): query_match{msg} {}
        Container(const Container&) = delete;
        Container(Container&&) = default;

        void set_parent (Container* new_parent) {
                assert(this != new_parent);
                assert(!new_parent->is_reachable(this));
                if (new_parent == parent)
                        return;
                if (parent)
                        parent->remove_child(*this);
                if (new_parent)
                        new_parent->add_child(*this);
                else
                        parent = new_parent;
                assert(this->parent != this);
        }

        void add_child (Container& new_child) {
               assert(!new_child.parent);
               new_child.parent = this;
               children.emplace(&new_child);
        }
        void promote_children () {
                for_each_child([&](auto&& child){
                        child->parent = {};
                        if (parent)
                                parent->add_child(*child);
                });
                children.clear();
                if (parent)
                        parent->remove_child(*this);
                is_nuked = true;
                assert(!parent);
                assert(children.empty());
        }
        void remove_child (Container& child) {
                if (!has_child(child))
                        g_warning("not my child");
                //assert(has_child(child));
                child.parent = {};
                children.erase(&child);
                assert(!has_child(child));
        }

        bool has_child (Container& child) const {
                return children.find(&child) != children.cend();
        }

        bool is_reachable(Container* other) const {
                return ur_parent() == other->ur_parent();
        }

        void borrow_query_match (Container& other) {
                assert(!query_match);
                assert(other.query_match);
                query_match = other.query_match;
                is_borrowed_query_match = true;
                if (parent) { // and renew (for sorting)
                        auto p{parent};
                        parent->remove_child(*this);
                        p->add_child(*this);
                        assert(parent->has_child(*this));
                }
        }

        template <typename Func> void for_each_child (Func&& func) {
                auto it{children.begin()};
                while (it != children.end()) {
                        auto next = std::next(it);
                        func(*it);
                        it = next;
                }
        }

        bool is_empty() const {
                return !query_match || is_borrowed_query_match;
        }

        Option<QueryMatch&> query_match;
        bool                is_borrowed_query_match{};
        bool                is_nuked{};

        Container*           parent{};
        children_type        children;

private:
        const Container* ur_parent() const  {
                assert(this->parent != this);
                return parent ? parent->ur_parent() : this;
        }
};

static std::ostream&
operator<<(std::ostream& os, const Container& container)
{
        os << "container: " << std::right << std::setw(10) << &container
           << ": parent: "  << std::right << std::setw(10) << container.parent
           << "\n  children: ";

        for (auto&& c: container.children)
                os << std::right << std::setw(10) << c << " ";

        os << (container.is_nuked ? " nuked" : "")
           << (container.is_borrowed_query_match ? " borrowed" : "");

        if (container.query_match)
                os << "\n  " << container.query_match.value();

        return os;
}


using IdTable = std::unordered_map<std::string, Container>;
using DupTable = std::multimap<std::string, Container>;
//template <typename QueryResultsType> using DupsVec = std::vector<decltype(QueryResultsType::value_type)>;

static void
handle_duplicates (IdTable& id_table, DupTable& dup_table)
{
        size_t n{};

        for (auto&& dup: dup_table) {
                const auto msgid{dup.first};
                auto it = id_table.find(msgid);
                if (it == id_table.end())
                        continue;

                // add duplicates as fake children
                char buf[32];
                ::snprintf(buf, sizeof(buf), "bastard-%zu", ++n);
                it->second.add_child(
                        id_table.emplace(buf, std::move(dup.second)).first->second);
        }
}

template <typename QueryResultsType>
static IdTable
determine_id_table (QueryResultsType& qres, MuMsgFieldId sortfield_id)
{
        // 1. For each query_match
        IdTable id_table;
        DupTable dups;
        for (auto&& mi: qres) {
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
                auto c_it = id_table.find(msgid);
                auto& container = [&]()->Container& {
                        if (c_it != id_table.end()) {
                                assert(!c_it->second.query_match);
                                c_it->second.query_match = mi.query_match();
                                return c_it->second;
                        } else {
                                // Else:
                                // Create a new Container object holding this query_match (query-match);
                                // Index the Container by Query_Match-ID
                                return id_table.emplace(msgid, mi.query_match()).first->second;
                        }
                }();

                // We sort by date (ascending), *except* for the root; we don't
                // know what query_matchs will be at the root level yet, so remember
                // both. Moreover, even when sorting the top-level in descending
                // order, still sort the thread levels below that in ascending
                // order.
                if (sortfield_id != MU_MSG_FIELD_ID_NONE)
                        container.query_match->sort_key = mi.opt_string(sortfield_id).value_or("");
                container.query_match->date_key         = mi.opt_string(MU_MSG_FIELD_ID_DATE).value_or("");

                // remember the subject, we use it to determine the (sub)thread subject
                container.query_match->subject = mi.opt_string(MU_MSG_FIELD_ID_SUBJECT).value_or("");

                // 1.B
                // For each element in the query_match's References field:
                Container* parent_ref_container{};
                for (const auto& ref: mi.references()) {
                        //   grand_<n>-parent -> grand_<n-1>-parent -> ... -> parent.

                        // Find a Container object for the given Query_Match-ID; If it exists, use it;
                        // otherwise make one with a null Query_Match.
                        auto ref_container = [&]()->Container* {
                                auto ref_it = id_table.find(ref);
                                if (ref_it == id_table.end())
                                        ref_it = id_table.emplace(ref,Nothing).first;
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
                        if (parent_ref_container && !ref_container->parent &&
                            !parent_ref_container->is_reachable(ref_container))
                                        parent_ref_container->add_child(*ref_container);

                        parent_ref_container = ref_container;
                }

                // Add the query_match to the chain.
                if (parent_ref_container && !container.parent &&
                    !parent_ref_container->is_reachable(&container)) {
                        parent_ref_container->add_child(container);
                }
        }

        // non-JWZ: add duplicate messages.
        handle_duplicates (id_table, dups);

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
prune_empty_containers (Container& container)
{
        container.for_each_child([](auto&& child){prune_empty_containers(*child);});

        // Never nuke these.
        if (!container.is_empty())
                return;

        if (container.children.empty()) {
                // If it is an empty container with no children, nuke it.
                if (container.parent) {
                        if (!container.parent->has_child(container)) {
                                container.parent = {};
                                g_warning ("unexpected parent->child relation");
                        } else
                                container.parent->remove_child(container);
                }
                container.is_nuked = true;
                return;
        }
        // If the Container is empty, but does have children, remove this
        // container but promote its children to this level (that is, splice them in
        // to the current child list.)
        //
        // Do not promote the children if doing so would promote them to the root
        // set -- unless there is only one child, in which case, do.
        //const auto rootset_child{!container.parent->parent};
        if (container.parent || container.children.size() == 1) {
                container.promote_children();
                container.is_nuked = true;
        } else if (!container.children.empty()){
                // so an empty container with children. Copy the query info of the first
                // child, for sorting -- so the sort key "bubbles up". Renew
                // it so the sorting workes out.
                auto& first_child{*container.children.begin()};
                container.borrow_query_match(*first_child);
        }
}


static void
prune_empty_containers (IdTable& id_table)
{
        for (auto&& item: id_table) {
                if (!item.second.parent)
                        prune_empty_containers(item.second);
        }
}


/// Sorting.
///
/// We start the sorting from the rout-vec, ie. the set of of parentless conainers.
///
/// We need to sort the rootset by whatever the sortkey is (subject, date, ...); however under the
/// rotset we stricly sort in ascending order by date.  Containers with empty query_matchs have the
/// sort key from the first of their children (recursively).
//
// Note, children are already stored in a (sorted) std::set, based on their date. That's correct for
// all but the top-level (root) containers; so, we just need fix those.
//

// the root_vec is the sorted vec of top-level (parent-less) containers.
using RootVec = std::vector<Container*>;
static RootVec
determine_root_vec(IdTable& id_table, bool descending)
{
        RootVec root_vec;

        for (auto&& item: id_table) {
                Container* c{&item.second};
                if (!c || !c->query_match || c->parent || c->is_nuked)
                        continue;
                root_vec.emplace_back(c);
        }

        std::sort(root_vec.begin(), root_vec.end(),
                  [&](Container*& c1, Container*& c2)->bool {
#ifdef BUILD_TESTS
                          if (descending)
                                  return c2->query_match->sort_key < c1->query_match->sort_key;
                           else
                                   return c1->query_match->sort_key < c2->query_match->sort_key;
#else
                          // the non-testing case, the "descending" part is handled
                          // in the "decider"
                          return c1->query_match->sort_key < c2->query_match->sort_key;
#endif /*BUILD_TESTS*/
                  });

        return root_vec;
}



static bool
update_container_query_match (Container& container,
                              ThreadPathVec& pvec,
                              size_t segment_size, bool descending,
                              const std::string& prev_subject="")
{
        if (container.is_empty())
                return false; // nothing to update.

        auto& qmatch{*container.query_match};

        if (!container.parent)
                qmatch.flags |= QueryMatch::Flags::Root;
        else if (container.parent->is_empty())
                qmatch.flags |= QueryMatch::Flags::Orphan;

        if (!container.children.empty())
                qmatch.flags |= QueryMatch::Flags::HasChild;

        // calculate the "thread-subject", which is for UI
        // purposes.
        if (qmatch.has_flag(QueryMatch::Flags::Root) ||
            (qmatch.subject.find(prev_subject) > 5))
            qmatch.flags |= QueryMatch::Flags::ThreadSubject;

        if (descending && container.parent) {
                // trick xapian by giving it "inverse" sorting key so our
                // ascending-date sorted threads stay in that order
                pvec.back() = ((1U << (4 * segment_size)) - 1) - pvec.back();
        }

        qmatch.thread_path  = to_string(pvec, segment_size);
        qmatch.thread_level = pvec.size() - 1;

        // ensure thread root comes before its children
        if (descending)
                qmatch.thread_path += ":z";

        return true;
}



static void
sort_siblings (Container::children_type& siblings,
               const ThreadPathVec& parent_path_vec,
               size_t segment_size, bool descending,
               const std::string& last_subject="")
{
        if (siblings.empty())
                return;

        // sort by date.
        auto compare =[](const Container *c1, const Container *c2) {
                const auto cmp{std::strcmp(c1->query_match->date_key.c_str(),
                                           c2->query_match->date_key.c_str())};
                // sort by date, or, if the same, by addresss
                return cmp ? cmp < 0 : c1 < c2;
        };
        std::set<Container*, decltype(compare)> sorted_siblings{compare};
        for (auto&& container: siblings)
                sorted_siblings.emplace(std::move(container));

        const auto first{*sorted_siblings.begin()};
        if (first->query_match)
                first->query_match->flags |= QueryMatch::Flags::First;
        const auto last{*(--sorted_siblings.end())};
        if (last->query_match)
                last->query_match->flags |= QueryMatch::Flags::Last;

        size_t idx{0};
        std::string siblings_last_subject{last_subject};
        ThreadPathVec thread_path_vec{parent_path_vec};
        for (auto&& c: sorted_siblings) {
                thread_path_vec.emplace_back(idx++);
                if (update_container_query_match (*c, thread_path_vec,
                                                  segment_size, descending,
                                                  siblings_last_subject)) {
                        siblings_last_subject = c->query_match->subject;
                }
                if (!c->children.empty())
                        sort_siblings (c->children, thread_path_vec,
                                       segment_size, descending,
                                       siblings_last_subject);

                thread_path_vec.pop_back();
        }
}


static void
sort_siblings (IdTable& id_table, bool descending)
{
        if (id_table.empty())
                return;

        auto root_vec{determine_root_vec(id_table, descending)}; // sorted

        const auto seg_size = static_cast<size_t>(
                std::ceil(std::log2(id_table.size())/4.0));
        /*note: 4 == std::log2(16)*/

        ThreadPathVec path_vec;
        auto idx{0U};

        for (auto&& c: root_vec) {
                path_vec.emplace_back(idx++);
                update_container_query_match (*c, path_vec, seg_size, descending);
                sort_siblings (c->children, path_vec, seg_size, descending);
                path_vec.pop_back();
        }
}

static std::ostream&
operator<<(std::ostream& os, const IdTable& id_table)
{
        std::set<std::string> ids;
        for (auto&& item: id_table) {
                if (item.second.query_match)
                        ids.emplace(item.second.query_match->thread_path);
        }

        for (auto&& id: ids) {
                auto it = std::find_if(id_table.begin(), id_table.end(), [&](auto&& item) {
                        return item.second.query_match && item.second.query_match->thread_path == id;
                });
                assert(it != id_table.end());
                os << it->first << ": " << it->second << '\n';
        }
        return os;
}


template<typename Results> static void
calculate_threads_real (Results& qres, MuMsgFieldId sort_field,
                   bool descending)
{
        // Step 1: build the id_table
        auto id_table{determine_id_table(qres, sort_field)};

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

        if (g_test_verbose())
                std::cout << "*** id-table:\n" << id_table << "\n";
}

void
Mu::calculate_threads (Mu::QueryResults& qres, MuMsgFieldId sort_field,
                       bool descending)
{
        calculate_threads_real(qres, sort_field, descending);
}

#ifdef BUILD_TESTS

struct MockQueryResult {
        MockQueryResult(const std::string& message_id_arg,
                        const std::string& sort_key_arg,
                        const std::string& date_key_arg,
                        const std::vector<std::string>& refs_arg={}):
                message_id_{message_id_arg},
                sort_key_{sort_key_arg},
                date_key_{date_key_arg},
                refs_{refs_arg}
                {}
        MockQueryResult(const std::string& message_id_arg,
                        const std::vector<std::string>& refs_arg={}):
                MockQueryResult(message_id_arg, "", "", refs_arg) {}
        Option<std::string>             message_id()  const { return message_id_;}
        Option<std::string>             path()        const { return path_;}
        QueryMatch&                     query_match()       { return query_match_;}
        const QueryMatch&               query_match() const { return query_match_;}
        const std::vector<std::string>& references()  const { return refs_;}

        Option<std::string>             opt_string(MuMsgFieldId id) const {
                if (id == MU_MSG_FIELD_ID_DATE)
                        return date_key_;
                else
                        return sort_key_;
        }
        Option<std::string>      path_{"/"};
        std::string              message_id_;
        QueryMatch               query_match_{};
        std::string              sort_key_;
        std::string              date_key_;
        std::vector<std::string> refs_;
};

using MockQueryResults = std::vector<MockQueryResult>;


G_GNUC_UNUSED static std::ostream&
operator<<(std::ostream& os, const MockQueryResults& qrs)
{
        for (auto&& mi: qrs)
                os << mi.query_match().thread_path << " :: "
                   << mi.message_id().value_or("<none>") <<  std::endl;

        return os;
}

static void
calculate_threads (MockQueryResults& qres, MuMsgFieldId sort_field,
                   bool descending)
{
        calculate_threads_real(qres, sort_field, descending);
}

using Expected = std::vector<std::pair<std::string, std::string>>;


static void
assert_thread_paths (const MockQueryResults& qrs, const Expected& expected)
{
        for (auto&& exp: expected) {
                auto it = std::find_if(qrs.begin(), qrs.end(), [&](auto&& qr){
                        return qr.message_id().value_or("") == exp.first ||
                                qr.path().value_or("") == exp.first;
                });
                g_assert_true (it != qrs.end());
                g_assert_cmpstr(exp.second.c_str(), ==, it->query_match().thread_path.c_str());
        }
}

static void
test_sort_ascending()
{
        auto results = MockQueryResults {
                MockQueryResult{ "m1", "a", "1",  {"m2"} },
                MockQueryResult{ "m2", "b", "2",  {"m3"} },
                MockQueryResult{ "m3", "c", "3",  {}},
                MockQueryResult{ "m4", "d", "4",  {}}
        };

        calculate_threads(results, MU_MSG_FIELD_ID_SUBJECT, false);

        assert_thread_paths (results, {
                        { "m1", "0:0:0"},
                        { "m2", "0:0"  },
                        { "m3", "0"    },
                        { "m4", "1"    }
                });
}


static void
test_sort_descending()
{
        auto results = MockQueryResults {
                MockQueryResult{ "m1", "a", "1",  {"m2"} },
                MockQueryResult{ "m2", "b", "2",  {"m3"} },
                MockQueryResult{ "m3", "c", "3",  {}},
                MockQueryResult{ "m4", "d", "4",  {}}
        };

        calculate_threads(results, MU_MSG_FIELD_ID_SUBJECT, true);

        assert_thread_paths (results, {
                        { "m1", "1:f:f:z"},
                        { "m2", "1:f:z"  },
                        { "m3", "1:z"    },
                        { "m4", "0:z"    }
                });
}

static void
test_id_table_inconsistent()
{
        auto results = MockQueryResults {
                MockQueryResult{ "m1", "a", "1", {"m2"} },
                MockQueryResult{ "m2", "b", "2", {"m1"} },
                MockQueryResult{ "m3", "c", "3", {"m3"} }, // self ref
                MockQueryResult{ "m4", "d", "4", {"m3", "m5"} },
                MockQueryResult{ "m5", "e", "5", {"m4", "m4"} }, // dup parent
        };

        calculate_threads(results, MU_MSG_FIELD_ID_DATE, false);
        assert_thread_paths (results, {
                        { "m2", "0"},
                        { "m1", "0:0" },
                        { "m3", "1"},
                        { "m5", "1:0"  },
                        { "m4", "1:0:0"},
                });
}

static void
test_dups_dup_last()
{
        MockQueryResult r1 { "m1", "a", "1", {} };
        r1.query_match().flags |= QueryMatch::Flags::Leader;
        r1.path_ = "/path1";

        MockQueryResult r1_dup { "m1", "a", "1", {} };
        r1_dup.query_match().flags |= QueryMatch::Flags::Duplicate;
        r1_dup.path_ = "/path2";

        auto results = MockQueryResults {r1, r1_dup };

        calculate_threads(results, MU_MSG_FIELD_ID_DATE, false);

        assert_thread_paths (results, {
                        { "/path1", "0"},
                        { "/path2", "0:0" },
                });
}

static void
test_dups_dup_first()
{
        // now dup becomes the leader; this will _demote_
        // r1.

        MockQueryResult r1_dup { "m1", "a", "1", {} };
        r1_dup.query_match().flags |= QueryMatch::Flags::Duplicate;
        r1_dup.path_ = "/path1";

        MockQueryResult r1 { "m1", "a", "1", {} };
        r1.query_match().flags |= QueryMatch::Flags::Leader;
        r1.path_ = "/path2";

        auto results = MockQueryResults { r1_dup, r1 };

        calculate_threads(results, MU_MSG_FIELD_ID_DATE, false);

        assert_thread_paths (results, {
                        { "/path2", "0"},
                        { "/path1", "0:0" },
                });
}


static void
test_do_not_prune_root_empty_with_children()
{
        // m7 should not be nuked
        auto results = MockQueryResults {
                MockQueryResult{ "x1", "a", "1",  {"m7"} },
                MockQueryResult{ "x2", "b", "2",  {"m7"} },
        };

        calculate_threads(results, MU_MSG_FIELD_ID_SUBJECT, false);

        assert_thread_paths (results, {
                        { "x1", "0:0"},
                        { "x2", "0:1"  },
                });
}

static void
test_prune_root_empty_with_child()
{
        // m7 should be nuked
        auto results = MockQueryResults {
                MockQueryResult{ "m1", "a", "1",  {"m7"} },
        };

        calculate_threads(results, MU_MSG_FIELD_ID_SUBJECT, false);

        assert_thread_paths (results, {
                        { "m1", "0"},
                });
}

static void
test_prune_empty_with_children()
{
        // m6 should be nuked
        auto results = MockQueryResults {
                MockQueryResult{ "m1", "a", "1",  {"m7", "m6"} },
                MockQueryResult{ "m2", "b", "2",  {"m7", "m6"} },
        };

        calculate_threads(results, MU_MSG_FIELD_ID_SUBJECT, false);

        assert_thread_paths (results, {
                        { "m1", "0:0"},
                        { "m2", "0:1"  },
                });
}


static void
test_thread_info_ascending()
{
        // m6 should be nuked
        auto results = MockQueryResults {
                MockQueryResult{ "m1", "a", "1", {}},
                MockQueryResult{ "m2", "b", "2",  {}},
                MockQueryResult{ "m3", "c", "3",  {"m2"}},
                MockQueryResult{ "m4", "d", "4",  {"m2"}},

        };
        calculate_threads(results, MU_MSG_FIELD_ID_DATE, false);

        assert_thread_paths (results, {
                        { "m1", "0"},
                        { "m2", "1"  },
                        { "m3", "1:0"},
                        { "m4", "1:1"  },
                });

        g_assert_true (results[0].query_match().has_flag(
                               QueryMatch::Flags::Root));
        g_assert_true (results[1].query_match().has_flag(
                               QueryMatch::Flags::Root | QueryMatch::Flags::HasChild));
        g_assert_true (results[2].query_match().has_flag(
                               QueryMatch::Flags::First));
        g_assert_true (results[3].query_match().has_flag(
                               QueryMatch::Flags::Last));
}



int
main (int argc, char *argv[]) try
{
        g_test_init (&argc, &argv, NULL);

        g_test_add_func ("/threader/sort/ascending", test_sort_ascending);
        g_test_add_func ("/threader/sort/decending", test_sort_descending);

        g_test_add_func ("/threader/id-table-inconsistent", test_id_table_inconsistent);
        g_test_add_func ("/threader/dups/dup-last",  test_dups_dup_last);
        g_test_add_func ("/threader/dups/dup-first",  test_dups_dup_first);

        g_test_add_func ("/threader/prune/prune-root-empty-with-children",
                         test_prune_empty_with_children);
        g_test_add_func ("/threader/prune/do-not-prune-root-empty-with-children",
                         test_do_not_prune_root_empty_with_children);
        g_test_add_func ("/threader/prune/prune-root-empty-with-child",
                         test_prune_root_empty_with_child);
        g_test_add_func ("/threader/prune/prune-empty-with-child",
                         test_prune_empty_with_children);
        g_test_add_func ("/threader/thread-info/ascending",
                         test_thread_info_ascending);

        return g_test_run ();

} catch (const std::runtime_error& re) {
        std::cerr << re.what() << "\n";
        return 1;
} catch (...) {
        std::cerr << "caught exception\n";
        return 1;
}

#endif /*BUILD_TESTS*/
