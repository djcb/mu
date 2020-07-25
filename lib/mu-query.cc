/*
** Copyright (C) 2008-2017 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <stdexcept>
#include <string>
#include <cctype>
#include <cstring>
#include <sstream>

#include <stdlib.h>
#include <xapian.h>
#include <glib/gstdio.h>

#include "mu-query.h"
#include "mu-msg-fields.h"

#include "mu-msg-iter.h"

#include "utils/mu-str.h"
#include "utils/mu-date.h"
#include <utils/mu-utils.hh>

#include <query/mu-proc-iface.hh>
#include <query/mu-xapian.hh>

using namespace Mu;

struct MuProc: public Mu::ProcIface {

	MuProc (const Xapian::Database& db): db_{db} {}

	static MuMsgFieldId field_id (const std::string& field) {

		if (field.empty())
			return MU_MSG_FIELD_ID_NONE;

		MuMsgFieldId id = mu_msg_field_id_from_name (field.c_str(), FALSE);
		if (id != MU_MSG_FIELD_ID_NONE)
			return id;
		else if (field.length() == 1)
			return mu_msg_field_id_from_shortcut (field[0], FALSE);
		else
			return MU_MSG_FIELD_ID_NONE;
	}

	std::string
	process_value (const std::string& field,
		       const std::string& value) const override {
		const auto id = field_id (field);
		if (id == MU_MSG_FIELD_ID_NONE)
			return value;
		switch(id) {
		case MU_MSG_FIELD_ID_PRIO: {
			if (!value.empty())
				return std::string(1, value[0]);
		} break;

		case MU_MSG_FIELD_ID_FLAGS: {
			const auto flag = mu_flag_char_from_name (value.c_str());
			if (flag)
				return std::string(1, tolower(flag));
		} break;

		default:
			break;
		}

		return value; // XXX prio/flags, etc. alias
	}

	void add_field (std::vector<FieldInfo>& fields, MuMsgFieldId id) const {

		const auto shortcut = mu_msg_field_shortcut(id);
		if (!shortcut)
			return; // can't be searched

		const auto name = mu_msg_field_name (id);
		const auto pfx  = mu_msg_field_xapian_prefix (id);

		if (!name || !pfx)
			return;

		fields.push_back ({{name}, {pfx},
				   (bool)mu_msg_field_xapian_index(id),
				   id});
	}

	std::vector<FieldInfo>
	process_field (const std::string& field) const override {

		std::vector<FieldInfo> fields;

		if (field == "contact" || field == "recip") { // multi fields
			add_field (fields, MU_MSG_FIELD_ID_TO);
			add_field (fields, MU_MSG_FIELD_ID_CC);
			add_field (fields, MU_MSG_FIELD_ID_BCC);
			if (field == "contact")
				add_field (fields, MU_MSG_FIELD_ID_FROM);
		} else if (field == "") {
			add_field (fields, MU_MSG_FIELD_ID_TO);
			add_field (fields, MU_MSG_FIELD_ID_CC);
			add_field (fields, MU_MSG_FIELD_ID_BCC);
			add_field (fields, MU_MSG_FIELD_ID_FROM);
			add_field (fields, MU_MSG_FIELD_ID_SUBJECT);
			add_field (fields, MU_MSG_FIELD_ID_BODY_TEXT);
		} else {
			const auto id = field_id (field.c_str());
			if (id != MU_MSG_FIELD_ID_NONE)
				add_field (fields, id);
		}

		return fields;
	}

	bool is_range_field (const std::string& field) const override {
		const auto id = field_id (field.c_str());
		if (id == MU_MSG_FIELD_ID_NONE)
			return false;
		else
			return mu_msg_field_is_range_field (id);
	}

	Range process_range (const std::string& field, const std::string& lower,
			     const std::string& upper) const override {

		const auto id = field_id (field.c_str());
		if (id == MU_MSG_FIELD_ID_NONE)
			return { lower, upper };

		std::string	l2 = lower;
		std::string	u2 = upper;

		if (id == MU_MSG_FIELD_ID_DATE) {
			l2 = Mu::date_to_time_t_string (lower, true);
			u2 = Mu::date_to_time_t_string (upper, false);
		} else if (id == MU_MSG_FIELD_ID_SIZE) {
			l2 = Mu::size_to_string (lower, true);
			u2 = Mu::size_to_string (upper, false);
		}

		return { l2, u2 };
	}

	std::vector<std::string>
	process_regex (const std::string& field, const std::regex& rx) const override {

		const auto id = field_id (field.c_str());
		if (id == MU_MSG_FIELD_ID_NONE)
			return {};

		char pfx[] = {  mu_msg_field_xapian_prefix(id), '\0' };

		std::vector<std::string> terms;
		for (auto it = db_.allterms_begin(pfx); it != db_.allterms_end(pfx); ++it) {
			if (std::regex_search((*it).c_str() + 1, rx)) // avoid copy
				terms.push_back(*it);
		}

		return terms;
	}

	const Xapian::Database& db_;
};

struct _MuQuery {
public:
	_MuQuery (MuStore *store): _store(mu_store_ref(store)) {}
	~_MuQuery () { mu_store_unref (_store); }

	Xapian::Database& db() const {
		const auto db = reinterpret_cast<Xapian::Database*>
			(mu_store_get_read_only_database (_store));
		if (!db)
			throw Mu::Error(Error::Code::NotFound, "no database");
		return *db;
	}
private:
	MuStore *_store;
};

static const Xapian::Query
get_query (MuQuery *mqx, const char* searchexpr, bool raw, GError **err) try {

	Mu::WarningVec warns;
	const auto tree = Mu::parse (searchexpr, warns,
				      std::make_unique<MuProc>(mqx->db()));
	for (auto&& w: warns)
		std::cerr << w << std::endl;

	return Mu::xapian_query (tree);

} catch (...) {
	mu_util_g_set_error (err,MU_ERROR_XAPIAN_QUERY,
			     "parse error in query");
	throw;
}

MuQuery*
mu_query_new (MuStore *store, GError **err)
{
	g_return_val_if_fail (store, NULL);

	try {
		return new MuQuery (store);
	} MU_XAPIAN_CATCH_BLOCK_G_ERROR_RETURN (err, MU_ERROR_XAPIAN, 0);

	return 0;
}

void
mu_query_destroy (MuQuery *self)
{
	try { delete self; } MU_XAPIAN_CATCH_BLOCK;
}


/* this function is for handling the case where a DatabaseModified
 * exception is raised. We try to reopen the database, and run the
 * query again. */
static MuMsgIter *
try_requery (MuQuery *self, const char* searchexpr, MuMsgFieldId sortfieldid,
	     int maxnum, MuQueryFlags flags, GError **err)
{
	try {
		/* let's assume that infinite regression is
		 * impossible */
		self->db().reopen();
		g_message ("reopening db after modification");
		return mu_query_run (self, searchexpr, sortfieldid,
				     maxnum, flags, err);

	} MU_XAPIAN_CATCH_BLOCK_G_ERROR_RETURN (err, MU_ERROR_XAPIAN, 0);
}


static MuMsgIterFlags
msg_iter_flags (MuQueryFlags flags)
{
	MuMsgIterFlags iflags;

	iflags = MU_MSG_ITER_FLAG_NONE;

	if (flags & MU_QUERY_FLAG_DESCENDING)
		iflags |= MU_MSG_ITER_FLAG_DESCENDING;
	if (flags & MU_QUERY_FLAG_SKIP_UNREADABLE)
		iflags |= MU_MSG_ITER_FLAG_SKIP_UNREADABLE;
	if (flags & MU_QUERY_FLAG_SKIP_DUPS)
		iflags |= MU_MSG_ITER_FLAG_SKIP_DUPS;
	if (flags & MU_QUERY_FLAG_THREADS)
		iflags |= MU_MSG_ITER_FLAG_THREADS;

	return iflags;
}



static Xapian::Enquire
get_enquire (MuQuery *self, const char *searchexpr, MuMsgFieldId sortfieldid,
	     bool descending, bool raw, GError **err)
{
	Xapian::Enquire enq (self->db());

	try {
		if (raw)
			enq.set_query(Xapian::Query(Xapian::Query(searchexpr)));
		else if (!mu_str_is_empty(searchexpr) &&
		    g_strcmp0 (searchexpr, "\"\"") != 0) /* NULL or "" or """" */
			enq.set_query(get_query (self, searchexpr, raw, err));
		else/* empty or "" means "matchall" */
			enq.set_query(Xapian::Query::MatchAll);
	} catch (...) {
		mu_util_g_set_error (err, MU_ERROR_XAPIAN_QUERY,
				     "parse error in query");
		throw;
	}

	enq.set_cutoff(0,0);
	return enq;
}

/*
 * record all threadids for the messages; also 'orig_set' receives all
 * original matches (a map msgid-->docid), so we can make sure the
 * originals are not seen as 'duplicates' later (when skipping
 * duplicates).  We want to favor the originals over the related
 * messages, when skipping duplicates.
 */
static GHashTable*
get_thread_ids (MuMsgIter *iter, GHashTable **orig_set)
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


static Xapian::Query
get_related_query (MuMsgIter *iter, GHashTable **orig_set)
{
	GHashTable *hash;
	GList *id_list, *cur;
	std::vector<Xapian::Query> qvec;
	static std::string pfx (1, mu_msg_field_xapian_prefix
				(MU_MSG_FIELD_ID_THREAD_ID));

	/* orig_set receives the hash msgid->docid of the set of
	 * original matches */
	hash = get_thread_ids (iter, orig_set);
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


static void
get_related_messages (MuQuery *self, MuMsgIter **iter, int maxnum,
                      MuMsgFieldId sortfieldid, MuQueryFlags flags,
                      Xapian::Query orig_query)
{
	GHashTable *orig_set;
	Xapian::Enquire enq (self->db());
	MuMsgIter *rel_iter;
	const bool inc_related = flags & MU_QUERY_FLAG_INCLUDE_RELATED;

	orig_set = NULL;
	Xapian::Query new_query = get_related_query (*iter, &orig_set);
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


MuMsgIter*
mu_query_run (MuQuery *self, const char *searchexpr, MuMsgFieldId sortfieldid,
	      int maxnum, MuQueryFlags flags, GError **err)
{
	g_return_val_if_fail (self, NULL);
	g_return_val_if_fail (searchexpr, NULL);
	g_return_val_if_fail (mu_msg_field_id_is_valid (sortfieldid)	||
			      sortfieldid    == MU_MSG_FIELD_ID_NONE,
			      NULL);
	try {
		MuMsgIter *iter;
		MuQueryFlags first_flags;
		const bool threads     = flags & MU_QUERY_FLAG_THREADS;
		const bool inc_related = flags & MU_QUERY_FLAG_INCLUDE_RELATED;
		const bool descending  = flags & MU_QUERY_FLAG_DESCENDING;
		const bool raw         = flags & MU_QUERY_FLAG_RAW;
		Xapian::Enquire enq (get_enquire(self, searchexpr, sortfieldid,
						 descending, raw, err));

		/* when we're doing a 'include-related query', wea're actually
		 * doing /two/ queries; one to get the initial matches, and
		 * based on that one to get all messages in threads in those
		 * matches.
		 */

		/* get the 'real' maxnum if it was specified as < 0 */
		maxnum = maxnum < 0 ? self->db().get_doccount() : maxnum;
		/* Calculating threads involves two queries, so do the calculation only in
		 * the second query instead of in both.
		 */
		if (threads)
			first_flags = (MuQueryFlags)(flags & ~MU_QUERY_FLAG_THREADS);
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
			get_related_messages (self, &iter, maxnum, sortfieldid, flags,
			                      enq.get_query());

		if (err && *err && (*err)->code == MU_ERROR_XAPIAN_MODIFIED) {
			g_clear_error (err);
			return try_requery (self, searchexpr, sortfieldid,
					    maxnum, flags, err);
		} else
			return iter;

	} MU_XAPIAN_CATCH_BLOCK_G_ERROR_RETURN (err, MU_ERROR_XAPIAN, 0);
}


size_t
mu_query_count_run (MuQuery *self, const char *searchexpr) try
{
	g_return_val_if_fail (self, 0);
	g_return_val_if_fail (searchexpr, 0);

        const auto enq{get_enquire(self, searchexpr,MU_MSG_FIELD_ID_NONE, false, false, NULL)};
        auto mset(enq.get_mset(0, self->db().get_doccount()));
        mset.fetch();

        return mset.size();

} MU_XAPIAN_CATCH_BLOCK_RETURN (0);




char*
mu_query_internal_xapian (MuQuery *self, const char *searchexpr, GError **err)
{
	g_return_val_if_fail (self, NULL);
	g_return_val_if_fail (searchexpr, NULL);

	try {
		Xapian::Query query (get_query(self, searchexpr, false, err));
		return g_strdup(query.get_description().c_str());

	} MU_XAPIAN_CATCH_BLOCK_RETURN(NULL);
}


char*
mu_query_internal (MuQuery *self, const char *searchexpr,
		   gboolean warn, GError **err)
{
	g_return_val_if_fail (self, NULL);
	g_return_val_if_fail (searchexpr, NULL);

	try {
		Mu::WarningVec warns;
		const auto tree = Mu::parse (searchexpr, warns,
					      std::make_unique<MuProc>(self->db()));
		std::stringstream ss;
		ss << tree;

		if (warn) {
			for (auto&& w: warns)
				std::cerr << w << std::endl;
		}

		return g_strdup(ss.str().c_str());

	} MU_XAPIAN_CATCH_BLOCK_RETURN(NULL);
}
