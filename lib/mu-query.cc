/*
** Copyright (C) 2008-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <stdlib.h>

#include <xapian.h>
#include <glib/gstdio.h>

#include "mu-query.h"
#include "mu-msg-fields.h"

#include "mu-msg-iter.h"

#include "mu-util.h"
#include "mu-str.h"
#include "mu-date.h"

/*
 * custom parser for date ranges
 */
class MuDateRangeProcessor : public Xapian::StringValueRangeProcessor {
public:
	MuDateRangeProcessor():
		Xapian::StringValueRangeProcessor(
			(Xapian::valueno)MU_MSG_FIELD_ID_DATE) {}

	Xapian::valueno operator()(std::string &begin, std::string &end) {

		if (!clear_prefix (begin))
			return Xapian::BAD_VALUENO;

		 begin = to_sortable (begin, true);
		 end   = to_sortable (end, false);

		if (begin > end)
			throw Xapian::QueryParserError
				("end time is before begin");

		return (Xapian::valueno)MU_MSG_FIELD_ID_DATE;
	}
private:
	std::string to_sortable (std::string& s, bool is_begin) {

		const char* str;
		time_t t;

		// note: if s is empty and not is_begin, xapian seems
		// to repeat it.
		if (s.empty() || g_str_has_suffix (s.c_str(), "..")) {
			str = mu_date_complete_s ("", is_begin);
		} else {
			str = mu_date_interpret_s (s.c_str(),
						   is_begin ? TRUE: FALSE);
			str = mu_date_complete_s (str, is_begin ? TRUE: FALSE);
			t   = mu_date_str_to_time_t (str, TRUE /*local*/);
			str = mu_date_time_t_to_str_s (t, FALSE /*UTC*/);
		}

		return s = std::string(str);
	}


	bool clear_prefix (std::string& begin) {

		const std::string colon (":");
		const std::string name (mu_msg_field_name
					(MU_MSG_FIELD_ID_DATE) + colon);
		const std::string shortcut (
			std::string(1, mu_msg_field_shortcut
				    (MU_MSG_FIELD_ID_DATE)) + colon);

		if (begin.find (name) == 0) {
			begin.erase (0, name.length());
			return true;
		} else if (begin.find (shortcut) == 0) {
			begin.erase (0, shortcut.length());
			return true;
		} else
			return false;
	}
};


class MuSizeRangeProcessor : public Xapian::NumberValueRangeProcessor {
public:
	MuSizeRangeProcessor():
		Xapian::NumberValueRangeProcessor(MU_MSG_FIELD_ID_SIZE) {
	}

	Xapian::valueno operator()(std::string &begin, std::string &end) {

		if (!clear_prefix (begin))
			return Xapian::BAD_VALUENO;

		if (!substitute_size (begin) || !substitute_size (end))
			return Xapian::BAD_VALUENO;

		/* swap if b > e */
		if (begin > end)
			std::swap (begin, end);

		begin = Xapian::sortable_serialise (atol(begin.c_str()));
		end = Xapian::sortable_serialise (atol(end.c_str()));

		return (Xapian::valueno)MU_MSG_FIELD_ID_SIZE;
	}
private:
	bool clear_prefix (std::string& begin) {

		const std::string colon (":");
		const std::string name (mu_msg_field_name
					(MU_MSG_FIELD_ID_SIZE) + colon);
		const std::string shortcut (
			std::string(1, mu_msg_field_shortcut
				    (MU_MSG_FIELD_ID_SIZE)) + colon);

		if (begin.find (name) == 0) {
			begin.erase (0, name.length());
			return true;
		} else if (begin.find (shortcut) == 0) {
			begin.erase (0, shortcut.length());
			return true;
		} else
			return false;
	}

	bool substitute_size (std::string& size) {
		gchar str[16];
		gint64 num = mu_str_size_parse_bkm(size.c_str());
		if (num < 0)
			throw Xapian::QueryParserError ("invalid size");
		snprintf (str, sizeof(str), "%" G_GUINT64_FORMAT, num);
		size = str;
		return true;
	}
};



static void add_prefix (MuMsgFieldId field, Xapian::QueryParser* qparser);

struct _MuQuery {
public:
	_MuQuery (MuStore *store): _store(mu_store_ref(store)) {

		_qparser.set_database (db());
		_qparser.set_default_op (Xapian::Query::OP_AND);

		_qparser.add_valuerangeprocessor (&_date_range_processor);
		_qparser.add_valuerangeprocessor (&_size_range_processor);

		mu_msg_field_foreach ((MuMsgFieldForeachFunc)add_prefix,
				      &_qparser);

		/* add some convenient special prefixes */
		add_special_prefixes ();
	}

	~_MuQuery () { mu_store_unref (_store); }

	Xapian::Database& db() const {
		Xapian::Database* db;
		db = reinterpret_cast<Xapian::Database*>
			(mu_store_get_read_only_database (_store));
		if (!db)
			throw std::runtime_error ("no database");
		return *db;
	}
	Xapian::QueryParser& query_parser () { return _qparser; }

private:
	void add_special_prefixes () {
		char pfx[] = { '\0', '\0' };

		/* add 'contact' as a shortcut for From/Cc/Bcc/To: */
		pfx[0] = mu_msg_field_xapian_prefix(MU_MSG_FIELD_ID_FROM);
		_qparser.add_prefix (MU_MSG_FIELD_PSEUDO_CONTACT, pfx);
		pfx[0] = mu_msg_field_xapian_prefix(MU_MSG_FIELD_ID_TO);
		_qparser.add_prefix (MU_MSG_FIELD_PSEUDO_CONTACT, pfx);
		pfx[0] = mu_msg_field_xapian_prefix(MU_MSG_FIELD_ID_CC);
		_qparser.add_prefix (MU_MSG_FIELD_PSEUDO_CONTACT, pfx);
		pfx[0] = mu_msg_field_xapian_prefix(MU_MSG_FIELD_ID_BCC);
		_qparser.add_prefix (MU_MSG_FIELD_PSEUDO_CONTACT, pfx);

		/* add 'recip' as a shortcut for Cc/Bcc/To: */
		pfx[0] = mu_msg_field_xapian_prefix(MU_MSG_FIELD_ID_TO);
		_qparser.add_prefix (MU_MSG_FIELD_PSEUDO_RECIP, pfx);
		pfx[0] = mu_msg_field_xapian_prefix(MU_MSG_FIELD_ID_CC);
		_qparser.add_prefix (MU_MSG_FIELD_PSEUDO_RECIP, pfx);
		pfx[0] = mu_msg_field_xapian_prefix(MU_MSG_FIELD_ID_BCC);
		_qparser.add_prefix (MU_MSG_FIELD_PSEUDO_RECIP, pfx);
	}

	Xapian::QueryParser	_qparser;
	MuDateRangeProcessor	_date_range_processor;
	MuSizeRangeProcessor	_size_range_processor;

	MuStore *_store;
};

static const Xapian::Query
get_query (MuQuery *mqx, const char* searchexpr, GError **err)
{
	Xapian::Query query;
	char *preprocessed;

	preprocessed = mu_query_preprocess (searchexpr, err);
	if (!preprocessed)
		throw std::runtime_error
			("parse error while preprocessing query");

	try {
		query = mqx->query_parser().parse_query
			(preprocessed,
			 Xapian::QueryParser::FLAG_BOOLEAN          |
			 Xapian::QueryParser::FLAG_PURE_NOT         |
			 Xapian::QueryParser::FLAG_AUTO_SYNONYMS    |
			 Xapian::QueryParser::FLAG_WILDCARD	    |
			 Xapian::QueryParser::FLAG_BOOLEAN_ANY_CASE
			 );
		g_free (preprocessed);
		return query;

	} catch (...) {
		mu_util_g_set_error (err,MU_ERROR_XAPIAN_QUERY,
				     "parse error in query");
		g_free (preprocessed);
		throw;
	}
}



static void
add_prefix (MuMsgFieldId mfid, Xapian::QueryParser* qparser)
{
	if (!mu_msg_field_xapian_index(mfid) &&
	    !mu_msg_field_xapian_term(mfid) &&
	    !mu_msg_field_xapian_contact(mfid))
		return;
	try {
		const std::string  pfx
			(1, mu_msg_field_xapian_prefix (mfid));
		const std::string shortcut
			(1, mu_msg_field_shortcut (mfid));

		if (mu_msg_field_uses_boolean_prefix (mfid)) {
		 	qparser->add_boolean_prefix
		 		(mu_msg_field_name(mfid), pfx);
		 	qparser->add_boolean_prefix (shortcut, pfx);
		} else {
			qparser->add_prefix
				(mu_msg_field_name(mfid), pfx);
		 	qparser->add_prefix (shortcut, pfx);
		}

		// all fields are also matched implicitly, without
		// any prefix
		qparser->add_prefix ("", pfx);

	} MU_XAPIAN_CATCH_BLOCK;
}

MuQuery*
mu_query_new (MuStore *store, GError **err)
{
	g_return_val_if_fail (store, NULL);

	if (mu_store_count (store, err) == 0) {
		g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_XAPIAN_IS_EMPTY,
			     "database is empty");
		return 0;
	}

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


/* preprocess a query to make them a bit more promiscuous */
char*
mu_query_preprocess (const char *query, GError **err)
{
	GSList *parts, *cur;
	gchar *myquery;

	g_return_val_if_fail (query, NULL);

	/* convert the query to a list of query terms, and escape them
	 * separately */
	parts = mu_str_esc_to_list (query);
	if (!parts)
		return NULL;

	for (cur = parts; cur; cur = g_slist_next(cur)) {
		char *data;
		data = (gchar*)cur->data;
		cur->data = mu_str_process_query_term (data);
		g_free (data);
		/* run term fixups */
		data = (gchar*)cur->data;
		cur->data = mu_str_xapian_fixup_terms (data);
		g_free (data);
	}

	myquery = mu_str_from_list (parts, ' ');
	mu_str_free_list (parts);

	return myquery ? myquery : g_strdup ("");
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
		MU_WRITE_LOG ("reopening db after modification");
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
	     bool descending, GError **err)
{
	Xapian::Enquire enq (self->db());

	try {
		/* empty or "" means "matchall" */
		if (!mu_str_is_empty(searchexpr) &&
		    g_strcmp0 (searchexpr, "\"\"") != 0) /* NULL or "" or """" */
			enq.set_query(get_query (self, searchexpr, err));
		else
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
include_related (MuQuery *self, MuMsgIter **iter, int maxnum,
		 MuMsgFieldId sortfieldid, MuQueryFlags flags)
{
	GHashTable *orig_set;
	Xapian::Enquire enq (self->db());
	MuMsgIter *rel_iter;

	orig_set = NULL;
	enq.set_query(get_related_query (*iter, &orig_set));
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
		MuMsgIter	*iter;
		MuQueryFlags	 first_flags;
		bool		 inc_related  = flags & MU_QUERY_FLAG_INCLUDE_RELATED;
		bool		 descending   = flags & MU_QUERY_FLAG_DESCENDING;
		Xapian::Enquire enq (get_enquire(self, searchexpr, sortfieldid,
						 descending, err));

		/* when we're doing a 'include-related query', we're
		 * actually doing /two/ queries; one to get the
		 * initial matches, and based on that one to get all
		 * messages in threads in those matches.
		 */

		/* get the 'real' maxnum if it was specified as < 0 */
		maxnum = maxnum < 0 ? self->db().get_doccount() : maxnum;
		/* if we do a include-related query, it's wasted
		 * effort to calculate threads already in the first
		 * query since we can do it in the second one
		 */
		first_flags = inc_related ? (flags & ~MU_QUERY_FLAG_THREADS) : flags;
		iter   = mu_msg_iter_new (
			reinterpret_cast<XapianEnquire*>(&enq),
			maxnum,
			/* with inc_related, we do the sorting in the
			 * second query
			 */
			inc_related ? MU_MSG_FIELD_ID_NONE : sortfieldid,
			msg_iter_flags (first_flags),
			err);
		/*
		 * if we want related messages, do a second query,
		 * based on the message ids / refs of the first one
		 * */
		if (inc_related)
			include_related (self, &iter, maxnum, sortfieldid,
					 flags);

		if (err && *err && (*err)->code == MU_ERROR_XAPIAN_MODIFIED) {
			g_clear_error (err);
			return try_requery (self, searchexpr, sortfieldid,
					    maxnum, flags, err);
		} else
			return iter;

	} MU_XAPIAN_CATCH_BLOCK_G_ERROR_RETURN (err, MU_ERROR_XAPIAN, 0);
}


char*
mu_query_as_string (MuQuery *self, const char *searchexpr, GError **err)
{
	g_return_val_if_fail (self, NULL);
	g_return_val_if_fail (searchexpr, NULL);

	try {
		Xapian::Query query (get_query(self, searchexpr, err));
		return g_strdup(query.get_description().c_str());

	} MU_XAPIAN_CATCH_BLOCK_RETURN(NULL);
}
