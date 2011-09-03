/*
** Copyright (C) 2008-2011 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

		str = mu_date_interpret_s (s.c_str(), is_begin ? TRUE: FALSE);
		str = mu_date_complete_s (str, is_begin ? TRUE: FALSE);
		t   = mu_date_str_to_time_t (str, TRUE /*local*/);
		str = mu_date_time_t_to_str (t, FALSE /*UTC*/);

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

		mu_msg_field_foreach ((MuMsgFieldForEachFunc)add_prefix,
				      &_qparser);
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

	preprocessed = mu_query_preprocess (searchexpr);

	try {
		query = mqx->query_parser().parse_query
			(preprocessed,
			 Xapian::QueryParser::FLAG_BOOLEAN          |
			 Xapian::QueryParser::FLAG_PURE_NOT         |
			 Xapian::QueryParser::FLAG_WILDCARD	    |
			 Xapian::QueryParser::FLAG_AUTO_SYNONYMS    |
			 Xapian::QueryParser::FLAG_BOOLEAN_ANY_CASE);
		g_free (preprocessed);
		return query;

	} catch (...) {
		/* some error occured */
		g_set_error (err, 0, MU_ERROR_XAPIAN_QUERY,
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

		if (!mu_msg_field_needs_prefix(mfid))
			qparser->add_prefix ("", pfx);

	} MU_XAPIAN_CATCH_BLOCK;
}

MuQuery*
mu_query_new (MuStore *store, GError **err)
{
	g_return_val_if_fail (store, NULL);

	if (mu_store_count (store, err) == 0) {
		g_set_error (err, 0, MU_ERROR_XAPIAN_IS_EMPTY,
			     "database is empty");
		return 0;
	}

	try {
		return new MuQuery (store);

	} MU_XAPIAN_CATCH_BLOCK_G_ERROR_RETURN (err,
						MU_ERROR_XAPIAN, NULL);
	return 0;
}


void
mu_query_destroy (MuQuery *self)
{
	try { delete self; } MU_XAPIAN_CATCH_BLOCK;
}


/* preprocess a query to make them a bit more permissive */
char*
mu_query_preprocess (const char *query)
{
	gchar *my_query;

	g_return_val_if_fail (query, NULL);
	my_query = g_strdup (query);

	/* remove accents and turn to lower-case */
	mu_str_normalize_in_place (my_query, TRUE);
	/* escape '@', single '_' and ':' if it's not following a
	 * xapian-pfx with '_' */
	mu_str_ascii_xapian_escape_in_place (my_query);

	return my_query;
}


MuMsgIter*
mu_query_run (MuQuery *self, const char* searchexpr, gboolean threads,
	      MuMsgFieldId sortfieldid, gboolean ascending,
	      GError **err)
{
	g_return_val_if_fail (self, NULL);
	g_return_val_if_fail (searchexpr, NULL);
	g_return_val_if_fail (mu_msg_field_id_is_valid (sortfieldid) ||
			      sortfieldid == MU_MSG_FIELD_ID_NONE,
			      NULL);
	try {
		Xapian::Enquire enq (self->db());

		/* note, when our result will be *threaded*, we sort
		 * there, and don't let Xapian do any sorting */
		if (!threads && sortfieldid != MU_MSG_FIELD_ID_NONE)
			enq.set_sort_by_value ((Xapian::valueno)sortfieldid,
					       ascending ? true : false);

		if (!mu_str_is_empty(searchexpr)) /* NULL or "" */
			enq.set_query(get_query (self, searchexpr, err));
		else
			enq.set_query(Xapian::Query::MatchAll);

		enq.set_cutoff(0,0);

		return mu_msg_iter_new (
			(XapianEnquire*)&enq,
			self->db().get_doccount(), threads,
			threads ? sortfieldid : MU_MSG_FIELD_ID_NONE);

	} MU_XAPIAN_CATCH_BLOCK_RETURN(NULL);
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


