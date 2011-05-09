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
#include "mu-msg-iter-priv.hh"

#include "mu-util.h"
#include "mu-str.h"

/*
 * custom parser for date ranges
 */
class MuDateRangeProcessor : public Xapian::ValueRangeProcessor {
public:
	MuDateRangeProcessor() {}

	Xapian::valueno operator()(std::string &begin, std::string &end) {

		if (!clear_prefix (begin))
			return Xapian::BAD_VALUENO;

		substitute_date (begin);
		substitute_date (end);

		normalize_date (begin);
		normalize_date (end);
		
		complete_date (begin, 12, true);
		complete_date (end, 12, false);
		
		return (Xapian::valueno)MU_MSG_PSEUDO_FIELD_ID_DATESTR;
	}
private:
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
	
	void substitute_date (std::string& date) {
		char datebuf[13];
		time_t now = time(NULL);
		
		if (date == "today") {
			strftime(datebuf, sizeof(datebuf), "%Y%m%d0000",
				 localtime(&now));
			date = datebuf;
		} else if (date == "now") {
			strftime(datebuf, sizeof(datebuf), "%Y%m%d%H%M",
				 localtime(&now));
			date = datebuf;
		} else {
			time_t t;
			t = mu_str_date_parse_hdwmy (date.c_str());
			if (t != (time_t)-1) {
				strftime(datebuf, sizeof(datebuf), "%Y%m%d%H%M",
					 localtime(&t));
				date = datebuf;
			}
		}		
	}
		
	void normalize_date (std::string& date) {
		std::string cleanup;
		for (unsigned i = 0; i != date.length(); ++i) {
			char k = date[i];
			if (std::isdigit(k))
				cleanup += date[i];
			else if (k != ':' && k != '-' && k != '/' && k != '.' &&
				 k != ',')
				throw std::runtime_error ("error in date str");
		}
		date = cleanup;
	}
	
	void complete_date (std::string& date, size_t len,
			    bool is_begin) const {

		const std::string bsuffix ("00000101000000");
		const std::string esuffix ("99991231235959");

		if (is_begin)
			date = std::string (date + bsuffix.substr (date.length()));
		else
			date = std::string (date + esuffix.substr (date.length()));

		date = date.substr (0, len);
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

		begin = Xapian::sortable_serialise(atol(begin.c_str()));
		end = Xapian::sortable_serialise(atol(end.c_str()));
		
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
		guint64 num = mu_str_size_parse_kmg  (size.c_str());
		if (num == G_MAXUINT64)
			return false;
		snprintf (str, sizeof(str), "%" G_GUINT64_FORMAT, num);
		size = str;
		return true;
	}
};



static void add_prefix (MuMsgFieldId field, Xapian::QueryParser* qparser);

struct _MuQuery {
	_MuQuery (const char* dbpath):
		_db (Xapian::Database(dbpath)) {
		
		_qparser.set_database (_db);
		_qparser.set_default_op (Xapian::Query::OP_AND);

		_qparser.add_valuerangeprocessor (&_date_range_processor);
		_qparser.add_valuerangeprocessor (&_size_range_processor);
		
		mu_msg_field_foreach ((MuMsgFieldForEachFunc)add_prefix,
				      &_qparser);
	}
		
	Xapian::Database	_db;
	Xapian::QueryParser	_qparser;
	MuDateRangeProcessor	_date_range_processor;
	MuSizeRangeProcessor	_size_range_processor;
};

static bool
set_query (MuQuery *mqx, Xapian::Query& q, const char* searchexpr,
	   GError **err)  {
	try {
		q = mqx->_qparser.parse_query
			(searchexpr,
			 Xapian::QueryParser::FLAG_BOOLEAN          |
			 Xapian::QueryParser::FLAG_PURE_NOT         |
			 Xapian::QueryParser::FLAG_WILDCARD	    |
			 Xapian::QueryParser::FLAG_AUTO_SYNONYMS    |
			 Xapian::QueryParser::FLAG_BOOLEAN_ANY_CASE);

		return true;
		
	} MU_XAPIAN_CATCH_BLOCK;
	
	/* some error occured */
	g_set_error (err, 0, MU_ERROR_QUERY, "parse error in query '%s'",
		     searchexpr);
	
	return false;
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

		if (mfid == MU_MSG_FIELD_ID_FLAGS || mfid == MU_MSG_FIELD_ID_PRIO) {
			qparser->add_prefix
				(mu_msg_field_name(mfid), pfx);
			qparser->add_prefix (shortcut, pfx);
		} else if (mfid == MU_MSG_FIELD_ID_MAILDIR ||
		      mfid == MU_MSG_FIELD_ID_MSGID) {
			qparser->add_boolean_prefix
				(mu_msg_field_name(mfid), pfx);
			qparser->add_boolean_prefix (shortcut, pfx);
		} else {
			qparser->add_boolean_prefix
				(mu_msg_field_name(mfid), pfx);
			qparser->add_boolean_prefix (shortcut, pfx);
			qparser->add_prefix ("", pfx);
		}
	} MU_XAPIAN_CATCH_BLOCK;
}

MuQuery*
mu_query_new (const char* xpath, GError **err)
{
	g_return_val_if_fail (xpath, NULL);
	
	if (!mu_util_check_dir (xpath, TRUE, FALSE)) {
		g_set_error (err, 0, MU_ERROR_XAPIAN_DIR,
			     "'%s' is not a readable xapian dir", xpath);
		return NULL;
	}
		
	if (mu_util_xapian_needs_upgrade (xpath)) {
		g_set_error (err, 0, MU_ERROR_XAPIAN_NOT_UPTODATE,
			     "%s is not up-to-date, needs a full update",
			     xpath);
		return NULL;
	}

	if (mu_util_xapian_is_empty (xpath)) 
		g_warning ("database %s is empty; nothing to do", xpath);

	try {

		return new MuQuery (xpath);

	} MU_XAPIAN_CATCH_BLOCK_G_ERROR_RETURN (err, MU_ERROR_XAPIAN, NULL); 

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
mu_query_run (MuQuery *self, const char* searchexpr,
	      MuMsgFieldId sortfieldid, gboolean ascending,
	      size_t batchsize, GError **err)  
{
	g_return_val_if_fail (self, NULL);
	g_return_val_if_fail (searchexpr, NULL);	
	g_return_val_if_fail (mu_msg_field_id_is_valid (sortfieldid) ||
			      sortfieldid == MU_MSG_FIELD_ID_NONE,
			      NULL);
	try {
		Xapian::Query query;
		char *preprocessed;	

		preprocessed = mu_query_preprocess (searchexpr);

		if (!set_query(self, query, preprocessed, err)) {
			g_free (preprocessed);
			return NULL;
		}
		g_free (preprocessed);
		
		Xapian::Enquire enq (self->_db);

		if (batchsize == 0)
			batchsize = self->_db.get_doccount();

		if (sortfieldid != MU_MSG_FIELD_ID_NONE)
			enq.set_sort_by_value ((Xapian::valueno)sortfieldid,
					       ascending ? true : false);
		enq.set_query(query);
		enq.set_cutoff(0,0);
		
		return mu_msg_iter_new (enq, batchsize);
		
	} MU_XAPIAN_CATCH_BLOCK_RETURN(NULL);
}

char*
mu_query_as_string (MuQuery *self, const char *searchexpr, GError **err) 
{
	g_return_val_if_fail (self, NULL);
	g_return_val_if_fail (searchexpr, NULL);
		
	try {
		Xapian::Query query;
		char *preprocessed;
		
		preprocessed = mu_query_preprocess (searchexpr);
		
		if (!set_query(self, query, preprocessed, err)) {
			g_free (preprocessed);
			return NULL;
		}
		g_free (preprocessed);

		return g_strdup(query.get_description().c_str());
		
	} MU_XAPIAN_CATCH_BLOCK_RETURN(NULL);
}


