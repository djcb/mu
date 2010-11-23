/* 
** Copyright (C) 2008-2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include "mu-util-db.h"
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

		substitute_date (begin, true);
		substitute_date (end, false);

		normalize_date (begin);
		normalize_date (end);
		
		complete_date (begin, 12, true);
		complete_date (end, 12, false);
		
		return (Xapian::valueno)MU_MSG_PSEUDO_FIELD_ID_DATESTR;
	}
private:
	bool clear_prefix (std::string& begin) {
		
		const std::string colon (":");
		const std::string name (mu_msg_field_name (MU_MSG_FIELD_ID_DATE) + colon);
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
	
	void substitute_date (std::string& date, bool is_begin) {
		char datebuf[13];
		time_t now = time(NULL);
		if (is_begin) {
			if (date == "today") {
				strftime(datebuf, sizeof(datebuf), "%Y%m%d0000",
					 localtime(&now));
				date = datebuf;
			} else if (date == "epoch")
				date = "197001010000";
		} else { /* end */
			if (date == "now") {
				strftime(datebuf, sizeof(datebuf), "%Y%m%d%H%M",
					 localtime(&now));
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
	
	void complete_date (std::string& date, size_t len, bool is_begin) const {

		const std::string bsuffix ("00000101000000");
		const std::string esuffix ("99991231235959");

		if (is_begin)
			date = std::string (date + bsuffix.substr (date.length()), len);
		else
			date = std::string (date + esuffix.substr (date.length()), len);

	}
};


static void add_prefix (MuMsgFieldId field, Xapian::QueryParser* qparser);

struct _MuQuery {
	Xapian::Database*		_db;
	Xapian::QueryParser*		_qparser;
	Xapian::Sorter*			_sorters[MU_MSG_FIELD_TYPE_NUM];
	Xapian::ValueRangeProcessor*	_range_processor;
};

gboolean
init_mu_query (MuQuery *mqx, const char* dbpath)
{
	mqx->_db      = 0;
	mqx->_qparser = 0;
	
	try {
		mqx->_db      = new Xapian::Database(dbpath);
		mqx->_qparser = new Xapian::QueryParser;
		
		mqx->_qparser->set_database (*mqx->_db);
		mqx->_qparser->set_default_op (Xapian::Query::OP_AND);

		mqx->_range_processor = new MuDateRangeProcessor ();
		mqx->_qparser->add_valuerangeprocessor
			(mqx->_range_processor);
		
		memset (mqx->_sorters, 0, sizeof(mqx->_sorters));
		mu_msg_field_foreach ((MuMsgFieldForEachFunc)add_prefix,
				      (gpointer)mqx->_qparser);
		
		return TRUE;

	} MU_XAPIAN_CATCH_BLOCK;

	try {
		delete mqx->_db;
		delete mqx->_qparser;

	} MU_XAPIAN_CATCH_BLOCK;
	
	return FALSE;
}

	
static void
uninit_mu_query (MuQuery *mqx)
{
	try {
		delete mqx->_db;
		delete mqx->_qparser;
		delete mqx->_range_processor;
		
		for (int i = 0; i != MU_MSG_FIELD_TYPE_NUM; ++i) 
			delete mqx->_sorters[i];
		
	} MU_XAPIAN_CATCH_BLOCK;
}


static Xapian::Query
get_query  (MuQuery * mqx, const char* searchexpr, int *err = 0)  {
	
	try {
		return mqx->_qparser->parse_query
			(searchexpr,
			 Xapian::QueryParser::FLAG_BOOLEAN          |
			 Xapian::QueryParser::FLAG_PURE_NOT         |
			 Xapian::QueryParser::FLAG_AUTO_SYNONYMS    |
			 Xapian::QueryParser::FLAG_BOOLEAN_ANY_CASE);
		
	} MU_XAPIAN_CATCH_BLOCK;
	
	if (err)
		*err  = 1;
	
	return Xapian::Query();
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
mu_query_new (const char* xpath)
{
	MuQuery *mqx;
	
	g_return_val_if_fail (xpath, NULL);

	if (!mu_util_check_dir (xpath, TRUE, FALSE)) {
		g_warning ("'%s' is not a readable xapian dir", xpath);
		return NULL;
	}

	if (mu_util_db_is_empty (xpath)) {
		g_warning ("database %s is empty; nothing to do", xpath);
		return NULL;
	}
	
	if (!mu_util_db_version_up_to_date (xpath)) {
		g_warning ("%s is not up-to-date, needs a full update",
			   xpath);
		return NULL;
	}
	
	mqx = g_new (MuQuery, 1);

	if (!init_mu_query (mqx, xpath)) {
		g_critical ("failed to initialize the Xapian query object");
		g_free (mqx);
		return NULL;
	}
	
	return mqx;
}


void
mu_query_destroy (MuQuery *self)
{
	if (!self)
		return;
	
	uninit_mu_query (self);
	g_free (self);
}

struct _CheckPrefix {
	const char		*pfx;
	guint			 len;
	gboolean		 match;
};
typedef struct _CheckPrefix	 CheckPrefix;

static void
each_check_prefix (MuMsgFieldId mfid, CheckPrefix *cpfx)
{
	const char *field_name;
	char field_shortcut;

	if (!cpfx || cpfx->match)
		return;
	
	field_shortcut = mu_msg_field_shortcut (mfid);
	if (field_shortcut == cpfx->pfx[0]) {
		cpfx->match = TRUE;
		return;
	}

	field_name = mu_msg_field_name (mfid);
	if (field_name &&
	    strncmp (cpfx->pfx, field_name, cpfx->len) == 0) {
		cpfx->match = TRUE;
		return;
	}
}


/* colon is a position inside q pointing at a ':' character. function
 * determines whether the prefix is a registered prefix (like
 * 'subject' or 'from' or 's') */
static gboolean
is_xapian_prefix (const char *q, const char *colon)
{
	const char *cur;
	
	if (colon == q)
		return FALSE; /* : at beginning, not a prefix */
	
	/* track back from colon until a boundary or beginning of the
	 * str */
	for (cur = colon - 1; cur >= q; --cur) {

		if (cur == q || !isalpha (*(cur-1))) {

			CheckPrefix cpfx;
			memset (&cpfx, 0, sizeof(CheckPrefix));

			cpfx.pfx   = cur;
			cpfx.len   = (colon - cur);
			cpfx.match = FALSE;
			
			mu_msg_field_foreach ((MuMsgFieldForEachFunc)
					      each_check_prefix,
					      &cpfx);
			
			return (cpfx.match);
		}
	}
	
	return FALSE;
}
	
/* preprocess a query to make them a bit more permissive */
char*
mu_query_preprocess (const char *query)
{
	gchar *my_query;
	gchar *cur;

	g_return_val_if_fail (query, NULL);
	
	/* translate the the searchexpr to all lowercase; this
	 * will fixes some of the false-negatives. A full fix
	 * probably requires some custom query parser.
	 */
	my_query = mu_str_normalize(query, TRUE);
	
	for (cur = my_query; *cur; ++cur) {
		if (*cur == ':') /* we found a ':' */
			 /* if there's a registered xapian prefix before the
			  * ':', don't touch it. Otherwise replace ':' with
			  * a space'... ugly...
			  */			 
			if (!is_xapian_prefix (my_query, cur))
				*cur = ' ';
	}
	
	return my_query;
}


MuMsgIter*
mu_query_run (MuQuery *self, const char* searchexpr,
	      MuMsgFieldId sortfieldid, gboolean ascending,
	      size_t batchsize)  
{
	g_return_val_if_fail (self, NULL);
	g_return_val_if_fail (searchexpr, NULL);	
	g_return_val_if_fail (mu_msg_field_id_is_valid (sortfieldid) ||
			      sortfieldid == MU_MSG_FIELD_ID_NONE, NULL);
	
	try {
		char *preprocessed;	
		int err (0);
		
		preprocessed = mu_query_preprocess (searchexpr);

		Xapian::Query q(get_query(self, preprocessed, &err));
		if (err) {
			g_warning ("Error in query '%s'", preprocessed);
			g_free (preprocessed);
			return NULL;
		}
		g_free (preprocessed);
		
		Xapian::Enquire enq (*self->_db);

		if (batchsize == 0)
			batchsize = self->_db->get_doccount();
		
		if (sortfieldid != MU_MSG_FIELD_ID_NONE) 
			enq.set_sort_by_value (
				(Xapian::valueno)sortfieldid,
				ascending ? true : false);

		enq.set_query(q);
		enq.set_cutoff(0,0);

		return mu_msg_iter_new (enq, batchsize);
		
	} MU_XAPIAN_CATCH_BLOCK_RETURN(NULL);
}

char*
mu_query_as_string  (MuQuery *self, const char *searchexpr) 
{
	g_return_val_if_fail (self, NULL);
	g_return_val_if_fail (searchexpr, NULL);
		
	try {
		char *preprocessed;
		int err (0);
		
		preprocessed = mu_query_preprocess (searchexpr);
		
		Xapian::Query q(get_query(self, preprocessed, &err));
		if (err)
			g_warning ("Error in query '%s'", preprocessed);

		g_free (preprocessed);

		return err ? NULL : g_strdup(q.get_description().c_str());
		
	} MU_XAPIAN_CATCH_BLOCK_RETURN(NULL);
}


