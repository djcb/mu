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

#include <stdlib.h>
#include <xapian.h>
#include <glib/gstdio.h>
#include <string.h>
#include <string>

#include "mu-query.h"

#include "mu-msg-iter.h"
#include "mu-msg-iter-priv.hh"

#include "mu-util.h"
#include "mu-util-db.h"
#include "mu-msg-str.h"

static void add_prefix (const MuMsgField* field,
			Xapian::QueryParser* qparser);

struct _MuQuery {
	Xapian::Database*         _db;
	Xapian::QueryParser*	  _qparser;
	Xapian::Sorter*           _sorters[MU_MSG_FIELD_TYPE_NUM];
};

gboolean
init_mu_query (MuQuery *mqx, const char* dbpath)
{
	mqx->_db      = 0;
	mqx->_qparser = 0;
	
	try {
		mqx->_db      = new Xapian::Database(dbpath);
		mqx->_qparser = new Xapian::QueryParser;
		
		mqx->_qparser->set_database(*mqx->_db);
		mqx->_qparser->set_default_op(Xapian::Query::OP_AND);
		mqx->_qparser->set_stemming_strategy
			(Xapian::QueryParser::STEM_ALL);

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
			 Xapian::QueryParser::FLAG_PHRASE           |
			 Xapian::QueryParser::FLAG_BOOLEAN_ANY_CASE);

	} MU_XAPIAN_CATCH_BLOCK;
	
	if (err)
		*err  = 1;
	
	return Xapian::Query();
}

static void
add_prefix (const MuMsgField* field, Xapian::QueryParser* qparser)
{
	if (!mu_msg_field_xapian_index(field) &&
	    !mu_msg_field_xapian_term(field) &&
	    !mu_msg_field_xapian_contact(field))
		return;

	const std::string prefix (mu_msg_field_xapian_prefix(field));
	
	qparser->add_boolean_prefix
		(std::string(mu_msg_field_name(field)), prefix);
	qparser->add_boolean_prefix
		(std::string(mu_msg_field_shortcut(field)), prefix);
	
	/* make the empty string match this field too*/
	qparser->add_prefix ("", prefix);
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
	if (self) {
		uninit_mu_query (self);
		g_free (self);
	}
}

struct _CheckPrefix {
	const char		*pfx;
	guint			 len;
	gboolean		 match;
};
typedef struct _CheckPrefix	 CheckPrefix;

static void
each_check_prefix (const MuMsgField *field, CheckPrefix *cpfx)
{
	const char *field_name, *field_shortcut;

	if (cpfx->match)
		return;
	
	field_shortcut = mu_msg_field_shortcut (field);
	if (field_shortcut &&
	    strncmp (cpfx->pfx, field_shortcut, cpfx->len) == 0) {
		cpfx->match = TRUE;
		return;
	}

	field_name = mu_msg_field_name (field);
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
	my_query = mu_msg_str_normalize(query, TRUE);
	
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
	      const MuMsgField* sortfield, gboolean ascending,
	      size_t batchsize)  
{
	g_return_val_if_fail (self, NULL);
	g_return_val_if_fail (searchexpr, NULL);	
	
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
		
		if (sortfield) 
			enq.set_sort_by_value (
				(Xapian::valueno)mu_msg_field_id(sortfield),
				ascending);

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


