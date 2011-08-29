/* -*-mode: c++; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8-*- */
/*
** Copyright (C) 2008-2011 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#if HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include <cstdio>
#include <xapian.h>
#include <cstring>
#include <stdexcept>

#include "mu-store.h"
#include "mu-store-priv.hh" /* _MuStore */

#include "mu-msg.h"
#include "mu-msg-part.h"
#include "mu-store.h"
#include "mu-util.h"
#include "mu-str.h"
#include "mu-date.h"
#include "mu-flags.h"
#include "mu-contacts.h"


MuStore*
mu_store_new_read_only (const char* xpath, GError **err)
{
	g_return_val_if_fail (xpath, NULL);

	try {
		return  new _MuStore (xpath, NULL, true);

	} MU_XAPIAN_CATCH_BLOCK_G_ERROR(err,MU_ERROR_XAPIAN);

	return NULL;
}



unsigned
mu_store_count (MuStore *store)
{
	g_return_val_if_fail (store, 0);

	try {
		return store->db_read_only()->get_doccount();

	} MU_XAPIAN_CATCH_BLOCK;

	return 0;
}


const char*
mu_store_version (MuStore *store)
{
	g_return_val_if_fail (store, NULL);
	return store->version ();
}



char*
mu_store_get_metadata (MuStore *store, const char *key)
{
	g_return_val_if_fail (store, NULL);
	g_return_val_if_fail (key, NULL);

	try {
		const std::string val (store->db_read_only()->get_metadata (key));
		return val.empty() ? NULL : g_strdup (val.c_str());

	} MU_XAPIAN_CATCH_BLOCK;

	return NULL;
}


XapianDatabase*
mu_store_get_read_only_database (MuStore *store)
{
	g_return_val_if_fail (store, NULL);
	return (XapianWritableDatabase*)store->db_read_only();
}



gboolean
mu_store_contains_message (MuStore *store, const char* path)
{
	g_return_val_if_fail (store, FALSE);
	g_return_val_if_fail (path, FALSE);

	try {
		const std::string uid (store->get_message_uid(path));
		return store->db_read_only()->term_exists (uid) ? TRUE: FALSE;

	} MU_XAPIAN_CATCH_BLOCK_RETURN (FALSE);
}



time_t
mu_store_get_timestamp (MuStore *store, const char* msgpath)
{
	char *stampstr;
	time_t rv;

	g_return_val_if_fail (store, 0);
	g_return_val_if_fail (msgpath, 0);

	stampstr = mu_store_get_metadata (store, msgpath);
	if (!stampstr)
		return (time_t)0;

	rv = (time_t) g_ascii_strtoull (stampstr, NULL, 10);
	g_free (stampstr);

	return rv;
}



MuError
mu_store_foreach (MuStore *self,
		  MuStoreForeachFunc func, void *user_data)
{
	g_return_val_if_fail (self, MU_ERROR);
	g_return_val_if_fail (func, MU_ERROR);

	try {
		Xapian::Enquire enq (*self->db_read_only());

		enq.set_query  (Xapian::Query::MatchAll);
		enq.set_cutoff (0,0);

		Xapian::MSet matches
			(enq.get_mset (0, self->db_read_only()->get_doccount()));
		if (matches.empty())
			return MU_OK; /* database is empty */

		for (Xapian::MSet::iterator iter = matches.begin();
		     iter != matches.end(); ++iter) {
			Xapian::Document doc (iter.get_document());
			const std::string path(doc.get_value(MU_MSG_FIELD_ID_PATH));
			MuError res = func (path.c_str(), user_data);
			if (res != MU_OK)
				return res;
		}

	} MU_XAPIAN_CATCH_BLOCK_RETURN (MU_ERROR);

	return MU_OK;
}
