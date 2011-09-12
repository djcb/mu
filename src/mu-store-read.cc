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

/* get a unique id for this message; note, this function returns a
 * static buffer -- not reentrant */
const char*
_MuStore::get_message_uid (const char* path) {
	char pfx = 0;
	static char buf[PATH_MAX + 10];
	if (G_UNLIKELY(!pfx)) {
		pfx = mu_msg_field_xapian_prefix(MU_MSG_FIELD_ID_PATH);
			buf[0]=pfx;
	}
	std::strcpy (buf + 1, path);
	return buf;
}

/* get a unique id for this message; note, this function returns a
 * static buffer -- not reentrant */
const char*
_MuStore::get_message_uid (MuMsg *msg) {
	return get_message_uid (mu_msg_get_path(msg));
}



MuStore*
mu_store_new_read_only (const char* xpath, GError **err)
{
	g_return_val_if_fail (xpath, NULL);

	try {
		return new _MuStore (xpath);

	} catch (const MuStoreError& merr) {
		g_set_error (err, 0, merr.mu_error(), "%s",
			     merr.what().c_str());

	} MU_XAPIAN_CATCH_BLOCK_G_ERROR(err, MU_ERROR_XAPIAN);

	return NULL;
}


gboolean
mu_store_is_read_only (MuStore *store)
{
	g_return_val_if_fail (store, FALSE);

	try {
		return store->is_read_only() ? TRUE : FALSE;

	} MU_XAPIAN_CATCH_BLOCK_RETURN(FALSE);

}


unsigned
mu_store_count (MuStore *store, GError **err)
{
	g_return_val_if_fail (store, (unsigned)-1);

	try {
		return store->db_read_only()->get_doccount();

 	} MU_XAPIAN_CATCH_BLOCK_G_ERROR_RETURN(err, MU_ERROR_XAPIAN,
					       (unsigned)-1);
}


const char*
mu_store_version (MuStore *store)
{
	g_return_val_if_fail (store, NULL);
	return store->version ();
}


gboolean
mu_store_needs_upgrade (MuStore *store)
{
	g_return_val_if_fail (store, TRUE);

	return  (g_strcmp0 (mu_store_version (store),
			    MU_STORE_SCHEMA_VERSION)  == 0) ? FALSE : TRUE;

}


char*
mu_store_get_metadata (MuStore *store, const char *key, GError **err)
{
	g_return_val_if_fail (store, NULL);
	g_return_val_if_fail (key, NULL);

	try {
		const std::string val (store->db_read_only()->get_metadata (key));
		return val.empty() ? NULL : g_strdup (val.c_str());

	} MU_XAPIAN_CATCH_BLOCK_G_ERROR_RETURN(err, MU_ERROR_XAPIAN, NULL);
}


XapianDatabase*
mu_store_get_read_only_database (MuStore *store)
{
	g_return_val_if_fail (store, NULL);
	return (XapianWritableDatabase*)store->db_read_only();
}



gboolean
mu_store_contains_message (MuStore *store, const char* path, GError **err)
{
	g_return_val_if_fail (store, FALSE);
	g_return_val_if_fail (path, FALSE);

	try {
		const std::string uid (store->get_message_uid(path));
		return store->db_read_only()->term_exists (uid) ? TRUE: FALSE;

	} MU_XAPIAN_CATCH_BLOCK_G_ERROR_RETURN(err, MU_ERROR_XAPIAN, FALSE);

}



time_t
mu_store_get_timestamp (MuStore *store, const char* msgpath, GError **err)
{
	char *stampstr;
	time_t rv;

	g_return_val_if_fail (store, 0);
	g_return_val_if_fail (msgpath, 0);

	stampstr = mu_store_get_metadata (store, msgpath, err);
	if (!stampstr)
		return (time_t)0;

	rv = (time_t) g_ascii_strtoull (stampstr, NULL, 10);
	g_free (stampstr);

	return rv;
}



MuError
mu_store_foreach (MuStore *self,
		  MuStoreForeachFunc func, void *user_data, GError **err)
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

	} MU_XAPIAN_CATCH_BLOCK_G_ERROR_RETURN(err, MU_ERROR_XAPIAN,
					       MU_ERROR_XAPIAN);

	return MU_OK;
}



MuMsg*
mu_store_get_msg (MuStore *self, unsigned docid, GError **err)
{
	g_return_val_if_fail (self, NULL);
	g_return_val_if_fail (docid != 0, NULL);

	try {
		Xapian::Document *doc =
			new Xapian::Document (self->db_read_only()->get_document (docid));
		return mu_msg_new_from_doc ((XapianDocument*)doc, err);

	} MU_XAPIAN_CATCH_BLOCK_G_ERROR_RETURN (err, MU_ERROR_XAPIAN, 0);
}


