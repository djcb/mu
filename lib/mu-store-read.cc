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
#include <limits.h>
#include <stdlib.h>
#include <errno.h>

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


// note: not re-entrant
const char*
_MuStore::get_uid_term (const char* path)
{
	// combination of DJB, BKDR hash functions to get a 64 bit
	// value
	unsigned djbhash, bkdrhash, bkdrseed;
	unsigned u;

	char real_path[PATH_MAX + 1];
	static char hex[18];
	static const char uid_prefix =
		mu_msg_field_xapian_prefix(MU_MSG_FIELD_ID_UID);

	/* check profile to see if realpath is expensive; we need
	 * realpath here (and in mu-msg-file) to ensure that the same
	 * messages are only considered ones (ignore e.g. symlinks and
	 * '//' in paths) */

	// note: realpath fails when there's no file at path
	if (!realpath (path, real_path))
		strcpy (real_path, path);

	djbhash  = 5381;
	bkdrhash = 0;
	bkdrseed = 1313;

	for(u = 0; real_path[u]; ++u) {
		djbhash  = ((djbhash << 5) + djbhash) + real_path[u];
		bkdrhash = bkdrhash * bkdrseed + real_path[u];
	}

	snprintf (hex, sizeof(hex), "%c%08x%08x",
		  uid_prefix, djbhash, bkdrhash);

	return hex;
}


MuStore*
mu_store_new_read_only (const char* xpath, GError **err)
{
	g_return_val_if_fail (xpath, NULL);

	try {
		return new _MuStore (xpath);

	} catch (const MuStoreError& merr) {
		mu_util_g_set_error (err, merr.mu_error(), "%s",
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
			    MU_STORE_SCHEMA_VERSION) == 0) ? FALSE : TRUE;
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
		const std::string term (store->get_uid_term(path));
 		return store->db_read_only()->term_exists (term) ? TRUE: FALSE;

	} MU_XAPIAN_CATCH_BLOCK_G_ERROR_RETURN(err, MU_ERROR_XAPIAN, FALSE);

}


unsigned
mu_store_get_docid_for_path (MuStore *store, const char* path, GError **err)
{
	g_return_val_if_fail (store, FALSE);
	g_return_val_if_fail (path, FALSE);

	try {
		const std::string term (store->get_uid_term(path));
		Xapian::Query query (term);
		Xapian::Enquire enq (*store->db_read_only());

		enq.set_query (query);

		Xapian::MSet mset (enq.get_mset (0,1));
		if (mset.empty())
			throw MuStoreError (MU_ERROR_NO_MATCHES,
					    "message not found");

		return *mset.begin();

	} MU_XAPIAN_CATCH_BLOCK_G_ERROR_RETURN(err, MU_ERROR_XAPIAN,
					       MU_STORE_INVALID_DOCID);
}



time_t
mu_store_get_timestamp (MuStore *store, const char *msgpath, GError **err)
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
			new Xapian::Document
			(self->db_read_only()->get_document (docid));
		return mu_msg_new_from_doc ((XapianDocument*)doc, err);

	} MU_XAPIAN_CATCH_BLOCK_G_ERROR_RETURN (err, MU_ERROR_XAPIAN, 0);
}
