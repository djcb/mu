/* -*-mode: c++; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8-*- */
/*
** Copyright (C) 2008-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
std::string
_MuStore::get_uid_term (const char* path) const
{
	char real_path[PATH_MAX + 1];

	static const std::string uid_prefix (
		1, mu_msg_field_xapian_prefix(MU_MSG_FIELD_ID_UID));

	/* check profile to see if realpath is expensive; we need
	 * realpath here (and in mu-msg-file) to ensure that the same
	 * messages are only considered ones (ignore e.g. symlinks and
	 * '//' in paths) */

	// note: realpath fails when there's no file at path
	if (!realpath (path, real_path))
		strcpy (real_path, path);

	return std::string (uid_prefix + mu_util_get_hash (real_path));
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
mu_store_is_read_only (const MuStore *store)
{
	g_return_val_if_fail (store, FALSE);

	try {
		return store->is_read_only() ? TRUE : FALSE;

	} MU_XAPIAN_CATCH_BLOCK_RETURN(FALSE);

}


unsigned
mu_store_count (const MuStore *store, GError **err)
{
	g_return_val_if_fail (store, (unsigned)-1);

	try {
		return store->db_read_only()->get_doccount();

 	} MU_XAPIAN_CATCH_BLOCK_G_ERROR_RETURN(err, MU_ERROR_XAPIAN,
					       (unsigned)-1);
}


const char*
mu_store_version (const MuStore *store)
{
	g_return_val_if_fail (store, NULL);

	return store->version();
}


gboolean
mu_store_versions_match (const MuStore *store)
{
	g_return_val_if_fail (store, TRUE);

	return g_strcmp0 (mu_store_version (store),
			  MU_STORE_SCHEMA_VERSION) == 0;
}


char*
mu_store_get_metadata (const MuStore *store, const char *key, GError **err)
{
	g_return_val_if_fail (store, NULL);
	g_return_val_if_fail (store->db_read_only(), NULL);
	g_return_val_if_fail (key, NULL);

	try {
		std::string val;

		val = store->db_read_only()->get_metadata (key);
		if (!val.empty())
			return g_strdup (val.c_str());
		else
			return NULL;

	} MU_XAPIAN_CATCH_BLOCK_G_ERROR_RETURN(err, MU_ERROR_XAPIAN, NULL);
}


XapianDatabase*
mu_store_get_read_only_database (MuStore *store)
{
	g_return_val_if_fail (store, NULL);
	return (XapianWritableDatabase*)store->db_read_only();
}



gboolean
mu_store_contains_message (const MuStore *store, const char* path, GError **err)
{
	g_return_val_if_fail (store, FALSE);
	g_return_val_if_fail (path, FALSE);

	try {
		const std::string term (store->get_uid_term(path));
 		return store->db_read_only()->term_exists (term) ? TRUE: FALSE;

	} MU_XAPIAN_CATCH_BLOCK_G_ERROR_RETURN(err, MU_ERROR_XAPIAN, FALSE);

}


unsigned
mu_store_get_docid_for_path (const MuStore *store, const char* path, GError **err)
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
mu_store_get_timestamp (const MuStore *store, const char *msgpath, GError **err)
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
mu_store_get_msg (const MuStore *self, unsigned docid, GError **err)
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
