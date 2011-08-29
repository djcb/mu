/* -*-mode: c++; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8-*- */
/*
** Copyright (C) 2011  <djcb@djcbsoftware.nl>
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

#ifndef __MU_STORE_PRIV_HH__
#define __MU_STORE_PRIV_HH__

#if HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include <cstdio>
#include <xapian.h>
#include <cstring>
#include <stdexcept>

#include "mu-store.h"
#include "mu-contacts.h"

struct _MuStore {

/* by default, use transactions of 30000 messages */
#define MU_STORE_DEFAULT_BATCH_SIZE 30000
	/* http://article.gmane.org/gmane.comp.search.xapian.general/3656 */
#define MU_STORE_MAX_TERM_LENGTH 240

	_MuStore (const char *xpath, const char *contacts_cache, bool read_only):
		_in_transaction (false), _processed (0),
		_batch_size(MU_STORE_DEFAULT_BATCH_SIZE),
		_contacts(0), _version(0), _db(0), _read_only(read_only) {

		if (read_only)
			_db = new Xapian::Database (xpath);
		else
			_db = new Xapian::WritableDatabase (xpath,
							    Xapian::DB_CREATE_OR_OPEN);
		if (!check_version ())
			throw std::runtime_error
				("xapian db version check failed");

		if (contacts_cache) {
			_contacts = mu_contacts_new (contacts_cache);
			if (!_contacts) /* don't bail-out for this */
				throw std::runtime_error
					("failed to init contacts cache");
		}

		MU_WRITE_LOG ("%s: opened %s (batch size: %u)",
			      __FUNCTION__, xpath, batch_size());
	}

	~_MuStore () {
		try {
			g_free (_version);

			mu_contacts_destroy (_contacts);

			if (!_read_only)
				mu_store_flush (this);

			MU_WRITE_LOG ("closing xapian database with %d documents",
				      (int)db_read_only()->get_doccount());
			delete _db;

		} MU_XAPIAN_CATCH_BLOCK;
	}

	/* get a unique id for this message; note, this function returns a
	 * static buffer -- not reentrant */
	const char* get_message_uid (const char* path) {
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
	const char* get_message_uid (MuMsg *msg) {
		return get_message_uid (mu_msg_get_path(msg));
	}

	MuContacts* contacts() { return _contacts; }

	const char* version ()  {
		g_free (_version);
		return _version =
			mu_store_get_metadata (this, MU_STORE_VERSION_KEY);
	}

	void begin_transaction () {
		try {
			db_writable()->begin_transaction();
			set_in_transaction (true);
		} MU_XAPIAN_CATCH_BLOCK;
	}


	void commit_transaction () {
		try {
			set_in_transaction (false);
			db_writable()->commit_transaction();
		} MU_XAPIAN_CATCH_BLOCK;
	}

	void rollback_transaction () {
		try {
			set_in_transaction (false);
			db_writable()->cancel_transaction();
		} MU_XAPIAN_CATCH_BLOCK;
	}

	Xapian::WritableDatabase* db_writable() {
		if (G_UNLIKELY(_read_only))
			throw std::runtime_error
				("database is read-only");
		return (Xapian::WritableDatabase*)_db;
	}

	Xapian::Database* db_read_only() const {
		return _db;
	}

	size_t batch_size () const { return _batch_size;}
	size_t set_batch_size (size_t n)  {
		return _batch_size = ( n == 0) ? MU_STORE_DEFAULT_BATCH_SIZE : n;
	}

	bool   in_transaction () const { return _in_transaction; }
	bool   set_in_transaction (bool in_tx) { return _in_transaction = in_tx; }

	int    processed () const { return _processed; }
	int    set_processed (int n) { return _processed = n;}
	int    inc_processed () { return ++_processed; }

private:

	bool check_version () {
		const gchar *version;
		version = mu_store_version (this);

		/* no version yet? it must be a new db then; we'll set the version */
		if (!version)  {
			if (!mu_store_set_metadata (this, MU_STORE_VERSION_KEY,
						    MU_XAPIAN_DB_VERSION)) {
				g_warning ("failed to set database version");
				return FALSE;
			}
			return TRUE; /* ok, done. */
		}

		/* we have a version, but is it the right one? */
		if (std::strcmp (version, MU_XAPIAN_DB_VERSION) != 0) {
			g_warning ("expected db version %s, but got %s",
				   MU_XAPIAN_DB_VERSION,
				   version ? version : "<none>" );
			return FALSE;
		}

		return TRUE;
	}

	/* transaction handling */
	bool   _in_transaction;
	int    _processed;
	size_t  _batch_size;  /* batch size of a xapian transaction */

	/* contacts object to cache all the contact information */
	MuContacts *_contacts;
	mutable char *_version;

	Xapian::Database *_db;
	bool _read_only;
};


#endif /*__MU_STORE_PRIV_HH__*/
