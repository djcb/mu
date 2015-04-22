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
#include <unistd.h>

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



MuStore*
mu_store_ref (MuStore *store)
{
	g_return_val_if_fail (store, NULL);
	store->ref();
	return store;
}

MuStore*
mu_store_unref (MuStore *store)
{
	g_return_val_if_fail (store, NULL);

	if (store->unref() == 0) {
		try { delete store; } MU_XAPIAN_CATCH_BLOCK;
	}

	return NULL;
}




static char*
xapian_get_metadata (const gchar *xpath, const gchar *key)
{
	g_return_val_if_fail (xpath, NULL);
	g_return_val_if_fail (key, NULL);

	if (!access(xpath, F_OK) == 0) {
		g_warning ("cannot access %s: %s", xpath, strerror(errno));
		return NULL;
	}

	try {
		Xapian::Database db (xpath);
		const std::string val(db.get_metadata (key));
		return val.empty() ? NULL : g_strdup (val.c_str());

	} MU_XAPIAN_CATCH_BLOCK;

	return NULL;
}

char*
mu_store_database_version (const gchar *xpath)
{
	g_return_val_if_fail (xpath, NULL);

	return xapian_get_metadata (xpath, MU_STORE_VERSION_KEY);
}


gboolean
mu_store_database_is_locked (const gchar *xpath)
{
	g_return_val_if_fail (xpath, FALSE);

	try {
		Xapian::WritableDatabase db (xpath, Xapian::DB_OPEN);
	} catch (const Xapian::DatabaseLockError& xer) {
		return TRUE;
	} catch (const Xapian::Error &xer) {
		g_warning ("%s: error: %s", __func__,
			   xer.get_msg().c_str());
	}

	return FALSE;
}


void
mu_store_set_my_addresses (MuStore *store, const char **my_addresses)
{
	g_return_if_fail (store);

	store->set_my_addresses (my_addresses);
}
