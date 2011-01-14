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

#if HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include <cstring>
#include <errno.h>
#include <xapian.h>

#include "mu-util.h"


char*
mu_util_xapian_get_metadata (const gchar *xpath, const gchar *key)
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


gboolean
mu_util_xapian_set_metadata (const gchar *xpath,
			     const gchar *key, const gchar *val)
{
	g_return_val_if_fail (xpath, NULL);
	g_return_val_if_fail (key, NULL);
	g_return_val_if_fail (val, NULL);
	
	if (!access(xpath, F_OK) == 0) {
		g_warning ("cannot access %s: %s", xpath, strerror(errno));
		return FALSE;
	}
	
	try {			
		Xapian::WritableDatabase db (xpath, Xapian::DB_OPEN);
		db.set_metadata (key, val);
		return TRUE;
		
	} MU_XAPIAN_CATCH_BLOCK;
	
	return FALSE;
}


char*
mu_util_xapian_dbversion (const gchar *xpath)
{
	g_return_val_if_fail (xpath, NULL);

	return mu_util_xapian_get_metadata (xpath, MU_STORE_VERSION_KEY);
}

gboolean
mu_util_xapian_needs_upgrade (const gchar *xpath)
{
	char *version;
	gboolean rv;
	
	g_return_val_if_fail (xpath, TRUE);

	version = mu_util_xapian_dbversion (xpath);
	
	if (g_strcmp0 (version, MU_XAPIAN_DB_VERSION) == 0)
		rv = FALSE;
	else
		rv = TRUE;

	g_free (version);

	return rv;
}


gboolean
mu_util_xapian_is_empty (const gchar* xpath)
{
	g_return_val_if_fail (xpath, TRUE);
	
	/* it's 'empty' (non-existant) */
	if (access(xpath, F_OK) != 0 && errno == ENOENT)
		return TRUE;

	try {
		Xapian::Database db (xpath);
		return db.get_doccount() == 0 ? TRUE : FALSE;
			
	} MU_XAPIAN_CATCH_BLOCK;
	
	return FALSE;
}

gboolean
mu_util_xapian_clear (const gchar *xpath)
{
	g_return_val_if_fail (xpath, FALSE);
	
	try {
		Xapian::WritableDatabase db
			(xpath, Xapian::DB_CREATE_OR_OVERWRITE);
		db.flush ();
		
		MU_WRITE_LOG ("emptied database %s", xpath);
		
		return TRUE;
		
	} MU_XAPIAN_CATCH_BLOCK;
	
	return FALSE;
}


gboolean
mu_util_xapian_is_locked (const gchar *xpath)
{
	g_return_val_if_fail (xpath, FALSE);

	try {
		Xapian::WritableDatabase db (xpath, Xapian::DB_OPEN);
	} catch (const Xapian::DatabaseLockError& xer) {
		return TRUE;
	} catch (const Xapian::Error &xer) {
		g_warning ("%s: error: %s", __FUNCTION__, xer.get_msg().c_str());
	}
	
	return FALSE;
}
