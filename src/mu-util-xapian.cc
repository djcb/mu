/*
** Copyright (C) 2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "config.h"
#include <xapian.h>

#include <cstring>
#include <errno.h>

#include "mu-util.h"
#include "mu-util-xapian.h"

char*
mu_util_xapian_db_version (const gchar *xpath)
{
	g_return_val_if_fail (xpath, NULL);

	if (!access(xpath, F_OK) == 0) {
		g_warning ("cannot access %s: %s", xpath, strerror(errno));
		return NULL;
	}
	
	try {			
		Xapian::Database db (xpath);
		const std::string version
			(db.get_metadata (MU_XAPIAN_VERSION_KEY));
		
		return version.empty() ? NULL : g_strdup (version.c_str());
		
	} MU_XAPIAN_CATCH_BLOCK;

	return NULL;
}


gboolean
mu_util_xapian_db_version_up_to_date (const gchar *xpath)
{
	gchar *version;
	gboolean uptodate;

	g_return_val_if_fail (xpath, FALSE);
		
	version = mu_util_xapian_db_version (xpath);
	if (!version)
		return FALSE;
	
	uptodate = (std::strcmp (version, MU_XAPIAN_DB_VERSION) == 0); 
	g_free (version);

	return uptodate;
}


gboolean
mu_util_xapian_db_is_empty (const gchar* xpath)
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
mu_util_xapian_clear_database (const gchar *xpath)
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

