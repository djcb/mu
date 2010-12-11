/*
** Copyright (C) 2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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


#ifndef __MU_ERROR_H__
#define __MU_ERROR_H__

enum _MuError {
	/* general xapian related error */
	MU_ERROR_XAPIAN,
	/* xapian dir is not accessible */
	MU_ERROR_XAPIAN_DIR,
	/* database version is not uptodate (ie. not compatible with
	 * the version that mu expects) */
	MU_ERROR_XAPIAN_NOT_UPTODATE,
	/* missing data for a document */
	MU_ERROR_XAPIAN_MISSING_DATA,
	/* (parsnng) error in the query */ 
	MU_ERROR_QUERY,
	/* gmime parsing related error */
	MU_ERROR_GMIME,

	/* File errors */
	MU_ERROR_FILE_INVALID_SOURCE,
	MU_ERROR_FILE_INVALID_NAME,
	MU_ERROR_FILE_CANNOT_LINK,
	MU_ERROR_FILE_CANNOT_OPEN,
	MU_ERROR_FILE_CANNOT_READ,
	MU_ERROR_FILE_CANNOT_CREATE,
	MU_FILE_ERROR_CANNOT_MKDIR,
	MU_FILE_ERROR_STAT_FAILED,
	MU_FILE_ERROR_READDIR_FAILED,
	/* generic file-related error */
	MU_ERROR_FILE,
	
	/* some other, internal error */
	MU_ERROR_INTERNAL
};
typedef enum _MuError MuError;

#endif /*__MU_ERROR_H__*/
