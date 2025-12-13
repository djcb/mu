/*
** Copyright (C) 2025 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <string>
#include "mu-option.hh"
#include "mu-result.hh"

#ifndef MU_STORE_LABELS_HH
#define MU_STORE_LABELS_HH

namespace Mu {

class Store; // fwd declaration

/**
 * Export labels to a file
 *
 * If path is not specified, use a file in the current directory
 * If path ends in '/', write file in the path-directory
 *
 * @param store a store object
 * @param query for the message whose labels to export (empty for "all")
 * @param path the path or nothing
 *
 * @return either the output filename or some error
 */
Result<std::string> export_labels(const Store& store,
				  const std::string& query="",
				  Option<std::string> path={});

/**
 * Import labels from a file
 *
 * If path is not specified, use a file in the current directory
 *
 * @param store a store object
 * @param path the path to the file
 * @param dry_run only show what would be imported
 * @param quiet suppress output
 * @param verbose give verbose output
 *
 * @return Ok or some error
 */
Result<void> import_labels(Store&, const std::string& path, bool dry_run,
			   bool quiet, bool verbose);
} // Mu


#endif /*MU_STORE_LABELS_HH*/
