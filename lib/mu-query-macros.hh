/*
** Copyright (C) 2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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


#ifndef MU_QUERY_MACROS_HH__
#define MU_QUERY_MACROS_HH__

#include <string>
#include <memory>

#include <utils/mu-result.hh>
#include <utils/mu-option.hh>

#include "mu-config.hh"

namespace Mu {

class QueryMacros{
public:
	/**
	 * Construct QueryMacros object
	 *
	 * @param conf config object ref
	 */
	QueryMacros(const Config& conf);

	/**
	 * DTOR
	 */
	~QueryMacros();

	/**
	 * Read bookmarks (ie. macros) from a bookmark-file
	 *
	 * @param bookmarks_file path to the bookmarks file
	 *
	 * @return Ok or some error
	 */
	Result<void> load_bookmarks(const std::string& bookmarks_file);


	/**
	 * Find a macro (aka 'bookmark') by its name
	 *
	 * @param name the name of the bookmark
	 *
	 * @return the macro value or Nothing if not found
	 */
	Option<std::string> find_macro(const std::string& name) const;

private:
	struct Private;
	std::unique_ptr<Private> priv_;
};


} // namespace Mu

#endif /* MU_QUERY_MACROS_HH__ */
