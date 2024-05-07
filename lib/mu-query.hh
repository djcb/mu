/*
** Copyright (C) 2008-2024 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef __MU_QUERY_HH__
#define __MU_QUERY_HH__

#include <string>
#include <memory>

#include <mu-store.hh>
#include <mu-query-results.hh>
#include <utils/mu-utils.hh>
#include <utils/mu-option.hh>
#include <utils/mu-result.hh>
#include <message/mu-message.hh>

namespace Mu {

class Query {
public:
	/**
	 * Run a query on the store
	 *
	 * @param expr the search expression
	 * @param sortfield_id the sortfield-id. Default to Date
	 * @param flags query flags
	 * @param maxnum maximum number of results to return. 0 for 'no limit'
	 *
	 * @return the query-results or an error
	 */
	Result<QueryResults> run(const std::string&	expr,
				 Field::Id		sortfield_id = Field::Id::Date,
				 QueryFlags		flags	     = QueryFlags::None,
				 size_t			maxnum	     = 0) const;

	/**
	 * run a Xapian query to count the number of matches; for the syntax, please
	 * refer to the mu-query manpage
	 *
	 * @param expr the search expression; use "" to match all messages
	 *
	 * @return the number of matches
	 */
	size_t count(const std::string& expr = "") const;

	/**
	 * For debugging, get the internal string representation of the parsed
	 * query
	 *
	 * @param expr a xapian search expression
	 * @param xapian if true, show Xapian's internal representation,
	 * otherwise, mu's.

	 * @return the string representation of the query
	 */
	std::string parse(const std::string& expr, bool xapian) const;

private:
	friend class Store;

	/**
	 * Construct a new Query instance.
	 *
	 * @param store a MuStore object
	 */
	Query(const Store& store);
	/**
	 * DTOR
	 *
	 */
	~Query();

	struct Private;
	std::unique_ptr<Private> priv_;
};
} // namespace Mu

#endif /*__MU_QUERY_HH__*/
