/*
**  Copyright (C) 2017 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
**
**  This library is free software; you can redistribute it and/or
**  modify it under the terms of the GNU Lesser General Public License
**  as published by the Free Software Foundation; either version 2.1
**  of the License, or (at your option) any later version.
**
**  This library is distributed in the hope that it will be useful,
**  but WITHOUT ANY WARRANTY; without even the implied warranty of
**  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
**  Lesser General Public License for more details.
**
**  You should have received a copy of the GNU Lesser General Public
**  License along with this library; if not, write to the Free
**  Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
**  02110-1301, USA.
*/

#ifndef __PARSER_HH__
#define __PARSER_HH__

#include "utils/mu-utils.hh"
#include <string>
#include <vector>
#include <memory>

#include <mu-tree.hh>
#include <mu-store.hh>

// A simple recursive-descent parser for queries. Follows the Xapian syntax,
// but better handles non-alphanum; also implements regexp

namespace Mu {

/**
 * A parser warning
 *
 */
struct Warning {
	size_t            pos{}; /**< pos in string */
	const std::string msg;   /**< warning message */

	/**
	 * operator==
	 *
	 * @param rhs right-hand side
	 *
	 * @return true if rhs is equal to this; false otherwise
	 */
	bool operator==(const Warning& rhs) const { return pos == rhs.pos && msg == rhs.msg; }
};
using WarningVec = std::vector<Warning>;

/**
 * operator<<
 *
 * @param os an output stream
 * @param w a warning
 *
 * @return the updated output stream
 */
inline std::ostream&
operator<<(std::ostream& os, const Warning& w)
{
	os << w.pos << ":" << w.msg;
	return os;
}

class Parser {
      public:
	enum struct Flags { None = 0, UnitTest = 1 << 0 };

	/**
	 * Construct a query parser object
	 *
	 * @param store a store object ptr, or none
	 */
	Parser(const Store& store, Flags = Flags::None);
	/**
	 * DTOR
	 *
	 */
	~Parser();

	/**
	 * Parse a query string
	 *
	 * @param query a query string
	 * @param warnings vec to receive warnings
	 *
	 * @return a parse-tree
	 */

	Tree parse(const std::string& query, WarningVec& warnings) const;

      private:
	struct Private;
	std::unique_ptr<Private> priv_;
};

MU_ENABLE_BITOPS(Parser::Flags);

} // namespace Mu

#endif /* __PARSER_HH__ */
