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

#ifndef __TOKENIZER_HH__
#define __TOKENIZER_HH__

#include <string>
#include <vector>
#include <deque>
#include <ostream>
#include <stdexcept>

// A simple tokenizer, which turns a string into a deque of tokens
//
// It recognizes '(', ')', '*' 'and', 'or', 'xor', 'not'
//
// Note that even if we recognizes those at the lexical level, they might be demoted to mere strings
// when we're creating the parse tree.
//
// Furthermore, we detect ranges ("a..b") and regexps (/../) at the parser level, since we need a
// bit more context to resolve ambiguities.

namespace Mu {

// A token
struct Token {
	enum class Type {
		Data, /**< e .g., banana or date:..456 */

		// Brackets
		Open,  /**< ( */
		Close, /**< ) */

		// Unops
		Not, /**< logical not*/

		// Binops
		And, /**< logical and */
		Or,  /**< logical not */
		Xor, /**< logical xor */

		Empty, /**< nothing */
	};

	size_t            pos{};  /**< position in string */
	Type              type{}; /**< token type */
	const std::string str{};  /**< data for this token */

	/**
	 * operator==
	 *
	 * @param rhs right-hand side
	 *
	 * @return true if rhs is equal to this; false otherwise
	 */
	bool operator==(const Token& rhs) const
	{
		return pos == rhs.pos && type == rhs.type && str == rhs.str;
	}
};

/**
 * operator<<
 *
 * @param os an output stream
 * @param t a token type
 *
 * @return the updated output stream
 */
inline std::ostream&
operator<<(std::ostream& os, Token::Type t)
{
	switch (t) {
	case Token::Type::Data: os << "<data>"; break;

	case Token::Type::Open: os << "<open>"; break;
	case Token::Type::Close: os << "<close>"; break;

	case Token::Type::Not: os << "<not>"; break;
	case Token::Type::And: os << "<and>"; break;
	case Token::Type::Or: os << "<or>"; break;
	case Token::Type::Xor: os << "<xor>"; break;
	case Token::Type::Empty: os << "<empty>"; break;
	default: // can't happen, but pacify compiler
		throw std::runtime_error("<<bug>>");
	}

	return os;
}

/**
 * operator<<
 *
 * @param os an output stream
 * @param t a token
 *
 * @return the updated output stream
 */
inline std::ostream&
operator<<(std::ostream& os, const Token& t)
{
	os << t.pos << ": " << t.type;

	if (!t.str.empty())
		os << " [" << t.str << "]";

	return os;
}

/**
 * Tokenize a string into a vector of tokens. The tokenization always succeeds, ie., ignoring errors
 * such a missing end-".
 *
 * @param s a string
 *
 * @return a deque of tokens
 */
using Tokens = std::deque<Token>;
Tokens tokenize(const std::string& s);

} // namespace Mu

#endif /* __TOKENIZER_HH__ */
