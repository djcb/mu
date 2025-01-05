/*
** Copyright (C) 2023-2024 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-xapian-db.hh"

#include "utils/mu-sexp.hh"
#include "utils/mu-result.hh"
#include "mu-store.hh"

namespace Mu {
/*
 * Some useful symbol-sexps
 */
static inline const auto	placeholder_sym = "_"_sym;
static inline const auto	phrase_sym	= "phrase"_sym;
static inline const auto	regex_sym	= "regex"_sym;
static inline const auto	range_sym	= "range"_sym;
static inline const auto	wildcard_sym	= "wildcard"_sym;

static inline const auto	open_sym	= "("_sym;
static inline const auto	close_sym	= ")"_sym;

static inline const auto	and_sym		= "and"_sym;
static inline const auto	or_sym		= "or"_sym;
static inline const auto	xor_sym		= "xor"_sym;
static inline const auto	not_sym		= "not"_sym;
static inline const auto	and_not_sym	= "and-not"_sym;


/*
 * We take a query, then parse it into a human-readable s-expression and then
 * turn that s-expression into a Xapian query
 *
 * some query:
 *   "from:hello or subject:world"
 *
 * 1. tokenize-query
 *   => ((from "hello") or (subject "world"))
 *
 * 2. parse-query
 *   => (or (from "hello") (subject "world"))
 *
 * 3. xapian-query
 *   => Query((Fhello OR Sworld))
 * *
 */

/**
 * Analyze the query expression and express it as a Sexp-list with the sequence
 * of elements.
 *
 * @param expr a search expression
 *
 * @return Sexp with the sequence of elements
 */
Sexp process_query(const std::string& expr);

/**
 * Parse the query expression and create a parse-tree expressed as an Sexp
 * object (tree).
 *
 * Internally, this processes the stream into element (see process_query()) and
 * processes the tokens into a Sexp. This sexp is meant to be human-readable.
 *
 * @param expr a search expression
 * @param expand whether to expand combination-fields (such as '_', 'recip',
 * 'contacts')
 *
 * @return Sexp with the parse tree
 */
Sexp parse_query(const std::string& expr, bool expand=false);

/**
 * Make a Xapian Query for the given string expression.
 *
 * This uses parse_query() and turns the S-expression into a Xapian::Query.
 * Unlike mere parsing, this uses the information in the store to resolve
 * wildcard / regex queries.
 *
 * @param store the message store
 * @param expr a string expression
 * @param flavor type of parser to use
 *
 * @return a Xapian query result or an error.
 */
enum struct ParserFlags {
	None	      = 0 << 0,
	SupportNgrams = 1 << 0, /**< Support Xapian's Ngrams for CJK etc. handling */
	XapianParser  = 1 << 1, /**< For testing only, use Xapian's
				 * built-in QueryParser; this is not
				 * fully compatible with mu, only useful
				 * for debugging. */
};
Result<Xapian::Query> make_xapian_query(const Store& store, const std::string& expr,
					ParserFlags flag=ParserFlags::None) noexcept;

MU_ENABLE_BITOPS(ParserFlags);
} // namespace Mu
