/*
** Copyright (C) 2020 djcb <djcb@evergrey>
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

#ifndef MU_SEXP_PARSER_HH__
#define MU_SEXP_PARSER_HH__

#include <string>
#include <vector>

#include "utils/mu-error.hh"

namespace Mu {
namespace Sexp {

/// Simple s-expression parser that parses lists () and atoms (strings
/// ("-quoted), (positive) integers ([0..9]+) and symbol starting with alpha or
/// ':', then alphanum and '-')
///
/// (:foo (1234 "bar" nil) :quux (a b c))

/// Node type
enum struct Type { List, String, Integer, Symbol };

/// Parse node
struct Node {
        /**
         * Construct a new non-list node
         *
         * @param typearg the type of node
         * @param valuearg the value
         */
        Node(Type typearg, std::string&& valuearg):
                type{typearg}, value{std::move(valuearg)} {
                if (typearg == Type::List)
                        throw Error(Error::Code::Parsing,
                                    "atomic type cannot be a <list>");
        }

        /**
         * Construct a list node

         * @param childrenarg  the list children
         *
         * @return
         */
        explicit Node(std::vector<Node>&& childrenarg):
                type{Type::List}, children{std::move(childrenarg)}
                {}

        const Type              type; /**<  Type of node */
        const std::string       value; /**< String value of node (only for non-Type::List)*/
        const std::vector<Node> children; /**< Chiidren of node (only for Type::List) */
};

/**
 * Parse the string as an s-expressi9on.
 *
 * @param expr an s-expression string
 *
 * @return the parsed s-expression, or throw Error.
 */
Node parse(const std::string& expr);

static inline std::ostream&
operator<<(std::ostream& os, Sexp::Type id)
{
        switch (id) {
        case Sexp::Type::List:    os << "<list>"; break;
        case Sexp::Type::String:  os << "<string>"; break;
        case Sexp::Type::Integer: os << "<integer>"; break;
        case Sexp::Type::Symbol:  os << "<symbol>"; break;
        default: throw std::runtime_error ("unknown node type");
        }

        return os;
}

static inline std::ostream&
operator<<(std::ostream& os, const Sexp::Node& node)
{
        os << node.type;
        if (node.type == Sexp::Type::List) {
                os << '(';
                for (auto&& elm: node.children)
                        os <<  elm;
                os << ')';
        } else
                os << '{' << node.value << '}';

        return os;
}


} // Sexp


} // Mu

#endif /* MU_SEXP_PARSER_HH__ */
