/*
** Copyright (C) 2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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


#ifndef MU_SEXP_HH__
#define MU_SEXP_HH__

#include <string>
#include <vector>
#include <type_traits>

#include "utils/mu-utils.hh"
#include "utils/mu-error.hh"

namespace Mu {
namespace Sexp {

/// Simple s-expression parser & builder that parses lists () and atoms (strings
/// ("-quoted), (positive) integers ([0..9]+) and symbol starting with alpha or
/// ':', then alphanum and '-')
///
/// (:foo (1234 "bar" nil) :quux (a b c))


/// Parse node
struct Node {
        /// Node type
        enum struct Type { List, String, Number, Symbol };

        /**
         * Make a node of out of an s-expression string.
         *
         * @param expr a string containing an s-expression
         *
         * @return the parsed s-expression, or throw Error.
         */
        static Node make (const std::string& expr);

        /**
         * Make a node for a string/integer/symbol/list value
         *
         * @param val some value
          *
         * @return a node
         */
        static Node make_string (std::string&& val) { return Node{Type::String, std::move(val)}; }
        static Node make_string (const std::string& val) { return Node{Type::String, std::string{val}}; }
        static Node make_number (int val) { return Node{Type::Number, format("%d", val)}; }
        static Node make_symbol (std::string&& val) { return Node{Type::Symbol, std::move(val)}; }


        /// sequence of node objects
        struct Seq {
                /**
                 * Add an item to a node-sequence.
                 *
                 * @param node item to add; moved/consumed.
                 */
                void add(Node&& node)             {nodes_.emplace_back(std::move(node));}
                void add(Seq&& seq)               {add(make_list(std::move(seq)));}
                void add(std::string&& s)         {add(make_string(std::move(s)));}
                void add(int i)                   {add(make_number(i));}
                // disambiguate.
                void add_symbol(std::string&& s)  {add(make_symbol(std::move(s)));}

                /**
                 * Add a property to tne node sequence; i.e. a property-symbol
                 * (starting with ':') and some node
                 *
                 * @param name name (must start with ":"), and some value or list
                 * @param val
                 */
                template<typename T> void add_prop(std::string&& name, T&& val) {

                        if (name.empty() || name[0] != ':')
                                throw Error{Error::Code::InvalidArgument,
                                                "property names must start with ':' ('%s')",
                                                name.c_str()};

                        add(make_symbol(std::move(name)));
                        add(std::move(val));
                }
                void  add_prop(std::string&& name, const std::string& val) {
                        add_prop(std::move(name), std::string(val));
                }

                // deliberately limited stl-like

                /**
                 * Is this an empty sequence?
                 *
                 * @return true or false
                 */
                bool empty() const {return nodes_.empty();}

                /**
                 * Get the number of elsements in the sequence
                 *
                 * @return number of elmement.
                 */
                size_t size() const {return nodes_.size();}

                /**
                 * Get begin iterator of the sequence
                 *
                 * @return iterator
                 */
                const auto begin()          const { return nodes_.begin(); }
                /**
                 * Get the end iterator of the sequnce
                 *
                 * @return an iterator
                 */
                const auto end()            const { return nodes_.end(); }

                /**
                 * Get a const ref to the item at idx.
                 *
                 * @param idx index,  must be < size()
                 *
                 * @return const ref to the item.
                 */
                const auto at(size_t idx)   const { return nodes_.at(idx);}

        private:
                std::vector<Node> nodes_;
        };

        /**
         * Make a list node from a sequence
         *
         * @param seq a sequence of nodes
         *
         * @return a node
         */
        static Node make_list   (Seq&& seq) { return Node{std::move(seq)}; }

        /**
         * Convert a Sexp::Node to its string representation
         *
         * @return the string representation
         */
        std::string to_string() const;

        /**
         * Return the type of this Node.
         *
         * @return the type
         */
        Type type() const { return type_; }

        /// Some type helpers
        bool is_list()   const { return type() == Type::List; };
        bool is_string() const { return type() == Type::String; }
        bool is_number() const { return type() == Type::Number; }
        bool is_symbol() const { return type() == Type::Symbol; }
        bool is_nil()    const { return is_symbol() && value() == "nil"; }
        bool is_t()      const { return is_symbol() && value() == "t"; }

        /**
         * The elements of this node; invalid unless this is a list node.
         *
         * @return
         */
        const Seq& elements() const {
                if (type() != Type::List)
                        throw Error(Error::Code::InvalidArgument, "no elements for non-list");
                return seq_;
        }

        /**
         * The value of this node; invalid for list nodes.
         *
         * @return
         */
        const std::string& value() const {
                if (type_ == Type::List)
                        throw Error(Error::Code::InvalidArgument, "no value for list");
                return value_;
        }

private:
        /**
         * Construct a new non-list node
         *
         * @param typearg the type of node
         * @param valuearg the value
         */
        Node(Type typearg, std::string&& valuearg):
                type_{typearg}, value_{std::move(valuearg)} {
                if (typearg == Type::List)
                        throw Error(Error::Code::Parsing,
                                    "atomic type cannot be a <list>");
        }

        /**
         * Construct a list node
         *
         * @param kids the list's children
         */
        explicit Node(Seq&& seq):
                type_{Type::List}, seq_{std::move(seq)}
                {}


        const Type        type_; /**<  Type of node */
        const std::string value_;    /**< String value of node (only for
                                     * non-Type::Lst)*/
        const Seq         seq_;  /**< Children of node (only for
                                     * Type::Lst) */
};

static inline std::ostream&
operator<<(std::ostream& os, Sexp::Node::Type id)
{
        switch (id) {
        case Sexp::Node::Type::List:    os << "<list>"; break;
        case Sexp::Node::Type::String:  os << "<string>"; break;
        case Sexp::Node::Type::Number:  os << "<number>"; break;
        case Sexp::Node::Type::Symbol:  os << "<symbol>"; break;
        default: throw std::runtime_error ("unknown node type");
        }

        return os;
}

static inline std::ostream&
operator<<(std::ostream& os, const Sexp::Node& node)
{
        os << node.to_string();
        return os;
}

} // Sexp


} // Mu

#endif /* MU_SEXP_HH__ */
