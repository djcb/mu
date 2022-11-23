/*
**  Copyright (C) 2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef TREE_HH__
#define TREE_HH__

#include <vector>
#include <string>
#include <string_view>
#include <iostream>
#include <message/mu-fields.hh>

#include <utils/mu-option.hh>
#include <utils/mu-error.hh>

namespace Mu {

struct FieldValue {
	FieldValue(Field::Id idarg, const std::string valarg):
		field_id{idarg}, val1{valarg} {}
	FieldValue(Field::Id idarg, const std::string valarg1, const std::string valarg2):
		field_id{idarg}, val1{valarg1}, val2{valarg2} {}

	const Field& field() const { return field_from_id(field_id); }
	const std::string& value() const { return val1; }
	const std::pair<std::string, std::string> range() const { return { val1, val2 }; }

	const Field::Id		field_id;
	const std::string	val1;
	const std::string	val2;

};


/**
 * operator<<
 *
 * @param os an output stream
 * @param fval a field value.
 *
 * @return the updated output stream
 */
inline std::ostream&
operator<<(std::ostream& os, const FieldValue& fval)
{
	os << ' ' << quote(std::string{fval.field().name});

	if (fval.field().is_range())
		os << ' ' << quote(fval.range().first)
		   << ' ' << quote(fval.range().second);
	else
		os << ' ' << quote(fval.value());

	return os;
}

// A node in the parse tree
struct Node {
	enum class Type {
		Empty, // only for empty trees
		OpAnd,
		OpOr,
		OpXor,
		OpAndNot,
		OpNot,
		Value,
                ValueAtomic,
		Range,
		Invalid
	};

	Node(Type _type, FieldValue&& fval) : type{_type}, field_val{std::move(fval)} {}
	Node(Type _type) : type{_type} {}
	Node(Node&& rhs) = default;

	Type                  type;
	Option<FieldValue>    field_val;

	static constexpr std::string_view type_name(Type t) {
		switch (t) {
		case Type::Empty:
			return "";
		case Type::OpAnd:
			return "and";
		case Type::OpOr:
			return "or";
		case Type::OpXor:
			return "xor";
		case Type::OpAndNot:
			return "andnot";
		case Type::OpNot:
			return "not";
		case Type::Value:
			return "value";
		case Type::ValueAtomic:
			return "value_atomic";
		case Type::Range:
			return "range";
		case Type::Invalid:
			return "<invalid>";
		default:
			return "<error>";
		}
	}

	static constexpr bool is_binop(Type t) {
		return t == Type::OpAnd || t == Type::OpAndNot || t == Type::OpOr ||
		       t == Type::OpXor;
	}
};

inline std::ostream&
operator<<(std::ostream& os, const Node& t)
{
	os << Node::type_name(t.type);
	if (t.field_val)
		os << t.field_val.value();

	return os;
}

struct Tree {
	Tree(Node&& _node) : node(std::move(_node)) {}
	Tree(Tree&& rhs) = default;

	void add_child(Tree&& child) { children.emplace_back(std::move(child)); }
	bool empty() const { return node.type == Node::Type::Empty; }

	Node              node;
	std::vector<Tree> children;
};

inline std::ostream&
operator<<(std::ostream& os, const Tree& tree)
{
	os << '(' << tree.node;
	for (const auto& subtree : tree.children)
		os << subtree;
	os << ')';

	return os;
}

} // namespace Mu

#endif /* TREE_HH__ */
