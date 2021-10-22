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

#ifndef TREE_HH__
#define TREE_HH__

#include <vector>
#include <string>
#include <iostream>

#include <mu-data.hh>
#include <utils/mu-error.hh>

namespace Mu {

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
		Range,
		Invalid
	};

	Node(Type _type, std::unique_ptr<Data>&& _data) : type{_type}, data{std::move(_data)} {}
	Node(Type _type) : type{_type} {}
	Node(Node&& rhs) = default;

	Type                  type;
	std::unique_ptr<Data> data;

	static const char* type_name(Type t)
	{
		switch (t) {
		case Type::Empty: return ""; break;
		case Type::OpAnd: return "and"; break;
		case Type::OpOr: return "or"; break;
		case Type::OpXor: return "xor"; break;
		case Type::OpAndNot: return "andnot"; break;
		case Type::OpNot: return "not"; break;
		case Type::Value: return "value"; break;
		case Type::Range: return "range"; break;
		case Type::Invalid: return "<invalid>"; break;
		default: throw Mu::Error(Error::Code::Internal, "unexpected type");
		}
	}

	static constexpr bool is_binop(Type t)
	{
		return t == Type::OpAnd || t == Type::OpAndNot || t == Type::OpOr ||
		       t == Type::OpXor;
	}
};

inline std::ostream&
operator<<(std::ostream& os, const Node& t)
{
	os << Node::type_name(t.type);
	if (t.data)
		os << t.data;

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
