/*
** Copyright (C) 2017 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <xapian.h>
#include "parser/xapian.hh"

using namespace Mux;

static Xapian::Query
xapian_query_op (const Mux::Tree& tree)
{
	Xapian::Query::op op;

	switch (tree.node.type) {
	case Node::Type::OpNot: // OpNot x ::= <all> AND NOT x
		  if (tree.children.size() != 1)
			  throw std::runtime_error ("invalid # of children");
		  return Xapian::Query (Xapian::Query::OP_AND_NOT,
					Xapian::Query::MatchAll,
					xapian_query(tree.children.front()));
	case Node::Type::OpAnd: op    = Xapian::Query::OP_AND; break;
	case Node::Type::OpOr:  op    = Xapian::Query::OP_OR; break;
	case Node::Type::OpXor: op    = Xapian::Query::OP_XOR; break;
	case Node::Type::OpAndNot: op = Xapian::Query::OP_AND_NOT; break;
	default: throw std::runtime_error ("invalid op");	// bug
	}

	std::vector<Xapian::Query> childvec;
	for (const auto& subtree: tree.children)
		childvec.emplace_back(xapian_query(subtree));

	return Xapian::Query(op, childvec.begin(), childvec.end());
}

static Xapian::Query
xapian_query_value (const Mux::Tree& tree)
{
	const auto v = dynamic_cast<Value*> (tree.node.data.get());
	if (!v->phrase)
		return Xapian::Query(v->prefix + v->value);

	const auto parts = split (v->value, " ");

	std::vector<Xapian::Query> phvec;
	for (const auto p: parts)
		phvec.push_back(Xapian::Query(v->prefix + p));

	if (parts.empty())
		return Xapian::Query::MatchNothing; // shouldn't happen

	if (parts.size() == 1)
		return phvec.front();

	return Xapian::Query (Xapian::Query::OP_PHRASE,
			      phvec.begin(), phvec.end());
}

static Xapian::Query
xapian_query_range (const Mux::Tree& tree)
{
	const auto r = dynamic_cast<Range*> (tree.node.data.get());
	return Xapian::Query(Xapian::Query::OP_VALUE_RANGE,
			     (Xapian::valueno)r->id, r->lower, r->upper);
}


Xapian::Query
Mux::xapian_query (const Mux::Tree& tree)
{
	switch (tree.node.type) {
	case Node::Type::Empty:
		return Xapian::Query();
	case Node::Type::OpNot:
	case Node::Type::OpAnd:
	case Node::Type::OpOr:
	case Node::Type::OpXor:
	case Node::Type::OpAndNot:
		return xapian_query_op (tree);
	case Node::Type::Value:
		return xapian_query_value (tree);
	case Node::Type::Range:
		return xapian_query_range (tree);
	default:
		throw std::runtime_error ("invalid query"); // bug
	}
}
