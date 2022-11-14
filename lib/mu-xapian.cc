/*
** Copyright (C) 2017-2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <config.h>

#include <xapian.h>
#include "mu-xapian.hh"
#include <utils/mu-error.hh>

using namespace Mu;

static Xapian::Query
xapian_query_op(const Mu::Tree& tree)
{
	if (tree.node.type ==  Node::Type::OpNot) { // OpNot x ::= <all> AND NOT x
		if (tree.children.size() != 1)
			throw std::runtime_error("invalid # of children");
		return Xapian::Query(Xapian::Query::OP_AND_NOT,
				     Xapian::Query::MatchAll,
				     xapian_query(tree.children.front()));
	}

	const auto op = std::invoke([](Node::Type ntype) {
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wswitch-enum"
		switch (ntype) {
		case Node::Type::OpAnd:
			return Xapian::Query::OP_AND;
		case Node::Type::OpOr:
			return Xapian::Query::OP_OR;
		case Node::Type::OpXor:
			return Xapian::Query::OP_XOR;
		case Node::Type::OpAndNot:
			return Xapian::Query::OP_AND_NOT;
		case Node::Type::OpNot:
		default:
			throw Mu::Error(Error::Code::Internal, "invalid op"); // bug
		}
#pragma GCC diagnostic pop
	}, tree.node.type);

	std::vector<Xapian::Query> childvec;
	for (const auto& subtree : tree.children)
		childvec.emplace_back(xapian_query(subtree));

	return Xapian::Query(op, childvec.begin(), childvec.end());
}

static Xapian::Query
make_query(const FieldValue& fval, bool maybe_wildcard)
{
	const auto vlen{fval.value().length()};
	if (!maybe_wildcard || vlen <= 1 || fval.value()[vlen - 1] != '*')
		return Xapian::Query(fval.field().xapian_term(fval.value()));
	else
		return Xapian::Query(Xapian::Query::OP_WILDCARD,
				     fval.field().xapian_term(fval.value().substr(0, vlen - 1)));
}

static Xapian::Query
xapian_query_value(const Mu::Tree& tree)
{
	// indexable field implies it can be use with a phrase search.
	const auto& field_val{tree.node.field_val.value()};
	if (!field_val.field().is_indexable_term()) { //
		/* not an indexable field; no extra magic needed*/
		return make_query(field_val, true /*maybe-wildcard*/);
	}

        const bool is_atomic = tree.node.type == Node::Type::ValueAtomic;

	const auto parts{split(field_val.value(), " ")};
	if (parts.empty())
		return Xapian::Query::MatchNothing; // shouldn't happen
	else if (parts.size() == 1 && !is_atomic)
		return make_query(field_val, true /*maybe-wildcard*/);
        else if (is_atomic)
                return make_query(field_val, false /*maybe-wildcard*/);

	std::vector<Xapian::Query> phvec;
	for (const auto& p : parts) {
		FieldValue fv{field_val.field_id, p};
		phvec.emplace_back(make_query(fv, false /*no wildcards*/));
	}

	return Xapian::Query(Xapian::Query::OP_PHRASE, phvec.begin(), phvec.end());
}

static Xapian::Query
xapian_query_range(const Mu::Tree& tree)
{
	const auto& field_val{tree.node.field_val.value()};

	return Xapian::Query(Xapian::Query::OP_VALUE_RANGE,
			     field_val.field().value_no(),
			     field_val.range().first,
			     field_val.range().second);
}

Xapian::Query
Mu::xapian_query(const Mu::Tree& tree)
{
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wswitch-enum"
	switch (tree.node.type) {
	case Node::Type::Empty:
		return Xapian::Query();
	case Node::Type::OpNot:
	case Node::Type::OpAnd:
	case Node::Type::OpOr:
	case Node::Type::OpXor:
	case Node::Type::OpAndNot:
		return xapian_query_op(tree);
	case Node::Type::Value:
	case Node::Type::ValueAtomic:
		return xapian_query_value(tree);
	case Node::Type::Range:
		return xapian_query_range(tree);
	default:
		throw Mu::Error(Error::Code::Internal, "invalid query"); // bug
	}
#pragma GCC diagnostic pop
}
