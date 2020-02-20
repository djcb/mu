/*
**  Copyright (C) 2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include "mu-parser.hh"
#include "mu-tokenizer.hh"
#include "utils/mu-utils.hh"
#include "utils/mu-error.hh"

using namespace Mu;

// 3 precedence levels: units (NOT,()) > factors (OR) > terms (AND)

// query	->      <term-1> | ε
// <term-1>	->	<factor-1> <term-2> | ε
// <term-2>	->	OR|XOR <term-1> | ε
// <factor-1>	->	<unit> <factor-2> | ε
// <factor-2>	->	[AND]|AND NOT <factor-1> | ε
// <unit>	->	[NOT] <term-1> | ( <term-1> ) | <data>
// <data>       ->      <value> | <range> | <regex>
// <value>      ->      [field:]value
// <range>      ->      [field:][lower]..[upper]
// <regex>      ->      [field:]/regex/


#define BUG(...) Mu::Error (Error::Code::Internal, format("%u: BUG: ",__LINE__)	\
				     + format(__VA_ARGS__))

static Token
look_ahead (const Mu::Tokens& tokens)
{
	return tokens.front();
}

static Mu::Tree
empty()
{
	return {{Node::Type::Empty}};
}

static Mu::Tree term_1 (Mu::Tokens& tokens, ProcPtr proc, WarningVec& warnings);


static Mu::Tree
value (const ProcIface::FieldInfoVec& fields, const std::string& v,
       size_t pos, ProcPtr proc, WarningVec& warnings)
{
	auto val = utf8_flatten(v);

	if (fields.empty())
		throw BUG("expected one or more fields");

	if (fields.size() == 1) {
		const auto item = fields.front();
		return Tree({Node::Type::Value,
			     std::make_unique<Value>(
				     item.field, item.prefix, item.id,
				     proc->process_value(item.field, val),
				     item.supports_phrase)});
	}

	// a 'multi-field' such as "recip:"
	Tree tree(Node{Node::Type::OpOr});
	for (const auto& item: fields)
		tree.add_child (Tree({Node::Type::Value,
				      std::make_unique<Value>(
					      item.field, item.prefix, item.id,
					      proc->process_value(item.field, val),
					      item.supports_phrase)}));
	return tree;
}

static Mu::Tree
regex (const ProcIface::FieldInfoVec& fields, const std::string& v,
       size_t pos, ProcPtr proc, WarningVec& warnings)
{
	if (v.length() < 2)
		throw BUG("expected regexp, got '%s'", v.c_str());

	const auto rxstr = utf8_flatten(v.substr(1, v.length()-2));

 	try {
		Tree tree(Node{Node::Type::OpOr});
		const auto rx = std::regex (rxstr);
		for (const auto& field: fields) {
			const auto terms = proc->process_regex (field.field, rx);
			for (const auto& term: terms) {
				tree.add_child (Tree(
					{Node::Type::Value,
					 std::make_unique<Value>(field.field, "",
								 field.id, term)}));
			}
		}

		if (tree.children.empty())
			return empty();
		else
			return tree;

	} catch (...) {
		// fallback
		warnings.push_back ({pos, "invalid regexp"});
		return value (fields, v, pos, proc, warnings);
	}
}



static Mu::Tree
range (const ProcIface::FieldInfoVec& fields, const std::string& lower,
       const std::string& upper, size_t pos, ProcPtr proc,
       WarningVec& warnings)
{
	if (fields.empty())
		throw BUG("expected field");

	const auto& field = fields.front();
	if (!proc->is_range_field(field.field))
		return value (fields, lower + ".." + upper, pos, proc, warnings);

	auto prange = proc->process_range (field.field, lower, upper);
	if (prange.lower > prange.upper)
		prange = proc->process_range (field.field, upper, lower);

	return Tree({Node::Type::Range,
			     std::make_unique<Range>(field.field, field.prefix, field.id,
						     prange.lower, prange.upper)});
}


static Mu::Tree
data (Mu::Tokens& tokens, ProcPtr proc, WarningVec& warnings)
{
	const auto token = look_ahead(tokens);
	if (token.type != Token::Type::Data)
		warnings.push_back ({token.pos, "expected: value"});

	tokens.pop_front();

	std::string field, val;
	const auto col = token.str.find (":");
	if (col != 0 && col != std::string::npos && col != token.str.length()-1) {
		field = token.str.substr(0, col);
		val = token.str.substr(col + 1);
	} else
		val = token.str;

	auto fields = proc->process_field (field);
	if (fields.empty()) {// not valid field...
		warnings.push_back ({token.pos, format ("invalid field '%s'", field.c_str())});
		fields = proc->process_field ("");
		// fallback, treat the whole of foo:bar as a value
		return value (fields, field + ":" + val, token.pos, proc, warnings);
	}

	// does it look like a regexp?
	if (val.length() >=2 )
		if (val[0] == '/' && val[val.length()-1] == '/')
			return regex (fields, val, token.pos, proc, warnings);

	// does it look like a range?
	const auto dotdot = val.find("..");
	if (dotdot != std::string::npos)
		return range(fields, val.substr(0, dotdot), val.substr(dotdot + 2),
			     token.pos, proc, warnings);
	else if (proc->is_range_field(fields.front().field)) {
		// range field without a range - treat as field:val..val
		return range (fields, val, val, token.pos, proc, warnings);
	}

	// if nothing else, it's a value.
	return value (fields, val, token.pos, proc, warnings);
}

static Mu::Tree
unit (Mu::Tokens& tokens, ProcPtr proc, WarningVec& warnings)
{
	if (tokens.empty()) {
		warnings.push_back ({0, "expected: unit"});
		return empty();
	}

	const auto token = look_ahead (tokens);

	if (token.type == Token::Type::Not) {
		tokens.pop_front();
		Tree tree{{Node::Type::OpNot}};
		tree.add_child(unit (tokens, proc, warnings));
		return tree;
	}

	if (token.type == Token::Type::Open) {
		tokens.pop_front();
		auto tree = term_1 (tokens, proc, warnings);
		if (tokens.empty())
			warnings.push_back({token.pos, "expected: ')'"});
		else {
			const auto token2 = look_ahead(tokens);
			if (token2.type == Token::Type::Close)
				tokens.pop_front();
			else {
				warnings.push_back(
				{token2.pos,
				 std::string("expected: ')' but got ") +
				 token2.str});
			}

		}
		return tree;
	}

	return data (tokens, proc, warnings);
}

static Mu::Tree factor_1 (Mu::Tokens& tokens, ProcPtr proc,
			   WarningVec& warnings);

static Mu::Tree
factor_2 (Mu::Tokens& tokens, Node::Type& op, ProcPtr proc,
	  WarningVec& warnings)
{
	if (tokens.empty())
		return empty();

	const auto token = look_ahead(tokens);

	switch (token.type) {
	case Token::Type::And: {
		tokens.pop_front();
		op = Node::Type::OpAnd;
	} break;

	case Token::Type::Open:
	case Token::Type::Data:
	case Token::Type::Not:
		op = Node::Type::OpAnd; // implicit AND
		break;

	default:
		return empty();
	}

	return factor_1 (tokens, proc, warnings);
}

static Mu::Tree
factor_1 (Mu::Tokens& tokens, ProcPtr proc, WarningVec& warnings)
{
	Node::Type op { Node::Type::Invalid };

	auto t  = unit (tokens, proc, warnings);
	auto a2 = factor_2 (tokens, op, proc, warnings);

	if (a2.empty())
		return t;

	Tree tree {{op}};
	tree.add_child(std::move(t));
	tree.add_child(std::move(a2));

	return tree;
}


static Mu::Tree
term_2 (Mu::Tokens& tokens, Node::Type& op, ProcPtr proc,
	WarningVec& warnings)
{
	if (tokens.empty())
		return empty();

	const auto token = look_ahead (tokens);

	switch (token.type) {
	case Token::Type::Or:
		op = Node::Type::OpOr;
		break;
	case Token::Type::Xor:
		op = Node::Type::OpXor;
		break;
	default:
		if (token.type != Token::Type::Close)
			warnings.push_back({token.pos, "expected OR|XOR"});
		return empty();
	}

	tokens.pop_front();

	return term_1 (tokens, proc, warnings);
}

static Mu::Tree
term_1 (Mu::Tokens& tokens, ProcPtr proc, WarningVec& warnings)
{
	Node::Type op { Node::Type::Invalid };

	auto t  = factor_1 (tokens, proc, warnings);
	auto o2 = term_2 (tokens, op, proc, warnings);

	if (o2.empty())
		return t;
	else {
		Tree tree {{op}};
		tree.add_child(std::move(t));
		tree.add_child(std::move(o2));
		return tree;
	}
}

static Mu::Tree
query (Mu::Tokens& tokens, ProcPtr proc, WarningVec& warnings)
{
	if (tokens.empty())
		return empty ();
	else
		return term_1 (tokens, proc, warnings);
}

Mu::Tree
Mu::parse (const std::string& expr, WarningVec& warnings, ProcPtr proc)
{
	try {
		auto tokens = tokenize (expr);
		return query (tokens, proc, warnings);

	} catch (const std::runtime_error& ex) {
		std::cerr << ex.what() << std::endl;
		return empty();
	}
}
