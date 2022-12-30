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

#include <algorithm>
#include <limits>

#include "mu-tokenizer.hh"
#include "utils/mu-utils.hh"
#include "utils/mu-error.hh"
#include "utils/mu-regex.hh"
#include "message/mu-message.hh"

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

#define BUG(...)                                                                                   \
	Mu::Error(Error::Code::Internal, format("%u: BUG: ", __LINE__) + format(__VA_ARGS__))

/**
 * Get the "shortcut"/internal fields for the the given fieldstr or empty if there is none
 *
 * @param fieldstr a fieldstr, e.g "subject" or "s" for the subject field
 *
 * @return a vector with "exploded" values, with a code and a fullname. E.g. "s" might map
 * to [<"S","subject">], while "recip" could map to [<"to", "T">, <"cc", "C">, <"bcc", "B">]
 */
struct FieldInfo {
	const std::string field;
	const std::string prefix;
	bool              supports_phrase;
	Field::Id         id;
};
using FieldInfoVec = std::vector<FieldInfo>;
struct Parser::Private {
	Private(const Store& store, Parser::Flags flags) : store_{store}, flags_{flags} {}

	std::vector<std::string> process_regex(const std::string& field,
					       const Regex&  rx) const;

	Mu::Tree term_1(Mu::Tokens& tokens, WarningVec& warnings) const;
	Mu::Tree term_2(Mu::Tokens& tokens, Node::Type& op, WarningVec& warnings) const;
	Mu::Tree factor_1(Mu::Tokens& tokens, WarningVec& warnings) const;
	Mu::Tree factor_2(Mu::Tokens& tokens, Node::Type& op, WarningVec& warnings) const;
	Mu::Tree unit(Mu::Tokens& tokens, WarningVec& warnings) const;
	Mu::Tree data(Mu::Tokens& tokens, WarningVec& warnings) const;
	Mu::Tree range(const FieldInfoVec& fields,
		       const std::string&  lower,
		       const std::string&  upper,
		       size_t              pos,
		       WarningVec&         warnings) const;
	Mu::Tree regex(const FieldInfoVec& fields,
		       const std::string&  v,
		       size_t              pos,
		       WarningVec&         warnings) const;
	Mu::Tree value(const FieldInfoVec& fields,
		       const std::string&  v,
		       size_t              pos,
		       WarningVec&         warnings) const;

      private:
	const Store& store_;
	const Parser::Flags  flags_;
};

static std::string
process_value(const std::string& field, const std::string& value)
{
	const auto id_opt{field_from_name(field)};
	if (id_opt) {
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wswitch-enum"
		switch (id_opt->id) {
		case Field::Id::Priority: {
			if (!value.empty())
				return std::string(1, value[0]);
		} break;
		case Field::Id::Flags:
			if (const auto info{flag_info(value)}; info)
				return std::string(1, info->shortcut_lower());
			break;
		default:
			break;
		}
#pragma GCC diagnostic pop
	}

	return value; // XXX prio/flags, etc. alias
}

static void
add_field(std::vector<FieldInfo>& fields, Field::Id field_id)
{
	const auto field{field_from_id(field_id)};
	if (!field.shortcut)
		return; // can't be searched

	fields.emplace_back(FieldInfo{std::string{field.name}, field.xapian_term(),
			    field.is_indexable_term(), field_id});
}

static std::vector<FieldInfo>
process_field(const std::string& field_str, Parser::Flags flags)
{
	std::vector<FieldInfo> fields;
	if (any_of(flags & Parser::Flags::UnitTest)) {
		add_field(fields, Field::Id::MessageId);
		return fields;
	}

	if (field_str == "contact" || field_str == "recip") { // multi fields
		add_field(fields, Field::Id::To);
		add_field(fields, Field::Id::Cc);
		add_field(fields, Field::Id::Bcc);
		if (field_str == "contact")
			add_field(fields, Field::Id::From);
	} else if (field_str.empty()) {
		add_field(fields, Field::Id::To);
		add_field(fields, Field::Id::Cc);
		add_field(fields, Field::Id::Bcc);
		add_field(fields, Field::Id::From);
		add_field(fields, Field::Id::Subject);
		add_field(fields, Field::Id::BodyText);
	} else if (const auto field_opt{field_from_name(field_str)}; field_opt)
		add_field(fields, field_opt->id);

	return fields;
}

static bool
is_range_field(const std::string& field_str)
{
	if (const auto field_opt{field_from_name(field_str)}; !field_opt)
		return false;
	else
		return field_opt->is_range();
}

struct MyRange {
	std::string lower;
	std::string upper;
};

static MyRange
process_range(const std::string& field_str,
	      const std::string& lower, const std::string& upper)
{
	const auto field_opt{field_from_name(field_str)};
	if (!field_opt)
		return {lower, upper};

	std::string l2 = lower;
	std::string u2 = upper;
	constexpr auto upper_limit = std::numeric_limits<int64_t>::max();

	if (field_opt->id == Field::Id::Date || field_opt->id == Field::Id::Changed) {
		l2 = to_lexnum(parse_date_time(lower, true).value_or(0));
		u2 = to_lexnum(parse_date_time(upper, false).value_or(upper_limit));
	} else if (field_opt->id == Field::Id::Size) {
		l2 = to_lexnum(parse_size(lower, true).value_or(0));
		u2 = to_lexnum(parse_size(upper, false).value_or(upper_limit));
	}

	return {l2, u2};
}

std::vector<std::string>
Parser::Private::process_regex(const std::string& field_str,
			       const Regex& rx) const
{
	const auto field_opt{field_from_name(field_str)};
	if (!field_opt)
		return {};

	const auto prefix{field_opt->xapian_term()};
	std::vector<std::string> terms;
	store_.for_each_term(field_opt->id, [&](auto&& str) {
		auto val{str.c_str() + 1}; // strip off the Xapian prefix.
		if (rx.matches(val))
			terms.emplace_back(std::move(val));
		return true;
	});

	return terms;
}

static Token
look_ahead(const Mu::Tokens& tokens)
{
	return tokens.front();
}

static Mu::Tree
empty()
{
	return {{Node::Type::Empty}};
}

Mu::Tree
Parser::Private::value(const FieldInfoVec& fields,
		       const std::string&  v,
		       size_t              pos,
		       WarningVec&         warnings) const
{
	auto val = utf8_flatten(v);

	if (fields.empty())
		throw BUG("expected one or more fields");

	if (fields.size() == 1) {
		const auto item = fields.front();
		return Tree({Node::Type::Value,
				FieldValue{item.id, process_value(item.field, val)}});
	}

	// a 'multi-field' such as "recip:"
	Tree tree(Node{Node::Type::OpOr});
	for (const auto& item : fields)
		tree.add_child(Tree({Node::Type::Value,
					FieldValue{item.id,
					     process_value(item.field, val)}}));
	return tree;
}

Mu::Tree
Parser::Private::regex(const FieldInfoVec& fields,
		       const std::string&  v,
		       size_t              pos,
		       WarningVec&         warnings) const
{
	if (v.length() < 2)
		throw BUG("expected regexp, got '%s'", v.c_str());

	const auto rxstr = utf8_flatten(v.substr(1, v.length() - 2));

	try {
		Tree tree(Node{Node::Type::OpOr});
		const auto rx = Regex::make(rxstr, G_REGEX_OPTIMIZE);
		if (!rx)
			throw rx.error();
		for (const auto& field : fields) {
			const auto terms = process_regex(field.field, *rx);
			for (const auto& term : terms) {
				tree.add_child(Tree({Node::Type::ValueAtomic,
							FieldValue{field.id, term}}));
			}
		}

		if (tree.children.empty())
			return empty();
		else
			return tree;

	} catch (...) {
		// fallback
		warnings.push_back({pos, "invalid regexp"});
		return value(fields, v, pos, warnings);
	}
}

Mu::Tree
Parser::Private::range(const FieldInfoVec& fields,
		       const std::string&  lower,
		       const std::string&  upper,
		       size_t              pos,
		       WarningVec&         warnings) const
{
	if (fields.empty())
		throw BUG("expected field");

	const auto& field = fields.front();
	if (!is_range_field(field.field))
		return value(fields, lower + ".." + upper, pos, warnings);

	auto prange = process_range(field.field, lower, upper);
	if (prange.lower > prange.upper)
		prange = process_range(field.field, upper, lower);

	return Tree({Node::Type::Range,
			FieldValue{field.id, prange.lower, prange.upper}});
}

Mu::Tree
Parser::Private::data(Mu::Tokens& tokens, WarningVec& warnings) const
{
	const auto token = look_ahead(tokens);
	if (token.type != Token::Type::Data)
		warnings.push_back({token.pos, "expected: value"});

	tokens.pop_front();

	std::string field, val;
	const auto  col = token.str.find(":");
	if (col != 0 && col != std::string::npos && col != token.str.length() - 1) {
		field = token.str.substr(0, col);
		val   = token.str.substr(col + 1);
	} else
		val = token.str;

	auto fields = process_field(field, flags_);
	if (fields.empty()) { // not valid field...
		warnings.push_back({token.pos, format("invalid field '%s'", field.c_str())});
		fields = process_field("", flags_);
		// fallback, treat the whole of foo:bar as a value
		return value(fields, field + ":" + val, token.pos, warnings);
	}

	// does it look like a regexp?
	if (val.length() >= 2)
		if (val[0] == '/' && val[val.length() - 1] == '/')
			return regex(fields, val, token.pos, warnings);

	// does it look like a range?
	const auto dotdot = val.find("..");
	if (dotdot != std::string::npos)
		return range(fields,
			     val.substr(0, dotdot),
			     val.substr(dotdot + 2),
			     token.pos,
			     warnings);
	else if (is_range_field(fields.front().field)) {
		// range field without a range - treat as field:val..val
		return range(fields, val, val, token.pos, warnings);
	}

	// if nothing else, it's a value.
	return value(fields, val, token.pos, warnings);
}

Mu::Tree
Parser::Private::unit(Mu::Tokens& tokens, WarningVec& warnings) const
{
	if (tokens.empty()) {
		warnings.push_back({0, "expected: unit"});
		return empty();
	}

	const auto token = look_ahead(tokens);

	if (token.type == Token::Type::Not) {
		tokens.pop_front();
		Tree tree{{Node::Type::OpNot}};
		tree.add_child(unit(tokens, warnings));
		return tree;
	}

	if (token.type == Token::Type::Open) {
		tokens.pop_front();
		auto tree = term_1(tokens, warnings);
		if (tokens.empty())
			warnings.push_back({token.pos, "expected: ')'"});
		else {
			const auto token2 = look_ahead(tokens);
			if (token2.type == Token::Type::Close)
				tokens.pop_front();
			else {
				warnings.push_back(
				    {token2.pos,
				     std::string("expected: ')' but got ") + token2.str});
			}
		}
		return tree;
	}

	return data(tokens, warnings);
}

Mu::Tree
Parser::Private::factor_2(Mu::Tokens& tokens, Node::Type& op, WarningVec& warnings) const
{
	if (tokens.empty())
		return empty();

	const auto token = look_ahead(tokens);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wswitch-enum"
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
#pragma GCC diagnostic pop

	return factor_1(tokens, warnings);
}

Mu::Tree
Parser::Private::factor_1(Mu::Tokens& tokens, WarningVec& warnings) const
{
	Node::Type op{Node::Type::Invalid};

	auto t  = unit(tokens, warnings);
	auto a2 = factor_2(tokens, op, warnings);

	if (a2.empty())
		return t;

	Tree tree{{op}};
	tree.add_child(std::move(t));
	tree.add_child(std::move(a2));

	return tree;
}

Mu::Tree
Parser::Private::term_2(Mu::Tokens& tokens, Node::Type& op, WarningVec& warnings) const
{
	if (tokens.empty())
		return empty();

	const auto token = look_ahead(tokens);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wswitch-enum"
	switch (token.type) {
	case Token::Type::Or: op = Node::Type::OpOr; break;
	case Token::Type::Xor: op = Node::Type::OpXor; break;
	default:
		if (token.type != Token::Type::Close)
			warnings.push_back({token.pos, "expected OR|XOR"});
		return empty();
	}
#pragma GCC diagnostic pop

	tokens.pop_front();

	return term_1(tokens, warnings);
}

Mu::Tree
Parser::Private::term_1(Mu::Tokens& tokens, WarningVec& warnings) const
{
	Node::Type op{Node::Type::Invalid};

	auto t  = factor_1(tokens, warnings);
	auto o2 = term_2(tokens, op, warnings);

	if (o2.empty())
		return t;
	else {
		Tree tree{{op}};
		tree.add_child(std::move(t));
		tree.add_child(std::move(o2));
		return tree;
	}
}

Mu::Parser::Parser(const Store& store, Parser::Flags flags) :
	priv_{std::make_unique<Private>(store, flags)}
{
}

Mu::Parser::~Parser() = default;

Mu::Tree
Mu::Parser::parse(const std::string& expr, WarningVec& warnings) const
{
	try {
		auto tokens = tokenize(expr);
		if (tokens.empty())
			return empty();
		else
			return priv_->term_1(tokens, warnings);

	} catch (const std::runtime_error& ex) {
		std::cerr << ex.what() << std::endl;
		return empty();
	}
}
