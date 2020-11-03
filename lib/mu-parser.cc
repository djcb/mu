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
#include <algorithm>

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



/**
 * Get the "shortcut"/internal fields for the the given fieldstr or empty if there is none
 *
 * @param fieldstr a fieldstr, e.g "subject" or "s" for the subject field
 *
 * @return a vector with "exploded" values, with a code and a fullname. E.g. "s" might map
 * to [<"S","subject">], while "recip" could map to [<"to", "T">, <"cc", "C">, <"bcc", "B">]
 */
struct FieldInfo {
        const std::string	field;
        const std::string	prefix;
        bool			supports_phrase;
        unsigned		id;
};
using FieldInfoVec = std::vector<FieldInfo>;

struct Parser::Private {
        Private(const Store& store): store_{store} {}

        std::vector<std::string> process_regex (const std::string& field,
                                                const std::regex& rx) const;

        Mu::Tree term_1 (Mu::Tokens& tokens,  WarningVec& warnings) const;
        Mu::Tree term_2 (Mu::Tokens& tokens, Node::Type& op, WarningVec& warnings) const;
        Mu::Tree factor_1 (Mu::Tokens& tokens, WarningVec& warnings) const;
        Mu::Tree factor_2 (Mu::Tokens& tokens, Node::Type& op, WarningVec& warnings) const;
        Mu::Tree unit (Mu::Tokens& tokens, WarningVec& warnings) const;
        Mu::Tree data (Mu::Tokens& tokens, WarningVec& warnings) const;
        Mu::Tree range (const FieldInfoVec& fields, const std::string& lower,
                        const std::string& upper, size_t pos, WarningVec& warnings) const;
        Mu::Tree regex (const FieldInfoVec& fields, const std::string& v,
                        size_t pos,  WarningVec& warnings) const;
        Mu::Tree value (const FieldInfoVec& fields, const std::string& v,
                        size_t pos, WarningVec& warnings) const;
private:
        const Store& store_;
};

static MuMsgFieldId
field_id (const std::string& field)
{

        if (field.empty())
                return MU_MSG_FIELD_ID_NONE;

        MuMsgFieldId id = mu_msg_field_id_from_name (field.c_str(), FALSE);
        if (id != MU_MSG_FIELD_ID_NONE)
                return id;
        else if (field.length() == 1)
                return mu_msg_field_id_from_shortcut (field[0], FALSE);
        else
                return MU_MSG_FIELD_ID_NONE;
}

static std::string
process_value (const std::string& field, const std::string& value)
{
        const auto id = field_id (field);
        if (id == MU_MSG_FIELD_ID_NONE)
                return value;
        switch(id) {
        case MU_MSG_FIELD_ID_PRIO: {
                if (!value.empty())
                        return std::string(1, value[0]);
        } break;

        case MU_MSG_FIELD_ID_FLAGS: {
                const auto flag = mu_flag_char_from_name (value.c_str());
                if (flag)
                        return std::string(1, tolower(flag));
        } break;

        default:
                break;
        }

        return value; // XXX prio/flags, etc. alias
}

static void
add_field (std::vector<FieldInfo>& fields, MuMsgFieldId id)
{
        const auto shortcut = mu_msg_field_shortcut(id);
        if (!shortcut)
                return; // can't be searched

        const auto name = mu_msg_field_name (id);
        const auto pfx  = mu_msg_field_xapian_prefix (id);

        if (!name || !pfx)
                return;

        fields.push_back ({{name}, {pfx},
                        (bool)mu_msg_field_xapian_index(id),
                        id});
}

static std::vector<FieldInfo>
process_field (const std::string& field)
{

        std::vector<FieldInfo> fields;

        if (field == "contact" || field == "recip") { // multi fields
                add_field (fields, MU_MSG_FIELD_ID_TO);
                add_field (fields, MU_MSG_FIELD_ID_CC);
                add_field (fields, MU_MSG_FIELD_ID_BCC);
                if (field == "contact")
                        add_field (fields, MU_MSG_FIELD_ID_FROM);
        } else if (field == "") {
                add_field (fields, MU_MSG_FIELD_ID_TO);
                add_field (fields, MU_MSG_FIELD_ID_CC);
                add_field (fields, MU_MSG_FIELD_ID_BCC);
                add_field (fields, MU_MSG_FIELD_ID_FROM);
                add_field (fields, MU_MSG_FIELD_ID_SUBJECT);
                add_field (fields, MU_MSG_FIELD_ID_BODY_TEXT);
        } else {
                const auto id = field_id (field.c_str());
                if (id != MU_MSG_FIELD_ID_NONE)
                        add_field (fields, id);
        }

        return fields;
}

static bool
is_range_field (const std::string& field)
{
        const auto id = field_id (field.c_str());
        if (id == MU_MSG_FIELD_ID_NONE)
                return false;
        else
                return mu_msg_field_is_range_field (id);
}

struct MyRange {
        std::string lower;
        std::string upper;
};

static MyRange
process_range (const std::string& field, const std::string& lower,
               const std::string& upper)
{
        const auto id = field_id (field.c_str());
        if (id == MU_MSG_FIELD_ID_NONE)
                return { lower, upper };

        std::string	l2 = lower;
        std::string	u2 = upper;

        if (id == MU_MSG_FIELD_ID_DATE) {
                l2 = Mu::date_to_time_t_string (lower, true);
                u2 = Mu::date_to_time_t_string (upper, false);
        } else if (id == MU_MSG_FIELD_ID_SIZE) {
                l2 = Mu::size_to_string (lower, true);
                u2 = Mu::size_to_string (upper, false);
        }

        return { l2, u2 };
}

std::vector<std::string>
Parser::Private::process_regex (const std::string& field, const std::regex& rx) const
{
        const auto id = field_id (field.c_str());
        if (id == MU_MSG_FIELD_ID_NONE)
                return {};

        char pfx[] = {  mu_msg_field_xapian_prefix(id), '\0' };

        std::vector<std::string> terms;
        store_.for_each_term(pfx,[&](auto&& str){
                if (std::regex_search(str.c_str() + 1, rx)) // avoid copy
                        terms.emplace_back(str);
                return true;
        });

        return terms;
}

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

Mu::Tree
Parser::Private::value (const FieldInfoVec& fields, const std::string& v,
                        size_t pos, WarningVec& warnings) const
{
	auto val = utf8_flatten(v);

	if (fields.empty())
		throw BUG("expected one or more fields");

	if (fields.size() == 1) {
		const auto item = fields.front();
		return Tree({Node::Type::Value,
			     std::make_unique<Value>(
				     item.field, item.prefix, item.id,
				     process_value(item.field, val),
				     item.supports_phrase)});
	}

	// a 'multi-field' such as "recip:"
	Tree tree(Node{Node::Type::OpOr});
	for (const auto& item: fields)
		tree.add_child (Tree({Node::Type::Value,
				      std::make_unique<Value>(
					      item.field, item.prefix, item.id,
					      process_value(item.field, val),
					      item.supports_phrase)}));
	return tree;
}

Mu::Tree
Parser::Private::regex (const FieldInfoVec& fields, const std::string& v,
                        size_t pos,  WarningVec& warnings) const
{
	if (v.length() < 2)
		throw BUG("expected regexp, got '%s'", v.c_str());

	const auto rxstr = utf8_flatten(v.substr(1, v.length()-2));

 	try {
		Tree tree(Node{Node::Type::OpOr});
		const auto rx = std::regex (rxstr);
		for (const auto& field: fields) {
			const auto terms = process_regex (field.field, rx);
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
		return value (fields, v, pos, warnings);
	}
}



Mu::Tree
Parser::Private::range (const FieldInfoVec& fields, const std::string& lower,
                        const std::string& upper, size_t pos, WarningVec& warnings) const
{
	if (fields.empty())
		throw BUG("expected field");

	const auto& field = fields.front();
	if (!is_range_field(field.field))
		return value (fields, lower + ".." + upper, pos, warnings);

	auto prange = process_range (field.field, lower, upper);
	if (prange.lower > prange.upper)
		prange = process_range (field.field, upper, lower);

	return Tree({Node::Type::Range,
                        std::make_unique<Range>(field.field, field.prefix, field.id,
                                                prange.lower, prange.upper)});
}

Mu::Tree
Parser::Private::data (Mu::Tokens& tokens, WarningVec& warnings) const
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

	auto fields = process_field (field);
	if (fields.empty()) {// not valid field...
		warnings.push_back ({token.pos, format ("invalid field '%s'", field.c_str())});
		fields = process_field ("");
		// fallback, treat the whole of foo:bar as a value
		return value (fields, field + ":" + val, token.pos, warnings);
	}

	// does it look like a regexp?
	if (val.length() >=2 )
		if (val[0] == '/' && val[val.length()-1] == '/')
			return regex (fields, val, token.pos, warnings);

	// does it look like a range?
	const auto dotdot = val.find("..");
	if (dotdot != std::string::npos)
		return range(fields, val.substr(0, dotdot), val.substr(dotdot + 2),
			     token.pos, warnings);
	else if (is_range_field(fields.front().field)) {
		// range field without a range - treat as field:val..val
		return range (fields, val, val, token.pos, warnings);
	}

	// if nothing else, it's a value.
	return value (fields, val, token.pos, warnings);
}

Mu::Tree
Parser::Private::unit (Mu::Tokens& tokens, WarningVec& warnings) const
{
	if (tokens.empty()) {
		warnings.push_back ({0, "expected: unit"});
		return empty();
	}

	const auto token = look_ahead (tokens);

	if (token.type == Token::Type::Not) {
		tokens.pop_front();
		Tree tree{{Node::Type::OpNot}};
		tree.add_child(unit (tokens, warnings));
		return tree;
	}

	if (token.type == Token::Type::Open) {
		tokens.pop_front();
		auto tree = term_1 (tokens, warnings);
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

	return data (tokens, warnings);
}

Mu::Tree
Parser::Private::factor_2 (Mu::Tokens& tokens, Node::Type& op,
                           WarningVec& warnings) const
{
	if (tokens.empty())
		return empty();

	const auto token = look_ahead(tokens);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored   "-Wswitch-enum"
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

	return factor_1 (tokens, warnings);
}


Mu::Tree
Parser::Private::factor_1 (Mu::Tokens& tokens, WarningVec& warnings) const
{
	Node::Type op { Node::Type::Invalid };

	auto t  = unit (tokens, warnings);
	auto a2 = factor_2 (tokens, op, warnings);

	if (a2.empty())
		return t;

	Tree tree {{op}};
	tree.add_child(std::move(t));
	tree.add_child(std::move(a2));

	return tree;
}


Mu::Tree
Parser::Private::term_2 (Mu::Tokens& tokens, Node::Type& op, WarningVec& warnings) const
{
	if (tokens.empty())
		return empty();

	const auto token = look_ahead (tokens);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored   "-Wswitch-enum"
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
#pragma GCC diagnostic pop

	tokens.pop_front();

	return term_1 (tokens, warnings);
}

Mu::Tree
Parser::Private::term_1 (Mu::Tokens& tokens,  WarningVec& warnings) const
{
	Node::Type op { Node::Type::Invalid };

	auto t  = factor_1 (tokens, warnings);
	auto o2 = term_2 (tokens, op, warnings);

	if (o2.empty())
		return t;
	else {
		Tree tree {{op}};
		tree.add_child(std::move(t));
		tree.add_child(std::move(o2));
		return tree;
	}
}

Mu::Parser::Parser(const Store& store):
        priv_{std::make_unique<Private>(store)}
{}

Mu::Parser::~Parser() = default;


Mu::Tree
Mu::Parser::parse (const std::string& expr, WarningVec& warnings) const
{
	try {
		auto tokens = tokenize (expr);
                if (tokens.empty())
                        return empty ();
                else
                        return priv_->term_1 (tokens, warnings);

	} catch (const std::runtime_error& ex) {
		std::cerr << ex.what() << std::endl;
		return empty();
	}
}
