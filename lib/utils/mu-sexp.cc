/*
** Copyright (C) 2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
**
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

#include "mu-sexp.hh"
#include "mu-utils.hh"

#include <sstream>
#include <array>

using namespace Mu;

__attribute__((format(printf, 2, 0))) static Mu::Error
parsing_error(size_t pos, const char* frm, ...)
{
	va_list args;
	va_start(args, frm);
	auto msg = vformat(frm, args);
	va_end(args);

	if (pos == 0)
		return Mu::Error(Error::Code::Parsing, "%s", msg.c_str());
	else
		return Mu::Error(Error::Code::Parsing, "%zu: %s", pos, msg.c_str());
}
static size_t
skip_whitespace(const std::string& s, size_t pos)
{
	while (pos != s.size()) {
		if (s[pos] == ' ' || s[pos] == '\t' || s[pos] == '\n')
			++pos;
		else
			break;
	}

	return pos;
}

static Sexp parse(const std::string& expr, size_t& pos);

static Sexp
parse_list(const std::string& expr, size_t& pos)
{
	if (expr[pos] != '(') // sanity check.
		throw parsing_error(pos, "expected: '(' but got '%c", expr[pos]);

	Sexp::List list;

	++pos;
	while (expr[pos] != ')' && pos != expr.size())
		list.add(parse(expr, pos));

	if (expr[pos] != ')')
		throw parsing_error(pos, "expected: ')' but got '%c'", expr[pos]);
	++pos;
	return Sexp::make_list(std::move(list));
}

// parse string
static Sexp
parse_string(const std::string& expr, size_t& pos)
{
	if (expr[pos] != '"') // sanity check.
		throw parsing_error(pos, "expected: '\"'' but got '%c", expr[pos]);

	bool        escape{};
	std::string str;
	for (++pos; pos != expr.size(); ++pos) {
		auto kar = expr[pos];
		if (escape && (kar == '"' || kar == '\\')) {
			str += kar;
			escape = false;
			continue;
		}

		if (kar == '"')
			break;
		else if (kar == '\\')
			escape = true;
		else
			str += kar;
	}

	if (escape || expr[pos] != '"')
		throw parsing_error(pos, "unterminated string '%s'", str.c_str());

	++pos;
	return Sexp::make_string(std::move(str));
}

static Sexp
parse_integer(const std::string& expr, size_t& pos)
{
	if (!isdigit(expr[pos]) && expr[pos] != '-') // sanity check.
		throw parsing_error(pos, "expected: <digit> but got '%c", expr[pos]);

	std::string num; // negative number?
	if (expr[pos] == '-') {
		num = "-";
		++pos;
	}

	for (; isdigit(expr[pos]); ++pos)
		num += expr[pos];

	return Sexp::make_number(::atoi(num.c_str()));
}

static Sexp
parse_symbol(const std::string& expr, size_t& pos)
{
	if (!isalpha(expr[pos]) && expr[pos] != ':') // sanity check.
		throw parsing_error(pos, "expected: <alpha>|: but got '%c", expr[pos]);

	std::string symbol(1, expr[pos]);
	for (++pos; isalnum(expr[pos]) || expr[pos] == '-'; ++pos)
		symbol += expr[pos];

	return Sexp::make_symbol(std::move(symbol));
}

static Sexp
parse(const std::string& expr, size_t& pos)
{
	pos = skip_whitespace(expr, pos);

	if (pos == expr.size())
		throw parsing_error(pos, "expected: character '%c", expr[pos]);

	const auto kar  = expr[pos];
	const auto node = [&]() -> Sexp {
		if (kar == '(')
			return parse_list(expr, pos);
		else if (kar == '"')
			return parse_string(expr, pos);
		else if (isdigit(kar) || kar == '-')
			return parse_integer(expr, pos);
		else if (isalpha(kar) || kar == ':')
			return parse_symbol(expr, pos);
		else
			throw parsing_error(pos, "unexpected character '%c", kar);
	}();

	pos = skip_whitespace(expr, pos);

	return node;
}

Sexp
Sexp::make_parse(const std::string& expr)
{
	size_t pos{};
	auto   node{::parse(expr, pos)};

	if (pos != expr.size())
		throw parsing_error(pos, "trailing data starting with '%c'", expr[pos]);

	return node;
}

std::string
Sexp::to_sexp_string() const
{
	std::stringstream sstrm;

	switch (type()) {
	case Type::List: {
		sstrm << '(';
		bool first{true};
		for (auto&& child : list()) {
			sstrm << (first ? "" : " ") << child.to_sexp_string();
			first = false;
		}
		sstrm << ')';

		if (any_of(formatting_opts & FormattingOptions::SplitList))
			sstrm << '\n';
		break;
	}
	case Type::String:
		sstrm << quote(value());
		break;
	case Type::Raw:
		sstrm << value();
		break;
	case Type::Number:
	case Type::Symbol:
	case Type::Empty:
	default: sstrm << value();
	}

	return sstrm.str();
}

// LCOV_EXCL_START

std::string
Sexp::to_json_string() const
{
	std::stringstream sstrm;

	switch (type()) {
	case Type::List: {
		// property-lists become JSON objects
		if (is_prop_list()) {
			sstrm << "{";
			auto it{list().begin()};
			bool first{true};
			while (it != list().end()) {
				sstrm << (first ? "" : ",") << quote(it->value()) << ":";
				++it;
				sstrm << it->to_json_string();
				++it;
				first = false;
			}
			sstrm << "}";
			if (any_of(formatting_opts & FormattingOptions::SplitList))
			sstrm << '\n';
		} else { // other lists become arrays.
			sstrm << '[';
			bool first{true};
			for (auto&& child : list()) {
				sstrm << (first ? "" : ", ") << child.to_json_string();
				first = false;
			}
			sstrm << ']';
			if (any_of(formatting_opts & FormattingOptions::SplitList))
				sstrm << '\n';
		}
		break;
	}
	case Type::String:
		sstrm << quote(value());
		break;
	case Type::Raw: // FIXME: implement this.
		break;

	case Type::Symbol:
		if (is_nil())
			sstrm << "false";
		else if (is_t())
			sstrm << "true";
		else
			sstrm << quote(value());
		break;
	case Type::Number:
	case Type::Empty:
	default: sstrm << value();
	}

	return sstrm.str();
}

// LCOV_EXCL_STOP
