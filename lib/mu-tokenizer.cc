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

#include "mu-tokenizer.hh"
#include "utils/mu-utils.hh"

#include <cctype>
#include <iostream>
#include <algorithm>

using namespace Mu;

static bool
is_separator(char c)
{
	if (isblank(c))
		return true;

	const auto seps = std::string("()");
	return seps.find(c) != std::string::npos;
}

static Mu::Token
op_or_value(size_t pos, const std::string& val)
{
	auto s = val;
	std::transform(s.begin(), s.end(), s.begin(), ::tolower);

	if (s == "and")
		return Token{pos, Token::Type::And, val};
	else if (s == "or")
		return Token{pos, Token::Type::Or, val};
	else if (s == "xor")
		return Token{pos, Token::Type::Xor, val};
	else if (s == "not")
		return Token{pos, Token::Type::Not, val};
	else
		return Token{pos, Token::Type::Data, val};
}

static void
unread_char(std::string& food, char kar, size_t& pos)
{
	food = kar + food;
	--pos;
}

static Mu::Token
eat_token(std::string& food, size_t& pos)
{
	bool        quoted{};
	bool        escaped{};
	std::string value{};

	while (!food.empty()) {
		const auto kar = food[0];
		food.erase(0, 1);
		++pos;

		if (kar == '\\') {
			escaped = !escaped;
			if (escaped)
				continue;
		}

		if (kar == '"') {
			if (!escaped && quoted)
				return Token{pos, Token::Type::Data, value};
			else {
				quoted = true;
				continue;
			}
		}

		if (!quoted && !escaped && is_separator(kar)) {
			if (!value.empty() && kar != ':') {
				unread_char(food, kar, pos);
				return op_or_value(pos, value);
			}

			if (quoted || isblank(kar))
				continue;

			switch (kar) {
			case '(': return {pos, Token::Type::Open, "("};
			case ')': return {pos, Token::Type::Close, ")"};
			default: break;
			}
		}

		value += kar;
		escaped = false;
	}

	return {pos, Token::Type::Data, value};
}

Mu::Tokens
Mu::tokenize(const std::string& s)
{
	Tokens tokens{};

	std::string food = utf8_clean(s);
	size_t      pos{0};

	if (s.empty())
		return {};

	while (!food.empty())
		tokens.emplace_back(eat_token(food, pos));

	return tokens;
}
