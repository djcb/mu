/*
** Copyright (C) 2020 djcb <djcb@evergrey>
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


#include "mu-sexp-parser.hh"
#include "mu-utils.hh"

using namespace Mu;
using namespace Sexp;

__attribute__((format(printf, 2, 0))) static Mu::Error
parsing_error(size_t pos, const char* frm, ...)
{
        va_list args;
        va_start(args, frm);
        auto msg = format(frm, args);
        va_end(args);

        if (pos == 0)
                return Mu::Error(Error::Code::Parsing, "%s", msg);
        else
                return Mu::Error(Error::Code::Parsing, "%zu: %s", msg.c_str());
}
static size_t
skip_whitespace (const std::string& s, size_t pos)
{
        while (pos != s.size()) {
                if (s[pos] == ' ' || s[pos] == '\t' || s[pos] == '\n')
                        ++pos;
                else
                        break;
        }

        return pos;
}

static Node parse (const std::string& expr, size_t& pos);

static Node
parse_list (const std::string& expr, size_t& pos)
{
        if (expr[pos] != '(') // sanity check.
                throw parsing_error(pos, "expected: '(' but got '%c", expr[pos]);

        std::vector<Node> children;

        ++pos;
        while (expr[pos] != ')' && pos != expr.size())
                children.emplace_back(parse(expr, pos));

        if (expr[pos] != ')')
                throw parsing_error(pos, "expected: ')' but got '%c'", expr[pos]);
        ++pos;
        return Node{std::move(children)};
}

// parse string
static Node
parse_string (const std::string& expr, size_t& pos)
{
        if (expr[pos] != '"') // sanity check.
                throw parsing_error(pos, "expected: '\"'' but got '%c", expr[pos]);

        bool escape{};
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
        return Node{Type::String, std::move(str)};
}

static Node
parse_integer (const std::string& expr, size_t& pos)
{
        if (!isdigit(expr[pos])) // sanity check.
                throw parsing_error(pos, "expected: <digit> but got '%c", expr[pos]);

        std::string num;
        for (; isdigit(expr[pos]); ++pos)
                num += expr[pos];

        return  Node {Type::Integer, std::move(num)};
}

static Node
parse_symbol (const std::string& expr, size_t& pos)
{
        if (!isalpha(expr[pos]) && expr[pos] != ':') // sanity check.
                throw parsing_error(pos, "expected: <alpha>|: but got '%c", expr[pos]);

        std::string symbol(1, expr[pos]);
        for  (++pos; isalnum(expr[pos]) || expr[pos] == '-'; ++pos)
                symbol += expr[pos];

        return  Node { Type::Symbol, std::move(symbol)};
}


static Node
parse (const std::string& expr, size_t& pos)
{
        pos = skip_whitespace(expr, pos);

        if  (pos == expr.size())
                throw parsing_error(pos, "expected: character '%c", expr[pos]);

        const auto kar = expr[pos];
        const auto node =[&]() -> Node {
                if (kar == '(')
                        return parse_list (expr, pos);
                else if (kar == '"')
                        return parse_string(expr, pos);
                else if (isdigit(kar))
                        return parse_integer(expr, pos);
                else if (isalpha(kar) ||  kar == ':')
                        return parse_symbol(expr, pos);
                else
                        throw parsing_error(pos, "unexpected character '%c" + kar);
        }();

        pos = skip_whitespace(expr, pos);

        return node;
}

Node
Sexp::parse (const std::string& expr)
{
        size_t pos{};
        auto node{::parse (expr, pos)};

        if (pos != expr.size())
                throw parsing_error(pos, "trailing data starting with '%c'", expr[pos]);

        return node;
}
