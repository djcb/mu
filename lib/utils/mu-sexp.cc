/*
** Copyright (C) 2022-2024 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <atomic>
#include <sstream>
#include <array>

using namespace Mu;

template<typename...T> static Mu::Error
parsing_error(size_t pos, fmt::format_string<T...> frm, T&&... args)
{
	const auto&& msg{fmt::format(frm, std::forward<T>(args)...)};
	if (pos == 0)
		return Mu::Error(Error::Code::Parsing, "{}", msg);
	else
		return Mu::Error(Error::Code::Parsing, "{}: {}", pos, msg);
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

static Result<Sexp> parse(const std::string& expr, size_t& pos);

static Result<Sexp>
parse_list(const std::string& expr, size_t& pos)
{
	if (expr[pos] != '(') // sanity check.
		return Err(parsing_error(pos, "expected: '(' but got '{}", expr[pos]));

	Sexp lst{};

	++pos;
	while (pos < expr.size() && expr[pos] != ')') {
		if (auto&& item = parse(expr, pos); item)
			lst.add(std::move(*item));
		else
			return Err(item.error());
	}

	if (pos >= expr.size())
		return Err(parsing_error(pos, "expected: ')'"));
	else if (expr[pos] != ')')
		return Err(parsing_error(pos, "expected: ')' but got '{}'", expr[pos]));

	++pos;
	return Ok(std::move(lst));
}

static Result<Sexp>
parse_string(const std::string& expr, size_t& pos)
{
	if (expr[pos] != '"') // sanity check.
		return Err(parsing_error(pos, "expected: '\"'' but got '{}", expr[pos]));

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
		return Err(parsing_error(pos, "unterminated string '{}'", str));

	++pos;
	return Ok(Sexp{std::move(str)});
}


static Result<Sexp>
parse_integer(const std::string& expr, size_t& pos)
{
	if (!isdigit(expr[pos]) && expr[pos] != '-') // sanity check.
		return Err(parsing_error(pos, "expected: <digit> but got '{}", expr[pos]));

	std::string num; // negative number?
	if (expr[pos] == '-') {
		num = "-";
		++pos;
	}

	for (; isdigit(expr[pos]); ++pos)
		num += expr[pos];

	return Ok(Sexp{::atoi(num.c_str())});
}

static Result<Sexp>
parse_symbol(const std::string& expr, size_t& pos)
{
	if (!isalpha(expr[pos]) && expr[pos] != ':') // sanity check.
		return Err(parsing_error(pos, "expected: <alpha>|: but got '{}", expr[pos]));

	std::string symb(1, expr[pos]);
	for (++pos; isalnum(expr[pos]) || expr[pos] == '-'; ++pos)
		symb += expr[pos];

	return Ok(Sexp{Sexp::Symbol{symb}});
}

static Result<Sexp>
parse(const std::string& expr, size_t& pos)
{
	pos = skip_whitespace(expr, pos);

	if (pos == expr.size())
		return Err(parsing_error(pos, "expected: character '{}", expr[pos]));

	const auto kar  = expr[pos];
	const auto sexp = std::invoke([&]() -> Result<Sexp> {
		if (kar == '(')
			return parse_list(expr, pos);
		else if (kar == '"')
			return parse_string(expr, pos);
		else if (isdigit(kar) || kar == '-')
			return parse_integer(expr, pos);
		else if (isalpha(kar) || kar == ':')
			return parse_symbol(expr, pos);
		else
			return Err(parsing_error(pos, "unexpected character '{}", kar));
	});

	if (sexp)
		pos = skip_whitespace(expr, pos);

	return sexp;
}

Result<Sexp>
Sexp::parse(const std::string& expr)
{
	size_t pos{};
	auto res = ::parse(expr, pos);
	if (!res)
		return res;
	else if (pos != expr.size())
		return Err(parsing_error(pos, "trailing data starting with '{}'", expr[pos]));
	else
		return res;
}

std::string
Sexp::to_string(Format fopts) const
{
	std::stringstream sstrm;
	const auto splitp{any_of(fopts & Format::SplitList)};
	const auto typeinfop{any_of(fopts & Format::TypeInfo)};

	if (listp()) {
		sstrm << '(';
		bool first{true};
		for(auto&& elm: list()) {
			sstrm << (first ? "" : " ") << elm.to_string(fopts);
			first = false;
		}
		sstrm << ')';
		if (splitp)
			sstrm << '\n';
	} else if (stringp())
		sstrm << quote(string());
	else if (numberp())
		sstrm << number();
	else if (symbolp())
		sstrm << symbol().name;

	if (typeinfop)
		sstrm << '<' << Sexp::type_name(type())  << '>';

	return sstrm.str();
}

// LCOV_EXCL_START

// convert emacs-timestamp (a list) to a unix-tstamp
static uint64_t
unix_tstamp(const Sexp& emacs_tstamp)
{
	if (!emacs_tstamp.listp() || emacs_tstamp.list().size() < 2)
		throw std::runtime_error("unexpected type");

	const auto& lst{emacs_tstamp.list()};
	return (lst.at(0).number() << 16) | lst.at(1).number();
}

std::string
Sexp::to_json_string(Format fopts) const
{
	std::stringstream sstrm;

	switch (type()) {
	case Type::List: {
		// property-lists become JSON objects
		if (plistp()) {
			sstrm << "{";
			auto it{list().begin()};
			bool first{true};
			while (it != list().end()) {
				const auto key{it->symbol().name};
				sstrm << (first ? "" : ",")  << quote(key) << ":";
				++it;
				const auto emacs_tstamp{*it};
				sstrm << emacs_tstamp.to_json_string();
				++it;
				first = false;
				// special-case: tstamp-fields also get a "unix" value,
				// which are easier to work with than the "emacs" timestamps
				if (key == ":date" || key == ":changed")
					sstrm << "," << quote(key + "-unix") << ":"
					      << unix_tstamp(emacs_tstamp);
			}
			sstrm << "}";
			if (any_of(fopts & Format::SplitList))
			sstrm << '\n';
		} else { // other lists become arrays.
			sstrm << '[';
			bool first{true};
			for (auto&& child : list()) {
				sstrm << (first ? "" : ", ") << child.to_json_string();
				first = false;
			}
			sstrm << ']';
			if (any_of(fopts & Format::SplitList))
				sstrm << '\n';
		}
		break;
	}
	case Type::String:
		sstrm << quote(string());
		break;
	case Type::Symbol:
		if (nilp())
			sstrm << "false";
		else if (symbol() == "t")
			sstrm << "true";
		else
			sstrm << quote(symbol().name);
		break;
	case Type::Number:
		sstrm << number();
		break;
	default:
		break;
	}

	return sstrm.str();
}



Sexp&
Sexp::del_prop(const std::string& pname)
{
	if (auto kill_it = find_prop(pname, begin(), end()); kill_it != cend())
		list().erase(kill_it, kill_it + 2);
	return *this;
}


Sexp::const_iterator
Sexp::find_prop(const std::string& s,
		Sexp::const_iterator b, Sexp::const_iterator e)  const
{
	for (auto&& it = b; it != e && it+1 != e; it += 2)
		if (it->symbolp() && it->symbol() == s)
			return it;
	return e;
}

Sexp::iterator
Sexp::find_prop(const std::string& s,
		Sexp::iterator b, Sexp::iterator e)
{
	for (auto&& it = b; it != e && it+1 != e; it += 2)
		if (it->symbolp() && it->symbol() == s)
			return it;
	return e;
}


bool
Sexp::plistp(Sexp::const_iterator b, Sexp::const_iterator e) const
{
	if (b == e)
		return true;
	else if (b + 1 == e)
		return false;
	else
		return b->symbolp() && plistp(b + 2, e);
}


// LCOV_EXCL_STOP

#if BUILD_TESTS

#include "mu-test-utils.hh"

static void
test_list()
{
	{
		Sexp s;
		g_assert_true(s.listp());
		g_assert_true(s.to_string() == "()");
		g_assert_true(Sexp::type_name(s.type()) == "list");
		g_assert_true(s.empty());
	}

	{
		Sexp::List items = {
			Sexp("hello"),
			Sexp(123),
			Sexp::Symbol("world")
		};
		const Sexp s{std::move(items)};
		g_assert_false(s.empty());
		g_assert_cmpuint(s.size(),==,3);
		g_assert_true(s.to_string() == "(\"hello\" 123 world)");


		/* copy */
		Sexp s2 = s;
		g_assert_true(s2.to_string() == "(\"hello\" 123 world)");

		/* move */
		Sexp s3 = std::move(s2);
		g_assert_true(s3.to_string() == "(\"hello\" 123 world)");

		s3.clear();
		g_assert_true(s3.empty());
	}

}

static void
test_string()
{
	{
		Sexp s("hello");
		g_assert_true(s.stringp());
		g_assert_true(s.string()=="hello");
		g_assert_true(s.to_string()=="\"hello\"");
		g_assert_true(Sexp::type_name(s.type()) == "string");
	}

	{
		// Sexp s(std::string_view("hel\"lo"));
		// g_assert_true(s.is_string());
		// g_assert_cmpstr(s.string().c_str(),==,"hel\"lo");
		// g_assert_cmpstr(s.to_string().c_str(),==,"\"hel\\\"lo\"");
	}
}

static void
test_number()
{
	{
		Sexp s(123);
		g_assert_true(s.numberp());
		g_assert_cmpint(s.number(),==,123);
		g_assert_true(s.to_string() == "123");
		g_assert_true(Sexp::type_name(s.type()) == "number");
	}

	{
		Sexp s(true);
		g_assert_true(s.numberp());
		g_assert_cmpint(s.number(),==,1);
		g_assert_true(s.to_string()=="1");
	}
}

static void
test_symbol()
{
	{
		Sexp s{Sexp::Symbol("hello")};
		g_assert_true(s.symbolp());
		g_assert_true(s.symbol()=="hello");
		g_assert_true (s.to_string()=="hello");
		g_assert_true(Sexp::type_name(s.type()) == "symbol");
	}

	{
		Sexp s{"hello"_sym};
		g_assert_true(s.symbolp());
		g_assert_true(s.symbol()=="hello");
		g_assert_true (s.to_string()=="hello");
	}

}

static void
test_multi()
{
	Sexp s{"abc", 123, Sexp::Symbol{"def"}};
	g_assert_true(s.to_string() == "(\"abc\" 123 def)");
}


static void
test_add()
{
	{
		Sexp s{"abc", 123};
		s.add("def"_sym);
		g_assert_true(s.to_string() == "(\"abc\" 123 def)");
	}
}

static void
test_add_multi()
{
	{
		Sexp s{"abc", 123};
		s.add("def"_sym, 456, Sexp{"boo", 2});
		g_assert_true(s.to_string() == "(\"abc\" 123 def 456 (\"boo\" 2))");
	}

		{
		Sexp s{"abc", 123};
		Sexp t{"boo", 2};
		s.add("def"_sym, 456, t);
		g_assert_true(s.to_string() == "(\"abc\" 123 def 456 (\"boo\" 2))");
	}

}

static void
test_plist()
{
	Sexp s;
	s.put_props("hello", "world"_sym, "foo", 123, "bar"_sym, "cuux");
	g_assert_true(s.to_string() == R"((hello world foo 123 bar "cuux"))");

	s.put_props("hello", 12345);
	g_assert_true(s.to_string() == R"((foo 123 bar "cuux" hello 12345))");
}


static void
check_parse(const std::string& expr, const std::string& expected)
{
	auto sexp = Sexp::parse(expr);
	assert_valid_result(sexp);
	assert_equal(to_string(*sexp), expected);
}

static void
test_parser()
{
	check_parse(":foo-123", ":foo-123");
	check_parse("foo", "foo");
	check_parse(R"(12345)", "12345");
	check_parse(R"(-12345)", "-12345");
	check_parse(R"((123 bar "cuux"))", "(123 bar \"cuux\")");

	check_parse(R"("foo\"bar\"cuux")", "\"foo\\\"bar\\\"cuux\"");

	check_parse(R"("foo
bar")",
		    "\"foo\nbar\"");
}

static void
test_parser_fail()
{
	g_assert_false(!!Sexp::parse("\""));
	g_assert_false(!!Sexp::parse("123abc"));
	g_assert_false(!!Sexp::parse("("));
	g_assert_false(!!Sexp::parse(")"));
	g_assert_false(!!Sexp::parse("(hello (boo))))"));

	g_assert_true(Sexp::type_name(static_cast<Sexp::Type>(-1)) == "<error>");
}


int
main(int argc, char* argv[])
try {
	mu_test_init(&argc, &argv);

	g_test_add_func("/sexp/list", test_list);
	g_test_add_func("/sexp/string", test_string);
	g_test_add_func("/sexp/number", test_number);
	g_test_add_func("/sexp/symbol", test_symbol);
	g_test_add_func("/sexp/multi",  test_multi);
	g_test_add_func("/sexp/add",  test_add);
	g_test_add_func("/sexp/add-multi",  test_add_multi);
	g_test_add_func("/sexp/plist",  test_plist);
	g_test_add_func("/sexp/parser", test_parser);
	g_test_add_func("/sexp/parser-fail", test_parser_fail);

	return g_test_run();

} catch (const std::runtime_error& re) {
	mu_printerrln("{}", re.what());
	return 1;
}


#endif /*BUILD_TESTS*/
