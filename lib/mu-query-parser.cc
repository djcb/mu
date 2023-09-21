/*
** Copyright (C) 2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-query-parser.hh"

#include <string_view>
#include <variant>
#include <type_traits>
#include <iostream>

#include "utils/mu-utils.hh"
#include "utils/mu-sexp.hh"
#include "utils/mu-option.hh"
#include <glib.h>
#include "utils/mu-utils-file.hh"

using namespace Mu;

// Sexp extensions...
static Sexp&
prepend(Sexp& s, Sexp&& e)
{
	s.list().insert(s.list().begin(), std::move(e));
	return s;
}

static Option<Sexp&>
second(Sexp& s)
{
	if (s.listp() && !s.empty() && s.cbegin() + 1 != s.cend())
		return *(s.begin()+1);
	else
		return Nothing;
}


static bool
looks_like_matcher(const Sexp& sexp)
{
	// all the "terminal values" (from the Mu parser's pov)
	const std::array<Sexp::Symbol, 5> value_syms = {
		placeholder_sym, phrase_sym, regex_sym, range_sym, wildcard_sym
	};

	if (!sexp.listp() || sexp.empty() || !sexp.front().symbolp())
		return false;

	const auto symbol{sexp.front().symbol()};
	if (seq_some(value_syms, [&](auto &&sym) { return symbol == sym; }))
		return true;
	else if (!!field_from_name(symbol.name) || field_is_combi(symbol.name))
		 return true;
	else
		return false;
}

struct ParseContext {
	bool				expand;
	std::vector<std::string>	warnings;
};




/**
 * Indexable fields become _phrase_ fields if they contain
 * wordbreakable data;
 *
 * @param field
 * @param val
 *
 * @return
 */
static Option<Sexp>
phrasify(const Field& field, const Sexp& val)
{
	if (!field.is_phrasable_term() || !val.stringp())
		return Nothing; // nothing to phrasify

	auto words{utf8_wordbreak(val.string())};
	if (words.find(' ') == std::string::npos)
		return Nothing; // nothing to phrasify

	auto phrase = Sexp {
		Sexp::Symbol{field.name},
		Sexp{phrase_sym, Sexp{std::move(words)}}};

	// if the field both a normal term & phrasable, match both
	// if they are different
	if (val.string() != words)
		return Sexp{or_sym,
			Sexp {Sexp::Symbol{field.name}, Sexp(val.string())},
			std::move(phrase)};
	else
		return phrase;
}


/*
 * Grammar
 *
 * query   -> factor { (<OR> | <XOR>)  factor }
 * factor  -> unit { [<AND>]  unit }
 * unit    -> matcher | <NOT> query | <(> query  <)>
 * matcher
 */

static Sexp query(Sexp& tokens, ParseContext& ctx);


static Sexp
matcher(Sexp& tokens, ParseContext& ctx)
{
	if (tokens.empty())
		return {};

	auto val{*tokens.head()};
	tokens.pop_front();
	/* special case: if we find some non-matcher type here, we need to second-guess the token */
	if (!looks_like_matcher(val))
		val = Sexp{placeholder_sym, val.symbol().name};

	const auto fieldsym{val.front().symbol()};

	// Note the _expand_ case is what we use when processing the query 'for real';
	// the non-expand case is only to have a bit more human-readable Sexp for use
	// mu find's '--analyze'
	//
	// Re: phrase-fields We map something like 'subject:hello-world'
	// to
	//    (or (subject "hello-world" (subject (phrase "hello world"))))

	if (ctx.expand) { /* should we expand meta-fields? */
		auto fields = fields_from_name(fieldsym == placeholder_sym ? "" : fieldsym.name);
		if (!fields.empty()) {
			Sexp vals{};
			vals.add(or_sym);
			for (auto&& field: fields)
				if (auto&& phrase{phrasify(field, *second(val))}; phrase)
					vals.add(std::move(*phrase));
				else
					vals.add(Sexp{Sexp::Symbol{field.name}, Sexp{*second(val)}});
			val = std::move(vals);
		}

	}

	if (auto&& field{field_from_name(fieldsym.name)}; field) {
		if (auto&& phrase(phrasify(*field, *second(val))); phrase)
			val = std::move(*phrase);
	}

	return val;
}

static Sexp
unit(Sexp& tokens, ParseContext& ctx)
{
	if (tokens.head_symbolp(not_sym)) { /* NOT */
		tokens.pop_front();
		Sexp sub{unit(tokens, ctx)};

		/* special case: interpret "not" as a matcher instead; */
		if (sub.empty())
			return matcher(prepend(tokens, Sexp{placeholder_sym, not_sym.name}), ctx);

		/* we try to optimize: double negations are removed */
		if (sub.head_symbolp(not_sym))
			return *second(sub);
		else
			return Sexp(not_sym, std::move(sub));

	} else if (tokens.head_symbolp(open_sym)) { /* ( sub) */
		tokens.pop_front();
		Sexp sub{query(tokens, ctx)};
		if (tokens.head_symbolp(close_sym))
			tokens.pop_front();
		else {
			//g_warning("expected <)>");
		}
		return sub;
	}

	/* matcher */
	return matcher(tokens, ctx);
}


static Sexp
factor(Sexp& tokens, ParseContext& ctx)
{
	Sexp un = unit(tokens, ctx);

	/* query 'a b' is to be interpreted as 'a AND b';
	 *
	 * we need an implicit AND if the head symbol is either
	 * a matcher (value) or the start of a sub-expression */
	auto implicit_and = [&]() {
		if (tokens.head_symbolp(open_sym))
			return true;
		else if (tokens.head_symbolp(not_sym)) // turn a lone 'not' -> 'and not'
			return true;
		else if (auto&& head{tokens.head()}; head)
			return looks_like_matcher(*head);
		else
			return false;
	};

	Sexp uns;
	while (true) {
		if (tokens.head_symbolp(and_sym))
			tokens.pop_front();
		else if (!implicit_and())
			break;

		if (auto&& un2 = unit(tokens, ctx); !un2.empty())
			uns.add(std::move(un2));
		else
			break;
	}

	if (!uns.empty()) {
		un = Sexp{and_sym, std::move(un)};
		un.add_list(std::move(uns));
	}

	return un;
}

static Sexp
query(Sexp& tokens, ParseContext& ctx)
{
	/* note: we flatten (or (or ( or ...)) etc. here;
	 * for optimization (since Xapian likes flat trees) */

	Sexp fact = factor(tokens, ctx);
	Sexp or_factors, xor_factors;
	while (true) {
		auto factors = std::invoke([&]()->Option<Sexp&> {

				if (tokens.head_symbolp(or_sym))
					return or_factors;
				else if (tokens.head_symbolp(xor_sym))
					return xor_factors;
				else
					return Nothing;
			});

		if (!factors)
			break;

		tokens.pop_front();
		factors->add(factor(tokens, ctx));
	}

	// a bit clumsy...

	if (!or_factors.empty() && xor_factors.empty()) {
		fact = Sexp{or_sym, std::move(fact)};
		fact.add_list(std::move(or_factors));
	} else if (or_factors.empty() && !xor_factors.empty()) {
		fact = Sexp{xor_sym, std::move(fact)};
		fact.add_list(std::move(xor_factors));
	} else if (!or_factors.empty() && !xor_factors.empty()) {
		fact = Sexp{or_sym, std::move(fact)};
		fact.add_list(std::move(or_factors));
		prepend(xor_factors, xor_sym);
		fact.add(std::move(xor_factors));
	}

	return fact;
}

Sexp
Mu::parse_query(const std::string& expr, bool expand)
{
	ParseContext context;
	context.expand = expand;

	if (auto&& items = process_query(expr); !items.listp())
		throw std::runtime_error("tokens must be a list-sexp");
	else
		return query(items, context);
}


#if defined(BUILD_PARSE_QUERY)||defined(BUILD_PARSE_QUERY_EXPAND)
int
main (int argc, char *argv[])
{
	if (argc < 2) {
		mu_printerrln("expected: {} <query>", argv[0]);
		return 1;
	}

	std::string expr;
	for (auto i = 1; i < argc; ++i) {
		expr += argv[i];
		expr += " ";
	}

	auto&& sexp = parse_query(expr,
#ifdef BUILD_PARSE_QUERY_EXPAND
				       true/*expand*/
#else
				       false/*don't expand*/
#endif
		);
	mu_println("{}", sexp.to_string());
	return 0;
}
#endif // BUILD_PARSE_QUERY || BUILD_PARSE_QUERY_EXPAND



#if BUILD_TESTS
/*
 * Tests.
 *
 */

#include "utils/mu-test-utils.hh"

using TestCase = std::pair<std::string, std::string>;

static void
test_parser_basic()
{
	std::vector<TestCase> cases = {
		// single term
		TestCase{R"(a)", R"((_ "a"))"},
		// a and b
		TestCase{R"(a and b)", R"((and (_ "a") (_ "b")))"},
		// a and b and c
		TestCase{R"(a and b and c)", R"((and (_ "a") (_ "b") (_ "c")))"},
		// a or b
		TestCase{R"(a or b)", R"((or (_ "a") (_ "b")))"},
		// a or b and c
		TestCase{R"(a or b and c)", R"((or (_ "a") (and (_ "b") (_ "c"))))"},
		// a and b or c
		TestCase{R"(a and b or c)", R"((or (and (_ "a") (_ "b")) (_ "c")))"},
		// not a
		TestCase{R"(not a)", R"((not (_ "a")))"},
		// lone not
		TestCase{R"(not)", R"((_ "not"))"},
		// a and (b or c)
		TestCase{R"(a and (b or c))", R"((and (_ "a") (or (_ "b") (_ "c"))))"},
		// not a and not b
		TestCase{R"(not a and b)", R"((and (not (_ "a")) (_ "b")))"},
		// a not b
		TestCase{R"(a not b)", R"((and (_ "a") (not (_ "b"))))"},
	};

	for (auto&& test: cases) {
		auto&& sexp{parse_query(test.first)};
		//mu_message ("'{}' <=> '{}'", sexp.to_string(), test.second);
		assert_equal(sexp.to_string(), test.second);
	}
}

static void
test_parser_recover()
{
	std::vector<TestCase> cases = {
		// implicit AND
		TestCase{R"(a b)", R"((and (_ "a") (_ "b")))"},
		// a or or (second to be used as value)
		TestCase{R"(a or and)", R"((or (_ "a") (_ "and")))"},
		// missing end )
		TestCase{R"(a and ()", R"((_ "a"))"},
		// missing end )
		TestCase{R"(a and (b)", R"((and (_ "a") (_ "b")))"},
	};

	for (auto&& test: cases) {
		auto&& sexp{parse_query(test.first)};
		assert_equal(sexp.to_string(), test.second);
	}
}


static void
test_parser_fields()
{
	std::vector<TestCase> cases = {
		// simple field
		TestCase{R"(s:hello)", R"((subject "hello"))"},
		// field, wildcard, regexp
		TestCase{R"(subject:a* recip:/b/)",
			R"((and (subject (wildcard "a")) (recip (regex "b"))))"},
		TestCase{R"(from:hello or subject:world)",
			R"((or (from "hello") (subject "world")))"},
	};

	for (auto&& test: cases) {
		auto&& sexp{parse_query(test.first)};
		assert_equal(sexp.to_string(), test.second);
	}
}

static void
test_parser_expand()
{
	std::vector<TestCase> cases = {
		// simple field
		TestCase{R"(recip:a)", R"((or (to "a") (cc "a") (bcc "a")))"},
		// field, wildcard, regexp
		TestCase{R"(a*)",
			R"((or (to (wildcard "a")) (cc (wildcard "a")) (bcc (wildcard "a")) (from (wildcard "a")) (subject (wildcard "a")) (body (wildcard "a")) (embed (wildcard "a"))))"},
		TestCase{R"(a xor contact:b)",
			R"((xor (or (to "a") (cc "a") (bcc "a") (from "a") (subject "a") (body "a") (embed "a")) (or (to "b") (cc "b") (bcc "b") (from "b"))))"}
	};

	for (auto&& test: cases) {
		auto&& sexp{parse_query(test.first, true/*expand*/)};
		assert_equal(sexp.to_string(), test.second);
	}
}


static void
test_parser_range()
{
	std::vector<TestCase> cases = {
		TestCase{R"(size:1)", R"((size (range "1" "1")))"},
		TestCase{R"(size:2..)", R"((size (range "2" "")))"},
		TestCase{R"(size:..1k)", R"((size (range "" "1024")))"},
		TestCase{R"(size:..)", R"((size (range "" "")))"},
	};

	for (auto&& test: cases) {
		auto&& sexp{parse_query(test.first, true/*expand*/)};
		assert_equal(sexp.to_string(), test.second);
	}
}

static void
test_parser_optimize()
{
	std::vector<TestCase> cases = {
		TestCase{R"(not a)", R"((not (_ "a")))"},
		TestCase{R"(not not a)", R"((_ "a"))"},
		TestCase{R"(not not not a)", R"((not (_ "a")))"},
		TestCase{R"(not not not not a)", R"((_ "a"))"},
	};


	for (auto&& test: cases) {
		auto&& sexp{parse_query(test.first)};
		assert_equal(sexp.to_string(), test.second);
	}
}

int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	g_test_add_func("/query-parser/basic", test_parser_basic);
	g_test_add_func("/query-parser/recover", test_parser_recover);
	g_test_add_func("/query-parser/fields", test_parser_fields);
	g_test_add_func("/query-parser/range", test_parser_range);
	g_test_add_func("/query-parser/expand", test_parser_expand);
	g_test_add_func("/query-parser/optimize", test_parser_optimize);

	return g_test_run();
}

#endif /*BUILD_TESTS*/
