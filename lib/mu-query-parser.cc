/*
** Copyright (C) 2023-2026 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
	const auto value_syms = std::to_array({
		placeholder_sym, phrase_sym, regex_sym, range_sym, wildcard_sym
	});

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
	bool	expand{};
	size_t	depth{}; /* current parenthesis-nesting depth */
};

/* parsing is best-effort; deeper nesting than this is ignored
 * (this also caps the recursion depth) */
constexpr size_t MaxDepth{100};

/**
 * A cursor over the flat token-list, so popping is O(1)
 */
struct TokenStream {
	explicit TokenStream(Sexp& tokens): toks_{tokens.list()} {}

	bool empty() const { return pos_ >= toks_.size(); }
	Option<Sexp&> head() {
		if (empty())
			return Nothing;
		else
			return toks_[pos_];
	}
	bool head_symbolp(const Sexp::Symbol& sym) const {
		return pos_ < toks_.size() && toks_[pos_].symbolp(sym);
	}
	void pop_front() { ++pos_; }

private:
	Sexp::List&	toks_;
	size_t		pos_{};
};

/**
 * Indexable fields become _phrase_ fields if they contain
 * wordbreakable data;
 *
 * @param field
 * @param val
 *
 * @return an s-expr or nothing
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

static Sexp query(TokenStream& tokens, ParseContext& ctx);


static Sexp
finalize_matcher(Sexp&& val, ParseContext& ctx)
{
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
		if (!fields.empty() && second(val)) {
			Sexp vals{};
			vals.add(or_sym);
			for (auto&& field: fields) {
				if (auto&& phrase{phrasify(field, *second(val))}; phrase)
					vals.add(std::move(*phrase));
				else
					vals.add(Sexp{Sexp::Symbol{field.name},
							Sexp{*second(val)}});
			}
			val = std::move(vals);
		}

	}

	if (auto&& field{field_from_name(fieldsym.name)}; field) {
		if (auto&& v{second(val)}; v)
			if (auto&& phrase{phrasify(*field, *v)}; phrase)
				val = std::move(*phrase);
	}

	return std::move(val);
}

static Sexp
matcher(TokenStream& tokens, ParseContext& ctx)
{
	if (tokens.empty())
		return {};

	auto val{*tokens.head()};
	tokens.pop_front();
	/* special case: if we find some non-matcher type here, we need to second-guess the token */
	if (!looks_like_matcher(val))
		val = Sexp{placeholder_sym, val.symbol().name};

	return finalize_matcher(std::move(val), ctx);
}

static Sexp
unit(TokenStream& tokens, ParseContext& ctx)
{
	if (tokens.head_symbolp(not_sym)) { /* NOT */
		/* handle (chains of) NOTs iteratively; parity decides */
		bool neg{};
		while (tokens.head_symbolp(not_sym)) {
			tokens.pop_front();
			neg = !neg;
		}
		Sexp sub{unit(tokens, ctx)};

		/* special case: interpret a trailing "not" as a matcher instead */
		if (sub.empty()) {
			sub = finalize_matcher(Sexp{placeholder_sym, not_sym.name}, ctx);
			neg = !neg;
		}

		if (!neg)
			return sub;

		/* we try to optimize: double negations are removed */
		if (sub.head_symbolp(not_sym))
			return *second(sub);
		else
			return Sexp(not_sym, std::move(sub));

	} else if (tokens.head_symbolp(open_sym)) { /* ( sub) */
		tokens.pop_front();
		if (ctx.depth >= MaxDepth) /* nested too deeply; bail out */
			return {};
		++ctx.depth;
		Sexp sub{query(tokens, ctx)};
		--ctx.depth;
		if (tokens.head_symbolp(close_sym))
			tokens.pop_front();
		return sub;
	}

	/* matcher */
	return matcher(tokens, ctx);
}


static Sexp
factor(TokenStream& tokens, ParseContext& ctx)
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
query(TokenStream& tokens, ParseContext& ctx)
{
	/* process a left-associative chain of factors, separated by
	 * <OR>/<XOR>. Chains of the same operator are flattened, i.e.
	 * (or (or a b) c) => (or a b c), since Xapian likes flat trees */

	Sexp fact = factor(tokens, ctx);
	while (true) {
		const Sexp::Symbol* opsym{};
		if (tokens.head_symbolp(or_sym))
			opsym = &or_sym;
		else if (tokens.head_symbolp(xor_sym))
			opsym = &xor_sym;
		else
			break;

		tokens.pop_front();
		Sexp rhs = factor(tokens, ctx);
		if (rhs.empty())
			break; /* trailing op; ignore */

		if (!fact.head_symbolp(*opsym))
			fact = Sexp{*opsym, std::move(fact)};
		fact.add(std::move(rhs));
	}

	return fact;
}

Sexp
Mu::parse_query(const std::string& expr, bool expand)
{
	ParseContext context;
	context.expand = expand;

	auto items = process_query(expr);
	if (!items.listp())
		throw std::runtime_error("tokens must be a list-sexp");

	TokenStream tokens{items};
	return query(tokens, context);
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
		// or-chains are flattened
		TestCase{R"(a or b or c)", R"((or (_ "a") (_ "b") (_ "c")))"},
		// a or b and c
		TestCase{R"(a or b and c)", R"((or (_ "a") (and (_ "b") (_ "c"))))"},
		// a and b or c
		TestCase{R"(a and b or c)", R"((or (and (_ "a") (_ "b")) (_ "c")))"},
		// mixed or/xor associate to the left
		TestCase{R"(a or b xor c)", R"((xor (or (_ "a") (_ "b")) (_ "c")))"},
		TestCase{R"(a xor b or c)", R"((or (xor (_ "a") (_ "b")) (_ "c")))"},
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
		// trailing operator is dropped
		TestCase{R"(a or)", R"((_ "a"))"},
		// quoted operators are matchers, not operators
		TestCase{R"(foo "and" bar)", R"((and (_ "foo") (_ "and") (_ "bar")))"},
	};

	for (auto&& test: cases) {
		auto&& sexp{parse_query(test.first)};
		assert_equal(sexp.to_string(), test.second);
	}
}

static void
test_parser_pathological()
{
	// pathological queries parse (possibly partially) without
	// crashes or quadratic slow-down.

	std::string parens(10000, '(');
	parens += "a";
	parens.append(10000, ')');
	g_assert_true(parse_query(parens).listp());

	std::string nots;
	for (auto i = 0; i != 10000; ++i)
		nots += "not ";
	nots += "a"; // even number of nots
	assert_equal(parse_query(nots).to_string(), R"((_ "a"))");
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
	g_test_add_func("/query-parser/pathological", test_parser_pathological);
	g_test_add_func("/query-parser/fields", test_parser_fields);
	g_test_add_func("/query-parser/range", test_parser_range);
	g_test_add_func("/query-parser/expand", test_parser_expand);
	g_test_add_func("/query-parser/optimize", test_parser_optimize);

	return g_test_run();
}

#endif /*BUILD_TESTS*/
