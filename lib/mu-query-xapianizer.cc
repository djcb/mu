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

#include "config.h"
#include "mu-query-parser.hh"

#include <string_view>
#include <variant>
#include <array>
#include <type_traits>

#include "utils/mu-option.hh"
#include <glib.h>
#include "utils/mu-utils-file.hh"

using namespace Mu;

// backward compat
#ifndef HAVE_XAPIAN_FLAG_NGRAMS
#define FLAG_NGRAMS FLAG_CJK_NGRAM
#endif /*HAVE_XAPIAN_FLAG_NGRAMS*/

/**
 * Expand terms for scripts without explicit word-breaks (e.g.
 * Chinese/Japanese/Korean) in the way that Xapian expects it -
 * use Xapian's built-in QueryParser just for that.
 */
static Result<Xapian::Query>
ngram_expand(const Field& field, const std::string& str)
{
	Xapian::QueryParser qp;
	const auto pfx{std::string(1U, field.xapian_prefix())};

	qp.set_default_op(Xapian::Query::OP_OR);

	return qp.parse_query(str, Xapian::QueryParser::FLAG_NGRAMS, pfx);
}


static Option<Sexp>
tail(Sexp&& s)
{
	if (!s.listp() || s.empty())
		return Nothing;

	s.list().erase(s.list().begin(), s.list().begin() + 1);

	return s;
}

Option<std::string>
head_symbol(const Sexp& s)
{
	if (!s.listp() || s.empty() || !s.head() || !s.head()->symbolp())
		return Nothing;

	return s.head()->symbol().name;
}


Option<std::string>
string_nth(const Sexp& args, size_t n)
{
	if (!args.listp() || args.size() < n + 1)
		return Nothing;

	if (auto&& item{args.list().at(n)}; !item.stringp())
		return Nothing;
	else
		return item.string();
}

static Result<Xapian::Query>
phrase(const Field& field, Sexp&& s)
{
	if (!field.is_phrasable_term())
		return Err(Error::Code::InvalidArgument,
			   "field {} does not support phrases", field.name);

	if (s.size() == 1 && s.front().stringp()) {
		auto&& words{split(s.front().string(), " ")};
		std::vector<Xapian::Query> phvec;
		phvec.reserve(words.size());
		for(auto&& w: words)
			phvec.emplace_back(Xapian::Query{field.xapian_term(std::move(w))});
		return Xapian::Query{Xapian::Query::OP_PHRASE,
			phvec.begin(), phvec.end()};
	} else
		return Err(Error::Code::InvalidArgument,
			   "invalid phrase for field {}: '{}'", field.name, s.to_string());
}

static Result<Xapian::Query>
regex(const Store& store, const Field& field, const std::string& rx_str)
{
	auto&& str{utf8_flatten(rx_str)};
	auto&& rx{Regex::make(str, G_REGEX_OPTIMIZE)};
	if (!rx) {
		mu_warning("invalid regexp: '{}': {}", str, rx.error().what());
		return Xapian::Query::MatchNothing;
	}

	std::vector<Xapian::Query> rxvec;
	store.for_each_term(field.id, [&](auto&& str) {
		if (auto&& val{str.data() + 1}; rx->matches(val))
			rxvec.emplace_back(field.xapian_term(std::string_view{val}));
		return true;
	});

	return Xapian::Query(Xapian::Query::OP_OR, rxvec.begin(), rxvec.end());
}



static Result<Xapian::Query>
range(const Field& field, Sexp&& s)
{
	auto&& r0{string_nth(s, 0)};
	auto&& r1{string_nth(s, 1)};
	if (!r0 || !r1)
		return Err(Error::Code::InvalidArgument, "expected 2 range values");

	// in the sexp, we use iso date/time for human readability; now convert to
	// time_t
	auto iso_to_lexnum=[](const std::string& s)->Option<std::string> {
		if (s.empty())
			return s;
		if (auto&& t{parse_date_time(s, true, true/*utc*/)}; !t)
			return Nothing;
		else
			return to_lexnum(*t);
	};

	if (field == Field::Id::Date || field == Field::Id::Changed) {
		// iso -> time_t
		r0 = iso_to_lexnum(*r0);
		r1 = iso_to_lexnum(*r1);
	} else if (field == Field::Id::Size) {
		if (!r0->empty())
			r0 = to_lexnum(::atoll(r0->c_str()));
		if (!r1->empty())
			r1 = to_lexnum(::atoll(r1->c_str()));
	} else
		return Err(Error::Code::InvalidArgument,
			   "unsupported range field {}", field.name);

	if (r0->empty() && r1->empty())
		return Xapian::Query::MatchNothing; // empty range matches nothing.
	else if (r0->empty() && !r1->empty())
		return Xapian::Query(Xapian::Query::OP_VALUE_LE,
				     field.value_no(), *r1);
	else if (!r0->empty() && r1->empty())
		return Xapian::Query(Xapian::Query::OP_VALUE_GE,
				     field.value_no(), *r0);
	else
		return Xapian::Query(Xapian::Query::OP_VALUE_RANGE,
				     field.value_no(), *r0, *r1);
}



using OpPair = std::pair<const std::string_view, Xapian::Query::op>;
static constexpr std::array<OpPair, 4> LogOpPairs = {{
		{ "and", Xapian::Query::OP_AND },
		{ "or", Xapian::Query::OP_OR },
		{ "xor", Xapian::Query::OP_XOR },
		{ "not", Xapian::Query::OP_AND_NOT }
	}};

static Option<Xapian::Query::op>
find_log_op(const std::string& opname)
{
	for (auto&& p: LogOpPairs)
		if (p.first == opname)
			return p.second;

	return Nothing;
}

static Result<Xapian::Query> parse(const Store& store, Sexp&& s, Mu::ParserFlags flags);

static Result<Xapian::Query>
parse_logop(const Store& store, Xapian::Query::op op, Sexp&& args, Mu::ParserFlags flags)
{
	if (!args.listp() || args.empty())
		return Err(Error::Code::InvalidArgument,
			   "expected non-empty list but got", args.to_string());

	std::vector<Xapian::Query> qs;
	for (auto&& elm: args.list()) {
		if (auto&& q{parse(store, std::move(elm), flags)}; !q)
			return Err(std::move(q.error()));
		else
			qs.emplace_back(std::move(*q));
	}

	switch(op) {
	case Xapian::Query::OP_AND_NOT:
		// TODO: optimize AND_NOT
		if (qs.size() != 1)
			return Err(Error::Code::InvalidArgument,
				   "expected single argument for NOT");
		else
			return Xapian::Query{op, Xapian::Query::MatchAll, qs.at(0)};

	case Xapian::Query::OP_AND:
	case Xapian::Query::OP_OR:
	case Xapian::Query::OP_XOR:
		return Xapian::Query(op, qs.begin(), qs.end());

	default:
		return Err(Error::Code::InvalidArgument, "unexpected xapian op");
	}
}


static Result<Xapian::Query>
parse_field_matcher(const Store& store, const Field& field,
		    const std::string& match_sym, Sexp&& args)
{
	auto&& str0{string_nth(args, 0)};

	if (match_sym == wildcard_sym.name && str0)
		return Xapian::Query{Xapian::Query::OP_WILDCARD,
			field.xapian_term(*str0)};
	else if (match_sym == range_sym.name && !!str0)
		return range(field, std::move(args));
	else if (match_sym == regex_sym.name && !!str0)
		return regex(store, field, *str0);
	else if (match_sym == phrase_sym.name)
			return phrase(field, std::move(args));

	return Err(Error::Code::InvalidArgument,
		   "invalid field '{}'/'{}' matcher: {}",
		   field.name, match_sym, args.to_string());
}

static Result<Xapian::Query>
parse_basic(const Field &field, Sexp &&vals, Mu::ParserFlags flags)
{
	auto ngrams = any_of(flags & ParserFlags::SupportNgrams);
	if (!vals.stringp())
		return Err(Error::Code::InvalidArgument, "expected string");

	auto&& val{vals.string()};

	switch (field.id) {
	case Field::Id::Flags:
		if (auto&& finfo{flag_info(val)}; finfo)
			return Xapian::Query{field.xapian_term(finfo->shortcut_lower())};
		else
			return Err(Error::Code::InvalidArgument, "invalid flag '{}'", val);
	case Field::Id::Priority:
		if (auto&& prio{priority_from_name(val)}; prio)
			return Xapian::Query{field.xapian_term(to_char(*prio))};
		else
			return Err(Error::Code::InvalidArgument, "invalid priority '{}'", val);
	default: {
		auto q{Xapian::Query{field.xapian_term(val)}};
		if (ngrams) { // special case: cjk; see if we can create an expanded query.
			if (field.is_phrasable_term() && contains_unbroken_script(val))
				if (auto&& ng{ngram_expand(field, val)}; ng)
					return ng;
		}
		return q;
	}}
}

static Result<Xapian::Query>
parse(const Store& store, Sexp&& s, Mu::ParserFlags flags)
{
	auto&& headsym{head_symbol(s)};
	if (!headsym)
		return Err(Error::Code::InvalidArgument,
			   "expected (symbol ...) but got {}", s.to_string());

	// ie., something like (or|and| ... ....)
	if (auto&& logop{find_log_op(*headsym)}; logop) {
		if (auto&& args{tail(std::move(s))}; !args)
			return Err(Error::Code::InvalidArgument,
				   "expected (logop ...) but got {}",
				   s.to_string());
		else
			return parse_logop(store, *logop, std::move(*args), flags);

	}
	// something like (field ...)
	else if (auto&& field{field_from_name(*headsym)}; field) {

		auto&& rest{tail(std::move(s))};
		if (!rest || rest->empty())
			return Err(Error::Code::InvalidArgument,
				   "expected field-value or field-matcher");

		auto&& matcher{rest->front()};
		// field-value: (field "value"); ensure "value" is there
		if (matcher.stringp())
			return parse_basic(*field, std::move(matcher), flags);

		// otherwise, we expect a field-matcher, e.g. (field (phrase "a b c"))
		// ensure the matcher is a list starting with a symbol
		auto&& match_sym{head_symbol(matcher)};
		if (!match_sym)
			return Err(Error::Code::InvalidArgument,
				   "expected field-matcher");

		if (auto&& args{tail(std::move(matcher))}; !args)
			return Err(Error::Code::InvalidArgument, "expected matcher arguments");
		else
			return parse_field_matcher(store, *field,
						   *match_sym, std::move(*args));
	}
	return Err(Error::Code::InvalidArgument, "unexpected sexp {}", s.to_string());
}

/* LCOV_EXCL_START*/
// parse the way Xapian's internal parser does it; for testing.
static Xapian::Query
xapian_query_classic(const std::string& expr, Mu::ParserFlags flags)
{
	Xapian::QueryParser xqp;

	// add prefixes
	field_for_each([&](auto&& field){

		if (!field.is_searchable())
			return;

		const auto prefix{std::string(1U, field.xapian_prefix())};
		std::vector<std::string> names = {
			std::string{field.name},
			std::string(1U, field.shortcut)
		};
		if (!field.alias.empty())
			names.emplace_back(std::string{field.alias});

		for (auto&& name: names)
			xqp.add_prefix(name, prefix);
	});

	auto xflags = Xapian::QueryParser::FLAG_PHRASE |
		Xapian::QueryParser::FLAG_BOOLEAN |
		Xapian::QueryParser::FLAG_WILDCARD;

	if (any_of(flags & ParserFlags::SupportNgrams))
		xflags |= Xapian::QueryParser::FLAG_NGRAMS;

	xqp.set_default_op(Xapian::Query::OP_AND);
	return xqp.parse_query(expr, xflags);
}
/* LCOV_EXCL_STOP*/

Result<Xapian::Query>
Mu::make_xapian_query(const Store& store, const std::string& expr, Mu::ParserFlags flags) noexcept
{
	if (any_of(flags & Mu::ParserFlags::XapianParser))
		return xapian_query_classic(expr, flags);

	return parse(store, Mu::parse_query(expr,  true/*expand*/), flags);
}


#ifdef BUILD_XAPIANIZE_QUERY
int
main (int argc, char *argv[])
{
	if (argc < 2) {
		mu_printerrln("expected: parse-query <query>");
		return 1;
	}

	auto store = Store::make(runtime_path(Mu::RuntimePath::XapianDb));
	if (!store) {
		mu_printerrln("error: {}", store.error());
		return 2;
	}

	std::string expr;
	for (auto i = 1; i < argc; ++i) {
		expr += argv[i];
		expr += " ";
	}

	if (auto&& query{make_xapian_query(*store, expr)}; !query) {
		mu_printerrln("error: {}", query.error());
		return 1;
	} else
		mu_println("mu: {}", query->get_description());

	if (auto&& query{make_xapian_query(*store, expr, ParserFlags::XapianParser)}; !query) {
		mu_printerrln("error: {}", query.error());
		return 2;
	} else
		mu_println("xp: {}", query->get_description());

	return 0;


}
#endif /*BUILD_XAPIANIZE_QUERY*/

#if BUILD_TESTS
/*
 * Tests.
 *
 */

#include "utils/mu-test-utils.hh"

using TestCase = std::pair<std::string, std::string>;

static void
test_sexp()
{
	/* tail */
	g_assert_false(!!tail(Sexp{}));
	auto t = tail(Sexp{1,2,3});
	g_assert_true(!!t && t->listp() && t->size() == 2);

	/* head_symbol */
	g_assert_false(!!head_symbol(Sexp{}));
	assert_equal(head_symbol(Sexp{"foo"_sym, 1, 2}).value_or("bar"), "foo");

	/* string_nth */
	g_assert_false(!!string_nth(Sexp{}, 123));
	g_assert_false(!!string_nth(Sexp{1, 2, 3}, 1));
	assert_equal(string_nth(Sexp{"aap", "noot", "mies"}, 2).value_or("wim"), "mies");
}


static void
test_xapian()
{
	allow_warnings();

	auto&& testhome{unwrap(make_temp_dir())};
	auto&& dbpath{runtime_path(RuntimePath::XapianDb, testhome)};
	auto&& store{unwrap(Store::make_new(dbpath, join_paths(testhome, "test-maildir")))};

	// Xapian internal format (get_description()) is _not_ guaranteed
	// to be the same between versions
	auto&& zz{make_xapian_query(store, R"(subject:"hello world")")};
	assert_valid_result(zz);
	/* LCOV_EXCL_START*/
	if (zz->get_description() != R"(Query((Shello world OR (Shello PHRASE 2 Sworld))))") {
		mu_println("{}", zz->get_description());
		if (mu_test_mu_hacker()) {
			// in the mu hacker case, we want to be warned if Xapian changed.
			g_critical("xapian version mismatch");
			g_assert_true(false);
		} else {
			g_test_skip("incompatible xapian descriptions");
			return;
		}
	}
	/* LCOV_EXCL_STOP*/

	std::vector<TestCase> cases = {

		TestCase{R"(i:87h766tzzz.fsf@gnus.org)", R"(Query(I87h766tzzz.fsf@gnus.org))"},
		TestCase{R"(subject:foo to:bar)", R"(Query((Sfoo AND Tbar)))"},
		TestCase{R"(subject:"cuux*")", R"(Query(WILDCARD SYNONYM Scuux))"},
		TestCase{R"(subject:"hello world")",
			R"(Query((Shello world OR (Shello PHRASE 2 Sworld))))"},
		TestCase{R"(subject:/boo/")", R"(Query())"},

		// logic
		TestCase{R"(not)", R"(Query((Tnot OR Cnot OR Hnot OR Fnot OR Snot OR Bnot OR Enot)))"},
		TestCase{R"(from:a and (from:b or from:c))", R"(Query((Fa AND (Fb OR Fc))))"},
		// optimize?
		TestCase{R"(not from:a and to:b)", R"(Query(((<alldocuments> AND_NOT Fa) AND Tb)))"},
		TestCase{R"(cc:a not bcc:b)", R"(Query((Ca AND (<alldocuments> AND_NOT Hb))))"},

		// ranges.
		TestCase{R"(size:1..10")", R"(Query(VALUE_RANGE 17 g1 ga))"},
		TestCase{R"(size:10..1")", R"(Query(VALUE_RANGE 17 g1 ga))"},
		TestCase{R"(size:10..")",  R"(Query(VALUE_GE 17 ga))"},
		TestCase{R"(size:..10")",  R"(Query(VALUE_LE 17 ga))"},
		TestCase{R"(size:10")",    R"(Query(VALUE_RANGE 17 ga ga))"}, // change?
		TestCase{R"(size:..")",    R"(Query())"},
	};

	for (auto&& test: cases) {
		auto&& xq{make_xapian_query(store, test.first)};
		assert_valid_result(xq);
		assert_equal(xq->get_description(), test.second);
	}

	remove_directory(testhome);
}

int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	Xapian::QueryParser qp;

	g_test_add_func("/query-parser/sexp",       test_sexp);
	g_test_add_func("/query-parser/xapianizer", test_xapian);

	return g_test_run();
}

#endif /*BUILD_TESTS*/
