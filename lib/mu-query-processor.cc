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

#include "utils/mu-option.hh"
#include <glib.h>
#include "utils/mu-utils-file.hh"

using namespace Mu;

/**
 * An 'Element' here is a rather rich version of what is traditionally
 * considered a (lexical) token.
 *
 * We try to determine as much as possible during the analysis phase; which is
 * quite a bit (given the fairly simple query language), and the parsing phase
 * only has to deal with the putting these elements in a tree.
 *
 * During analysis:
 * 1) separate the query into a sequence strings
 * 2) for each of these strings
 *     - Does it look like an Op? ('or', 'and' etc.) --> Op
 *     - Otherwise: treat as a Basic field ([field]:value)
 *       - Whitespace in value? -> promote to Phrase
 *       - otherwise:
 *          - Is value a regex (in /<regex>/) -> promote to Regex
 *          - Is value a wildcard (ends in '*') -> promote to Wildcard
 *          - is value a range (a..b) -> promote to Range
 *
 * After analysis, we have the sequence of element as a Sexp, which can then be
 * fed to the parser. We attempt to make the Sexp as human-readable as possible.
 */
struct Element {
	enum struct Bracket { Open, Close} ;
	enum struct Op      { And, Or, Xor, Not, AndNot };

	template<typename ValueType>
	struct FieldValue {
		FieldValue(const ValueType& v): field{}, value{v}{}

		template<typename StringType>
		FieldValue(const StringType& fname, const ValueType& v):
			field{std::string{fname}}, value{v}{}
		template<typename StringType>
		FieldValue(const Option<StringType>& fname, const ValueType& v) {
			if (fname)
				field = std::string{*fname};
			value = v;
		}

		Option<std::string>	field{};
		ValueType		value{};
	};
	struct Basic: public FieldValue<std::string> {using FieldValue::FieldValue;};
	struct Regex: public FieldValue<std::string> {using FieldValue::FieldValue;};
	struct Wildcard: public FieldValue<std::string> {using FieldValue::FieldValue;};
	struct Range: public FieldValue<std::pair<std::string, std::string>> {
		using FieldValue::FieldValue; };

	using ValueType = std::variant<
		/*  */
		Bracket,
		/* op */
		Op,
		/* string values */
		std::string,
		/* value types */
		Basic,
		Regex,
		Wildcard,
		Range
		>;

	// helper
	template <typename T, typename U>
	struct decay_equiv:
		std::is_same<typename std::decay<T>::type, U>::type {};

	Element(Bracket b): value{b} {}
	Element(Op op): value{op} {}

	template<typename T,
		 typename std::enable_if<std::is_base_of<class FieldValue<T>, T>::value>::type = 0>
	Element(const std::string& field, const T& val): value{T{field, val}} {}

	Element(const std::string& val): value{val} {}

	template<typename T>
	Option<T&> get_opt()  {
		if (std::holds_alternative<T>(value))
			return std::get<T>(value);
		else
			return Nothing;
	}

	Sexp sexp() const {
		return std::visit([](auto&& arg)->Sexp {

			auto field_sym = [](const Option<std::string>& field) {
				return field ? Sexp::Symbol{*field} : placeholder_sym;
			};

			using T = std::decay_t<decltype(arg)>;

			if constexpr (std::is_same_v<T, Bracket>) {
				switch(arg) {
				case Bracket::Open:
					return open_sym;
				case Bracket::Close:
					return close_sym;
				default:
					throw std::logic_error("invalid bracket type");
				}
			} else if constexpr (std::is_same_v<T, Op>) {
				switch(arg) {
				case Op::And:
					return and_sym;
				case Op::Or:
					return or_sym;
				case Op::Xor:
					return xor_sym;
				case Op::Not:
					return not_sym;
				case Op::AndNot:
					return and_not_sym;
				default:
					throw std::logic_error("invalid op type");
				}
			} else if constexpr (std::is_same_v<T, Basic>) {
				return Sexp { field_sym(arg.field), arg.value };
			} else if constexpr (std::is_same_v<T, Regex>) {
				return Sexp { field_sym(arg.field), Sexp{ regex_sym, arg.value}};
			} else if constexpr (std::is_same_v<T, Wildcard>) {
				return Sexp { field_sym(arg.field), Sexp{ wildcard_sym, arg.value}};
			} else if constexpr (std::is_same_v<T, Range>) {
				return Sexp {field_sym(arg.field),
					Sexp{ range_sym, arg.value.first, arg.value.second }};
			} else if constexpr (std::is_same_v<T, std::string>) {
				throw std::logic_error("no bare strings should be here");
			} else
				throw std::logic_error("uninvited visitor");
		}, value);
	}

	ValueType value;
};

using Elements = std::vector<Element>;



/**
 * Remove first character from string and return it.
 *
 * @param[in,out] str a string
 * @param[in,out] pos position in _original_ string
 *
 * @return a char or 0 if there is none.
 */
static char
read_char(std::string& str, size_t& pos)
{
	if (str.empty())
		return {};

	auto kar{str.at(0)};
	str.erase(0, 1);
	++pos;

	return kar;
}

/**
 * Restore kar at the beginning of the string
 *
 * @param[in,out] str a string
 * @param[in,out] pos position in _original_ string
 * @param kar a character
 */
static void
unread_char(std::string& str, size_t& pos, char kar)
{
	str = kar + str;
	--pos;
}


/**
 * Remove the the next element from the string and return it
 *
 * @param[in,out] str a string
 * @param[in,out] pos position in _original_ string *
 *
 * @return an Element or Nothing
 */
static Option<Element>
next_element(std::string& str, size_t& pos)
{
	bool        quoted{}, escaped{};
	std::string value{};

	auto is_separator = [](char c) {  return c == ' '|| c == '(' || c == ')'; };

	while (!str.empty()) {

		auto kar = read_char(str, pos);

		if (kar == '\\') {
			escaped = !escaped;
			if (escaped)
				continue;
		}

		if (kar == '"' && !escaped) {
			if (!escaped && quoted)
				return Element{value};
			else {
				quoted = true;
				continue;
			}
		}

		if (!quoted && !escaped && is_separator(kar)) {
			if (!value.empty()) {
				unread_char(str, pos, kar);
				return Element{value};
			}

			if (quoted || kar == ' ')
				continue;

			switch (kar) {
			case '(':
				return Element{Element::Bracket::Open};
			case ')':
				return Element{Element::Bracket::Close};
			default:
				break;
			}
		}

		value += kar;
		escaped = false;
	}

	if (value.empty())
		return Nothing;
	else
		return Element{value};
}


static Option<Element>
opify(Element&& element)
{
	auto&& str{element.get_opt<std::string>()};
	if (!str)
		return element;

	static const std::unordered_map<std::string, Element::Op> ops = {
		{ "and", Element::Op::And },
		{ "or",  Element::Op::Or},
		{ "xor", Element::Op::Xor },
		{ "not", Element::Op::Not },
		// AndNot only appears during parsing.
	};

	if (auto&& it = ops.find(utf8_flatten(*str)); it != ops.end())
		element.value = it->second;

	return element;
}

static Option<Element>
basify(Element&& element)
{
	auto&& str{element.get_opt<std::string>()};
	if (!str)
		return element;

	const auto pos = str->find(':');
	if (pos == std::string::npos) {
		element.value = Element::Basic{*str};
		return element;
	}

	const auto fname{str->substr(0, pos)};
	if (auto&& field{field_from_name(fname)}; field) {
		auto val{str->substr(pos + 1)};
		if (field == Field::Id::Flags) {
			if (auto&& finfo{flag_info(val)}; finfo)
				element.value = Element::Basic{field->name,
					std::string{finfo->name}};
			else
				element.value = Element::Basic{*str};
		} else if (field == Field::Id::Priority) {
			if (auto&& prio{priority_from_name(val)}; prio)
				element.value = Element::Basic{field->name,
					std::string{priority_name(*prio)}};
			else
				element.value = Element::Basic{*str};
		} else
			element.value = Element::Basic{std::string{field->name},
				str->substr(pos + 1)};
	} else if (field_is_combi(fname))
		element.value = Element::Basic{fname, str->substr(pos +1)};
	else
		element.value = Element::Basic{*str};

	return element;
}

static Option<Element>
wildcardify(Element&& element)
{
	auto&& basic{element.get_opt<Element::Basic>()};
	if (!basic)
		return element;

	auto&& val{basic->value};
	if (val.size() < 2 || val[val.size()-1] != '*')
		return element;

	val.erase(val.size() - 1);
	element.value = Element::Wildcard{basic->field, val};

	return element;
}

static Option<Element>
regexpify(Element&& element)
{
	auto&& str{element.get_opt<Element::Basic>()};
	if (!str)
		return element;

	auto&& val{str->value};
	if (val.size() < 3 || val[0] != '/' || val[val.size()-1] != '/')
		return element;

	val.erase(val.size() - 1);
	val.erase(0, 1);
	element.value = Element::Regex{str->field, std::move(val)};

	return element;
}

// handle range-fields: Size, Date, Changed
static Option<Element>
rangify(Element&& element)
{
	auto&& str{element.get_opt<Element::Basic>()};
	if (!str)
		return element;

	if (!str->field)
		return element;

	auto&& field = field_from_name(*str->field);
	if (!field || !field->is_range())
		return element;

	/* yes: get the range */
	auto&& range = std::invoke([&]()->std::pair<std::string, std::string> {
		const auto val{str->value};
		const auto pos{val.find("..")};

		if (pos == std::string::npos)
			return { val, val };
		else
			return {val.substr(0, pos), val.substr(pos + 2)};
	});

	if (field->id == Field::Id::Size) {
		int64_t s1{range.first.empty() ? -1 :
			parse_size(range.first, false/*first*/).value_or(-1)};
		int64_t s2{range.second.empty() ? -1 :
			parse_size(range.second, true/*last*/).value_or(-1)};
		if (s2 >= 0 && s1 > s2)
			std::swap(s1, s2);
		element.value = Element::Range{str->field,
					   {s1 < 0 ? "" : std::to_string(s1),
					    s2 < 0 ? "" : std::to_string(s2)}};

	} else if (field->id == Field::Id::Date || field->id == Field::Id::Changed) {
		auto tstamp=[](auto&& str, auto&& first)->int64_t {
			return str.empty() ? -1 :
				parse_date_time(str, first ,false/*local*/).value_or(-1);
		};
		int64_t lower{tstamp(range.first, true/*lower*/)};
		int64_t upper{tstamp(range.second, false/*upper*/)};
		if (lower >= 0 && upper >= 0 && lower > upper) {
			// can't simply swap due to rounding up/down
			lower = tstamp(range.second, true/*lower*/);
			upper = tstamp(range.first,  false/*upper*/);
		}
		// use "Zulu" time.
		element.value =  Element::Range{
			str->field,
			{lower < 0 ? "" :
			 mu_format("{:%FT%TZ}",mu_time(lower, true/*utc*/)),
			 upper < 0 ? "" :
			 mu_format("{:%FT%TZ}", mu_time(upper, true/*utc*/))}};
	}

	return element;
}

static Elements
process(const std::string& expr)
{
	Elements elements{};
	size_t offset{0};

	/* all control chars become SPC */
	std::string str{expr};
	for (auto& c: str)
		c = ::iscntrl(c) ? ' ' : c;

	while(!str.empty()) {
		auto&& element = next_element(str, offset)
			.and_then(opify)
			.and_then(basify)
			.and_then(regexpify)
			.and_then(wildcardify)
			.and_then(rangify);
		if (element)
			elements.emplace_back(std::move(element.value()));
	}

	return elements;
}

Sexp
Mu::process_query(const std::string& expr)
{
	const auto& elements{::process(expr)};

	Sexp sexp{};
	for (auto&& elm: elements)
		sexp.add(elm.sexp());

	return sexp;
}

#ifdef BUILD_PROCESS_QUERY
int
main (int argc, char *argv[])
{
	if (argc < 2) {
		mu_printerrln("expected: process-query <query>");
		return 1;
	}

	std::string expr;
	for (auto i = 1; i < argc; ++i) {
		expr += argv[i];
		expr += " ";
	}

	auto sexp = process_query(expr);
	mu_println("{}", sexp.to_string());

	return 0;
}
#endif /*BUILD_ANALYZE_QUERY*/

#if BUILD_TESTS
/*
 *
 * Tests.
 *
 */

#include "utils/mu-test-utils.hh"

using TestCase = std::pair<std::string, std::string>;

static void
test_processor()
{
	std::vector<TestCase> cases = {
		// basics
		TestCase{R"(hello world)", R"(((_ "hello") (_ "world")))"},
		TestCase{R"(maildir:/"hello world")", R"(((maildir "/hello world")))"},
		TestCase{R"(flag:deleted)", R"(((_ "flag:deleted")))"} // non-existing flags
	};

	for (auto&& test: cases) {
		auto&& sexp{process_query(test.first)};
		assert_equal(sexp.to_string(), test.second);
	}
}

int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	g_test_add_func("/query-parser/processor", test_processor);

	return g_test_run();
}

#endif /*BUILD_TESTS*/
