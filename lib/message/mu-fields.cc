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

#include "mu-fields.hh"
#include "mu-flags.hh"
#include "utils/mu-utils.hh"

#include "utils/mu-test-utils.hh"

using namespace Mu;

const Mu::CombiFields&
Mu::combi_fields()
{
	static CombiFields cfields = {
		CombiField{ "recip",
		  { field_from_id(Field::Id::To),
		    field_from_id(Field::Id::Cc),
		    field_from_id(Field::Id::Bcc)}},
		CombiField { "contact",
		  { field_from_id(Field::Id::To),
		    field_from_id(Field::Id::Cc),
		    field_from_id(Field::Id::Bcc),
		    field_from_id(Field::Id::From)}},
		CombiField { "related",
		  { field_from_id(Field::Id::MessageId),
		    field_from_id(Field::Id::References)}
		},
		CombiField { "",
		  { field_from_id(Field::Id::To),
		    field_from_id(Field::Id::Cc),
		    field_from_id(Field::Id::Bcc),
		    field_from_id(Field::Id::From),
		    field_from_id(Field::Id::Subject),
		    field_from_id(Field::Id::BodyText),
		    field_from_id(Field::Id::EmbeddedText),
		  }}
	};

	return cfields;
}

const Mu::FieldsVec&
Mu::fields_from_name(const std::string& name) {

	static const FieldsVec empty;
	const auto& cfields{combi_fields()};
	const auto it = seq_find_if(cfields, [&](auto cfield) {
		return cfield.name == name;
	});

	return it == cfields.end() ? empty : it->fields;
}

bool
Mu::field_is_combi(const std::string& name)
{
	return name != "" && seq_some(combi_fields(),[&](auto cfield) {
		return cfield.name == name;
	});
}

std::string
Field::xapian_term(const std::string& s) const
{
	const auto start{std::string(1U, xapian_prefix())};
	if (const auto& size = s.size(); size == 0)
		return start;

	std::string res{start};
	res.reserve(s.size() + 10);

	/* slightly optimized common pure-ascii. */
	if (G_LIKELY(g_str_is_ascii(s.c_str()))) {
		res += s;
		for (auto i = 1; res[i]; ++i)
			res[i] = g_ascii_tolower(res[i]);
	} else
		res += utf8_flatten(s);

	if (G_UNLIKELY(res.size() > MaxTermLength))
		res.erase(MaxTermLength);

	return res;
}

/**
 * compile-time checks
 */
constexpr bool
validate_field_ids()
{
	for (auto id = 0U; id != Field::id_size(); ++id) {
		const auto field_id = static_cast<Field::Id>(id);
		if (field_from_id(field_id).id != field_id)
			return false;
	}
	return true;
}

constexpr bool
validate_field_shortcuts()
{
#ifdef BUILD_TESTS
	std::array<size_t, 26> no_dups = {0};
#endif /*BUILD_TESTS*/
	for (auto id = 0U; id != Field::id_size(); ++id) {
		const auto field_id = static_cast<Field::Id>(id);
		const auto shortcut = field_from_id(field_id).shortcut;
		if (shortcut != 0 &&
		    (shortcut < 'a' || shortcut > 'z'))
			return false;
#ifdef BUILD_TESTS
		if (shortcut != 0) {
			if (++no_dups[static_cast<size_t>(shortcut-'a')] > 1) {
				mu_critical("shortcut '{}' is duplicated", shortcut);
				return false;
			}
		}
#endif
	}

	return true;
}


constexpr /*static*/ bool
validate_field_flags()
{
	for (auto&& field: Fields) {
		/* - A field has at most one of Phrasable, Boolean */
		size_t flagnum{};

		if (field.is_phrasable_term())
			++flagnum;
		if (field.is_boolean_term())
			++flagnum;

		if (flagnum > 1) {
			//mu_warning("invalid field {}", field.name);
			return false;
		}
	}

	return true;
}

/*
 * tests... also build as runtime-tests, so we can get coverage info
 */
#ifdef BUILD_TESTS
#define static_assert g_assert_true
#endif /*BUILD_TESTS*/


[[maybe_unused]]
static void
test_ids()
{
	static_assert(validate_field_ids());
}

[[maybe_unused]]
static void
test_shortcuts()
{
	static_assert(validate_field_shortcuts());
}

[[maybe_unused]]
static void
test_prefix()
{
	static_assert(field_from_id(Field::Id::Subject).xapian_prefix() == 'S');
}

[[maybe_unused]]
static void
test_field_flags()
{
	static_assert(validate_field_flags());
}

#ifdef BUILD_TESTS


static void
test_field_from_name()
{
	g_assert_true(field_from_name("s")->id == Field::Id::Subject);
	g_assert_true(field_from_name("subject")->id == Field::Id::Subject);
	g_assert_false(!!field_from_name("8"));
	g_assert_false(!!field_from_name(""));

	g_assert_true(field_from_name("").value_or(field_from_id(Field::Id::Bcc)).id ==
		      Field::Id::Bcc);
}

static void
test_xapian_term()
{
	using namespace std::string_literals;
	using namespace std::literals;

	assert_equal(field_from_id(Field::Id::Subject).xapian_term(""s), "S");
	assert_equal(field_from_id(Field::Id::Subject).xapian_term("boo"s), "Sboo");

	assert_equal(field_from_id(Field::Id::From).xapian_term('x'), "Fx");
	assert_equal(field_from_id(Field::Id::To).xapian_term("boo"sv), "Tboo");

	auto s1 = field_from_id(Field::Id::Subject).xapian_term(std::string(MaxTermLength - 1, 'x'));
	auto s2 = field_from_id(Field::Id::Subject).xapian_term(std::string(MaxTermLength, 'x'));
	g_assert_cmpuint(s1.length(), ==, s2.length());
}

int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	g_test_add_func("/message/fields/ids", test_ids);
	g_test_add_func("/message/fields/shortcuts", test_shortcuts);
	g_test_add_func("/message/fields/from-name", test_field_from_name);
	g_test_add_func("/message/fields/prefix", test_prefix);
	g_test_add_func("/message/fields/xapian-term", test_xapian_term);
	g_test_add_func("/message/fields/flags", test_field_flags);

	return g_test_run();
}
#endif /*BUILD_TESTS*/
