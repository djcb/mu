/*
** Copyright (C) 2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-message-fields.hh"

using namespace Mu;

std::optional<MessageField::Id>
message_field_id(const std::string& name_or_shortcut)
{
	const auto it = std::find_if(
		MessageFields.begin(), MessageFields.end(),
		[&](auto&& field)->bool {
			return field.name == name_or_shortcut ||
				(name_or_shortcut.size() == 1 &&
				   name_or_shortcut[0] == field.shortcut);
		});

	if (it == MessageFields.end())
		return std::nullopt;
	else
		return it->id;
}


/**
 * compile-time checks
 */
constexpr bool
validate_message_field_ids()
{
	for (auto id = 0U; id != MessageField::id_size(); ++id) {
		const auto field_id = static_cast<MessageField::Id>(id);
		if (message_field(field_id).id != field_id)
			return false;
	}
	return true;
}

constexpr bool
validate_message_field_shortcuts()
{
	for (auto id = 0U; id != MessageField::id_size(); ++id) {
		const auto field_id = static_cast<MessageField::Id>(id);
		const auto shortcut = message_field(field_id).shortcut;
		if (shortcut != 0 &&
		    (shortcut < 'a' || shortcut > 'z'))
			return false;
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
	static_assert(validate_message_field_ids());
}

[[maybe_unused]]
static void
test_shortcuts()
{
	static_assert(validate_message_field_shortcuts());
}

#ifdef BUILD_TESTS
int
main(int argc, char* argv[])
{
	g_test_init(&argc, &argv, NULL);

	g_test_add_func("/message/fields/ids", test_ids);
	g_test_add_func("/message/fields/shortcuts", test_shortcuts);

	return g_test_run();
}
#endif /*BUILD_TESTS*/
