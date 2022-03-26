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

#include "mu-contact.hh"
#include "mu-message.hh"

#include <gmime/gmime.h>
#include <glib.h>

using namespace Mu;

std::string
Contact::display_name() const
{
	if (name.empty())
		return email;
	else
		return name + " <" + email + '>';
}

Mu::Contacts
Mu::make_contacts(InternetAddressList* addr_lst,
		  Field::Id field_id, int64_t message_date)
{
	Contacts contacts;
	size_t num{};

	g_return_val_if_fail(addr_lst, contacts);

	auto lst_len{internet_address_list_length(addr_lst)};
	contacts.reserve(lst_len);
	for (auto i = 0; i != lst_len; ++i) {

		auto&& addr{internet_address_list_get_address(addr_lst, i)};
		const auto name{internet_address_get_name(addr)};

		if (G_UNLIKELY(!INTERNET_ADDRESS_IS_MAILBOX(addr)))
			continue;

		const auto email{internet_address_mailbox_get_addr (
				INTERNET_ADDRESS_MAILBOX(addr))};
		if (G_UNLIKELY(!email))
			continue;

		contacts.push_back(Contact{email, name ? name : "",
				field_id, message_date});
		++num;
	}

	return contacts;
}


Mu::Contacts
Mu::make_contacts(const std::string& addrs,
		  Field::Id field_id,
		  int64_t message_date)
{
	auto addr_list = internet_address_list_parse(NULL, addrs.c_str());
	if (!addr_list) {
		g_warning("no addresses found in '%s'", addrs.c_str());
		return {};
	}

	auto contacts{make_contacts(addr_list, field_id, message_date)};
	g_object_unref(addr_list);

	return contacts;
}


size_t
Mu::lowercase_hash(const std::string& s)
{
	std::size_t djb = 5381; // djb hash
	for (const auto c : s)
		djb = ((djb << 5) + djb) +
			static_cast<size_t>(g_ascii_tolower(c));
	return djb;
}

#ifdef BUILD_TESTS
/*
 * Tests.
 *
 */

#include "utils/mu-utils.hh"

static void
test_ctor_foo()
{
	Contact c{
		"foo@example.com",
		"Foo Bar",
		Field::Id::Bcc,
		1645214647
	};

	assert_equal(c.email, "foo@example.com");
	assert_equal(c.name, "Foo Bar");
	g_assert_true(c.field_id == Field::Id::Bcc);
	g_assert_cmpuint(c.message_date,==,1645214647);

	assert_equal(c.display_name(), "Foo Bar <foo@example.com>");
}


static void
test_ctor_blinky()
{
	Contact c{
		"bar@example.com",
		"Blinky",
		1645215014,
		true, /* personal */
		13, /*freq*/
		12345 /* tstamp */
	};

	assert_equal(c.email, "bar@example.com");
	assert_equal(c.name, "Blinky");
	g_assert_true(c.personal);
	g_assert_cmpuint(c.frequency,==,13);
	g_assert_cmpuint(c.tstamp,==,12345);
	g_assert_cmpuint(c.message_date,==,1645215014);

	assert_equal(c.display_name(), "Blinky <bar@example.com>");
}

static void
test_ctor_cleanup()
{
	Contact c{
		"bar@example.com",
		"Bli\nky",
		1645215014,
		true, /* personal */
		13, /*freq*/
		12345 /* tstamp */
	};

	assert_equal(c.email, "bar@example.com");
	assert_equal(c.name, "Bli ky");
	g_assert_true(c.personal);
	g_assert_cmpuint(c.frequency,==,13);
	g_assert_cmpuint(c.tstamp,==,12345);
	g_assert_cmpuint(c.message_date,==,1645215014);

	assert_equal(c.display_name(), "Bli ky <bar@example.com>");
}



static void
test_make_contacts()
{
	const auto str = "Abc <boo@example.com>, "
		"Def <baa@example.com>, "
		"Ghi <zzz@example.com>";
	InternetAddressList *lst{
		internet_address_list_parse(NULL, str)};

	g_assert_true(lst);
	const auto addrs{make_contacts(lst, Field::Id::Cc, 54321 )};
	g_object_unref(lst);

	g_assert_cmpuint(addrs.size(),==,3);

	const auto addrs2{make_contacts(str, Field::Id::To, 12345 )};
	g_assert_cmpuint(addrs2.size(),==,3);

	assert_equal(addrs2[0].name, "Abc");
	assert_equal(addrs2[0].email, "boo@example.com");
	assert_equal(addrs2[1].name, "Def");
	assert_equal(addrs2[1].email, "baa@example.com");
	assert_equal(addrs2[2].name, "Ghi");
	assert_equal(addrs2[2].email, "zzz@example.com");
}

static void
test_make_contacts_2()
{
	const auto str = "Äbc <boo@example.com>, "
		"De\nf <baa@example.com>, "
		"\tGhi <zzz@example.com>";

	const auto addrs2{make_contacts(str, Field::Id::Bcc, 12345 )};
	g_assert_cmpuint(addrs2.size(),==,3);

	assert_equal(addrs2[0].name, "Äbc");
	assert_equal(addrs2[0].email, "boo@example.com");
	assert_equal(addrs2[1].name, "De f");
	assert_equal(addrs2[1].email, "baa@example.com");
	assert_equal(addrs2[2].name, "Ghi");
	assert_equal(addrs2[2].email, "zzz@example.com");
}


static void
test_make_contacts_err()
{
	allow_warnings();

	InternetAddressList *lst{ internet_address_list_parse(NULL, "")};
	g_assert_false(lst);

	const auto addrs{make_contacts("", Field::Id::To, 77777)};
	g_assert_true(addrs.empty());
}

int
main(int argc, char* argv[])
{
	g_test_init(&argc, &argv, NULL);
	g_mime_init();

	g_test_add_func("/message/contact/ctor-foo", test_ctor_foo);
	g_test_add_func("/message/contact/ctor-blinky", test_ctor_blinky);
	g_test_add_func("/message/contact/ctor-cleanup", test_ctor_cleanup);
	g_test_add_func("/message/contact/make-contacts", test_make_contacts);
	g_test_add_func("/message/contact/make-contacts-2", test_make_contacts_2);
	g_test_add_func("/message/contact/make-contacts-err", test_make_contacts_err);

	return g_test_run();
}
#endif /*BUILD_TESTS*/
