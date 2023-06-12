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
#include "utils/mu-utils.hh"
#include "utils/mu-regex.hh"
#include "mu-mime-object.hh"

#include <gmime/gmime.h>
#include <glib.h>
#include <string>

using namespace Mu;

const std::string rfc5322SpecialCharacters  = "()<>@,;:\\\"[]";

std::string
Contact::display_name() const
{
	auto needs_quoting= [](const std::string& n) {
		for (auto& c: n){
			// RFC 5322 3.2.3 defines special characters, Ascii Control
			// (CTL) characters (0-31), DEL (127) as requiring quoting.
			if ((c >= 0 && c <=31) || c == 127 || rfc5322SpecialCharacters.find(c) != std::string::npos){
				return true;
			}
		}
		return false;
	};

	if (name.empty())
		return email;
	else if (!needs_quoting(name))
		return name + " <" + email + '>';
	else
		return Mu::quote(name) + " <" + email + '>';
}

static Regex email_rx;

bool
Contact::has_valid_email() const
{
	/* regexp as per:
	 *   https://html.spec.whatwg.org/multipage/input.html#valid-e-mail-address
	 *
	 * "This requirement is a willful violation of RFC 5322, which defines a
	 * syntax for email addresses that is simultaneously too strict (before
	 * the "@" character), too vague (after the "@" character), and too lax
	 * (allowing comments, whitespace characters, and quoted strings in
	 * manners unfamiliar to most users) to be of practical use here."
	 */
	constexpr auto email_rx_str =
		R"(^[a-zA-Z0-9.!#$%&'*+\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$)";
	/* avoid std::regex here, since speed matters. PCRE is faster */
	if (!email_rx) {
		if (auto&& res = Regex::make(email_rx_str, G_REGEX_OPTIMIZE); !res) {
			g_error("BUG: error in regex: %s", res.error().what()); /* terminates process */
		} else
			email_rx = res.value();
	}

	return email_rx.matches(email);
}


std::string
Mu::to_string(const Mu::Contacts& contacts)
{
	std::string res;

	seq_for_each(contacts, [&](auto&& contact) {
		if (res.empty())
			res = contact.display_name();
		else
			res += ", " + contact.display_name();
	});

	return res;
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

#include "utils/mu-test-utils.hh"

static void
test_ctor_foo()
{
	Contact c{
		"foo@example.com",
		"Foo Bar",
		Contact::Type::Bcc,
		1645214647
	};

	assert_equal(c.email, "foo@example.com");
	assert_equal(c.name, "Foo Bar");
	g_assert_true(c.has_valid_email());
	g_assert_true(*c.field_id() == Field::Id::Bcc);
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
	g_assert_true(c.has_valid_email());
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
	g_assert_true(c.has_valid_email());
	g_assert_cmpuint(c.frequency,==,13);
	g_assert_cmpuint(c.tstamp,==,12345);
	g_assert_cmpuint(c.message_date,==,1645215014);

	assert_equal(c.display_name(), "Bli ky <bar@example.com>");
}

static void
test_encode()
{
	Contact c{
		"cassius@example.com",
		"Ali, Muhammad \"The Greatest\"",
		345,
		false, /* personal */
		333, /*freq*/
		768 /* tstamp */
	};

	assert_equal(c.email, "cassius@example.com");
	assert_equal(c.name, "Ali, Muhammad \"The Greatest\"");
	g_assert_true(c.has_valid_email());
	g_assert_false(c.personal);
	g_assert_cmpuint(c.frequency,==,333);
	g_assert_cmpuint(c.tstamp,==,768);
	g_assert_cmpuint(c.message_date,==,345);

	assert_equal(c.display_name(),
		     "\"Ali, Muhammad \\\"The Greatest\\\"\" <cassius@example.com>");
}


static void
test_sender()
{
	Contact c{"aa@example.com", "Anders Ångström",
		Contact::Type::Sender, 54321};

	assert_equal(c.email, "aa@example.com");
	assert_equal(c.name, "Anders Ångström");
	g_assert_true(c.has_valid_email());
	g_assert_false(c.personal);
	g_assert_cmpuint(c.frequency,==,1);
	g_assert_cmpuint(c.message_date,==,54321);

	g_assert_false(!!c.field_id());
}


static void
test_misc()
{
	g_assert_false(!!contact_type_from_field_id(Field::Id::Subject));
}

static void
test_broken_email()
{
	Contact c{"a***@@booa@example..com", "abcdef",
		Contact::Type::Sender, 54321};

	g_assert_false(c.has_valid_email());
}

int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);
	g_mime_init();

	g_test_add_func("/message/contact/ctor-foo", test_ctor_foo);
	g_test_add_func("/message/contact/ctor-blinky", test_ctor_blinky);
	g_test_add_func("/message/contact/ctor-cleanup", test_ctor_cleanup);
	g_test_add_func("/message/contact/encode", test_encode);

	g_test_add_func("/message/contact/sender", test_sender);
	g_test_add_func("/message/contact/misc", test_misc);
	g_test_add_func("/message/contact/broken-email", test_broken_email);

	return g_test_run();
}
#endif /*BUILD_TESTS*/
