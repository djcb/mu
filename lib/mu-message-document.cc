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

#include "mu-message-document.hh"
#include "mu-message.hh"

#include <cstdint>
#include <glib.h>
#include <numeric>
#include <algorithm>
#include <charconv>
#include <cinttypes>

#include <utils/mu-utils.hh>


using namespace Mu;
using namespace Mu::Message;

constexpr char SepaChar1 = 0xfe;
constexpr char SepaChar2 = 0xff;


static void
add_index_term(Xapian::Document& doc, const Field& field, const std::string& val)
{
	std::string flatval{utf8_flatten(val)};
	Xapian::TermGenerator termgen;
	termgen.set_document(doc);
	termgen.index_text(flatval);
}

static void
maybe_add_term(Xapian::Document& doc, const Field& field, const std::string& val)
{
	if (field.is_normal_term())
		doc.add_term(field.xapian_term());
	else if (field.is_indexable_term()) {
		add_index_term(doc, field, val);
	} else if (field.is_boolean_term())
		doc.add_boolean_term(field.xapian_term(val));
}

void
MessageDocument::add(Field::Id id, const std::string& val)
{
	const auto field{message_field(id)};

	if (field.is_value())
		doc_.add_value(field.value_no(), val);

	maybe_add_term(doc_, field, val);
}

void
MessageDocument::add(Field::Id id, const std::vector<std::string>& vals)
{
	const auto field{message_field(id)};

	if (field.is_value())
		doc_.add_value(field.value_no(), Mu::join(vals, SepaChar1));

	std::for_each(vals.begin(), vals.end(),
		      [&](const auto& val) { maybe_add_term(doc_, field, val); });
}


std::vector<std::string>
MessageDocument::string_vec_value(MessageField::Id field_id) const noexcept
{
	return Mu::split(string_value(field_id), SepaChar1);
}

void
MessageDocument::add(Field::Id id, const Contacts& contacts)
{
	const auto field{message_field(id)};
	std::vector<std::string> cvec;

	const std::string sepa2(1, SepaChar2);

	for (auto&& contact: contacts) {
		if (!contact.field_id || *contact.field_id != id)
			continue;

		doc_.add_term(contact.email);
		if (!contact.name.empty())
			add_index_term(doc_, field, contact.name);

		cvec.emplace_back(contact.email + sepa2 + contact.name);
	}

	if (!cvec.empty())
		doc_.add_value(field.value_no(), join(cvec, SepaChar1));
}

Contacts
MessageDocument::contacts_value(Field::Id id) const noexcept
{
	const auto vals{string_vec_value(id)};
	Contacts contacts;
	contacts.reserve(vals.size());

	for (auto&& s: vals) {

		const auto pos = s.find(SepaChar2);
		if (G_UNLIKELY(pos == std::string::npos)) {
			g_critical("invalid contact data '%s'", s.c_str());
			break;
		}

		contacts.emplace_back(s.substr(0, pos), s.substr(pos + 1), id);
	}

	return contacts;
}

static std::string
integer_to_string(int64_t val)
{
	char buf[18];
	buf[0] = 'f' + ::snprintf(buf + 1, sizeof(buf) - 1, "%" PRIx64, val);
	return buf;
}

static int64_t
string_to_integer(const std::string& str)
{
	if (str.empty())
		return 0;

	int64_t val{};
	std::from_chars(str.c_str() + 1, str.c_str() + str.size(), val, 16);

	return val;
}

void
MessageDocument::add(Field::Id id, int64_t val)
{
	/*
	 * Xapian stores everything (incl. numbers) as strings.
	 *
	 * we comply, by storing a number a base-16 and prefixing with 'f' +
	 * length; such that the strings are sorted in the numerical order.
	 */

	const auto field{message_field(id)};

	if (field.is_value())
		doc_.add_value(field.value_no(), integer_to_string(val));

	/* terms are not supported for numerical fields */
}

int64_t
MessageDocument::integer_value(MessageField::Id field_id) const noexcept
{
	return string_to_integer(string_value(field_id));
}

void
MessageDocument::add(Priority prio)
{
	constexpr auto field{message_field(Field::Id::Priority)};

	doc_.add_value(field.value_no(), std::string(1, to_char(prio)));
	doc_.add_boolean_term(field.xapian_term(to_char(prio)));
}


Priority
MessageDocument::priority_value() const noexcept
{
	const auto val{string_value(Field::Id::Priority)};
	return message_priority_from_char(val.empty() ? 'n' : val[0]);
}

void
MessageDocument::add(Flags flags)
{
	constexpr auto field{message_field(Field::Id::Flags)};

	doc_.add_value(field.value_no(), integer_to_string(static_cast<int64_t>(flags)));
	message_flag_infos_for_each([&](auto&& flag_info) {
		if (any_of(flag_info.flag & flags))
			doc_.add_boolean_term(field.xapian_term(flag_info.shortcut_lower()));
	});
}

Flags
MessageDocument::flags_value() const noexcept
{
	return static_cast<Flags>(integer_value(Field::Id::Flags));
}



#ifdef BUILD_TESTS

#define assert_same_contact(C1,C2) do {				\
	g_assert_cmpstr(C1.email.c_str(),==,C2.email.c_str());	\
	g_assert_cmpstr(C2.name.c_str(),==,C2.name.c_str());	\
	} while (0)

#define assert_same_contacts(CV1,CV2) do {			\
	g_assert_cmpuint(CV1.size(),==,CV2.size());		\
	for (auto i = 0U; i != CV1.size(); ++i)			\
		assert_same_contact(CV1[i], CV2[i]);		\
	} while(0)



static const MessageContacts test_contacts = {{
		MessageContact{"john@example.com", "John", Field::Id::Bcc},
		MessageContact{"ringo@example.com", "Ringo",  Field::Id::Bcc},
		MessageContact{"paul@example.com", "Paul", Field::Id::Cc},
		MessageContact{"george@example.com", "George",  Field::Id::Cc},
		MessageContact{"james@example.com", "James", Field::Id::From},
		MessageContact{"lars@example.com", "Lars",  Field::Id::To},
		MessageContact{"kirk@example.com", "Kirk", Field::Id::To},
		MessageContact{"jason@example.com", "Jason",  Field::Id::To}
	}};

static void
test_bcc()
{
	{
		MessageDocument doc;
		doc.add(Field::Id::Bcc, test_contacts);

		MessageContacts expected_contacts = {{
				MessageContact{"john@example.com", "John", Field::Id::Bcc},
				MessageContact{"ringo@example.com", "Ringo",  Field::Id::Bcc},
			}};
		const auto actual_contacts = doc.contacts_value(Field::Id::Bcc);
		assert_same_contacts(expected_contacts, actual_contacts);
	}

	{
		MessageDocument doc;
		MessageContacts contacts = {{
				MessageContact{"john@example.com", "John Lennon", Field::Id::Bcc},
				MessageContact{"ringo@example.com", "Ringo",  Field::Id::Bcc},
			}};
		doc.add(Field::Id::Bcc, contacts);
		auto db = Xapian::InMemory::open();

		db.add_document(doc.xapian_document());

	}

}

static void
test_cc()
{
	MessageDocument doc;
	doc.add(Field::Id::Cc, test_contacts);

	MessageContacts expected_contacts = {{
			MessageContact{"paul@example.com", "Paul", Field::Id::Cc},
			MessageContact{"george@example.com", "George",  Field::Id::Cc}
		}};
	const auto actual_contacts = doc.contacts_value(Field::Id::Cc);

	assert_same_contacts(expected_contacts, actual_contacts);
}


static void
test_from()
{
	MessageDocument doc;
	doc.add(Field::Id::From, test_contacts);

	MessageContacts expected_contacts = {{
			MessageContact{"james@example.com", "James", Field::Id::From},
		}};
	const auto actual_contacts = doc.contacts_value(Field::Id::From);

	assert_same_contacts(expected_contacts, actual_contacts);
}

static void
test_to()
{
	MessageDocument doc;
	doc.add(Field::Id::To, test_contacts);

	MessageContacts expected_contacts = {{
		MessageContact{"lars@example.com", "Lars",  Field::Id::To},
		MessageContact{"kirk@example.com", "Kirk", Field::Id::To},
		MessageContact{"jason@example.com", "Jason",  Field::Id::To}
		}};
	const auto actual_contacts = doc.contacts_value(Field::Id::To);

	assert_same_contacts(expected_contacts, actual_contacts);
}


static void
test_size()
{
	{
		MessageDocument doc;
		doc.add(Field::Id::Size, 12345);
		g_assert_cmpuint(doc.integer_value(Field::Id::Size),==,12345);
	}

	{
		MessageDocument doc;
		g_assert_cmpuint(doc.integer_value(Field::Id::Size),==,0);
	}
}


int
main(int argc, char* argv[])
{
	g_test_init(&argc, &argv, NULL);

	g_test_add_func("/message/document/bcc", test_bcc);
	g_test_add_func("/message/document/cc", test_cc);
	g_test_add_func("/message/document/from", test_from);
	g_test_add_func("/message/document/to", test_to);

	g_test_add_func("/message/document/size", test_size);

	return g_test_run();
}
#endif /*BUILD_TESTS*/
