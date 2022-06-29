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

#include "mu-document.hh"
#include "mu-message.hh"

#include <cstdint>
#include <glib.h>
#include <numeric>
#include <algorithm>
#include <charconv>
#include <cinttypes>

#include <string>
#include <utils/mu-utils.hh>

using namespace Mu;

constexpr uint8_t SepaChar1 = 0xfe;
constexpr uint8_t SepaChar2 = 0xff;

static void
add_search_term(Xapian::Document& doc, const Field& field, const std::string& val)
{
	if (field.is_normal_term()) {
		doc.add_term(field.xapian_term(val));
	} else if (field.is_boolean_term()) {
		doc.add_boolean_term(field.xapian_term(val));
	} else if (field.is_indexable_term()) {
		Xapian::TermGenerator termgen;
		termgen.set_document(doc);
		termgen.index_text(utf8_flatten(val), 1, field.xapian_term());
		 /* also add as 'normal' term, so some queries where the indexer
		  * eats special chars also match */
		if (field.id != Field::Id::BodyText &&
		    field.id != Field::Id::EmbeddedText) {
			doc.add_term(field.xapian_term(val));
		}
	} else
		throw std::logic_error("not a search term");
}


static std::string
make_prop_name(const Field& field)
{
	return ":" + std::string(field.name);
}

void
Document::add(Field::Id id, const std::string& val)
{
	const auto field{field_from_id(id)};

	if (field.is_value())
		xdoc_.add_value(field.value_no(), val);

	if (field.is_searchable())
		add_search_term(xdoc_, field, val);

	if (field.include_in_sexp())
		sexp_list().add_prop(make_prop_name(field),
				     Sexp::make_string(std::move(val)));
}

void
Document::add(Field::Id id, const std::vector<std::string>& vals)
{
	if (vals.empty())
		return;

	const auto field{field_from_id(id)};
	if (field.is_value())
		xdoc_.add_value(field.value_no(), Mu::join(vals, SepaChar1));

	if (field.is_searchable())
		std::for_each(vals.begin(), vals.end(),
			      [&](const auto& val) {
				      add_search_term(xdoc_, field, val); });

	if (field.include_in_sexp()) {
		Sexp::List elms;
		for(auto&& val: vals)
			elms.add(Sexp::make_string(val));
		sexp_list().add_prop(make_prop_name(field),
				     Sexp::make_list(std::move(elms)));
	}
}


std::vector<std::string>
Document::string_vec_value(Field::Id field_id) const noexcept
{
	return Mu::split(string_value(field_id), SepaChar1);
}

static Sexp
make_contacts_sexp(const Contacts& contacts)
{
	Sexp::List clist;

	seq_for_each(contacts, [&](auto&& c) {
		if (!c.name.empty())
			clist.add(Sexp::make_prop_list(
					  ":name",  Sexp::make_string(c.name),
					  ":email", Sexp::make_string(c.email)));
		else
			clist.add(Sexp::make_prop_list(
					  ":email", Sexp::make_string(c.email)));
	});

	return Sexp::make_list(std::move(clist));
}

void
Document::add(Field::Id id, const Contacts& contacts)
{
	if (contacts.empty())
		return;

	const auto field{field_from_id(id)};
	std::vector<std::string> cvec;

	const std::string sepa2(1, SepaChar2);

	Xapian::TermGenerator termgen;
	termgen.set_document(xdoc_);

	for (auto&& contact: contacts) {

		const auto cfield_id{contact.field_id()};
		if (!cfield_id || *cfield_id != id)
			continue;

		const auto e{contact.email};
		xdoc_.add_term(field.xapian_term(e));

		/* allow searching for address components, too */
		const auto atpos = e.find('@');
		if (atpos != std::string::npos && atpos < e.size() - 1) {
			xdoc_.add_term(field.xapian_term(e.substr(0, atpos)));
			xdoc_.add_term(field.xapian_term(e.substr(atpos + 1)));
		}

		if (!contact.name.empty())
			termgen.index_text(utf8_flatten(contact.name), 1,
					   field.xapian_term());
		cvec.emplace_back(contact.email + sepa2 + contact.name);
	}

	if (!cvec.empty())
		xdoc_.add_value(field.value_no(), join(cvec, SepaChar1));

	if (field.include_in_sexp())
		sexp_list().add_prop(make_prop_name(field),
				     make_contacts_sexp(contacts));

}

Contacts
Document::contacts_value(Field::Id id) const noexcept
{
	const auto vals{string_vec_value(id)};
	Contacts contacts;
	contacts.reserve(vals.size());

	const auto ctype{contact_type_from_field_id(id)};
	if (G_UNLIKELY(!ctype)) {
		g_critical("invalid field-id for contact-type: <%zu>",
			   static_cast<size_t>(id));
		return {};
	}

	for (auto&& s: vals) {

		const auto pos = s.find(SepaChar2);
		if (G_UNLIKELY(pos == std::string::npos)) {
			g_critical("invalid contact data '%s'", s.c_str());
			break;
		}

		contacts.emplace_back(s.substr(0, pos), s.substr(pos + 1), *ctype);
	}

	return contacts;
}

void
Document::add_extra_contacts(const std::string& propname, const Contacts& contacts)
{
	if (!contacts.empty())
		sexp_list().add_prop(std::string{propname},
				     make_contacts_sexp(contacts));
}


static Sexp
make_emacs_time_sexp(::time_t t)
{
	Sexp::List dlist;

	dlist.add(Sexp::make_number(static_cast<unsigned>(t >> 16)));
	dlist.add(Sexp::make_number(static_cast<unsigned>(t & 0xffff)));
	dlist.add(Sexp::make_number(0));

	return Sexp::make_list(std::move(dlist));
}

void
Document::add(Field::Id id, int64_t val)
{
	/*
	 * Xapian stores everything (incl. numbers) as strings.
	 *
	 * we comply, by storing a number a base-16 and prefixing with 'f' +
	 * length; such that the strings are sorted in the numerical order.
	 */

	const auto field{field_from_id(id)};

	if (field.is_value())
		xdoc_.add_value(field.value_no(), to_lexnum(val));

	if (field.include_in_sexp()) {
		if (field.is_time_t())
			sexp_list().add_prop(make_prop_name(field),
					     make_emacs_time_sexp(val));
		else
			sexp_list().add_prop(make_prop_name(field),
					     Sexp::make_number(val));
	}
}

int64_t
Document::integer_value(Field::Id field_id) const noexcept
{
	if (auto&& v{string_value(field_id)}; v.empty())
		return 0;
	else
		return from_lexnum(v);
}

void
Document::add(Priority prio)
{
	constexpr auto field{field_from_id(Field::Id::Priority)};

	xdoc_.add_value(field.value_no(), std::string(1, to_char(prio)));
	xdoc_.add_boolean_term(field.xapian_term(to_char(prio)));

	if (field.include_in_sexp())
		sexp_list().add_prop(make_prop_name(field),
				     Sexp::make_symbol_sv(priority_name(prio)));
}

Priority
Document::priority_value() const noexcept
{
	const auto val{string_value(Field::Id::Priority)};
	return priority_from_char(val.empty() ? 'n' : val[0]);
}

void
Document::add(Flags flags)
{
	constexpr auto field{field_from_id(Field::Id::Flags)};

	Sexp::List flaglist;
	xdoc_.add_value(field.value_no(), to_lexnum(static_cast<int64_t>(flags)));
	flag_infos_for_each([&](auto&& flag_info) {
		auto term=[&](){return field.xapian_term(flag_info.shortcut_lower());};
		if (any_of(flag_info.flag & flags)) {
			xdoc_.add_boolean_term(term());
			flaglist.add(Sexp::make_symbol_sv(flag_info.name));
		}
	});

	if (field.include_in_sexp())
		sexp_list().add_prop(make_prop_name(field),
				     Sexp::make_list(std::move(flaglist)));
}


Sexp::List&
Document::sexp_list()
{
	/* perhaps we need get the sexp_ from the document first? */
	if (sexp_list_.empty()) {
		const auto str{xdoc_.get_data()};
		if (!str.empty()) {
			Sexp sexp{Sexp::make_parse(str)};
			sexp_list_ = sexp.list();
		}
	}

	return sexp_list_;
}

std::string
Document::cached_sexp() const
{
	return xdoc_.get_data();
}

void
Document::update_cached_sexp(void)
{
	if (sexp_list_.empty())
		return; /* nothing to do; i.e. the exisiting sexp is still up to
			 * date */
	xdoc_.set_data(Sexp::make_list(Sexp::List{sexp_list()}).to_sexp_string());
}

Flags
Document::flags_value() const noexcept
{
	return static_cast<Flags>(integer_value(Field::Id::Flags));
}

void
Document::remove(Field::Id field_id)
{
	const auto field{field_from_id(field_id)};
	const auto pfx{field.xapian_prefix()};

	xapian_try([&]{

		if (auto&& val{xdoc_.get_value(field.value_no())}; !val.empty()) {
			// g_debug("removing value<%u>: '%s'", field.value_no(),
			//	val.c_str());
			xdoc_.remove_value(field.value_no());
		}

		std::vector<std::string> kill_list;
		for (auto&& it = xdoc_.termlist_begin();
		     it != xdoc_.termlist_end(); ++it)  {
			const auto term{*it};
			if (!term.empty() && term.at(0) == pfx)
				kill_list.emplace_back(term);
		}

		for (auto&& term: kill_list) {
			// g_debug("removing term '%s'", term.c_str());
			try {
				xdoc_.remove_term(term);
			} catch(const Xapian::InvalidArgumentError& xe) {
				g_critical("failed to remove '%s'", term.c_str());
			}
		}
	});

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



static const Contacts test_contacts = {{
		Contact{"john@example.com", "John", Contact::Type::Bcc},
		Contact{"ringo@example.com", "Ringo",  Contact::Type::Bcc},
		Contact{"paul@example.com", "Paul", Contact::Type::Cc},
		Contact{"george@example.com", "George",  Contact::Type::Cc},
		Contact{"james@example.com", "James", Contact::Type::From},
		Contact{"lars@example.com", "Lars",  Contact::Type::To},
		Contact{"kirk@example.com", "Kirk", Contact::Type::To},
		Contact{"jason@example.com", "Jason",  Contact::Type::To}
	}};

static void
test_bcc()
{
	{
		Document doc;
		doc.add(Field::Id::Bcc, test_contacts);

		Contacts expected_contacts = {{
				Contact{"john@example.com", "John",
					Contact::Type::Bcc},
				Contact{"ringo@example.com", "Ringo",
					Contact::Type::Bcc},
			}};
		const auto actual_contacts = doc.contacts_value(Field::Id::Bcc);
		assert_same_contacts(expected_contacts, actual_contacts);
	}

	{
		Document doc;
		Contacts contacts = {{
				Contact{"john@example.com", "John Lennon",
					Contact::Type::Bcc},
				Contact{"ringo@example.com", "Ringo",
					Contact::Type::Bcc},
			}};
		doc.add(Field::Id::Bcc, contacts);

		TempDir tempdir;
		auto db = Xapian::WritableDatabase(tempdir.path());
		db.add_document(doc.xapian_document());

		auto contacts2 = doc.contacts_value(Field::Id::Bcc);
		assert_same_contacts(contacts, contacts2);
	}

}

static void
test_cc()
{
	Document doc;
	doc.add(Field::Id::Cc, test_contacts);

	Contacts expected_contacts = {{
			Contact{"paul@example.com", "Paul", Contact::Type::Cc},
			Contact{"george@example.com", "George",  Contact::Type::Cc}
		}};
	const auto actual_contacts = doc.contacts_value(Field::Id::Cc);

	assert_same_contacts(expected_contacts, actual_contacts);
}


static void
test_from()
{
	Document doc;
	doc.add(Field::Id::From, test_contacts);

	Contacts expected_contacts = {{
			Contact{"james@example.com", "James", Contact::Type::From},
		}};
	const auto actual_contacts = doc.contacts_value(Field::Id::From);

	assert_same_contacts(expected_contacts, actual_contacts);
}

static void
test_to()
{
	Document doc;
	doc.add(Field::Id::To, test_contacts);

	Contacts expected_contacts = {{
		Contact{"lars@example.com", "Lars",  Contact::Type::To},
		Contact{"kirk@example.com", "Kirk", Contact::Type::To},
		Contact{"jason@example.com", "Jason",  Contact::Type::To}
		}};
	const auto actual_contacts = doc.contacts_value(Field::Id::To);

	assert_same_contacts(expected_contacts, actual_contacts);
}


static void
test_size()
{
	{
		Document doc;
		doc.add(Field::Id::Size, 12345);
		g_assert_cmpuint(doc.integer_value(Field::Id::Size),==,12345);
	}

	{
		Document doc;
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
