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

#ifndef MU_MESSAGE_CONTACT_HH__
#define MU_MESSAGE_CONTACT_HH__

#include <functional>
#include <string>
#include <vector>
#include <functional>
#include <cctype>
#include <cstring>
#include <cstdlib>
#include <ctime>

#include <utils/mu-option.hh>
#include "mu-fields.hh"

struct _InternetAddressList;

namespace Mu {

/**
 * Get the hash value for a lowercase value of s; useful for email-addresses
 *
 * @param s a string
 *
 * @return a hash value.
 */
size_t lowercase_hash(const std::string& s);

struct Contact {
	enum struct Type {
		None, Sender, From, ReplyTo, To, Cc, Bcc
	};

	/**
	 * Construct a new Contact
	 *
	 * @param email_ email address
	 * @param name_ name or empty
	 * @param type_ contact field type
	 * @param message_date_ data for the message for this contact
	 */
	Contact(const std::string& email_, const std::string& name_ = "",
		Type type_ = Type::None, ::time_t message_date_ = 0)
		: email{email_}, name{name_}, type{type_},
		  message_date{message_date_}, personal{}, frequency{1}, tstamp{}
		{ cleanup_name(); }

	/**
	 * Construct a new Contact
	 *
	 * @param email_ email address
	 * @param name_ name or empty
	 * @param message_date_ date of message this contact originate from
	 * @param personal_ is this a personal contact?
	 * @param freq_ how often was this contact seen?
	 * @param tstamp_ timestamp for last change
	 */
	Contact(const std::string& email_, const std::string& name_,
		       time_t message_date_, bool personal_, size_t freq_,
		       int64_t tstamp_)
	    : email{email_}, name{name_}, type{Type::None},
	      message_date{message_date_}, personal{personal_}, frequency{freq_},
	      tstamp{tstamp_}
	{ cleanup_name();}

	/**
	 * Get the "display name" for this contact:
	 *
	 * If there's a non-empty name, it's Jane Doe <email@example.com>
	 * otherwise it's just the e-mail address. Names with commas are quoted
	 * (with the quotes escaped).
	 *
	 * @return the display name
	 */
	std::string display_name() const;


	/**
	 * Does the contact contain a valid email address as per
	 *   https://html.spec.whatwg.org/multipage/input.html#valid-e-mail-address
	 * ?
	 *
	 * @return true or false
	 */
	bool has_valid_email() const;

	/**
	 * Operator==; based on the hash values (ie. lowercase e-mail address)
	 *
	 * @param rhs some other Contact
	 *
	 * @return true orf false.
	 */
	bool operator== (const Contact& rhs) const noexcept {
		return hash() == rhs.hash();
	}

	/**
	 * Get a hash-value for this contact, which gets lazily calculated. This
	 * * is for use with container classes. This uses the _lowercase_ email
	 * address.
	 *
	 * @return the hash
	 */
	size_t hash() const {
		static size_t cached_hash;
		if (cached_hash == 0) {
			cached_hash = lowercase_hash(email);
		}
		return  cached_hash;
	}

	/**
	 * Get the corresponding Field::Id (if any)
	 * for this contact.
	 *
	 * @return the field-id or Nothing.
	 */
	constexpr Option<Field::Id> field_id() const noexcept {
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wswitch-enum"
		switch(type) {
		case Type::Bcc:
			return Field::Id::Bcc;
		case Type::Cc:
			return Field::Id::Cc;
		case Type::From:
			return Field::Id::From;
		case Type::To:
			return Field::Id::To;
		default:
			return Nothing;
		}
#pragma GCC diagnostic pop
	}


	/*
	 * data members
	 */

	std::string	email;		/**< Email address for this contact.Not empty */
	std::string	name;		/**< Name for this contact; can be empty. */
	Type		type;		/**< Type of contact */
	int64_t		message_date;	/**< Date of the contact's message */
	bool		personal;	/**<  A personal message? */
	size_t		frequency;	/**< Frequency of this contact */
	int64_t		tstamp;		/**< Timestamp for this contact (internal use) */

private:
	void cleanup_name() { // replace control characters by spaces.
		for (auto& c: name)
			if (iscntrl(c))
				c = ' ';
	}
};

constexpr Option<Contact::Type>
contact_type_from_field_id(Field::Id id) noexcept {

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wswitch-enum"
	switch(id) {
	case Field::Id::Bcc:
		return Contact::Type::Bcc;
	case Field::Id::Cc:
		return Contact::Type::Cc;
	case Field::Id::From:
		return Contact::Type::From;
	case Field::Id::To:
		return Contact::Type::To;
	default:
		return Nothing;
	}
#pragma GCC diagnostic pop
}

using Contacts = std::vector<Contact>;

/**
 * Get contacts as a comma-separated list.
 *
 * @param contacts contacs
 *
 * @return string with contacts.
 */
std::string to_string(const Contacts& contacts);

} // namespace Mu

/**
 * Implement our hash int std::
 */
template<> struct std::hash<Mu::Contact> {
	std::size_t operator()(const Mu::Contact& c) const noexcept {
		return c.hash();
	}
};

#endif /* MU_CONTACT_HH__ */
