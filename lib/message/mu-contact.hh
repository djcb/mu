/*
** Copyright (C) 2022-2025 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

namespace Mu {
struct Contact {
	enum struct Type:char {
		None='\0', Sender='s', From='f',
		ReplyTo='r', To='t', Cc='c', Bcc='b'
	};

	/**
	 * Construct a new Contact
	 *
	 * @param email email address
	 * @param name name or empty
	 * @param type contact field type
	 * @param message_date data for the message for this contact
	 */
	Contact(const std::string& email, const std::string& name = {},
		Type type = {}, int64_t message_date ={})
		: email{email}, name{name}, type{type},
		  message_date{message_date}, personal{}, frequency{1}, tstamp{}
		{ cleanup_name(); }

	/**
	 * Construct a new Contact
	 *
	 * @param email email address
	 * @param name name or empty
	 * @param message_date date of message this contact originate from
	 * @param personal is this a personal contact?
	 * @param freq how often was this contact seen?
	 * @param tstamp timestamp for last change
	 */
	Contact(const std::string& email, const std::string& name,
		int64_t message_date, bool personal, size_t freq,
		int64_t tstamp)
	    : email{email}, name{name}, type{},
	      message_date{message_date}, personal{personal}, frequency{freq},
	      tstamp{tstamp}
	{ cleanup_name(); }

	/**
	 * Get the "display name" for this contact:
	 *
	 * If there is a non-empty name, it is of the form
	 *   Jane Doe <email@example.com>
	 * Otherwise it is just the e-mail address. Names with commas are quoted
	 * (with the quotes escaped).
	 *
	 * @return the display name
	 */
	std::string display_name() const;

	/**
	 * Does the contact contain a valid email address as per
	 *   https://html.spec.whatwg.org/multipage/input.html#valid-e-mail-address
	 * ?
	 * @return true or false
	 */
	bool has_valid_email() const;

	/**
	 * Operator==; based on the e-mail address only
	 *
	 * @param rhs some other Contact
	 *
	 * @return true or false.
	 */
	bool operator== (const Contact& rhs) const noexcept {
		return email == rhs.email;
	}
	/**
	 * Operator!=
	 *
	 * @param rhs some other Contact
	 *
	 * @return true or false.
	 */
	bool operator!= (const Contact& rhs) const noexcept {
		return !(*this == rhs);
	}

	static constexpr int64_t RecentOffset{15 * 24 * 3600};
	/**< Contacts seen after now - RecentOffset seconds are considered
	 * "recent" */

	/**
	 * operator<
	 *
	 * Less "relevant" contacts are smaller than more relevant.
	 *
	 * Used as a somewhat over-engineered way to sort by relevance
	 *
	 * This is currently used for the ordering in mu-cfind and
	 * auto-completion in mu4e, if the various completion methods don't
	 * override it...
	 *
	 * @param rhs some other contact
	 *
	 * @return true of false
	 */
	bool operator<(const Contact& rhs) const noexcept {
		// non-personal is less relevant.
		if (personal != rhs.personal)
			return personal < rhs.personal;
		// older is less relevant for recent messages
		if (message_date != rhs.message_date &&
		    (message_date > recently() || rhs.message_date > recently()))
		    return message_date < rhs.message_date;
		// less frequent is less relevant
		if (frequency != rhs.frequency)
			return frequency < rhs.frequency;
		// if all else fails, alphabetically
		return email < rhs.email;
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
	std::string	email{};	/**< Email address for this contact.Not empty */
	std::string	name{};		 /**< Name for this contact; can be empty. */
	Type		type{Type::None};/**< Type of contact */
	int64_t		message_date{}; /**< Date of the contact's message */
	bool		personal{};	/**<  A personal message? */
	size_t		frequency{};	/**< Frequency of this contact */
	int64_t		tstamp{};	/**< Timestamp for this contact (internal use) */

private:
	void cleanup_name() { // replace control characters by spaces.
		for (auto& c: name)
			if (iscntrl(c))
				c = ' ';
	}

	/**
	 * Oldest timestamp considered "recent"
	 *
	 * This is arbitrary of course
	 *
	 * @return timestamp
	 */
	static int64_t recently() {
		static const auto recent{::time({}) - RecentOffset};
		return recent;
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
		return std::hash<std::string>{}(c.email);
	}
};

#endif /* MU_CONTACT_HH__ */
