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

#include "mu-message-fields.hh"

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


struct MessageContact {
	/**
	 * Contact types
	 */
	enum struct Type {
		To,      /**< To recipient */
		From,    /**< Sender */
		Cc,      /**< Cc recipient */
		Bcc,     /**< Bcc recipient */
		ReplyTo, /**< Reply-To wannabe recipient */
		Unknown, /**< Unknown type */
	};

	/**
	 * Construct a new MessageContact
	 *
	 * @param email_ email address
	 * @param name_ name or empty
	 * @param type_ contact type
	 * @param message_date_ data for the message for this contact
	 */
	MessageContact(const std::string& email_, const std::string& name_ = "",
		       Type type_ = Type::Unknown, time_t message_date_ = 0)
	    : email{email_}, name{name_}, type{type_},
	      message_date{message_date_}, personal{}, frequency{1}, tstamp{}
		{ cleanup_name(); }

	/**
	 * Construct a new MessageContact
	 *
	 * @param email_ email address
	 * @param name_ name or empty
	 * @param message_date_ date of message this contact originate from
	 * @param personal_ is this a personal contact?
	 * @param freq_ how often was this contact seen?
	 * @param tstamp_ timestamp for last change
	 */
	MessageContact(const std::string& email_, const std::string& name_,
		       time_t message_date_, bool personal_, size_t freq_,
		       int64_t tstamp_)
	    : email{email_}, name{name_}, type{Type::Unknown},
	      message_date{message_date_}, personal{personal_}, frequency{freq_},
	      tstamp{tstamp_}
	{ cleanup_name();}

	/**
	 * Get the "display name" for this contact; basically, if there's a
	 * non-empty name, it's
	 *     Jane Doe <email@example.com>
	 * otherwise it's just the e-mail address.
	 *
	 * @return the display name
	 */
	std::string display_name() const;

	/**
	 * Operator==; based on the hash values (ie. lowercase e-mail address)
	 *
	 * @param rhs some other MessageContact
	 *
	 * @return true orf false.
	 */
	bool operator== (const MessageContact& rhs) const noexcept {
		return hash() == rhs.hash();
	}

	/**
	 * Get a hash-value for this contact, which gets lazily calculated. This
	 * is for use with container classes. This uses the _lowercase_ email
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
	 * Get the MessageField for this contact-type, if any.
	 *
	 * @return the message-field or nullopt.
	 */
	constexpr std::optional<MessageField> const field() {
		switch(type){
		case Type::From:
			return message_field(MessageField::Id::From);
		case Type::To:
			return message_field(MessageField::Id::To);
		case Type::Cc:
			return message_field(MessageField::Id::Cc);
		case Type::Bcc:
			return message_field(MessageField::Id::Bcc);
		default:
			return std::nullopt;
		}
	}

	/*
	 * data members
	 */

	std::string email;               /**< Email address for this contact.Not empty */
	std::string name;                /**< Name for this contact; can be empty. */
	Type        type{Type::Unknown}; /**< Type of contact */
	::time_t    message_date;        /**< date of the message from which the
					  * contact originates */
	bool    personal;                /**<  A personal message? */
	size_t  frequency;               /**< Frequency of this contact */
	int64_t tstamp;                  /**< Timestamp for this contact */

private:
	void cleanup_name() { // replace control characters by spaces.
		for (auto& c: name)
			if (iscntrl(c))
				c = ' ';
	}
};

using MessageContacts = std::vector<MessageContact>;

/**
 * Create a sequence of MessageContact objects from an InternetAddressList
 *
 * @param addr_lst an address list
 * @param type the type of addresses
 * @param message_date the date of the message from which the InternetAddressList
 * originates.
 *
 * @return a sequence of MessageContact objects.
 */
MessageContacts
make_message_contacts(/*const*/ struct _InternetAddressList* addr_lst,
			 MessageContact::Type type, ::time_t message_date);

/**
 * Create a sequence of MessageContact objects from an InternetAddressList
 *
 * @param addrs a string with one more valid addresses (as per internet_address_list_parse())
 * @param type the type of addresses
 * @param message_date the date of the message from which the addresses originate
 *
 * @return a sequence of MessageContact objects.
 */
MessageContacts
make_message_contacts(const std::string& addrs,
		      MessageContact::Type type, ::time_t message_date);
} // namespace Mu

/**
 * Implement our hash int std::
 */
template<> struct std::hash<Mu::MessageContact> {
	std::size_t operator()(const Mu::MessageContact& c) const noexcept {
		return c.hash();
	}
};




#endif /* MU_MESSAGE_CONTACT_HH__ */
