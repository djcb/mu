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

#ifndef MU_MESSAGE_DOCUMENT_HH__
#define MU_MESSAGE_DOCUMENT_HH__

#include <xapian.h>
#include <utility>
#include <string>
#include <vector>
#include "utils/mu-xapian-utils.hh"

#include "mu-message-fields.hh"
#include "mu-message-priority.hh"
#include "mu-message-flags.hh"
#include "mu-message-contact.hh"

namespace Mu {

/**
 * A MessageDocument describes the information about a message that is
 * or can be stored in the database.
 *
 */
class MessageDocument {
public:
	/**
	 * Construct a message for a new Xapian Document
	 *
	 */
	MessageDocument() {}

	/**
	 * Construct a message document based on on existing Xapian document.
	 *
	 * @param doc
	 */
	MessageDocument(const Xapian::Document& doc): doc_{doc} {}

	/**
	 * Copy CTOR
	 */
	MessageDocument(const MessageDocument&) = default;

	/**
	 * Move CTOR
	 *
	 */
	MessageDocument(MessageDocument&&) = default;

	/**
	 * Get a reference to the underlying Xapian document.
	 *
	 */
	const Xapian::Document& xapian_document() const { return doc_; }


	/*
	 * updating a document with terms & values
	 */

	/**
	 * Add a string value to the document
	 *
	 * @param field_id field id
	 * @param val string value
	 */
	void	add(MessageField::Id field_id, const std::string& val);

	/**
	 * Add a string-vec value to the document
	 *
	 * @param field_id field id
	 * @param val string-vec value
	 */
	void	add(MessageField::Id field_id, const std::vector<std::string>& vals);


	/**
	 * Add message-contacts to the document
	 *
	 * @param field_id field id
	 * @param contacts message contacts
	 */
	void    add(MessageField::Id id, const MessageContacts& contacts);

	/**
	 * Add an integer value to the document
	 *
	 * @param field_id field id
	 * @param val integer value
	 */
	void	add(MessageField::Id field_id, int64_t val);

	/**
	 * Add a message priority to the document
	 *
	 * @param prio priority
	 */
	void	add(MessagePriority prio);


	/**
	 * Add message flags to the document
	 *
	 * @param flags mesage flags.
	 */
	void	add(MessageFlags flags);

	/*
	 * Retrieving values
	 */


	/**
	 * Get a message-field as a string-value
	 *
	 * @param field_id id of the field to get.
	 *
	 * @return a string (empty if not found)
	 */
	std::string string_value(MessageField::Id field_id) const noexcept {
		return xapian_try([&]{
			return doc_.get_value(message_field(field_id).value_no());
		}, std::string{});
	}
	/**
	 * Get a vec of string values.
	 *
	 * @param field_id id of the field to get
	 *
	 * @return a string list
	 */
	std::vector<std::string> string_vec_value(MessageField::Id field_id) const noexcept;


	/**
	 * Get an integer value
	 *
	 * @param field_id id of the field to get
	 *
	 * @return an integer or 0 if not found.
	 */
	int64_t integer_value(MessageField::Id field_id) const noexcept;


	/**
	 * Get contacts
	 *
	 * @param field_id id of the contacts field to get
	 *
	 * @return an integer or 0 if not found.
	 */
	MessageContacts contacts_value(MessageField::Id id) const noexcept;


	/**
	 * Get the priority
	 *
	 * @return the message priority
	 */
	MessagePriority priority_value() const noexcept;

	/**
	 * Get the message flags
	 *
	 *
	 * @return flags
	 */
	MessageFlags    flags_value() const noexcept;


private:
	Xapian::Document doc_;
};



} // namepace Mu




#endif /* MU_MESSAGE_DOCUMENT_HH__ */
