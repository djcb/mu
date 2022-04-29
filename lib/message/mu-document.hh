/** Copyright (C) 2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef MU_DOCUMENT_HH__
#define MU_DOCUMENT_HH__

#include <xapian.h>
#include <utility>
#include <string>
#include <vector>
#include "utils/mu-xapian-utils.hh"

#include "mu-fields.hh"
#include "mu-priority.hh"
#include "mu-flags.hh"
#include "mu-contact.hh"
#include <utils/mu-option.hh>

namespace Mu {

/**
 * A Document describes the information about a message that is
 * or can be stored in the database.
 *
 */
class Document {
public:
	/**
	 * Construct a message for a new Xapian Document
	 *
	 */
	Document() {}

	/**
	 * Construct a message document based on on existing Xapian document.
	 *
	 * @param doc
	 */
	Document(const Xapian::Document& doc): xdoc_{doc} {}

	/**
	 * Copy CTOR
	 */
	Document(const Document& rhs) { *this = rhs; }
	/**
	 * Move CTOR
	 */
	Document(Document&& rhs) {*this = std::move(rhs); }

	/**
	 * Get a reference to the underlying Xapian document.
	 *
	 */
	const Xapian::Document& xapian_document() const { return xdoc_; }

	/* Copy assignment operator
	 *
	 * @param rhs some message
	 *
	 * @return a message ref
	 */
	Document& operator=(const Document& rhs) {
		if (this != &rhs)
			xdoc_ = rhs.xdoc_;
		return *this;
	}

	/**
	 * Move assignment operator
	 *
	 * @param rhs some message
	 *
	 * @return a message ref
	 */
	Document& operator=(Document&& rhs) {
		if (this != &rhs)
			xdoc_ = std::move(rhs.xdoc_);
		return *this;
	}

	/**
	 * Get the doc-id for this document
	 *
	 * @return the docid
	 */
	Xapian::docid docid() const { return xdoc_.get_docid(); }

	/*
	 * updating a document with terms & values
	 */

	/**
	 * Add a string value to the document
	 *
	 * @param field_id field id
	 * @param val string value
	 */
	void	add(Field::Id field_id, const std::string& val);

	/**
	 * Add a string-vec value to the document, if non-empty
	 *
	 * @param field_id field id
	 * @param val string-vec value
	 */
	void	add(Field::Id field_id, const std::vector<std::string>& vals);


	/**
	 * Add message-contacts to the document, if non-empty
	 *
	 * @param field_id field id
	 * @param contacts message contacts
	 */
	void    add(Field::Id id, const Contacts& contacts);


	/**
	 * Add an integer value to the document
	 *
	 * @param field_id field id
	 * @param val integer value
	 */
	void	add(Field::Id field_id, int64_t val);

	/**
	 * Add a message priority to the document
	 *
	 * @param prio priority
	 */
	void	add(Priority prio);


	/**
	 *  Add message flags to the document
	 *
	 * @param flags mesage flags.
	 */
	void	add(Flags flags);

	/**
	 * Generically adds an optional value, if set, to the document
	 *
	 * @param id the field 0d
	 * @param an optional value
	 */
	template<typename T> void add(Field::Id id, const Option<T>& val) {
		if (val)
			add(id, val.value());
	}

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
	std::string string_value(Field::Id field_id) const noexcept {
		return xapian_try([&]{
			return xdoc_.get_value(field_from_id(field_id).value_no());
		}, std::string{});
	}
	/**
	 * Get a vec of string values.
	 *
	 * @param field_id id of the field to get
	 *
	 * @return a string list
	 */
	std::vector<std::string> string_vec_value(Field::Id field_id) const noexcept;


	/**
	 * Get an integer value
	 *
	 * @param field_id id of the field to get
	 *
	 * @return an integer or 0 if not found.
	 */
	int64_t integer_value(Field::Id field_id) const noexcept;


	/**
	 * Get contacts
	 *
	 * @param field_id id of the contacts field to get
	 *
	 * @return a contacts list
	 */
	Contacts contacts_value(Field::Id id) const noexcept;

	/**
	 * Get the priority
	 *
	 * @return the message priority
	 */
	Priority priority_value() const noexcept;

	/**
	 * Get the message flags
	 *
	 *
	 * @return flags
	 */
	Flags    flags_value() const noexcept;

private:
	Xapian::Document xdoc_;
};

} // namepace Mu

#endif /* MU_DOCUMENT_HH__ */
