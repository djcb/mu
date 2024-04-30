/** Copyright (C) 2022-2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <utility>
#include <string>
#include <vector>

#include "mu-xapian-db.hh"
#include "mu-fields.hh"
#include "mu-priority.hh"
#include "mu-flags.hh"
#include "mu-contact.hh"
#include <utils/mu-option.hh>
#include <utils/mu-sexp.hh>

namespace Mu {

/**
 * A Document describes the information about a message that is
 * or can be stored in the database.
 *
 */
class Document {
public:
	enum struct Options {
		None	      = 0,
		SupportNgrams = 1 << 0,     /**< Support ngrams, as used in
					     * CJK and other languages. */
	};

	/**
	 * Construct a message for a new Xapian Document
	 *
	 * @param flags behavioral flags
	 */
	Document(Options opts = Options::None): options_{opts} {}

	/**
	 * Construct a message document based on an existing Xapian document.
	 *
	 * @param doc
	 * @param flags behavioral flags
	 */
	Document(const Xapian::Document& doc, Options opts = Options::None):
		xdoc_{doc}, options_{opts} {}

	/**
	 * DTOR
	 */
	~Document() {
		xapian_document(); // for side-effect up updating sexp.
	}

	/**
	 * Get a reference to the underlying Xapian document.
	 *
	 */
	const Xapian::Document& xapian_document() const;

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
	 * Add some extra contacts with the given propname; this is useful for
	 * ":reply-to" and ":list-post" which don't have a Field::Id and are
	 * only present in the sexp, not in the terms/values
	 *
	 * @param propname property name (e.g.,. ":reply-to")
	 * @param contacts contacts for this property.
	 */
	void add_extra_contacts(const std::string& propname,
				const Contacts& contacts);

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
	 * Remove values and terms for some field.
	 *
	 * @param field_id
	 */
	void remove(Field::Id field_id);

	/**
	 * Get the cached s-expression
	 *
	 * @return the cached s-expression
	 */
	const Sexp& sexp() const { return cached_sexp(); }

	/**
	 * Get the message s-expression as a string
	 *
	 * @return message s-expression string
	 */
	std::string sexp_str() const { return xdoc_.get_data(); }

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
	template<typename SexpType> void put_prop(const Field& field, SexpType&& val);
	template<typename SexpType> void put_prop(const std::string& pname, SexpType&& val);

	Sexp& cached_sexp() const {
		if (cached_sexp_.empty())
			if (auto&& s{Sexp::parse(xdoc_.get_data())}; s)
				cached_sexp_ = std::move(*s);
		return cached_sexp_;
	}

	mutable Xapian::Document	xdoc_;
	Options				options_;
	mutable Sexp			cached_sexp_;
	mutable bool			dirty_sexp_{};	/* xdoc's sexp is outdated */
};
MU_ENABLE_BITOPS(Document::Options);

} // namepace Mu

#endif /* MU_DOCUMENT_HH__ */
