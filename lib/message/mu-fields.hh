/*
** Copyright (C) 2022-2024 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef MU_FIELDS_HH__
#define MU_FIELDS_HH__

#include <cstdint>
#include <string_view>
#include <algorithm>
#include <array>
#include <mu-xapian-db.hh>
#include <utils/mu-utils.hh>
#include <utils/mu-option.hh>

namespace Mu {

// Xapian does not like terms much longer than this
constexpr auto MaxTermLength = 240;
// http://article.gmane.org/gmane.comp.search.xapian.general/3656 */

struct Field {
	/**
	 * Field Ids.
	 *
	 * Note, the Ids are also used as indices in the Fields array,
	 * so their numerical values must be 0...Count.
	 *
	 */
	enum struct Id {
		Bcc = 0,	/**< Blind Carbon-Copy */
		BodyText,	/**< Text body */
		Cc,		/**< Carbon-Copy */
		Changed,        /**< Last change time (think 'ctime') */
		Date,		/**< Message date */
		EmbeddedText,	/**< Embedded text in message */
		File,		/**< Filename */
		Flags,		/**< Message flags */
		From,		/**< Message sender */
		Language,	/**< Body language */
		Maildir,	/**< Maildir path */
		MailingList,	/**< Mailing list */
		MessageId,	/**< Message Id */
		MimeType,	/**< MIME-Type */
		Path,		/**< File-system Path */
		Priority,	/**< Message priority */
		References,	/**< All references (incl. Reply-To:) */
		Size,		/**< Message size (in bytes) */
		Subject,	/**< Message subject */
		Tags,		/**< Message Tags */
		ThreadId,	/**< Thread Id */
		To,		/**< To: recipient */
		//
		_count_         /**< Number of Ids */
	};

	/**
	 * Get the number of Id values.
	 *
	 * @return the number.
	 */
	static constexpr size_t id_size()
	{
		return static_cast<size_t>(Id::_count_);
	}

	constexpr Xapian::valueno value_no() const {
		return static_cast<Xapian::valueno>(id);
	}

	/**
	 * Field types
	 *
	 */
	enum struct Type {
		String,		/**< String */
		StringList,	/**< List of strings */
		ContactList,	/**< List of contacts */
		ByteSize,	/**< Size in bytes */
		TimeT,		/**< A time_t value */
		Integer,	/**< An integer */
	};

	constexpr bool is_string() const { return type == Type::String; }
	constexpr bool is_string_list() const { return type == Type::StringList; }
	constexpr bool is_byte_size() const { return type == Type::ByteSize; }
	constexpr bool is_time_t() const { return type == Type::TimeT; }
	constexpr bool is_integer() const { return type == Type::Integer; }
	constexpr bool is_numerical() const { return is_byte_size() || is_time_t() || is_integer(); }

	/**
	 * Field flags
	 * note: the differences for our purposes between a xapian field and a
	 * term: - there is only a single value for some item in per document
	 * (msg), ie.  one value containing the list of To: addresses - there
	 * can be multiple terms, each containing e.g. one of the To:
	 * addresses - searching uses terms, but to display some field, it
	 * must be in the value
	 *
	 * Rules (build-time enforced):
	 * - A field has at most one of PhrasableTerm, BooleanTerm, ContactTerm.
	 */

	enum struct Flag {
		/*
		 * Different kind of terms; at most one is true, and cannot be combined with
		 * Contact. Compile-time enforced.
		 */
		NormalTerm    = 1 << 0,
		/**< Field is a searchable term */
		BooleanTerm   = 1 << 1,
		/**< Field is a boolean search-term (i.e. at most one per message);
		 * wildcards do not work */
		PhrasableTerm = 1 << 2,
		/**< Field has phrasable/indexable text as term */
		/*
		 * Contact flag cannot be combined with any of the term flags.
		 * This is compile-time enforced.
		 */
		Contact = 1 << 10,
		/**< field contains one or more e-mail-addresses */
		Value   = 1 << 11,
		/**< Field value is stored (so the literal value can be retrieved) */

		Range	   = 1 << 21,

		IncludeInSexp = 1 << 24,
		/**< whether to include this field in the cached sexp. */

		/**< whether this is a range field (e.g., date, size)*/
		Internal      = 1 << 26
	};

	constexpr bool any_of(Flag some_flag) const{
		return (static_cast<int>(some_flag) & static_cast<int>(flags)) != 0;
	}

	constexpr bool is_phrasable_term()	const { return any_of(Flag::PhrasableTerm); }
	constexpr bool is_boolean_term()	const { return any_of(Flag::BooleanTerm); }
	constexpr bool is_normal_term()		const { return any_of(Flag::NormalTerm); }
	constexpr bool is_searchable()          const { return is_phrasable_term() ||
							       is_boolean_term() ||
							       is_normal_term(); }
	constexpr bool is_sortable()            const { return is_value(); }


	constexpr bool is_value()		const { return any_of(Flag::Value); }
	constexpr bool is_internal()		const { return any_of(Flag::Internal); }

	constexpr bool is_contact()		const { return any_of(Flag::Contact); }
	constexpr bool is_range()		const { return any_of(Flag::Range); }

	constexpr bool include_in_sexp()        const { return any_of(Flag::IncludeInSexp);}

	/**
	 * Field members
	 *
	 */
	Id               id;            /**< Id of the message field */
	Type             type;          /**< Type of the message field */
	std::string_view name;          /**< Name of the message field */
	std::string_view alias;         /**< Alternative name for the message field */
	std::string_view description;   /**< Decription of the message field */
	std::string_view example_query; /**< Example query */
	char             shortcut;      /**< Shortcut for the message field; a..z */
	Flag             flags;         /**< Flags */

	/**
	 * Convenience / helpers
	 *
	 */

	constexpr char xapian_prefix() const {
		/* xapian uses uppercase shortcuts; toupper is not constexpr */
		return shortcut == 0 ? 0 : shortcut - ('a' - 'A');
	}

	/**
	 * Get the xapian term; truncated to MaxTermLength and
	 * utf8-flattened.
	 *
	 * @param s
	 *
	 * @return the xapian term
	 */
	std::string xapian_term(const std::string& s="") const;
	std::string xapian_term(std::string_view sv) const {
		return xapian_term(std::string{sv});
	}
	std::string xapian_term(char c) const {
		return xapian_term(std::string(1, c));
	}
};

// equality
static inline constexpr bool operator==(const Field& f1, const Field& f2) { return f1.id == f2.id; }
static inline constexpr bool operator==(const Field& f1, const Field::Id id) { return f1.id == id; }


MU_ENABLE_BITOPS(Field::Flag);

/**
 * Sequence of _all_ message fields
 */
static constexpr std::array<Field, Field::id_size()>
    Fields = {
	{
	    {
		Field::Id::Bcc,
		Field::Type::ContactList,
		"bcc", {},
		"Blind carbon-copy recipient",
		"bcc:foo@example.com",
		'h',
		Field::Flag::Contact |
		Field::Flag::Value |
		Field::Flag::IncludeInSexp |
		Field::Flag::NormalTerm |
		Field::Flag::PhrasableTerm,
	    },
	    {
		Field::Id::BodyText,
		Field::Type::String,
		"body", {},
		"Message plain-text body",
		"body:capybara",
		'b',
		Field::Flag::PhrasableTerm,
	    },
	    {
		Field::Id::Cc,
		Field::Type::ContactList,
		"cc", {},
		"Carbon-copy recipient",
		"cc:quinn@example.com",
		'c',
		Field::Flag::Contact |
		Field::Flag::Value |
		Field::Flag::IncludeInSexp |
		Field::Flag::NormalTerm |
		Field::Flag::PhrasableTerm,
	    },
	    {
		Field::Id::Changed,
		Field::Type::TimeT,
		"changed", {},
		"Last change time",
		"changed:30M..",
		'k',
		Field::Flag::Value |
		Field::Flag::Range |
		Field::Flag::IncludeInSexp
	    },
	    {
		Field::Id::Date,
		Field::Type::TimeT,
		"date", {},
		"Message date",
		"date:20220101..20220505",
		'd',
		Field::Flag::Value |
		Field::Flag::Range |
		Field::Flag::IncludeInSexp
	    },
	    {
		Field::Id::EmbeddedText,
		Field::Type::String,
		"embed", {},
		"Embedded text",
		"embed:war OR embed:peace",
		'e',
		Field::Flag::PhrasableTerm
	    },
	    {
		Field::Id::File,
		Field::Type::String,
		"file", {},
		"Attachment file name",
		"file:/image\\.*.jpg/",
		'j',
		Field::Flag::BooleanTerm
	    },
	    {
		Field::Id::Flags,
		Field::Type::Integer,
		"flags", "flag",
		"Message properties",
		"flag:unread AND flag:personal",
		'g',
		Field::Flag::BooleanTerm |
		Field::Flag::Value |
		Field::Flag::IncludeInSexp
	    },
	    {
		Field::Id::From,
		Field::Type::ContactList,
		"from", {},
		"Message sender",
		"from:jimbo",
		'f',
		Field::Flag::Contact |
		Field::Flag::Value |
		Field::Flag::IncludeInSexp |
		Field::Flag::NormalTerm |
		Field::Flag::PhrasableTerm,
	    },
	    {
		Field::Id::Language,
		Field::Type::String,
		"language", "lang",
		"ISO 639-1 language code for body",
		"lang:nl",
		'a',
		Field::Flag::BooleanTerm |
		Field::Flag::Value |
		Field::Flag::IncludeInSexp
	    },
	    {
		Field::Id::Maildir,
		Field::Type::String,
		"maildir", {},
		"Maildir path for message",
		"maildir:/private/archive",
		'm',
		Field::Flag::BooleanTerm |
		Field::Flag::Value |
		Field::Flag::IncludeInSexp
	    },
	    {
		Field::Id::MailingList,
		Field::Type::String,
		"list", {},
		"Mailing list (List-Id:)",
		"list:mu-discuss.example.com",
		'v',
		Field::Flag::BooleanTerm |
		Field::Flag::Value |
		Field::Flag::IncludeInSexp
	    },
	    {
		Field::Id::MessageId,
		Field::Type::String,
		"message-id", "msgid",
		"Message-Id",
		"msgid:abc@123",
		'i',
		Field::Flag::BooleanTerm |
		Field::Flag::Value |
		Field::Flag::IncludeInSexp
	    },
	    {
		Field::Id::MimeType,
		Field::Type::String,
		"mime", "mime-type",
		"Attachment MIME-type",
		"mime:image/jpeg",
		'y',
		Field::Flag::BooleanTerm
	    },
	    {
		Field::Id::Path,
		Field::Type::String,
		"path", {},
		"File system path to message",
		"path:/a/b/Maildir/cur/msg:2,S",
		'l',
		Field::Flag::BooleanTerm |
		Field::Flag::Value |
		Field::Flag::IncludeInSexp
	    },
	    {
		Field::Id::Priority,
		Field::Type::Integer,
		"priority", "prio",
		"Priority",
		"prio:high",
		'p',
		Field::Flag::BooleanTerm |
		Field::Flag::Value |
		Field::Flag::IncludeInSexp
	    },
	    {
		Field::Id::References,
		Field::Type::StringList,
		"references", "ref",
		"References to related messages",
		"ref:E1rQJDx123@example.com",
		'r',
		Field::Flag::Value |
		Field::Flag::BooleanTerm |
		Field::Flag::IncludeInSexp
	    },
	    {
		Field::Id::Size,
		Field::Type::ByteSize,
		"size", {},
		"Message size in bytes",
		"size:1M..5M",
		'z',
		Field::Flag::Value |
		Field::Flag::Range |
		Field::Flag::IncludeInSexp
	    },
	    {
		Field::Id::Subject,
		Field::Type::String,
		"subject", {},
		"Message subject",
		"subject:wombat",
		's',
		Field::Flag::Value |
		Field::Flag::IncludeInSexp |
		Field::Flag::NormalTerm |
		Field::Flag::PhrasableTerm
	    },
	    {
		Field::Id::Tags,
		Field::Type::StringList,
		"tags", "tag",
		"Message tags",
		"tag:projectx",
		'x',
		Field::Flag::BooleanTerm |
		Field::Flag::Value |
		Field::Flag::IncludeInSexp
	    },
	    {
		Field::Id::ThreadId,
		Field::Type::String,
		"thread", {},
		"Thread a message belongs to",
		"thread:abcde789@example.com",
		'w',
		Field::Flag::BooleanTerm |
		Field::Flag::Value
	    },
	    {
		Field::Id::To,
		Field::Type::ContactList,
		"to", {},
		"Message recipient",
		"to:flimflam@example.com",
		't',
		Field::Flag::Contact |
		Field::Flag::Value |
		Field::Flag::IncludeInSexp |
		Field::Flag::NormalTerm |
		Field::Flag::PhrasableTerm,
	    },
	}};

/*
 * Convenience
 */

/**
 * Get the message field for the given Id.
 *
 * @param id of the message field
 *
 * @return ref of the message field.
 */
constexpr const Field&
field_from_id(Field::Id id)
{
	return Fields.at(static_cast<size_t>(id));
}

/**
 * Invoke func for each message-field
 *
 * @param func some callable
 */
template <typename Func>
constexpr void field_for_each(Func&& func) {
	for (const auto& field: Fields)
		func(field);
}

/**
 * Find a message field that satisfies some predicate
 *
 * @param pred the predicate (a callable)
 *
 * @return a message-field id, or nullopt if not found.
 */
template <typename Pred>
constexpr Option<Field> field_find_if(Pred&& pred) {
	for (auto&& field: Fields)
		if (pred(field))
			return field;
	return Nothing;
}

/**
 * Get the the message-field for the given name or shortcut
 *
 * @param name_or_shortcut
 *
 * @return the message-field or Nothing
 */
static inline
Option<Field> field_from_shortcut(char shortcut) {
	return field_find_if([&](auto&& field){
		return field.shortcut == shortcut;
	});
}
static inline
Option<Field> field_from_name(const std::string& name) {
	switch(name.length()) {
	case 0:
		return Nothing;
	case 1:
		return field_from_shortcut(name[0]);
	default:
		return field_find_if([&](auto&& field){
			return name == field.name || name == field.alias;
		});
	}
}

using FieldsVec = std::vector<Field>; /**< Vec of fields */

/**
 * Describe a combination-field, i.e., a pseudo-field that
 * expands to some real fields, as a query shortcut.
 */
struct CombiField {
	std::string_view name;	/**< name of the combi-field */
	FieldsVec fields;	/**< fields this resolves to */
};
using CombiFields = std::vector<CombiField>; /**< vec of combi-fields */

/**
 * Get information about the combination fields
 *
 * @return vector with information.
 */
const CombiFields&  combi_fields();

/**
 * Return combination-fields such
 * as "contact", "recip" and "" (empty)
 *
 * @param name combination field name
 *
 * @return list of matching fields
 */
const FieldsVec& fields_from_name(const std::string& name);

/**
 * Is the field a combination field?
 *
 * @param name name of the field
 *
 * @return true or false
 */
bool field_is_combi (const std::string& name);

/**
 * Get the Field::Id for some number, or nullopt if it does not match
 *
 * @param id an id number
 *
 * @return Field::Id  or nullopt
 */
static inline
Option<Field> field_from_number(size_t id)
{
	if (id >= static_cast<size_t>(Field::Id::_count_))
		return Nothing;
	else
		return field_from_id(static_cast<Field::Id>(id));
}


} // namespace Mu
#endif /* MU_FIELDS_HH__ */
