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

#ifndef MU_FIELDS_HH__
#define MU_FIELDS_HH__

#include <cstdint>
#include <string_view>
#include <algorithm>
#include <array>
#include <xapian.h>
#include <utils/mu-utils.hh>
#include <utils/mu-option.hh>

namespace Mu {

struct Field {
	/**
	 * Field Ids.
	 *
	 * Note, the Ids are also used as indices in the Fields array,
	 * so their numerical values must be 0...Count.
	 *
	 */
	enum struct Id {
		/*
		 * first all the string-based ones
		 */
		Bcc = 0,      /**< Blind Carbon-Copy */
		BodyHtml,     /**< HTML Body */
		BodyText,     /**< Text body */
		Cc,           /**< Carbon-Copy */
		EmbeddedText, /**< Embedded text in message */
		File,         /**< Filename */
		From,         /**< Message sender */
		Maildir,      /**< Maildir path */
		Mime,         /**< MIME-Type */
		MessageId,    /**< Message Id */
		Path,         /**< File-system Path */
		Subject,      /**< Message subject */
		To,           /**< To: recipient */
		/*
		 * string list items...
		 */
		References, /**< All references (incl. Reply-To:) */
		Tags,       /**< Message Tags */
		/*
		 * then the numerical ones
		 */
		Date,     /**< Message date */
		Flags,    /**< Message flags */
		Priority, /**< Message priority */
		Size,     /**< Message size (in bytes) */

		/* add new ones here... */
		MailingList, /**< Mailing list */
		ThreadId,    /**< Thread Id */

		/*
		 * <private>
		 */
		_count_ /**< Number of FieldIds */
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
	 * must be in the value (at least when using MuMsgIter)
	 *
	 * Rules (build-time enforced):
	 * - A field has at most one of Indexable, HasTerms, IsXapianBoolean and IsContact.
	 */

	enum struct Flag {
		/*
		 * Different kind of terms; at most one is true,
		 * and cannot be combined with IsContact. Compile-time enforced.
		 */
		NormalTerm    = 1 << 0,
		/**< Field is a searchable term */
		BooleanTerm   = 1 << 1,
		/**< Field is a boolean search-term (i.e. at most one per message);
		 * wildcards do not work */
		IndexableTerm = 1 << 2,
		/**< Field has indexable text as term */
		/*
		 * Contact flag cannot be combined with any of the term flags.
		 * This is compile-time enforced.
		 */
		Contact = 1 << 10,
		/**< field contains one or more e-mail-addresses */
		Value   = 1 << 11,
		/**< Field value is stored (so the literal value can be retrieved) */

		DoNotCache = 1 << 20,
		/**< don't cache this field in * the MuMsg cache */
		Range	   = 1 << 21
		/**< whether this is a range field (e.g., date, size)*/
	};

	constexpr bool any_of(Flag some_flag) const{
		return (static_cast<int>(some_flag) & static_cast<int>(flags)) != 0;
	}

	constexpr bool is_indexable_term()	const { return any_of(Flag::IndexableTerm); }
	constexpr bool is_boolean_term()	const { return any_of(Flag::BooleanTerm); }
	constexpr bool is_normal_term()		const { return any_of(Flag::NormalTerm); }
	constexpr bool is_searchable()          const { return is_indexable_term() ||
							       is_boolean_term() ||
							       is_normal_term(); }

	constexpr bool is_value()		const { return any_of(Flag::Value); }

	constexpr bool is_contact()		const { return any_of(Flag::Contact); }
	constexpr bool is_range()		const { return any_of(Flag::Range); }

	/**
	 * Field members
	 *
	 */
	Id               id;            /**< Id of the message field */
	Type             type;          /**< Type of the message field */
	std::string_view name;          /**< Name of the message field */
	std::string_view description;   /**< Decription of the message field */
	std::string_view example_query; /**< Example query */
	char             shortcut;      /**< Shortcut for the message field; a..z */
	Flag             flags;         /**< Flags */

	/**
	 * Convenience / helpers
	 *
	 */

	constexpr char xapian_prefix() const
	{ /* xapian uses uppercase shortcuts; toupper is not constexpr */
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

MU_ENABLE_BITOPS(Field::Flag);

/**
 * Sequence of _all_ message fields
 */
static constexpr std::array<Field, Field::id_size()>
    Fields = {
	{
	    // Bcc
	    {
		Field::Id::Bcc,
		Field::Type::ContactList,
		"bcc",
		"Blind carbon-copy recipient",
		"bcc:foo@example.com",
		'h',
		Field::Flag::Contact |
		Field::Flag::Value
	    },
	    // HTML Body
	    {
		Field::Id::BodyHtml,
		Field::Type::String,
		"body",
		"Message html body",
		{},
		{},
		{}
	    },
	    // Body
	    {
		Field::Id::BodyText,
		Field::Type::String,
		"body",
		"Message plain-text body",
		"body:capybara", // example
		'b',
		Field::Flag::IndexableTerm,
	    },
	    // Cc
	    {
		Field::Id::Cc,
		Field::Type::ContactList,
		"cc",
		"Carbon-copy recipient",
		"cc:quinn@example.com",
		'c',
		Field::Flag::Contact |
		Field::Flag::Value
	    },
	    // Embed
	    {
		Field::Id::EmbeddedText,
		Field::Type::String,
		"embed",
		"Embedded text",
		"embed:war OR embed:peace",
		'e',
		Field::Flag::IndexableTerm
	    },
	    // File
	    {
		Field::Id::File,
		Field::Type::String,
		"file",
		"Attachment file name",
		"file:/image\\.*.jpg/",
		'j',
		Field::Flag::NormalTerm
	    },
	    // From
	    {
		Field::Id::From,
		Field::Type::ContactList,
		"from",
		"Message sender",
		"from:jimbo",
		'f',
		Field::Flag::Contact |
		Field::Flag::Value
	    },
	    // Maildir
	    {
		Field::Id::Maildir,
		Field::Type::String,
		"maildir",
		"Maildir path for message",
		"maildir:/private/archive",
		'm',
		Field::Flag::BooleanTerm |
		Field::Flag::Value
	    },
	    // MIME
	    {
		Field::Id::Mime,
		Field::Type::String,
		"mime",
		"Attachment MIME-type",
		"mime:image/jpeg",
		'y',
		Field::Flag::NormalTerm
	    },
	    // Message-ID
	    {
		Field::Id::MessageId,
		Field::Type::String,
		"msgid",
		"Attachment MIME-type",
		"msgid:abc@123",
		'i',
		Field::Flag::BooleanTerm |
		Field::Flag::Value
	    },
	    // Path
	    {
		Field::Id::Path,
		Field::Type::String,
		"path",
		"File system path to message",
		{},
		'l',
		Field::Flag::BooleanTerm |
		Field::Flag::Value
	    },
	    // Subject
	    {
		Field::Id::Subject,
		Field::Type::String,
		"subject",
		"Message subject",
		"subject:wombat",
		's',
		Field::Flag::Value |
		Field::Flag::IndexableTerm
	    },
	    // To
	    {
		Field::Id::To,
		Field::Type::ContactList,
		"to",
		"Message recipient",
		"to:flimflam@example.com",
		't',
		Field::Flag::Contact |
		Field::Flag::Value
	    },
	    // References
	    {
		Field::Id::References,
		Field::Type::StringList,
		"refs",
		"Message references to other messages",
		{},
		'r',
		Field::Flag::Value
	    },
	    // Tags
	    {
		Field::Id::Tags,
		Field::Type::StringList,
		"tag",
		"Message tags",
		"tag:projectx",
		'x',
		Field::Flag::NormalTerm |
		Field::Flag::Value
	    },
	    // Date
	    {
		Field::Id::Date,
		Field::Type::TimeT,
		"date",
		"Message date",
		"date:20220101..20220505",
		'd',
		Field::Flag::Value |
		Field::Flag::Range
	    },
	    // Flags
	    {
		Field::Id::Flags,
		Field::Type::Integer,
		"flag",
		"Message properties",
		"flag:unread",
		'g',
		Field::Flag::NormalTerm |
		Field::Flag::Value
	    },
	    // Priority
	    {
		Field::Id::Priority,
		Field::Type::Integer,
		"prio",
		"Priority",
		"prio:high",
		'p',
		Field::Flag::BooleanTerm |
		Field::Flag::Value
	    },
	    // Size
	    {
		Field::Id::Size,
		Field::Type::ByteSize,
		"size",
		"Message size in bytes",
		"size:1M..5M",
		'z',
		Field::Flag::Value |
		Field::Flag::Range
	    },
	    // Mailing List
	    {
		Field::Id::MailingList,
		Field::Type::String,
		"list",
		"Mailing list (List-Id:)",
		"list:mu-discuss.googlegroups.com",
		'v',
		Field::Flag::BooleanTerm |
		Field::Flag::Value
	    },
	    // ThreadId
	    {
		Field::Id::ThreadId,
		Field::Type::String,
		"thread",
		"Thread a message belongs to",
		{},
		'w',
		Field::Flag::BooleanTerm |
		Field::Flag::Value
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
 * Get the the message-field id for the given name or shortcut
 *
 * @param name_or_shortcut
 *
 * @return the message-field-id or nullopt.
 */
static inline
Option<Field> field_from_shortcut(char shortcut) {
	return field_find_if([&](auto&& field){
		return field.shortcut == shortcut;
	});
}
static inline
Option<Field> field_from_name(const std::string& name) {
	if (name.length() == 1)
		return field_from_shortcut(name[0]);
	else
		return field_find_if([&](auto&& field){
			return field.name == name;
	});
}

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
