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

#ifndef MU_MESSAGE_FIELDS_HH__
#define MU_MESSAGE_FIELDS_HH__

#include <cstdint>
#include <string_view>
#include <algorithm>
#include <array>
#include <optional>
#include <xapian.h>
#include <utils/mu-utils.hh>

namespace Mu {

struct MessageField {
	/**
	 * Field Ids.
	 *
	 * Note, the Ids are also used as indices in the MessageFields array,
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
		Uid,          /**< Unique id for message (based on path) */
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
		_count_ /**< Number of MessageFieldIds */
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
		String,     /**< String */
		StringList, /**< List of strings */
		ByteSize,   /**< Size in bytes */
		TimeT,      /**< A time_t value */
		Integer,    /**< An integer */
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
	 */

	enum struct Flag {
		GMime	      = 1 << 0,
		/**< Field retrieved through gmime */
		FullText      = 1 << 1,
		/**< Field-text is indexed in xapian (i.e., the text is processed */
		Searchable    = 1 << 2,
		/**< Field is a searchable term */
		Value	      = 1 << 3,
		/**< Field value is stored (so the literal value can be retrieved) */
		Contact	      = 1 << 4,
		/**< field contains one or more * e-mail-addresses */
		XapianBoolean = 1 << 5,
		/**< use 'add_boolean_prefix' for Xapian	queries; wildcards do NOT WORK for such
		 * fields */
		DoNotCache    = 1 << 6,
		/**< don't cache this field in * the MuMsg cache */
		Range	      = 1 << 7	/**< whether this is a range field (e.g., date, size)*/
	};

	constexpr bool any_of(Flag some_flag) const{
		return (static_cast<int>(some_flag) & static_cast<int>(flags)) != 0;
	}

	constexpr bool is_gmime() const { return any_of(Flag::Value); }
	constexpr bool is_full_text() const { return any_of(Flag::FullText); }
	constexpr bool is_searchable() const { return any_of(Flag::Searchable); }
	constexpr bool is_value() const { return any_of(Flag::Value); }
	constexpr bool is_contact() const { return any_of(Flag::Contact); }
	constexpr bool is_xapian_boolean() const { return any_of(Flag::XapianBoolean); }
	constexpr bool is_range() const { return any_of(Flag::Range); }
	constexpr bool do_not_cache() const { return any_of(Flag::DoNotCache); }

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

	std::string xapian_term(const std::string& s="") const;
	std::string xapian_term(std::string_view sv) const;
	std::string xapian_term(char c) const;
};

MU_ENABLE_BITOPS(MessageField::Flag);

/**
 * Sequence of _all_ message fields
 */
static constexpr std::array<MessageField, MessageField::id_size()>
    MessageFields = {
	{

	    // Bcc
	    {
		MessageField::Id::Bcc,
		MessageField::Type::String,
		"bcc",
		"Blind carbon-copy recipient",
		"bcc:foo@example.com",
		'h',
		MessageField::Flag::GMime | MessageField::Flag::Contact |
		MessageField::Flag::Value},
	    // HTML Body
	    {
		MessageField::Id::BodyHtml,
		MessageField::Type::String,
		"body",
		"Message html body",
		{},
		{},
		MessageField::Flag::GMime | MessageField::Flag::DoNotCache},
	    // Body
	    {
		MessageField::Id::BodyText,
		MessageField::Type::String,
		"body",
		"Message plain-text body",
		"body:capybara", // example
		'b',
		MessageField::Flag::GMime | MessageField::Flag::FullText |
		MessageField::Flag::DoNotCache},

	    // Cc
	    {
		MessageField::Id::Cc,
		MessageField::Type::String,
		"cc",
		"Carbon-copy recipient",
		"cc:quinn@example.com",
		'c',
		MessageField::Flag::GMime | MessageField::Flag::Contact |
		MessageField::Flag::Value},

	    // Embed
	    {
		MessageField::Id::EmbeddedText,
		MessageField::Type::String,
		"embed",
		"Embedded text",
		"embed:war OR embed:peace",
		'e',
		MessageField::Flag::GMime | MessageField::Flag::FullText |
		MessageField::Flag::DoNotCache},
	    // File
	    {
		MessageField::Id::File,
		MessageField::Type::String,
		"file",
		"Attachment file name",
		"file:/image\\.*.jpg/",
		'j',
		MessageField::Flag::GMime | MessageField::Flag::Searchable |
		MessageField::Flag::DoNotCache},

	    // From
	    {
		MessageField::Id::From,
		MessageField::Type::String,
		"from",
		"Message sender",
		"from:jimbo",
		'f',
		MessageField::Flag::GMime | MessageField::Flag::Contact |
		MessageField::Flag::Value},
	    // Maildir
	    {
		MessageField::Id::Maildir,
		MessageField::Type::String,
		"maildir",
		"Maildir path for message",
		"maildir:/private/archive",
		'm',
		MessageField::Flag::GMime | MessageField::Flag::Searchable |
		MessageField::Flag::Value},
	    // MIME
	    {
		MessageField::Id::Mime,
		MessageField::Type::String,
		"mime",
		"Attachment MIME-type",
		"mime:image/jpeg",
		'y',
		MessageField::Flag::Searchable},
	    // Message-ID
	    {
		MessageField::Id::MessageId,
		MessageField::Type::String,
		"msgid",
		"Attachment MIME-type",
		"mime:image/jpeg",
		'i',
		MessageField::Flag::GMime |
		MessageField::Flag::Searchable |
		MessageField::Flag::Value},
	    // Path
	    {
		MessageField::Id::Path,
		MessageField::Type::String,
		"path",
		"File system path to message",
		{},
		'p',
		MessageField::Flag::GMime |
		MessageField::Flag::XapianBoolean |
		MessageField::Flag::Value},

	    // Subject
	    {
		MessageField::Id::Subject,
		MessageField::Type::String,
		"subject",
		"Message subject",
		"subject:wombat",
		's',
		MessageField::Flag::GMime |
		MessageField::Flag::Searchable |
		MessageField::Flag::Value |
		MessageField::Flag::FullText},

	    // To
	    {
		MessageField::Id::To,
		MessageField::Type::String,
		"to",
		"Message recipient",
		"to:flimflam@example.com",
		't',
		MessageField::Flag::GMime | MessageField::Flag::Contact |
		MessageField::Flag::Value},

	    // UID (internal)
	    {
		MessageField::Id::Uid,
		MessageField::Type::String,
		"uid",
		"Message recipient",
		{},
		'u',
		MessageField::Flag::Searchable},

	    // References
	    {
		MessageField::Id::References,
		MessageField::Type::StringList,
		"refs",
		"Message references to other messages",
		{},
		'r',
		MessageField::Flag::GMime | MessageField::Flag::Value},

	    // Tags
	    {
		MessageField::Id::Tags,
		MessageField::Type::StringList,
		"tag",
		"Message tags",
		"tag:projectx",
		'x',
		MessageField::Flag::GMime | MessageField::Flag::Searchable |
		MessageField::Flag::Value},

	    // Date
	    {
		MessageField::Id::Date,
		MessageField::Type::TimeT,
		"date",
		"Message date",
		"date:20220101..20220505",
		'd',
		MessageField::Flag::GMime | MessageField::Flag::Searchable |
		    MessageField::Flag::Value | MessageField::Flag::XapianBoolean |
		    MessageField::Flag::Range},

	    // Flags
	    {
		MessageField::Id::Flags,
		MessageField::Type::Integer,
		"flag",
		"Message properties",
		"flag:unread",
		'g',
		MessageField::Flag::GMime | MessageField::Flag::Searchable |
		    MessageField::Flag::Value},
	    // Priority
	    {
		MessageField::Id::Priority,
		MessageField::Type::Integer,
		"prio",
		"Priority",
		"prio:high",
		'p',
		MessageField::Flag::GMime |
		MessageField::Flag::Searchable |
		MessageField::Flag::Value},

	    // Size
	    {
		MessageField::Id::Size,
		MessageField::Type::ByteSize,
		"size",
		"Message size in bytes",
		"size:1M..5M",
		'z',
		MessageField::Flag::GMime |
		MessageField::Flag::Searchable |
		MessageField::Flag::Value |
		MessageField::Flag::Range},

	    // Mailing List
	    {
		MessageField::Id::MailingList,
		MessageField::Type::String,
		"list",
		"Mailing list (List-Id:)",
		"list:mu-discuss.googlegroups.com",
		'v',
		MessageField::Flag::GMime | MessageField::Flag::Searchable |
		MessageField::Flag::Value},

	    // ThreadId
	    {
		MessageField::Id::ThreadId,
		MessageField::Type::String,
		"thread",
		"Thread a message belongs to",
		{},
		'w',
		MessageField::Flag::Searchable},
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
constexpr const MessageField&
message_field(MessageField::Id id)
{
	return MessageFields.at(static_cast<size_t>(id));
}

/**
 * Invoke func for each message-field
 *
 * @param func some callable
 */
template <typename Func>
void message_field_for_each(Func&& func) {
	for (const auto& field: MessageFields)
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
std::optional<MessageField::Id> message_field_find_if(Pred&& pred) {
	for (auto&& field: MessageFields)
		if (pred(field))
			return field.id;
	return std::nullopt;
}

/**
 * Get the the message-field id for the given name or shortcut
 *
 * @param name_or_shortcut
 *
 * @return the message-field-id or nullopt.
 */
static inline
std::optional<MessageField::Id> message_field_id(char shortcut) {
	return message_field_find_if([&](auto&& field ){
		return field.shortcut == shortcut;
	});
}
static inline
std::optional<MessageField::Id> message_field_id(const std::string& name) {
	if (name.length() == 1)
		return message_field_id(name[0]);
	else
		return message_field_find_if([&](auto&& field){
			return field.name == name;
	});
}

/**
 * Get the MessageField::Id for some	number, or nullopt if it does not match
 *
 * @param id an id number
 *
 * @return MessageField::Id  or nullopt
 */
static inline
std::optional<MessageField::Id> message_field_id(size_t id)
{
	if (id >= static_cast<size_t>(MessageField::Id::_count_))
		return std::nullopt;
	else
		return static_cast<MessageField::Id>(id);
}


} // namespace Mu
#endif /* MU_MESSAGE_FIELDS_HH__ */
