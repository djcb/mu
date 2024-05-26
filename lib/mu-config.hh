/*
** Copyright (C) 2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef MU_CONFIG_HH__
#define MU_CONFIG_HH__

#include <cstdint>
#include <cinttypes>
#include <string_view>
#include <string>
#include <array>
#include <vector>
#include <variant>
#include <unordered_map>

#include "mu-xapian-db.hh"

#include <utils/mu-utils.hh>
#include <utils/mu-result.hh>
#include <utils/mu-option.hh>

namespace Mu {

struct Property {
	enum struct Id {
		BatchSize,	/**< Xapian batch-size */
		Contacts,       /**< Cache of contact information */
		Created,        /**<  Time of creation */
		IgnoredAddresses,/**< Email addresses ignored for the contacts-cache */
		LastChange,	/**< Time of last change */
		LastIndex,	/**< Time of last index */
		MaxMessageSize,	/**< Maximum message size (in bytes) */
		PersonalAddresses,	/**< List of personal e-mail addresses */
		RootMaildir,	/**< Root maildir path */
		SchemaVersion,	/**< Xapian DB schema version */
		SupportNgrams,  /**< Support ngrams for indexing & querying
				 *  for e.g. CJK languages */
		/* <private> */
		_count_		/* Number of Ids */
	};

	static constexpr size_t id_size = static_cast<size_t>(Id::_count_);
	/**< Number of Property::Ids */

	enum struct Flags {
		None	     = 0,	/**< Nothing in particular */
		ReadOnly     = 1 << 0, /**< Property is read-only for external use
					* (but can change from within the store) */
		Configurable = 1 << 1,	/**< A user-configurable parameter; name
					 * starts with 'conf-' */
		Internal     = 1 << 2,	/**< Mu-internal field */
	};
	enum struct Type {
		Boolean,        /**< Some boolean value */
		Number,		/**< Some number */
		Timestamp,      /**< Timestamp number */
		Path,           /**< Path string */
		String,		/**< A string */
		StringList,	/**< A list of strings */
	};

	using Value = std::variant<int64_t, std::string, std::vector<std::string> >;

	Id			id;
	Type			type;
	Flags			flags;
	std::string_view	name;
	std::string_view        default_val;
	std::string_view	description;
};

MU_ENABLE_BITOPS(Property::Flags);

class Config {
public:
	using	Id    = Property::Id;
	using	Type  = Property::Type;
	using	Flags = Property::Flags;
	using	Value = Property::Value;

	static constexpr std::array<Property, Property::id_size>
	properties    = {{
		{
			Id::BatchSize,
			Type::Number,
			Flags::Configurable,
			"batch-size",
			"50000",
			"Maximum number of changes in a database transaction"
		},
		{
			Id::Contacts,
			Type::String,
			Flags::Internal,
			"contacts",
			{},
			"Serialized contact information"
		},
		{
			Id::Created,
			Type::Timestamp,
			Flags::ReadOnly,
			MetadataIface::created_key,
			{},
			"Database creation time"
		},
		{
			Id::IgnoredAddresses,
			Type::StringList,
			Flags::Configurable,
			"ignored-addresses",
			{},
			"E-mail addresses ignored  for the contacts-cache, "
			"literal or /regexp/"
		},
		{
			Id::LastChange,
			Type::Timestamp,
			Flags::ReadOnly,
			MetadataIface::last_change_key,
			{},
			"Time when last change occurred"
		},
		{
			Id::LastIndex,
			Type::Timestamp,
			Flags::ReadOnly,
			"last-index",
			{},
			"Time when last indexing operation was completed"
		},
		{
			Id::MaxMessageSize,
			Type::Number,
			Flags::Configurable,
			"max-message-size",
			"100000000", // default max: 100M bytes
			"Maximum message size (in bytes); bigger messages are skipped"
		},
		{
			Id::PersonalAddresses,
			Type::StringList,
			Flags::Configurable,
			"personal-addresses",
			{},
			"Personal e-mail addresses, literal or /regexp/"
		},
		{
			Id::RootMaildir,
			Type::Path,
			Flags::ReadOnly,
			"root-maildir",
			{},
			"Absolute path of the top of the Maildir tree"
		},
		{
			Id::SchemaVersion,
			Type::Number,
			Flags::ReadOnly,
			"schema-version",
			{},
			"Version of the Xapian database schema"
		},
		{
			Id::SupportNgrams,
			Type::Boolean,
			Flags::Configurable,
			"support-ngrams",
			{},
			"Support n-grams for working with CJK and other languages"
		},
	}};

	/**
	 * Construct a new Config object.
	 *
	 * @param db The config-store (database); must stay valid for the
	 * lifetime of this config.
	 */
	Config(MetadataIface& cstore): cstore_{cstore}{}

	/**
	 * Get the property by its id
	 *
	 * @param id a property id (!= Id::_count_)
	 *
	 * @return the property
	 */
	template <Id ID>
	constexpr static const Property& property() {
		return properties[static_cast<size_t>(ID)];
	}

	/**
	 * Get a Property by its name.
	 *
	 * @param name The name
	 *
	 * @return the property or Nothing if not found
	 */
	static Option<const Property&> property(const std::string& name) {
		const auto pname{std::string_view(name.data(), name.size())};
		for (auto&& prop: properties)
			if (prop.name == pname)
				return prop;
		return Nothing;
	}

	/**
	 * Get the property value of the correct type
	 *
	 * @param prop_id a property id
	 *
	 * @return the value or Nothing
	 */
	template<Id ID>
	auto get() const {
		constexpr auto&& prop{property<ID>()};
		const auto str = std::invoke([&]()->std::string {
			const auto str = cstore_.metadata(std::string{prop.name});
			return str.empty() ? std::string{prop.default_val} : str;
		});
		if constexpr (prop.type == Type::Number)
			return static_cast<size_t>(str.empty() ? 0 : std::atoll(str.c_str()));
		if constexpr (prop.type == Type::Boolean)
			return static_cast<size_t>(str.empty() ? false :
						   std::atol(str.c_str()) != 0);
		else if constexpr (prop.type == Type::Timestamp)
			return static_cast<time_t>(str.empty() ? 0 : std::atoll(str.c_str()));
		else if constexpr (prop.type == Type::Path || prop.type == Type::String)
			return str;
		else if constexpr (prop.type == Type::StringList)
			return split(str, SepaChar1);

		throw std::logic_error("invalid prop " + std::string{prop.name});
	}

	/**
	 * Set a new value for some property
	 *
	 * @param prop_id property-id
	 * @param val the new value (of the correct type)
	 *
	 * @return Ok() or some error
	 */
	template<Id ID, typename T>
	Result<void> set(const T& val) {
		constexpr auto&& prop{property<ID>()};
		if (read_only())
			return Err(Error::Code::AccessDenied,
				   "cannot write to read-only db");

		const auto strval = std::invoke([&]{
			if constexpr (prop.type == Type::Number || prop.type == Type::Timestamp)
				return mu_format("{}", static_cast<int64_t>(val));
			if constexpr (prop.type == Type::Boolean)
				return val ? "1" : "0";
			else if constexpr (prop.type == Type::Path || prop.type == Type::String)
				return std::string{val};
			else if constexpr (prop.type == Type::StringList)
				return join(val, SepaChar1);
			else
				throw std::logic_error("invalid prop " + std::string{prop.name});
		});

		cstore_.set_metadata(std::string{prop.name}, strval);
		return Ok();
	}

	/**
	 * Is this a read-only Config?
	 *
	 *
	 * @return true or false
	 */
	bool read_only() const { return cstore_.read_only();};

	/**
	 * Import configurable settings to some other MetadataIface
	 *
	 * @param target some other metadata interface
	 */
	void import_configurable(const Config& src) const {
		for (auto&& prop: properties) {
			if (any_of(prop.flags & Flags::Configurable)) {
				const auto&& key{std::string{prop.name}};
				if (auto&& val{src.cstore_.metadata(key)}; !val.empty())
					cstore_.set_metadata(key, std::string{val});
			}
		}
	}

private:
	MetadataIface& cstore_;
};


} // namespace Mu

#endif /* MU_CONFIG_DB_HH__ */
