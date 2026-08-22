/*
** Copyright (C) 2023-2026 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "config.h"
#include "mu-cmd.hh"

#include <ranges>
#include <array>

#include <message/mu-message.hh>
#include "utils/mu-utils.hh"
#include "utils/mu-tabula.hh"

#include <glib.h>
#include <gmime/gmime.h>
#include <fmt/ostream.h>

using namespace Mu;
using namespace Mu::Tabula;

//template <> struct fmt::formatter<Table> : ostream_formatter {};

static void
colorify(Table& table, const Options& opts)
{
	if (opts.nocolor || table.rows.size() == 0)
		return;

	using enum fmt::color;
	constexpr auto colors = std::to_array<fmt::color>({
		green, blue, magenta, yellow, green, blue, magenta, yellow, gray,
	});

	for (auto [rownum, row]: table.rows | std::views::enumerate) {
		fmt::text_style ts{rownum == 0 ? fmt::emphasis::bold : fmt::text_style{}};
		for (auto [cellnum, cell]: row.cells | std::views::enumerate) {
			cell.ts = ts;
			if (rownum != 0)
				cell.ts |= fmt::fg(colors.at(cellnum % colors.size()));
		}
	}
}

static Result<void>
topic_fields(const Options& opts)
{
	using namespace std::string_literals;

	Table fields;
	fields.append(Row{"field-name", "alias", "short", "search",
			"value", "sexp", "example query", "description"});

	auto searchable=[&](const Field& field)->std::string {
		if (field.is_boolean_term())
			return "boolean";
		if (field.is_phrasable_term())
			return "phrase";
		if (field.is_contact())
			return "contact";
		if (field.is_range())
			return "range";
		return "no";
	};

	size_t row{};
	field_for_each([&](auto&& field){

		if (field.is_internal())
			return; // skip.

		fields.append(Row{mu_format("{}", field.name),
				  field.alias.empty() ? "" : mu_format("{}", field.alias),
				  field.shortcut ? mu_format("{}", field.shortcut) : ""s,
				  searchable(field),
				  field.is_value() ? "yes" : "no",
				  field.include_in_sexp() ? "yes" : "no",
				  field.example_query,
				  field.description});
		++row;
	});

	colorify(fields, opts);


	mu_println("# Message fields\n{}\n", fields);

	return Ok();
}

static Result<void>
topic_combi_fields(const Options& opts)
{
	using namespace std::string_literals;

	Table fields;
	fields.append(Row{"combi-field", "fields"});

	std::ranges::for_each(combi_fields(), [&](const auto& cfield) {

		std::string fnames;
		std::ranges::for_each(cfield.fields, [&](auto&& field) {
			if (!fnames.empty())
				fnames += ", ";
			fnames +=  mu_format("{}", field.name);
		});

		const std::string empty{"<empty>"};

		fields.append(Row{cfield.name.empty() ? empty :
				  mu_format("{}", cfield.name),
				  fnames});
	});

	colorify(fields, opts);
	mu_println("# Combination fields\n{}", fields);

	return Ok();
}

static Result<void>
topic_flags(const Options& opts)
{
	using namespace std::string_literals;

	Table flags;
	flags.append(Row{"flag", "shortcut", "category", "description"});

	flag_infos_for_each([&](const MessageFlagInfo& info) {

		const auto catname = std::invoke(
			[](MessageFlagCategory cat)->std::string {
				switch(cat){
				case MessageFlagCategory::Mailfile:
					return "file";
				case MessageFlagCategory::Maildir:
					return "maildir";
				case MessageFlagCategory::Content:
					return "content";
				case MessageFlagCategory::Pseudo:
					return "pseudo";
				default:
					return {};
				}
			}, info.category);

		flags.append(Row{mu_format("{}", info.name),
				mu_format("{}", info.shortcut),
				catname,
				std::string{info.description}});
	});

	colorify(flags, opts);

	mu_println("# Message flags\n{}", flags);

	return Ok();
}

static Result<void>
topic_store(const Mu::Store& store, const Options& opts)
{
	Table info;
	const auto conf{store.config()};

	info.append(Row{"property", "value", "description"});
	info.append(Row{"database-path", store.path(), "Path to xapian database"});
	info.append(Row{"message-number", mu_format("{}", store.size()),
		      "Number of messages in store"});
	for (const auto& prop: Config::properties) {
		if (any_of(prop.flags & (Property::Flags::System|Property::Flags::Internal)))
			continue;
		switch(prop.id) {
		case Config::Id::PersonalAddresses:
		case Config::Id::IgnoredAddresses: {
			const auto addrs{conf.decode<Config::Type::StringList>(conf.as_raw_string(prop))};
			for (auto& addr: addrs) {
				info.append(Row{std::string{prop.name}, addr, std::string{prop.description}});
			}
		} break;
		default:
			info.append(Row{std::string{prop.name},conf.as_display_string(prop),
				      std::string{prop.description}});
		}
	}
	colorify(info, opts);

	mu_println("{}", info);

	return Ok();
}

static Result<void>
topic_maildirs(const Mu::Store& store, const Options& opts)
{
	for (auto&& mdir: store.maildirs())
		mu_println("{}", mdir);

	return Ok();
}

static Result<void>
topic_mu(const Mu::Store& store, const Options& opts)
{
	Table info;
	const auto conf{store.config()};
	info.append(Row{"property", "value", "description"});

	for (const auto& prop: Config::properties) {
		if (any_of(prop.flags & Property::Flags::System)) {
			info.append(Row{Cell{std::string{prop.name}},
					Cell{conf.as_display_string(prop)},
					Cell{std::string{prop.description}}});
		}
	}

	colorify(info, opts);
	mu_println("{}\n", info);

	return Ok();
}


Result<void>
Mu::mu_cmd_info(const Mu::Store& store, const Options& opts)
{
	if (!locale_workaround())
		return Err(Error::Code::User, "failed to find a working locale");

	const auto topic{opts.info.topic};
	if (topic == "store")
		return topic_store(store, opts);
	else if (topic == "maildirs")
		return topic_maildirs(store, opts);
	else if (topic == "fields") {
		if (auto&& res{topic_fields(opts)}; !res)
			return res;
		std::cout << std::endl;
		if (auto&& res{topic_combi_fields(opts)}; !res)
			return res;
		std::cout << std::endl;
		return topic_flags(opts);
	} else if (topic == "mu") {
		return topic_mu(store, opts);
	} else {
		if (auto&& res{topic_mu(store, opts)}; !res)
			return res;

		MaybeAnsi col{!opts.nocolor};
		using Color = MaybeAnsi::Color;

		auto describe = [&](auto&& t, auto&& d)->std::string {
			return mu_format("{}{:<10}{} - {:>12}",
					 col.fg(Color::Green), t, col.reset(), d);
		};

		mu_println("\nother info topics ('mu info <topic>'):\n{}\n{}\n{}",
			   describe("store", "information about the message store (database)"),
			   describe("maildirs", "list the maildirs under the store's root-maildir"),
			   describe("fields",  "information about message fields"));
	}

	return Ok();
}
