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

#include "config.h"

#include "mu-cmd.hh"
#include <message/mu-message.hh>
#include "utils/mu-utils.hh"

#include <thirdparty/tabulate.hpp>

using namespace Mu;
using namespace tabulate;

static void
table_header(Table& table, const Options& opts)
{
	if (opts.nocolor)
		return;

	(*table.begin()).format()
		.font_style({FontStyle::bold})
		.font_color(Color::blue);

}

static Result<void>
topic_fields(const Options& opts)
{
	using namespace std::string_literals;

	Table fields;
	fields.add_row({"field-name", "alias", "short", "search",
			"value", "sexp", "example query", "description"});

	auto disp= [&](std::string_view sv)->std::string {
		if (sv.empty())
			return "";
		else
			return format("%.*s", STR_V(sv));
	};

	auto searchable=[&](const Field& field)->std::string {
		if (field.is_boolean_term())
			return "boolean";
		if (field.is_indexable_term())
			return "index";
		if (field.is_normal_term())
			return "yes";
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

		fields.add_row({format("%.*s", STR_V(field.name)),
				field.alias.empty() ? "" : format("%.*s", STR_V(field.alias)),
				field.shortcut ? format("%c", field.shortcut) : ""s,
				searchable(field),
				field.is_value() ? "yes" : "no",
				field.include_in_sexp() ? "yes" : "no",
				disp(field.example_query),
				disp(field.description)});
		++row;
	});

	table_header(fields, opts);

	std::cout << fields << '\n';

	return Ok();
}

static Result<void>
topic_flags(const Options& opts)
{
	using namespace tabulate;
	using namespace std::string_literals;

	Table flags;
	flags.add_row({"flag", "shortcut", "category", "description"});

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

		flags.add_row({format("%.*s", STR_V(info.name)),
				format("%c", info.shortcut),
				catname,
				std::string{info.description}});
	});

	table_header(flags, opts);

	std::cout << flags << '\n';
	return Ok();
}

static void
colorify(Table& table)
{
	for (auto&& row: table) {

		if (row.cells().size() < 2)
			continue;

		row.cells().at(0)->format().font_style({FontStyle::bold})
			.font_color(Color::green);
		row.cells().at(1)->format().font_color(Color::blue);
	}
}


static Result<void>
topic_store(const Mu::Store& store, const Options& opts)
{
	using namespace tabulate;

	auto tstamp = [](::time_t t)->std::string {
		if (t == 0)
			return "never";
		else
			return time_to_string("%c", t);
	};

	Table info;
	const auto conf{store.config()};
	info.add_row({"maildir", store.root_maildir()});
	info.add_row({"database-path", store.path()});
	info.add_row({"schema-version",
			format("%zu", conf.get<Config::Id::SchemaVersion>())});
	info.add_row({"max-message-size", format("%zu", conf.get<Config::Id::MaxMessageSize>())});
	info.add_row({"batch-size", format("%zu", conf.get<Config::Id::BatchSize>())});
	info.add_row({"created", tstamp(conf.get<Config::Id::Created>())});

	for (auto&& c : conf.get<Config::Id::PersonalAddresses>())
		info.add_row({"personal-address", c});
	for (auto&& c : conf.get<Config::Id::IgnoredAddresses>())
		info.add_row({"ignored-address", c});

	info.add_row({"messages in store", format("%zu", store.size())});
	info.add_row({"last-change", tstamp(store.statistics().last_change)});
	info.add_row({"last-index",  tstamp(store.statistics().last_index)});

	if (!opts.nocolor)
		colorify(info);

	std::cout << info << '\n';

	return Ok();
}

static Result<void>
topic_common(const Options& opts)
{
	Table info;

	using namespace tabulate;

	info.add_row({"mu version", std::string{VERSION}});
	info.add_row({"store schema-version", format("%u", MU_STORE_SCHEMA_VERSION)});
	info.add_row({"guile-support:",
#if BUILD_GUILE
			"yes"
#else
			"no"
#endif
		});
	info.add_row({"readline-support:",
#if HAVE_LIBREADLINE
			"yes"
#else
			"no"
#endif
		});

	info.add_row({"cld2 language support:",
#if HAVE_CLD2
			"yes"
#else
			"no"
#endif
		});

	if (!opts.nocolor)
		colorify(info);

	std::cout << info << '\n';

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
	else if (topic == "fields") {
		topic_fields(opts);
		return topic_flags(opts);
	} else if (topic == "common") {
		return topic_common(opts);
	} else {
		topic_common(opts);

		MaybeAnsi col{!opts.nocolor};
		using Color = MaybeAnsi::Color;

		auto topic = [&](const std::string& s)->std::string {
			return "  " + col.fg(Color::Green) + s.c_str() + col.reset();
		};

		std::cout << "\nother available info topics ('mu info <topic>'):\n"
			  << topic("store")  << "  - information about the message store (database)\n"
			  << topic("fields") << " - information about message fields\n";
	}

	return Ok();
}
