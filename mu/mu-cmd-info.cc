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

#include <glib.h>
#include <gmime/gmime.h>

#include <fmt/ostream.h>

#include <thirdparty/tabulate.hpp>

using namespace Mu;
using namespace tabulate;

template <> struct fmt::formatter<Table> : ostream_formatter {};

static void
colorify(Table& table, const Options& opts)
{
	if (opts.nocolor || table.size() == 0)
		return;

	for (auto&& c = 0U; c != table.row(0).size(); ++c) {
		switch (c) {
		case 0:
			table.column(c).format()
				.font_color(Color::green)
				.font_style({FontStyle::bold});
			break;
		case 1:
			table.column(c).format()
				.font_color(Color::blue);
			break;
		case 2:
			table.column(c).format()
				.font_color(Color::magenta);
			break;

		case 3:
			table.column(c).format()
				.font_color(Color::yellow);
			break;
		case 4:
			table.column(c).format()
				.font_color(Color::green);
			break;
		case 5:
			table.column(c).format()
				.font_color(Color::blue);
			break;
		case 6:
			table.column(c).format()
				.font_color(Color::magenta);
			break;

		case 7:
			table.column(c).format()
				.font_color(Color::yellow);
			break;
		default:
			table.column(c).format()
				.font_color(Color::grey);
			break;
		}
	}

	for (auto&& c = 0U; c != table.row(0).size(); ++c)
		table[0][c].format()
			.font_color(Color::white)
			.font_style({FontStyle::bold});
}


static Result<void>
topic_fields(const Options& opts)
{
	using namespace std::string_literals;

	Table fields;
	fields.add_row({"field-name", "alias", "short", "search",
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

		fields.add_row({mu_format("{}", field.name),
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

	std::cout << "# Message fields\n" << fields << '\n';

	return Ok();
}

static Result<void>
topic_combi_fields(const Options& opts)
{
	using namespace std::string_literals;

	Table fields;
	fields.add_row({"combi-field", "fields"});

	seq_for_each(combi_fields(), [&](const auto& cfield) {

		std::string fnames;
		seq_for_each(cfield.fields, [&](auto&& field) {
			if (!fnames.empty())
				fnames += ", ";
			fnames +=  mu_format("{}", field.name);
		});

		const std::string empty{"<empty>"};

		fields.add_row({cfield.name.empty() ? empty : mu_format("{}", cfield.name),
				fnames});
	});

	colorify(fields, opts);
	std::cout << "# Combination fields\n" << fields << '\n';

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

		flags.add_row({mu_format("{}", info.name),
				mu_format("{}", info.shortcut),
				catname,
				std::string{info.description}});
	});

	colorify(flags, opts);

	std::cout << "# Message flags\n" << flags << '\n';

	return Ok();
}

static Result<void>
topic_store(const Mu::Store& store, const Options& opts)
{
	auto tstamp = [](::time_t t)->std::string {
		if (t == 0)
			return "never";
		else
			return mu_format("{:%c}", mu_time(t));
	};

	Table info;
	const auto conf{store.config()};
	info.add_row({"property", "value"});
	info.add_row({"maildir", store.root_maildir()});
	info.add_row({"database-path", store.path()});
	info.add_row({"schema-version",
			mu_format("{}", conf.get<Config::Id::SchemaVersion>())});
	info.add_row({"max-message-size", mu_format("{}", conf.get<Config::Id::MaxMessageSize>())});
	info.add_row({"batch-size", mu_format("{}", conf.get<Config::Id::BatchSize>())});
	info.add_row({"created", tstamp(conf.get<Config::Id::Created>())});

	for (auto&& c : conf.get<Config::Id::PersonalAddresses>())
		info.add_row({"personal-address", c});
	for (auto&& c : conf.get<Config::Id::IgnoredAddresses>())
		info.add_row({"ignored-address", c});

	info.add_row({"messages in store", mu_format("{}", store.size())});
	info.add_row({"support-ngrams",  conf.get<Config::Id::SupportNgrams>() ? "yes" : "no"});

	info.add_row({"last-change", tstamp(store.statistics().last_change)});
	info.add_row({"last-index",  tstamp(store.statistics().last_index)});

	if (!opts.nocolor)
		colorify(info, opts);

	std::cout << info << '\n';

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
topic_mu(const Options& opts)
{
	Table info;

	using namespace tabulate;

	info.add_row({"property", "value", "description"});
	info.add_row({"mu-version", std::string{VERSION}, "Mu runtime version"});
	info.add_row({"xapian-version", Xapian::version_string(), "Xapian runtime version"});
	info.add_row({"gmime-version",
			mu_format("{}.{}.{}", gmime_major_version, gmime_minor_version,
				  gmime_micro_version), "GMime runtime version"});
	info.add_row({"glib-version",
			mu_format("{}.{}.{}", glib_major_version, glib_minor_version,
				  glib_micro_version), "GLib runtime version"});
	info.add_row({"schema-version", mu_format("{}", MU_STORE_SCHEMA_VERSION),
			"Version of mu's database schema"});

	info.add_row({"cld2-support",
#if HAVE_CLD2
			"yes"
#else
			"no"
#endif
		, "Support searching by language-code?"});

	info.add_row({"guile-support",
#if BUILD_GUILE
			"yes"
#else
			"no"
#endif
		, "GNU Guile 3.x scripting support?"});
	info.add_row({"readline-support",
#if HAVE_LIBREADLINE
			"yes"
#else
			"no"
#endif
		, "Better 'mu server' REPL for debugging"});

	if (!opts.nocolor)
		colorify(info, opts);

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
	else if (topic == "maildirs")
		return topic_maildirs(store, opts);
	else if (topic == "fields") {
		topic_fields(opts);
		std::cout << std::endl;
		topic_combi_fields(opts);
		std::cout << std::endl;
		topic_flags(opts);
	} else if (topic == "mu") {
		return topic_mu(opts);
	} else {
		topic_mu(opts);

		MaybeAnsi col{!opts.nocolor};
		using Color = MaybeAnsi::Color;

		auto topic = [&](auto&& t, auto&& d)->std::string {
			return mu_format("{}{:<10}{} - {:>12}",
					 col.fg(Color::Green), t, col.reset(), d);
		};

		mu_println("\nother info topics ('mu info <topic>'):\n{}\n{}\n{}",
			   topic("store", "information about the message store (database)"),
			   topic("maildirs", "list the maildirs under the store's root-maildir"),
			   topic("fields",  "information about message fields"));
	}

	return Ok();
}
