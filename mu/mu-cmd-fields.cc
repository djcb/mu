/*
** Copyright (C) 2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
**
** This program is free software; you can redistribute it and/or modify it under
** the terms of the GNU General Public License as published by the Free Software
** Foundation; either version 3, or (at your option) any later version.
**
** This program is distributed in the hope that it will be useful, but WITHOUT
** ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
** FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
** details.
**
** You should have received a copy of the GNU General Public License along with
** this program; if not, write to the Free Software Foundation, Inc., 51
** Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
**
*/
#include <iostream>
#include <functional>

#include "mu-cmd.hh"
#include <message/mu-message.hh>
#include "utils/mu-utils.hh"

#include "thirdparty/tabulate.hpp"


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

static void
show_fields(const Options& opts)
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
}

static void
show_flags(const Options& opts)
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
}



Result<void>
Mu::mu_cmd_fields(const Options& opts)
{
	if (!locale_workaround())
		return Err(Error::Code::User, "failed to find a working locale");

	std::cout << "#\n# message fields\n#\n";
	show_fields(opts);
	std::cout << "\n#\n# message flags\n#\n";
	show_flags(opts);

	return Ok();

}
