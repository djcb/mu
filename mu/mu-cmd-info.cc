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

#include <thirdparty/tabulate.hpp>

using namespace Mu;

Result<void>
Mu::mu_cmd_info(const Mu::Store& store, const Options& opts)
{
	using namespace tabulate;

	if (!locale_workaround())
		return Err(Error::Code::User, "failed to find a working locale");

	auto colorify = [](Table& table) {
		for (auto&& row: table) {

			if (row.cells().size() < 2)
				continue;

			row.cells().at(0)->format().font_style({FontStyle::bold})
				.font_color(Color::green);
			row.cells().at(1)->format().font_color(Color::blue);
		}
	};

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
