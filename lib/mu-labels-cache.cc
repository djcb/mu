/*
** Copyright (C) 2025 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-labels-cache.hh"
#include "mu-store.hh"
#include "message/mu-labels.hh"

using namespace Mu;

void
Mu::LabelsCache::serialize() const
{
	std::string s;
	for (const auto&[label, n]: label_map_)
		s += mu_format("{}{}{}\n", label, SepaChar2, n);

	config_.set<Config::Id::Labels>(s);
	mu_debug("labels: serialized {} change(s)", dirty_);
	dirty_ = 0;
}

Mu::LabelsCache::Map
Mu::LabelsCache::deserialize(const std::string& serialized) const
{
	Map  map;

	std::stringstream ss{serialized, std::ios_base::in};
	std::string       line;

	while (std::getline(ss, line)) {
		if (const auto parts =
		    Mu::split(line, SepaChar2); parts.size() != 2)
			mu_warning("error: '{}'", line);
		else
			map.emplace(std::move(parts[0]),
				    static_cast<std::size_t>(
					    g_ascii_strtoll(parts[1].c_str(),{}, 10)));
	}
	return map;
}

Result<void>
Mu::LabelsCache::restore(const Store& store)
{
	const auto res{store.run_query("")};
	if (!res)
		return Err(Error{Error::Code::Query,
				"failed to run query: {}",
				 *res.error().what()});
	label_map_.clear();
	++dirty_;

	for (auto&& item: *res) {
		if (auto &&msg{item.message()}; msg) {
			for (const auto& label: msg->labels())
				increase(label);
		}
	}

	serialize();

	return Ok();
}
