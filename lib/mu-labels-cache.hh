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


#ifndef MU_LABELS_CACHE_HH
#define MU_LABELS_CACHE_HH

#include <set>
#include <string>
#include <unordered_map>

#include "utils/mu-utils.hh"
#include "message/mu-labels.hh"

namespace Mu {

/**
 * The cache keeps track of what labels are being used. This can be used
 * for completion etc. and `mu label list`
 */
class LabelsCache {
public:
	// maps a label to a number of occurrences
	using Map = std::unordered_map<std::string, size_t>;
	/**
	 * CTOR
	 *
	 * Deserialize the map from a string
	 *
	 * @param serialized serialization string
	 */
	LabelsCache(const std::string serialized = {}): label_map_{deserialize(serialized)} {
	}

	/**
	 * Construct a new ContactsCache object
	 *
	 * @param config db configuration database object
	 */
	LabelsCache(Config& config) {


	}


	/**
	 * Add a label occurrence to the cache
	 *
	 * @param label
	 */
	void add(const std::string& label) {
		if (auto it = label_map_.find(label); it == label_map_.end())
			label_map_.insert({label, 1});
		else
			++it->second;
	}
	/**
	 * Remove label occurrence from the cache
	 *
	 * @param label
	 */
	void remove(const std::string& label) {
		if (auto it = label_map_.find(label); it != label_map_.end()) {
			if (it->second == 1)
				label_map_.erase(it);
			else
				--it->second;
		}
	}

	/**
	 * Update the cache with the the label changes
	 *
	 * @param updates a vector of delta-labels
	 */
	void update(const Labels::DeltaLabelVec& updates) {
		for(const auto& [delta, label]: updates) {
			switch(delta) {
			case Labels::Delta::Add:
				add(label);
				break;
			case Labels::Delta::Remove:
				remove(label);
				break;
			}
		}
	}

	/**
	 * Return a copy of the label-map
	 *
	 * @return the label-map
	 */
	Map label_map() const { return label_map_; }


	// serialization/deserialization could be optimized, but is not super
	// time-critical

	/**
	 * Serialize the cache into a string.
	 *
	 * @return serialized cache
	 */
	std::string serialize() const {
		std::string s;
		for (const auto&[label, n]: label_map_)
			s += mu_format("{}{}{}\n", label, SepaChar2, n);

		return s;
	}


	/**
	 * Deserialize the cache into a Map
	 *
	 * @return serialized cache
	 */
	Map deserialize(const std::string& serialized) const {

		Map       map;
		std::stringstream ss{serialized, std::ios_base::in};
		std::string       line;

		while (std::getline(ss, line)) {
			if (const auto parts = Mu::split(line, SepaChar2); parts.size() != 2)
				mu_warning("error: '{}'", line);
			else
				map.emplace(std::move(parts[0]),
					    static_cast<std::size_t>(g_ascii_strtoll(parts[1].c_str(),{}, 10)));
		}
		return map;
	}

private:
	Map label_map_;
};

} // namespace Mux
#endif /*MU_LABELS_CACHE_HH*/
