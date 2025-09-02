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
#include "utils/mu-option.hh"
#include "message/mu-labels.hh"

#include "mu-config.hh"

namespace Mu {

class Store; // fwd declaration

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
	 * Deserialize the map from the configuration.
	 *
	 * @param serialized serialization string
	 */
	LabelsCache(Config& config): config_{config},
				     label_map_{deserialize(config_.get<Config::Id::Labels>())},
				     dirty_{false}{}

	~LabelsCache() {
		if (dirty_)
			serialize();
	};

	/**
	 * Add a label occurrence to the cache
	 *
	 * @param label some label
	 */
	void increase(const std::string& label) {
		if (auto it = label_map_.find(label); it == label_map_.end())
			label_map_.insert({label, 1});
		else
			++it->second;
		++dirty_;
	}
	/**
	 * Remove a label occurrence from the cache
	 *
	 * Removes the label completely if this was the _last_ occurence.
	 *
	 * @param label some label
	 */
	void decrease(const std::string& label) {
		if (auto it = label_map_.find(label); it != label_map_.end()) {
			if (it->second == 1)
				label_map_.erase(it);
			else
				--it->second;
			++dirty_;
		}
	}

	/**
	 * Update the cache with the label changes
	 *
	 * @param updates a vector of delta-labels
	 */
	void update(const Labels::DeltaLabelVec& updates) {
		for(const auto& [delta, label]: updates) {
			switch(delta) {
			case Labels::Delta::Add:
				increase(label);
				break;
			case Labels::Delta::Remove:
				decrease(label);
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
	 * Serialize the cache into Config if there are changes.
	 *
	 */
	void serialize() const;

	/**
	 * Restore the labels-cache from the labels seen in the store.
	 *
	 * @param store a store
	 *
	 * @return Ok() or some error
	 */
	Result<void> restore(const Store& store);

private:
	/**
	 * Deserialize the cache into a Map
	 *
	 * @return serialized cache
	 */
	Map deserialize(const std::string& serialized) const;

	Config& config_;
	Map label_map_;
	mutable size_t dirty_{};
};

} // namespace Mux
#endif /*MU_LABELS_CACHE_HH*/
