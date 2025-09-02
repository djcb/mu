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

namespace Mu {

class Store;

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
	 * Add a label occurrence to the cache
	 *
	 * @param label some label
	 */
	void increase(const std::string& label) {
		if (auto it = label_map_.find(label); it == label_map_.end())
			label_map_.insert({label, 1});
		else
			++it->second;
		dirty_ = true;
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
			dirty_ = true;
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
	 * Serialize the cache into a string.
	 *
	 * Note: this also marks the cache a _non_ dirty;
	 *
	 * @return serialized cache
	 */
	[[nodiscard]] std::string serialize() const {
		std::string s;
		for (const auto&[label, n]: label_map_)
			s += mu_format("{}{}{}\n", label, SepaChar2, n);
		dirty_ = false;
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


	/**
	 * Restore the labels-cache from the labels seen in the store.
	 *
	 * @param store a store
	 *
	 * @return Ok() or some error
	 */
	Result<void> restore(const Store& store);


	/**
	 * Is the cache "dirty"?
	 *
	 * I.e. have there been changes since "serialize()" was called?
	 *
	 * @return true or false
	 */
	bool dirty() const {
		return dirty_;
	}

private:
	Map label_map_;
	mutable bool dirty_{};
};

/**
 * Export labels to a file
 *
 * If path is not specified, use a file in the current directory
 * If path ends in '/', write file in the path-directory
 *
 * @param store a store object
 * @param query for the message whose labels to export (empty for "all")
 * @param path the path or nothing
 *
 * @return either the output filename or some error
 */
Result<std::string> export_labels(const Store& store,
				  const std::string& query="",
				  Option<std::string> path={});

/**
 * Import labels from a file
 *
 * If path is not specified, use a file in the current directory
 *
 * @param store a store object
 * @param path the path to the file
 * @param dry_run only show what would be imported
 * @param quiet suppress output
 * @param verbose give verbose output
 *
 * @return Ok or some error
 */
Result<void> import_labels(Store&, const std::string& path, bool dry_run,
			   bool quiet, bool verbose);

} // namespace Mux
#endif /*MU_LABELS_CACHE_HH*/
