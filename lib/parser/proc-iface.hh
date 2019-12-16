/*
** Copyright (C) 2017 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
**
**  This library is free software; you can redistribute it and/or
**  modify it under the terms of the GNU Lesser General Public License
**  as published by the Free Software Foundation; either version 2.1
**  of the License, or (at your option) any later version.
**
**  This library is distributed in the hope that it will be useful,
**  but WITHOUT ANY WARRANTY; without even the implied warranty of
**  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
**  Lesser General Public License for more details.
**
**  You should have received a copy of the GNU Lesser General Public
**  License along with this library; if not, write to the Free
**  Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
**  02110-1301, USA.
*/
#ifndef __PROC_IFACE_HH__
#define __PROC_IFACE_HH__

#include <string>
#include <vector>
#include <tuple>
#include <regex>

namespace Mu {

struct ProcIface {

	virtual ~ProcIface() = default;

	/**
	 * Get the "shortcut"/internal fields for the the given fieldstr or empty if there is none
	 *
	 * @param fieldstr a fieldstr, e.g "subject" or "s" for the subject field
	 *
	 * @return a vector with "exploded" values, with a code and a fullname. E.g. "s" might map
	 * to [<"S","subject">], while "recip" could map to [<"to", "T">, <"cc", "C">, <"bcc", "B">]
	 */
	struct FieldInfo {
		const std::string	field;
		const std::string	prefix;
		bool			supports_phrase;
		unsigned		id;
	};
	using FieldInfoVec = std::vector<FieldInfo>;

	virtual FieldInfoVec process_field (const std::string& field) const = 0;

	/**
	 * Process a value
	 *
	 * @param field a field name
	 * @param value a value
	 *
	 * @return the processed value
	 */
	virtual std::string process_value (
		const std::string& field, const std::string& value) const = 0;

	/**
	 * Is this a range field?
	 *
	 * @param field some field
	 *
	 * @return true if it is a range-field; false otherwise.
	 */
	virtual bool is_range_field (const std::string& field) const = 0;


	/**
	 * Process a range field
	 *
	 * @param fieldstr a fieldstr, e.g "date" or "d" for the date field
	 * @param lower lower bound or empty
	 * @param upper upper bound or empty
	 *
	 * @return the processed range
	 */
	struct Range {
		std::string lower;
		std::string upper;
	};
	virtual Range process_range (const std::string& field, const std::string& lower,
				     const std::string& upper) const = 0;

	/**
	 *
	 *
	 * @param field
	 * @param rx
	 *
	 * @return
	 */
	virtual std::vector<std::string>
	process_regex (const std::string& field, const std::regex& rx) const = 0;

}; // ProcIface


struct DummyProc: public ProcIface { // For testing

	std::vector<FieldInfo>
	process_field (const std::string& field) const override {
		return {{ field, "x", false, 0 }};
	}

	std::string
	process_value (const std::string& field, const std::string& value) const override {
		return value;
	}

	bool is_range_field (const std::string& field) const override {
		return field == "range";
	}

	Range process_range (const std::string& field, const std::string& lower,
			     const std::string& upper) const override {
		return { lower, upper };
	}

	std::vector<std::string>
	process_regex (const std::string& field, const std::regex& rx) const override {
		return {};
	}
}; //Dummy


} // Mu

#endif /* __PROC_IFACE_HH__ */
