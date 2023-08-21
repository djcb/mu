/*
** Copyright (C) 2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef MU_PRIORITY_HH__
#define MU_PRIORITY_HH__

#include <array>
#include <string>
#include <string_view>
#include "mu-fields.hh"

namespace Mu {
/**
 * Message priorities
 *
 */

/**
 * The priority ids
 *
 */
enum struct Priority : char {
	Low    = 'l', /**< Low priority */
	Normal = 'n', /**< Normal priority */
	High   = 'h', /**< High priority */
};

/**
 * Sequence of all message priorities.
 */
static constexpr std::array<Priority, 3> AllMessagePriorities = {
    Priority::Low, Priority::Normal, Priority::High};

/**
 * Get the char for some priority
 *
 * @param id an id
 *
 * @return the char
 */
constexpr char
to_char(Priority prio)
{
	return static_cast<char>(prio);
}

/**
 * Get the priority for some character; unknown ones
 * become Normal.
 *
 * @param c some character
 */
constexpr Priority
priority_from_char(char c)
{
	switch (c) {
	case 'l':
		return Priority::Low;
	case 'h':
		return Priority::High;
	case 'n':
	default:
		return Priority::Normal;
	}
}

/**
 * Get the priority from their (internal) name, i.e., low/normal/high
 * or shortcut.
 *
 * @param pname
 *
 * @return the priority or none
 */
static inline Option<Priority>
priority_from_name(std::string_view pname)
{
	if (pname == "low" || pname == "l")
		return Priority::Low;
	else if (pname == "high" || pname == "h")
		return Priority::High;
	else if (pname == "normal" || pname == "n")
		return Priority::Normal;
	else
		return Nothing;
}


/**
 * Get the name for a given priority
 *
 * @return the name
 */
constexpr std::string_view
priority_name(Priority prio)
{
	switch (prio) {
	case Priority::Low:
		return "low";
	case Priority::High:
		return "high";
	case Priority::Normal:
	default:
		return "normal";
	}
}

/**
 * Get the name for a given priority (backward compatibility)
 *
 * @return the name
 */
constexpr const char*
priority_name_c_str(Priority prio)
{
	switch (prio) {
	case Priority::Low:
		return "low";
	case Priority::High:
		return "high";
	case Priority::Normal:
	default:
		return "normal";
	}
}

/**
 * Get a the message priority as a string
 *
 * @param prio priority
 *
 * @return a string
 */
std::string to_string(Priority prio);

} // namespace Mu

#endif /*MU_PRIORITY_HH_*/
