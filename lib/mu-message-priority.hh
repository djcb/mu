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

#ifndef MU_MESSAGE_PRIORITY_HH__
#define MU_MESSAGE_PRIORITY_HH__

#include <array>
#include <string>
#include <string_view>

namespace Mu {
/**
 * Message priorities
 *
 */

/**
 * The priority ids
 *
 */
enum struct MessagePriority : char {
	Low    = 'l', /**< Low priority */
	Normal = 'n', /**< Normal priority */
	High   = 'h', /**< High priority */
};

/**
 * Sequence of all message priorities.
 */
static constexpr std::array<MessagePriority, 3> AllMessagePriorities = {
    MessagePriority::Low, MessagePriority::Normal, MessagePriority::High};

/**
 * Get the char for some priority
 *
 * @param id an id
 *
 * @return the char
 */
constexpr char
to_char(MessagePriority prio)
{
	return static_cast<char>(prio);
}

/**
 * Get the priority for some character; unknown onws
 * become Normal.
 *
 * @param c some character
 */
constexpr MessagePriority
message_priority_from_char(char c)
{
	switch (c) {
	case 'l':
		return MessagePriority::Low;
	case 'h':
		return MessagePriority::High;
	case 'n':
	default:
		return MessagePriority::Normal;
	}
}

/**
 * Get the name for a given priority
 *
 * @return the name
 */
constexpr std::string_view
message_priority_name(MessagePriority prio)
{
	switch (prio) {
	case MessagePriority::Low:
		return "low";
	case MessagePriority::High:
		return "high";
	case MessagePriority::Normal:
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
message_priority_name_c_str(MessagePriority prio)
{
	switch (prio) {
	case MessagePriority::Low: return "low";
	case MessagePriority::High: return "high";
	case MessagePriority::Normal:
	default: return "normal";
	}
}

/**
 * Get a the message priority as a string
 *
 * @param prio priority
 *
 * @return a string
 */
std::string to_string(MessagePriority prio);

} // namespace Mu

#endif /*MU_MESSAGE_PRIORITY_HH_*/
