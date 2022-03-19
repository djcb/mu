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

#ifndef MU_MESSAGE_FLAGS_HH__
#define MU_MESSAGE_FLAGS_HH__

#include <algorithm>
#include <optional>
#include <string_view>
#include <array>
#include "utils/mu-utils.hh"

namespace Mu {

enum struct MessageFlags {
	None = 0, /**< No flags */
	/**
	 * next 6 are seen in the file-info part of maildir message file
	 * names, ie., in a name like "1234345346:2,<fileinfo>",
	 * <fileinfo> consists of zero or more of the following
	 * characters (in ascii order)
	 */
	Draft   = 1 << 0, /**< A draft message */
	Flagged = 1 << 1, /**< A flagged message */
	Passed  = 1 << 2, /**< A passed (forwarded) message */
	Replied = 1 << 3, /**< A replied message */
	Seen    = 1 << 4, /**< A seen (read) message */
	Trashed = 1 << 5, /**< A trashed message */

	/**
	 * decides on cur/ or new/ in the maildir
	 */
	New = 1 << 6, /**< A new message */

	/**
	 * content flags -- not visible in the filename, but used for
	 * searching
	 */
	Signed        = 1 << 7, /**< Cryptographically signed */
	Encrypted     = 1 << 8, /**< Encrypted */
	HasAttachment = 1 << 9, /**< Has an attachment */

	Unread = 1 << 10, /**< Unread; pseudo-flag, only for queries, so we can
			   * search for flag:unread, which is equivalent to
			   * 'flag:new OR NOT flag:seen' */
	/**
	 * other content flags
	 */
	MailingList = 1 << 11, /**< A mailing-list message */

	/*
	 * <private>
	 */
	_final_ = 1 << 12
};
MU_ENABLE_BITOPS(MessageFlags);

/**
 * Message flags category
 *
 */
enum struct MessageFlagCategory {
	None,     /**< Nothing */
	Mailfile, /**< Flag for a message file */
	Maildir,  /**< Flag for message file's location */
	Content,  /**< Message content flag */
	Pseudo    /**< Pseudo flag */
};

/**
 * Info about invidual message flags
 *
 */
struct MessageFlagInfo {
	MessageFlags        flag;     /**< The message flag */
	char                shortcut; /**< Shortcut character */
	std::string_view    name;     /**< Name of the flag */
	MessageFlagCategory category; /**< Flag category */

	/**
	 * Get the lower-case version of shortcut
	 *
	 * @return lower-case shortcut
	 */
	constexpr char shortcut_lower() const {
		return shortcut >= 'A' && shortcut <= 'Z' ?
			shortcut +  ('a' - 'A') : shortcut;
	}
};

/**
 * Array of all flag information.
 */
constexpr std::array<MessageFlagInfo, 12> AllMessageFlagInfos = {{
    MessageFlagInfo{MessageFlags::Draft,         'D', "draft",	 MessageFlagCategory::Mailfile},
    MessageFlagInfo{MessageFlags::Flagged,	 'F', "flagged", MessageFlagCategory::Mailfile},
    MessageFlagInfo{MessageFlags::Passed,	 'P', "passed",	 MessageFlagCategory::Mailfile},
    MessageFlagInfo{MessageFlags::Replied,	 'R', "replied", MessageFlagCategory::Mailfile},
    MessageFlagInfo{MessageFlags::Seen,		 'S', "seen",	 MessageFlagCategory::Mailfile},
    MessageFlagInfo{MessageFlags::Trashed,	 'T', "trashed", MessageFlagCategory::Mailfile},

    MessageFlagInfo{MessageFlags::New,		 'N', "new",	 MessageFlagCategory::Maildir},

    MessageFlagInfo{MessageFlags::Signed,	 'z', "signed",	 MessageFlagCategory::Content},
    MessageFlagInfo{MessageFlags::Encrypted,	 'x', "encrypted",
								 MessageFlagCategory::Content},
    MessageFlagInfo{MessageFlags::HasAttachment, 'a', "attach",
								 MessageFlagCategory::Content},

    MessageFlagInfo{MessageFlags::Unread,	 'u', "unread",	 MessageFlagCategory::Pseudo},

    MessageFlagInfo{MessageFlags::MailingList,	 'l', "list",	 MessageFlagCategory::Content},
}};


/**
 * Invoke some callable Func for each flag info
 *
 * @param func some callable
 */
template<typename Func>
constexpr void message_flag_infos_for_each(Func&& func)
{
	for (auto&& info: AllMessageFlagInfos)
		func(info);
}

/**
 * Get flag info for some flag
 *
 * @param flag a singular flag
 *
 * @return the MessageFlagInfo, or std::nullopt in case of error.
 */
constexpr const std::optional<MessageFlagInfo>
message_flag_info(MessageFlags flag)
{
	constexpr auto upper = static_cast<unsigned>(MessageFlags::_final_);
	const auto     val   = static_cast<unsigned>(flag);

	if (__builtin_popcount(val) != 1 || val >= upper)
		return std::nullopt;

	return AllMessageFlagInfos[static_cast<unsigned>(__builtin_ctz(val))];
}

/**
 * Get flag info for some flag
 *
 * @param shortcut shortcut character
 *
 * @return the MessageFlagInfo
 */
constexpr const std::optional<MessageFlagInfo>
message_flag_info(char shortcut)
{
	for (auto&& info : AllMessageFlagInfos)
		if (info.shortcut == shortcut)
			return info;

	return std::nullopt;
}

/**
 * Get flag info for some flag
 *
 * @param name of the message-flag.
 *
 * @return the MessageFlagInfo
 */
constexpr const std::optional<MessageFlagInfo>
message_flag_info(std::string_view name)
{
	for (auto&& info : AllMessageFlagInfos)
		if (info.name == name)
			return info;

	return std::nullopt;
}

/**
 * There are two string-based expression types for flags:
 * 1) 'absolute': replace the existing flags
 * 2_ 'delta'  : flags as a delta of existing flags.
 */

/**
 * Get the (OR'ed) flags corresponding to an expression.
 *
 * @param expr the expression (a sequence of flag shortcut characters)
 * @param ignore_invalid if @true, ignore invalid flags, otherwise return
 * nullopt if an invalid flag is encountered
 *
 * @return the (OR'ed) flags or MessageFlags::None
 */
constexpr std::optional<MessageFlags>
message_flags_from_absolute_expr(std::string_view expr, bool ignore_invalid = false)
{
	MessageFlags flags{MessageFlags::None};

	for (auto&& kar : expr) {
		if (const auto& info{message_flag_info(kar)}; !info) {
			if (!ignore_invalid)
				return std::nullopt;
		} else
			flags |= info->flag;
	}

	return flags;
}

/**
 * Calculate flags from existing flags and a delta expression
 *
 * Update @p flags with the flags in @p expr, where @p exprt consists of the the
 * normal flag shortcut characters, prefixed with either '+' or '-', which means
 * resp. "add this flag" or "remove this flag".
 *
 * So, e.g. "-N+S" would unset the NEW flag and set the SEEN flag, without
 * affecting other flags.
 *
 * @param expr delta expression
 * @param flags existing flags
 * @param ignore_invalid if @true, ignore invalid flags, otherwise return
 * nullopt if an invalid flag is encountered
 *
 * @return new flags, or nullopt in case of error
 */
constexpr std::optional<MessageFlags>
message_flags_from_delta_expr(std::string_view expr, MessageFlags flags,
			      bool ignore_invalid = false)
{
	if (expr.size() % 2 != 0)
		return std::nullopt;

	for (auto u = 0U; u != expr.size(); u += 2) {
		if (const auto& info{message_flag_info(expr[u + 1])}; !info) {
			if (!ignore_invalid)
				return std::nullopt;
		} else {
			switch (expr[u]) {
			case '+': flags |= info->flag; break;
			case '-': flags &= ~info->flag; break;
			default:
				if (!ignore_invalid)
					return std::nullopt;
				break;
			}
		}
	}

	return flags;
}

/**
 * Calculate the flags from either 'absolute' or 'delta' expressions
 *
 * @param expr a flag expression, either 'delta' or 'absolute'
 * @param flags optional: existing flags or none. Required for delta.
 *
 * @return either messages flags or std::nullopt in case of error.
 */
constexpr std::optional<MessageFlags>
message_flags_from_expr(std::string_view            expr,
			std::optional<MessageFlags> flags = std::nullopt)
{
	if (expr.empty())
		return std::nullopt;

	if (expr[0] == '+' || expr[0] == '-')
		return message_flags_from_delta_expr(
		    expr, flags.value_or(MessageFlags::None), true);
	else
		return message_flags_from_absolute_expr(expr, true);
}

/**
 * Filter out flags which are not in the given category
 *
 * @param flags flags
 * @param cat category
 *
 * @return filter flags
 */
constexpr MessageFlags
message_flags_filter(MessageFlags flags, MessageFlagCategory cat)
{
	for (auto&& info : AllMessageFlagInfos)
		if (info.category != cat)
			flags &= ~info.flag;
	return flags;
}

/**
 * Get a string representation of flags
 *
 * @param flags flags
 *
 * @return string as a sequence of message-flag shortcuts
 */
std::string message_flags_to_string(MessageFlags flags);

} // namespace Mu

#endif /* MU_MESSAGE_FLAGS_HH__ */
