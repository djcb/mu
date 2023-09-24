/*
** Copyright (C) 2022-2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef MU_FLAGS_HH__
#define MU_FLAGS_HH__

#include <algorithm>
#include <string_view>
#include <array>
#include <utils/mu-utils.hh>
#include <utils/mu-option.hh>

namespace Mu {

enum struct Flags {
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
	Personal    = 1 << 12, /**< A personal message (i.e., at least one of the
				* contact fields contains a personal address) */
	Calendar    = 1 << 13, /**< A calendar invitation */
	/*
	 * <private>
	 */
	_final_ = 1 << 14
};
MU_ENABLE_BITOPS(Flags);

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

	Flags			flag;		/**< The message flag */
	char			shortcut;	/**< Shortcut character;
						 * tolower(shortcut) must be
						 * unique for all flags */
	std::string_view	name;		/**< Name of the flag */
	MessageFlagCategory	category;	/**< Flag category */
	std::string_view        description;    /**< Description */

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
constexpr std::array<MessageFlagInfo, 14> AllMessageFlagInfos = {{
	MessageFlagInfo{Flags::Draft,        'D', "draft",	MessageFlagCategory::Mailfile,
	    "Draft (in progress)"
	},
	MessageFlagInfo{Flags::Flagged,	 'F', "flagged",	MessageFlagCategory::Mailfile,
		"User-flagged"
	},
	MessageFlagInfo{Flags::Passed,	 'P', "passed",		MessageFlagCategory::Mailfile,
		"Forwarded message"
	},
	MessageFlagInfo{Flags::Replied,	 'R', "replied",	MessageFlagCategory::Mailfile,
		"Replied-to"
	},
	MessageFlagInfo{Flags::Seen,	 'S', "seen",		MessageFlagCategory::Mailfile,
		"Viewed at least once"
	},
	MessageFlagInfo{Flags::Trashed,	 'T', "trashed",	MessageFlagCategory::Mailfile,
		"Marked for deletion"
	},
	MessageFlagInfo{Flags::New,	 'N', "new",		MessageFlagCategory::Maildir,
		"New message"
	},
	MessageFlagInfo{Flags::Signed,	 'z', "signed",		MessageFlagCategory::Content,
		"Cryptographically signed"
	},
	MessageFlagInfo{Flags::Encrypted, 'x', "encrypted",      MessageFlagCategory::Content,
		"Encrypted"
	},
	MessageFlagInfo{Flags::HasAttachment,'a', "attach",     MessageFlagCategory::Content,
		"Has at least one attachment"
	},
	MessageFlagInfo{Flags::Unread,	 'u', "unread",		MessageFlagCategory::Pseudo,
		"New or not seen message"
	},
	MessageFlagInfo{Flags::MailingList, 'l', "list",	MessageFlagCategory::Content,
		"Mailing list message"
	},
	MessageFlagInfo{Flags::Personal, 'q', "personal",	MessageFlagCategory::Content,
		"Personal message"
	},
	MessageFlagInfo{Flags::Calendar, 'c', "calendar",	MessageFlagCategory::Content,
		"Calendar invitation"
    },
}};


/**
 * Invoke some callable Func for each flag info
 *
 * @param func some callable
 */
template<typename Func>
constexpr void flag_infos_for_each(Func&& func)
{
	for (auto&& info: AllMessageFlagInfos)
		func(info);
}

/**
 * Get flag info for some flag
 *
 * @param flag a singular flag
 *
 * @return the MessageFlagInfo, or Nothing in case of error.
 */
constexpr const Option<MessageFlagInfo>
flag_info(Flags flag)
{
	constexpr auto upper = static_cast<unsigned>(Flags::_final_);
	const auto     val   = static_cast<unsigned>(flag);

	if (__builtin_popcount(val) != 1 || val >= upper)
		return Nothing;

	return AllMessageFlagInfos[static_cast<unsigned>(__builtin_ctz(val))];
}

/**
 * Get flag info for some flag
 *
 * @param shortcut shortcut character
 *
 * @return the MessageFlagInfo
 */
constexpr const Option<MessageFlagInfo>
flag_info(char shortcut)
{
	for (auto&& info : AllMessageFlagInfos)
		if (info.shortcut == shortcut)
			return info;

	return Nothing;
}

/**
 * Get flag info for some flag, either by its name of is shortcut
 *
 * @param name the name of the message-flag, or its shortcut
 *
 * @return the MessageFlagInfo or Nothing if not found
 */
constexpr const Option<MessageFlagInfo>
flag_info(std::string_view name)
{
	if (name.empty())
		return Nothing;

	for (auto&& info : AllMessageFlagInfos)
		if (info.name == name)
			return info;

	return flag_info(name.at(0));
}

/**
 * 'unread' is a pseudo-flag that means 'new or not seen'
 *
 * @param flags
 *
 * @return flags with unread added or removed.
 */
constexpr Flags
imply_unread(Flags flags)
{
	/* unread is a pseudo flag equivalent to 'new or not seen' */
	if (any_of(flags & Flags::New) || none_of(flags & Flags::Seen))
		return flags | Flags::Unread;
	else
		return flags & ~Flags::Unread;
}

/**
 * There are two string-based expression types for flags:
 * 1) 'absolute': replace the existing flags
 * 2) 'delta'  : flags as a delta of existing flags.
 */

/**
 * Get the (OR'ed) flags corresponding to an expression.
 *
 * @param expr the expression (a sequence of flag shortcut characters)
 * @param ignore_invalid if @true, ignore invalid flags, otherwise return
 * nullopt if an invalid flag is encountered
 *
 * @return the (OR'ed) flags or Flags::None
 */
constexpr Option<Flags>
flags_from_absolute_expr(std::string_view expr, bool ignore_invalid = false)
{
	Flags flags{Flags::None};

	for (auto&& kar : expr) {
		if (const auto& info{flag_info(kar)}; !info) {
			if (!ignore_invalid)
				return Nothing;
		} else
			flags |= info->flag;
	}

	return imply_unread(flags);
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
 * Nothing if an invalid flag is encountered
 *
 * @return new flags, or Nothing in case of error
 */
constexpr Option<Flags>
flags_from_delta_expr(std::string_view expr, Flags flags,
		      bool ignore_invalid = false)
{
	if (expr.size() % 2 != 0)
		return Nothing;

	for (auto u = 0U; u != expr.size(); u += 2) {
		if (const auto& info{flag_info(expr[u + 1])}; !info) {
			if (!ignore_invalid)
				return Nothing;
		} else {
			switch (expr[u]) {
			case '+': flags |= info->flag; break;
			case '-': flags &= ~info->flag; break;
			default:
				if (!ignore_invalid)
					return Nothing;
				break;
			}
		}
	}

	return imply_unread(flags);
}

/**
 * Calculate the flags from either 'absolute' or 'delta' expressions
 *
 * @param expr a flag expression, either 'delta' or 'absolute'
 * @param flags optional: existing flags or none. Required for delta.
 *
 * @return either messages flags or Nothing in case of error.
 */
constexpr Option<Flags>
flags_from_expr(std::string_view expr, Option<Flags> flags = Nothing)
{
	if (expr.empty())
		return Nothing;

	if (expr[0] == '+' || expr[0] == '-')
		return flags_from_delta_expr(
		    expr, flags.value_or(Flags::None), true);
	else
		return flags_from_absolute_expr(expr, true);
}

/**
 * Filter out flags which are not in the given category
 *
 * @param flags flags
 * @param cat category
 *
 * @return filtered flags
 */
constexpr Flags
flags_filter(Flags flags, MessageFlagCategory cat)
{
	for (auto&& info : AllMessageFlagInfos)
		if (info.category != cat)
			flags &= ~info.flag;
	return flags;
}

/**
 * Filter out any flags which are _not_ Maildir / Mailfile flags
 *
 * @param flags flags
 *
 * @return filtered flags
 */
constexpr Flags
flags_maildir_file(Flags flags)
{
	for (auto&& info : AllMessageFlagInfos)
		if (info.category != MessageFlagCategory::Maildir &&
		    info.category != MessageFlagCategory::Mailfile)
			flags &= ~info.flag;
	return flags;
}




/**
 * Return flags, where flags = new_flags but with unmutable_flag in the
 * result the same as in old_flags
 *
 * @param old_flags
 * @param new_flags
 * @param immutable_flag
 *
 * @return
 */
constexpr Flags
flags_keep_unmutable(Flags old_flags, Flags new_flags, Flags immutable_flag)
{
    if (any_of(old_flags & immutable_flag))
	return new_flags | immutable_flag;
    else
	return new_flags & ~immutable_flag;
}


/**
 * Get a string representation of flags
 *
 * @param flags flags
 *
 * @return string as a sequence of message-flag shortcuts
 */
std::string to_string(Flags flags);


/**
 * Get a string representation of Flags for fmt
 *
 * @param flags flags
 *
 * @return string as a sequence of message-flag shortcuts
 */
static inline auto format_as(const Flags& flags) {
	return to_string(flags);
}

} // namespace Mu

#endif /* MU_FLAGS_HH__ */
