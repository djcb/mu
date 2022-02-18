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

/*
 * implementation is almost completely in the header; here we just add some
 * compile-time tests.
 */

#include "mu-message-flags.hh"

using namespace Mu;

std::string
Mu::message_flags_to_string(MessageFlags flags)
{
	std::string str;

	for (auto&& info: AllMessageFlagInfos)
		if (any_of(info.flag & flags))
			str+=info.shortcut;

	return str;
}


/*
 * flags & flag-info
 */
constexpr bool
validate_message_info_flags()
{
	for (auto id = 0U; id != AllMessageFlagInfos.size(); ++id) {
		const auto flag = static_cast<MessageFlags>(1 << id);
		if (flag != AllMessageFlagInfos[id].flag)
			return false;
	}
	return true;
}

static_assert(AllMessageFlagInfos.size() ==
	      __builtin_ctz(static_cast<unsigned>(MessageFlags::_final_)));
static_assert(validate_message_info_flags());

static_assert(!!message_flag_info(MessageFlags::Encrypted));
static_assert(!message_flag_info(MessageFlags::None));
static_assert(!message_flag_info(static_cast<MessageFlags>(0)));
static_assert(!message_flag_info(static_cast<MessageFlags>(1<<AllMessageFlagInfos.size())),
	      "should be invalid");

/*
 * message_flag_info
 */
static_assert(message_flag_info('D')->flag == MessageFlags::Draft);
static_assert(message_flag_info('l')->flag == MessageFlags::MailingList);
static_assert(!message_flag_info('q'));

static_assert(message_flag_info("trashed")->flag == MessageFlags::Trashed);
static_assert(message_flag_info("attach")->flag == MessageFlags::HasAttachment);
static_assert(!message_flag_info("fnorb"));


static_assert(message_flag_info('D')->shortcut_lower() == 'd');
static_assert(message_flag_info('u')->shortcut_lower() == 'u');

/*
 * message_flags_from_expr
 */
static_assert(message_flags_from_absolute_expr("SRP").value() ==
	      (MessageFlags::Seen | MessageFlags::Replied | MessageFlags::Passed));
static_assert(message_flags_from_absolute_expr("Faul").value() ==
	      (MessageFlags::Flagged | MessageFlags::Unread |
	       MessageFlags::HasAttachment | MessageFlags::MailingList));

static_assert(!message_flags_from_absolute_expr("DRT?"));
static_assert(message_flags_from_absolute_expr("DRT?", true/*ignore invalid*/).value() ==
	      (MessageFlags::Draft | MessageFlags::Replied |
	       MessageFlags::Trashed));
static_assert(message_flags_from_absolute_expr("DFPNxulabcdef", true/*ignore invalid*/).value() ==
	      (MessageFlags::Draft|MessageFlags::Flagged|MessageFlags::Passed|
	       MessageFlags::New | MessageFlags::Encrypted |
	       MessageFlags::Unread | MessageFlags::MailingList |
	       MessageFlags::HasAttachment));

/*
 * message_flags_from_delta_expr
 */
static_assert(message_flags_from_delta_expr(
		      "+S-u-N", MessageFlags::New|MessageFlags::Unread).value() ==
	      MessageFlags::Seen);
static_assert(message_flags_from_delta_expr("+R+P-F", MessageFlags::Seen).value() ==
	      (MessageFlags::Seen|MessageFlags::Passed|MessageFlags::Replied));
/* '-B' is invalid */
static_assert(!message_flags_from_delta_expr("+R+P-B", MessageFlags::Seen));
/* '-B' is invalid, but ignore invalid */
static_assert(message_flags_from_delta_expr("+R+P-B", MessageFlags::Seen, true) == 
	      (MessageFlags::Replied|MessageFlags::Passed|MessageFlags::Seen));
static_assert(message_flags_from_delta_expr("+F+T-S", MessageFlags::None, true).value() ==
	      (MessageFlags::Flagged|MessageFlags::Trashed));

/*
 * message_flags_filter
 */
static_assert(message_flags_filter(message_flags_from_absolute_expr(
					   "DFPNxulabcdef", true/*ignore invalid*/).value(),
				   MessageFlagCategory::Mailfile) ==
	      (MessageFlags::Draft|MessageFlags::Flagged|MessageFlags::Passed));
