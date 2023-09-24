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

#include "mu-flags.hh"

using namespace Mu;

std::string
Mu::to_string(Flags flags)
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
		const auto flag = static_cast<Flags>(1 << id);
		if (flag != AllMessageFlagInfos[id].flag)
			return false;
	}
	return true;
}


/*
 * tests... also build as runtime-tests, so we can get coverage info
 */
#ifdef BUILD_TESTS
#define static_assert g_assert_true
#endif /*BUILD_TESTS*/

[[maybe_unused]] static void
test_basic()
{
	static_assert(AllMessageFlagInfos.size() ==
		      __builtin_ctz(static_cast<unsigned>(Flags::_final_)));
	static_assert(validate_message_info_flags());

	static_assert(!!flag_info(Flags::Encrypted));
	static_assert(!flag_info(Flags::None));
	static_assert(!flag_info(static_cast<Flags>(0)));
	static_assert(!flag_info(static_cast<Flags>(1<<AllMessageFlagInfos.size())));
}

/*
 * flag_info
 */
[[maybe_unused]] static void
test_flag_info()
{
	static_assert(flag_info('D')->flag == Flags::Draft);
	static_assert(flag_info('l')->flag == Flags::MailingList);
	static_assert(!flag_info('y'));

	static_assert(flag_info("trashed")->flag == Flags::Trashed);
	static_assert(flag_info("attach")->flag == Flags::HasAttachment);
	static_assert(!flag_info("fnorb"));


	static_assert(flag_info('D')->shortcut_lower() == 'd');
	static_assert(flag_info('u')->shortcut_lower() == 'u');
}

/*
 * flags_from_expr
 */
[[maybe_unused]] static void
test_flags_from_expr()
{
	static_assert(flags_from_absolute_expr("SRP").value() ==
		      (Flags::Seen | Flags::Replied | Flags::Passed));
	static_assert(flags_from_absolute_expr("Faul").value() ==
		      (Flags::Flagged | Flags::Unread |
		       Flags::HasAttachment | Flags::MailingList));

	/* note: unread is a special flag, _implied_ from "new or not seen" */
	static_assert(flags_from_absolute_expr("N").value() == (Flags::New|Flags::Unread));

	static_assert(!flags_from_absolute_expr("DRT?"));
	static_assert(flags_from_absolute_expr("DRT?", true/*ignore invalid*/).value() ==
		      (Flags::Draft | Flags::Replied |
		       Flags::Trashed | Flags::Unread));
	static_assert(flags_from_absolute_expr("DFPNxulabcdef", true/*ignore invalid*/).value() ==
		      (Flags::Draft|Flags::Flagged|Flags::Passed|
		       Flags::New | Flags::Encrypted |
		       Flags::Unread | Flags::MailingList | Flags::Calendar |
		       Flags::HasAttachment));
}


/*
 * flags_from_delta_expr
 */
[[maybe_unused]] static void
test_flags_from_delta_expr()
{
	static_assert(flags_from_delta_expr(
			      "+S-u-N", Flags::New|Flags::Unread).value() ==
		      Flags::Seen);

	/* note: unread is a special flag, _implied_ from "new or not seen" */
	static_assert(flags_from_delta_expr(
			      "+S-N", Flags::New|Flags::Unread).value() ==
		      Flags::Seen);
	static_assert(flags_from_delta_expr(
			      "-S", Flags::Seen).value() ==
		      Flags::Unread);

	static_assert(flags_from_delta_expr("+R+P-F", Flags::Seen).value() ==
		      (Flags::Seen|Flags::Passed|Flags::Replied));
	/* '-B' is invalid */
	static_assert(!flags_from_delta_expr("+R+P-B", Flags::Seen));
	/* '-B' is invalid, but ignore invalid */
	static_assert(flags_from_delta_expr("+R+P-B", Flags::Seen, true) ==
		      (Flags::Replied|Flags::Passed|Flags::Seen));
	static_assert(flags_from_delta_expr("+F+T-S", Flags::None, true).value() ==
		      (Flags::Flagged|Flags::Trashed|Flags::Unread));
}

/*
 * flags_filter
 */
[[maybe_unused]] static void
test_flags_filter()
{
	static_assert(flags_filter(flags_from_absolute_expr(
						   "DFPNxulabcdef", true/*ignore invalid*/).value(),
					   MessageFlagCategory::Mailfile) ==
		      (Flags::Draft|Flags::Flagged|Flags::Passed));
}



[[maybe_unused]] static void
test_flags_keep_unmutable()
{
	static_assert(flags_keep_unmutable((Flags::Seen|Flags::Passed),
					   (Flags::Flagged|Flags::Draft),
					   Flags::Replied) ==
		      (Flags::Flagged|Flags::Draft));
}



#ifdef BUILD_TESTS
int
main(int argc, char* argv[])
{
	g_test_init(&argc, &argv, NULL);

	g_test_add_func("/message/flags/basic", test_basic);
	g_test_add_func("/message/flags/flag-info", test_flag_info);
	g_test_add_func("/message/flags/flags-from-absolute-expr",
			test_flags_from_expr);
	g_test_add_func("/message/flags/flags-from-delta-expr",
			test_flags_from_delta_expr);
	g_test_add_func("/message/flags/flags-filter",
			test_flags_filter);
	g_test_add_func("/message/flags/flags-keep-unmutable",
			test_flags_keep_unmutable);

	return g_test_run();
}
#endif /*BUILD_TESTS*/
