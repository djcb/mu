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
 * The file-components, ie.
 *     1631819685.fb7b279bbb0a7b66.evergrey:2,RS
 *     => {
 *       "1631819685.fb7b279bbb0a7b66.evergrey",
 *       ':',
 *       "2,",
 *       "RS"
 *     }
 */
struct FileParts {
	std::string	base;
	char		separator;
	std::string	flags_suffix;;
};

static FileParts
message_file_parts(const std::string& file)
{
	const auto pos{file.find_last_of(":!;")};

	/* no suffix at all? */
	if (pos == std::string::npos ||
	    pos >= file.length() - 3 ||
	    file[pos + 1] != '2' ||
	    file[pos + 2] != ',')
		return FileParts{ file, ':', {}};

	return FileParts {
		file.substr(0, pos),
		file[pos],
		file.substr(pos + 3)
	};
}


struct DirFile {
	std::string dir;
	std::string file;
	bool is_new;
};

static Option<DirFile>
base_message_dir_file(const std::string& path)
{
	constexpr auto newdir{ G_DIR_SEPARATOR_S "new"};

	if (path.empty())
		return Nothing;

	char *dirname{g_path_get_dirname(path.c_str())};
	bool is_new{!!g_str_has_suffix(dirname, newdir)};

	std::string mdir{dirname, ::strlen(dirname) - 4};
	g_free(dirname);

	char *basename{g_path_get_basename(path.c_str())};
	std::string bname{basename};
	g_free(basename);

	return DirFile{std::move(mdir), std::move(bname), is_new};
}

Mu::Option<Mu::Flags>
Mu::flags_from_path(const std::string& path)
{	/*
	 * this gets us the source maildir filesystem path, the directory
	 * in which new/ & cur/ lives, and the source file
	 */
	auto dirfile{base_message_dir_file(path)};
	if (!dirfile)
		return Nothing;

	/* a message under new/ is just.. New. Filename is not considered */
	if (dirfile->is_new)
		return Flags::New;

	/* it's cur/ message, so parse the file name */
	const auto parts{message_file_parts(dirfile->file)};
	return flags_from_absolute_expr(parts.flags_suffix, true/*ignore invalid*/);
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
	static_assert(!flag_info('q'));

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

	static_assert(!flags_from_absolute_expr("DRT?"));
	static_assert(flags_from_absolute_expr("DRT?", true/*ignore invalid*/).value() ==
		      (Flags::Draft | Flags::Replied |
		       Flags::Trashed));
	static_assert(flags_from_absolute_expr("DFPNxulabcdef", true/*ignore invalid*/).value() ==
		      (Flags::Draft|Flags::Flagged|Flags::Passed|
		       Flags::New | Flags::Encrypted |
		       Flags::Unread | Flags::MailingList |
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
	static_assert(flags_from_delta_expr("+R+P-F", Flags::Seen).value() ==
		      (Flags::Seen|Flags::Passed|Flags::Replied));
	/* '-B' is invalid */
	static_assert(!flags_from_delta_expr("+R+P-B", Flags::Seen));
	/* '-B' is invalid, but ignore invalid */
	static_assert(flags_from_delta_expr("+R+P-B", Flags::Seen, true) ==
		      (Flags::Replied|Flags::Passed|Flags::Seen));
	static_assert(flags_from_delta_expr("+F+T-S", Flags::None, true).value() ==
		      (Flags::Flagged|Flags::Trashed));
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

	return g_test_run();
}
#endif /*BUILD_TESTS*/
