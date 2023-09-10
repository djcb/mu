/*
** Copyright (C) 2022-2023 Dirk-Jan C. Binnema <djcb.bulk@gmail.com>
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

#include "mu-message-file.hh"
#include "utils/mu-utils-file.hh"

using namespace Mu;

Result<std::string>
Mu::maildir_from_path(const std::string& path, const std::string& root)
{
	const auto pos = path.find(root);
	if (pos != 0 || path[root.length()] != '/')
		return Err(Error{Error::Code::InvalidArgument,
				"root '{}' is not a root for path '{}'", root, path});

	auto mdir{path.substr(root.length())};
	auto slash{mdir.rfind('/')};

	if (G_UNLIKELY(slash == std::string::npos) || slash < 4)
		return Err(Error{Error::Code::InvalidArgument,
				"invalid path: {}", path});
	mdir.erase(slash);
	auto subdir = mdir.data() + slash - 4;
	if (G_UNLIKELY(strncmp(subdir, "/cur", 4) != 0 && strncmp(subdir, "/new", 4)))
		return Err(Error::Code::InvalidArgument,
			   "cannot find '/new' or '/cur' - invalid path: {}", path);

	if (mdir.length() == 4)
		return "/";

	mdir.erase(mdir.length() - 4);

	return Ok(std::move(mdir));
}

Mu::FileParts
Mu::message_file_parts(const std::string& file)
{
	const auto pos{file.find_last_of(":!;")};

	/* no suffix at all? */
	if (pos == std::string::npos ||
	    pos > file.length() - 3 ||
	    file[pos + 1] != '2' ||
	    file[pos + 2] != ',')
		return FileParts{ file, ':', {}};

	return FileParts {
		file.substr(0, pos),
		file[pos],
		file.substr(pos + 3)
	};
}

Mu::Result<DirFile>
Mu::base_message_dir_file(const std::string& path)
{
	constexpr auto newdir{"/new"};

	const auto dname{dirname(path)};
	bool is_new{!!g_str_has_suffix(dname.c_str(), newdir)};

	std::string mdir{dname.substr(0, dname.size() - 4)};
	return Ok(DirFile{std::move(mdir), basename(path), is_new});
}

Mu::Result<Mu::Flags>
Mu::flags_from_path(const std::string& path)
{	/*
	 * this gets us the source maildir filesystem path, the directory
	 * in which new/ & cur/ lives, and the source file
	 */
	auto dirfile{base_message_dir_file(path)};
	if (!dirfile)
		return Err(std::move(dirfile.error()));

	/* a message under new/ is just.. New. Filename is not considered */
	if (dirfile->is_new)
		return Ok(Flags::New);

	/* it's cur/ message, so parse the file name */
	const auto parts{message_file_parts(dirfile->file)};
	auto flags{flags_from_absolute_expr(parts.flags_suffix,
					    true/*ignore invalid*/)};
	if (!flags) {
		/* LCOV_EXCL_START*/
		return Err(Error{Error::Code::InvalidArgument,
				"invalid flags ('{}')", parts.flags_suffix});
		/* LCOV_EXCL_STOP*/
	}

	/* of course, only _file_ flags are allowed */
	return Ok(flags_filter(flags.value(), MessageFlagCategory::Mailfile));
}


#ifdef BUILD_TESTS

#include "utils/mu-test-utils.hh"

static void
test_maildir_from_path()
{
	std::array<std::tuple<std::string, std::string, std::string>, 1> test_cases = {{
		{ "/home/foo/Maildir/hello/cur/msg123", "/home/foo/Maildir", "/hello" }
	}};

	for(auto&& tcase: test_cases)  {
		const auto res{maildir_from_path(std::get<0>(tcase), std::get<1>(tcase))};
		assert_valid_result(res);
		assert_equal(*res, std::get<2>(tcase));
	}

	g_assert_false(!!maildir_from_path("/home/foo/Maildir/cur/test1", "/home/bar"));
	g_assert_false(!!maildir_from_path("/x", "/x/y"));
	g_assert_false(!!maildir_from_path("/home/a/Maildir/b/xxx/test", "/home/a/Maildir"));
}

static void
test_base_message_dir_file()
{
	struct TestCase {
		const std::string path;
		DirFile expected;
	};
	std::array<TestCase, 1> test_cases = {{
			{ "/home/djcb/Maildir/foo/cur/msg:2,S",
			  { "/home/djcb/Maildir/foo", "msg:2,S", false } }
		}};
	for(auto&& tcase: test_cases)  {
		const auto res{base_message_dir_file(tcase.path)};
		assert_valid_result(res);
		assert_equal(res->dir, tcase.expected.dir);
		assert_equal(res->file, tcase.expected.file);
		g_assert_cmpuint(res->is_new, ==, tcase.expected.is_new);
	}
}

static void
test_flags_from_path()
{
	std::array<std::pair<std::string, Flags>, 5> test_cases = {{
		{"/home/foo/Maildir/test/cur/123456:2,FSR",
		 (Flags::Replied | Flags::Seen | Flags::Flagged)},
		{"/home/foo/Maildir/test/new/123456", Flags::New},
		{/* NOTE: when in new/, the :2,.. stuff is ignored */
			"/home/foo/Maildir/test/new/123456:2,FR",
			Flags::New},
		{"/home/foo/Maildir/test/cur/123456:2,DTP",
		 (Flags::Draft | Flags::Trashed | Flags::Passed)},
		{"/home/foo/Maildir/test/cur/123456:2,S", Flags::Seen}
	}};

	for (auto&& tcase: test_cases) {
		auto res{flags_from_path(tcase.first)};
		assert_valid_result(res);
		/* LCOV_EXCL_START*/
		if (g_test_verbose()) {
			mu_println("{} -> <{}>", tcase.first,
				   to_string(res.value()));
			g_assert_true(res.value() == tcase.second);
		}
		/*LCOV_EXCL_STOP*/
	}
}


int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	g_test_add_func("/message/file/maildir-from-path",
			test_maildir_from_path);
	g_test_add_func("/message/file/base-message-dir-file",
			test_base_message_dir_file);
	g_test_add_func("/message/file/flags-from-path", test_flags_from_path);

	return g_test_run();
}
#endif /*BUILD_TESTS*/
