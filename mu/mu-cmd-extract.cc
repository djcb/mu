/*
** Copyright (C) 2010-2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "config.h"
#include "mu-cmd.hh"
#include "utils/mu-utils.hh"
#include "utils/mu-utils-file.hh"
#include "utils/mu-regex.hh"
#include <message/mu-message.hh>

using namespace Mu;

static Result<void>
save_part(const Message::Part& part, size_t idx, const Options& opts)
{
	const auto targetdir = std::invoke([&]{
		const auto tdir{opts.extract.targetdir};
		return tdir.empty() ? tdir : tdir + G_DIR_SEPARATOR_S;
	});

	/* 'uncooked' isn't really _raw_; it means only doing some _minimal_
	 * cooking */
	const auto path{targetdir +
		part.cooked_filename(opts.extract.uncooked)
		.value_or(mu_format("part-{}", idx))};

	if (auto&& res{part.to_file(path, opts.extract.overwrite)}; !res)
		return Err(res.error());
	else if (opts.extract.play)
		return play(path);
	else
		return Ok();
}

static Result<void>
save_parts(const Message& message, const std::string& filename_rx,
	   const Options& opts)
{
	size_t partnum{}, saved_num{};
	for (auto&& part: message.parts()) {
		++partnum;
		// should we extract this part?
		const auto do_extract = std::invoke([&]() {

			if (opts.extract.save_all)
				return true;
			else if (opts.extract.save_attachments &&
			    part.looks_like_attachment())
				return true;
			else if (seq_some(opts.extract.parts,
				     [&](auto&& num){return num==partnum;}))
				return true;
			else if (!filename_rx.empty() && part.raw_filename()) {
				if (auto rx = Regex::make(filename_rx); !rx)
					throw rx.error();
				else if (rx->matches(*part.raw_filename()))
					return true;
			}
			return false;
		});

		if (!do_extract)
			continue;

		if (auto res = save_part(part, partnum, opts); !res)
			return res;

		++saved_num;
	}

	if (saved_num == 0)
		return Err(Error::Code::File,
			   "no {} extracted from this message",
			   opts.extract.save_attachments ? "attachments" : "parts");
	else
		return Ok();
}

#define color_maybe(C)                          \
	do {                                    \
		if (color)                      \
			fputs((C), stdout);     \
	} while (0)

static void
show_part(const MessagePart& part, size_t index, bool color)
{
	/* index */
	mu_print("  {} ", index);

	/* filename */
	color_maybe(MU_COLOR_GREEN);
	const auto fname{part.raw_filename()};
	fputs_encoded(fname.value_or("<none>"), stdout);
	fputs_encoded(" ", stdout);

	/* content-type */
	color_maybe(MU_COLOR_BLUE);
	const auto ctype{part.mime_type()};
	fputs_encoded(ctype.value_or("<none>"), stdout);

	/* /\* disposition *\/ */
	color_maybe(MU_COLOR_MAGENTA);
	mu_print_encoded(" [{}]", part.is_attachment() ? "attachment" : "inline");
	/* size */
	if (part.size() > 0) {
		color_maybe(MU_COLOR_CYAN);
		mu_print(" ({} bytes)", part.size());
	}

	color_maybe(MU_COLOR_DEFAULT);
	fputs("\n", stdout);
}

static Mu::Result<void>
show_parts(const Message& message, const Options& opts)
{
	size_t index{};
	mu_println("MIME-parts in this message:");
	for (auto&& part: message.parts())
		show_part(part, ++index, !opts.nocolor);

	return Ok();
}

Mu::Result<void>
Mu::mu_cmd_extract(const Options& opts)
{
	auto message = std::invoke([&]()->Result<Message>{
		const auto mopts{message_options(opts.extract)};
		if (!opts.extract.message.empty())
			return Message::make_from_path(opts.extract.message, mopts);

		const auto msgtxt = read_from_stdin();
		if (!msgtxt)
			return Err(msgtxt.error());
		else
			return Message::make_from_text(*msgtxt, {}, mopts);
	});

	if (!message)
		return Err(message.error());
	else if (opts.extract.parts.empty() &&
		 !opts.extract.save_attachments && !opts.extract.save_all &&
		 opts.extract.filename_rx.empty())
		return show_parts(*message, opts); /* show, don't save */

	if (!check_dir(opts.extract.targetdir, false/*!readable*/, true/*writeable*/))
		return Err(Error::Code::File,
			   "target '{}' is not a writable directory",
			   opts.extract.targetdir);

	return save_parts(*message, opts.extract.filename_rx, opts);
}




#ifdef BUILD_TESTS
/*
 * Tests.
 *
 */

#include <glib.h>
#include <glib/gstdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <utils/mu-regex.hh>
#include "utils/mu-test-utils.hh"


static gint64
get_file_size(const std::string& path)
{
	int         rv;
	struct stat statbuf;

	mu_info("ppatj {}", path);

	rv = stat(path.c_str(), &statbuf);
	if (rv != 0) {
		mu_debug ("error: {}", g_strerror (errno));
		return -1;
	}

	mu_debug("{} -> {} bytes", path, statbuf.st_size);

	return statbuf.st_size;
}

static void
test_mu_extract_02(void)
{
	TempDir temp_dir{};
	auto res= run_command({
			MU_PROGRAM, "extract", "--save-attachments",
			mu_format("--target-dir='{}'", temp_dir.path()),
			join_paths(MU_TESTMAILDIR2, "Foo", "cur", "mail5")});
	assert_valid_result(res);
	g_assert_true(res->standard_err.empty());

	g_assert_cmpuint(get_file_size(join_paths(temp_dir.path(), "custer.jpg")), >=, 15955);
	g_assert_cmpuint(get_file_size(join_paths(temp_dir.path(), "custer.jpg")), <=, 15960);
	g_assert_cmpuint(get_file_size(join_paths(temp_dir.path(), "sittingbull.jpg")), ==, 17674);
}

static void
test_mu_extract_03(void)
{
	TempDir temp_dir{};
	auto res= run_command({
			MU_PROGRAM, "extract", "--parts=3",
			mu_format("--target-dir='{}'", temp_dir.path()),
			join_paths(MU_TESTMAILDIR2, "Foo", "cur", "mail5")});
	assert_valid_result(res);
	g_assert_true(res->standard_err.empty());

	g_assert_true(g_access(join_paths(temp_dir.path(), "custer.jpg").c_str(), F_OK) == 0);
	g_assert_false(g_access(join_paths(temp_dir.path(), "sittingbull.jpg").c_str(), F_OK) == 0);
}

static void
test_mu_extract_overwrite(void)
{
	TempDir temp_dir{};
	auto res= run_command({
			MU_PROGRAM, "extract", "-a",
			mu_format("--target-dir='{}'", temp_dir.path()),
			join_paths(MU_TESTMAILDIR2, "Foo", "cur", "mail5")});
	assert_valid_result(res);
	g_assert_true(res->standard_err.empty());

	g_assert_true(g_access(join_paths(temp_dir.path(), "custer.jpg").c_str(), F_OK) == 0);
	g_assert_true(g_access(join_paths(temp_dir.path(), "sittingbull.jpg").c_str(), F_OK) == 0);


	/* now, it should fail, because we don't allow overwrites
	 * without --overwrite */
	auto res2 = run_command({
			MU_PROGRAM, "extract", "-a",
			mu_format("--target-dir='{}'", temp_dir.path()),
			join_paths(MU_TESTMAILDIR2, "Foo", "cur", "mail5")});

	assert_valid_result(res2);
	g_assert_false(res2->standard_err.empty());


	auto res3 = run_command({
			MU_PROGRAM, "extract", "-a", "--overwrite",
			mu_format("--target-dir='{}'", temp_dir.path()),
			join_paths(MU_TESTMAILDIR2, "Foo", "cur", "mail5")});

	assert_valid_result(res3);
	g_assert_true(res3->standard_err.empty());
}

static void
test_mu_extract_by_name(void)
{
	TempDir temp_dir{};
	auto res= run_command({
			MU_PROGRAM, "extract",
			mu_format("--target-dir='{}'", temp_dir.path()),
			join_paths(MU_TESTMAILDIR2, "Foo", "cur", "mail5"),
			"sittingbull.jpg"});
	assert_valid_result(res);
	g_assert_true(res->standard_err.empty());

	g_assert_true(g_access(join_paths(temp_dir.path(), "sittingbull.jpg").c_str(), F_OK) == 0);
	g_assert_false(g_access(join_paths(temp_dir.path(), "custer.jpg").c_str(), F_OK) == 0);
}


int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	g_test_add_func("/cmd/extract/02", test_mu_extract_02);
	g_test_add_func("/cmd/extract/03", test_mu_extract_03);
	g_test_add_func("/cmd/extract/overwrite", test_mu_extract_overwrite);
	g_test_add_func("/cmd/extract/by-name", test_mu_extract_by_name);

	return g_test_run();
}

#endif
