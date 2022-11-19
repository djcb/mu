/*
** Copyright (C) 2010-2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include "utils/mu-util.h"
#include "utils/mu-utils.hh"
#include <message/mu-message.hh>
#include <regex>

using namespace Mu;


static Result<void>
save_part(const Message::Part& part, size_t idx, const Options& opts)
{
	const auto targetdir = std::invoke([&]{
		const auto tdir{opts.extract.targetdir};
		return tdir.empty() ? tdir : tdir + G_DIR_SEPARATOR_S;
	});
	const auto path{targetdir +
		part.cooked_filename().value_or(format("part-%zu", idx))};

	if (auto&& res{part.to_file(path, opts.extract.overwrite)}; !res)
		return Err(res.error());

	if (opts.extract.play) {
		GError *err{};
		if (auto res{mu_util_play(path.c_str(), &err)};
		    res != MU_OK)
			return Err(Error::Code::Play, &err, "playing '%s' failed",
				   path.c_str());
	}

	return Ok();
}

static Result<void>
save_parts(const std::string& path, const std::string& filename_rx,
	   const Options& opts)
{
	auto message{Message::make_from_path(path, message_options(opts.extract))};
	if (!message)
		return Err(std::move(message.error()));

	size_t partnum{}, saved_num{};
	for (auto&& part: message->parts()) {
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
			else if (!filename_rx.empty() && part.raw_filename() &&
				 std::regex_match(*part.raw_filename(),
						  std::regex{filename_rx}))
				return true;
			else
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
			   "no %s extracted from this message",
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
	g_print("  %zu ", index);

	/* filename */
	color_maybe(MU_COLOR_GREEN);
	const auto fname{part.raw_filename()};
	mu_util_fputs_encoded(fname ? fname->c_str() : "<none>", stdout);

	mu_util_fputs_encoded(" ", stdout);

	/* content-type */
	color_maybe(MU_COLOR_BLUE);
	const auto ctype{part.mime_type()};
	mu_util_fputs_encoded(ctype ? ctype->c_str() :  "<none>", stdout);

	/* /\* disposition *\/ */
	color_maybe(MU_COLOR_MAGENTA);
	mu_util_print_encoded(" [%s]", part.is_attachment() ?
			      "attachment" : "inline");
	/* size */
	if (part.size() > 0) {
		color_maybe(MU_COLOR_CYAN);
		g_print(" (%zu bytes)", part.size());
	}

	color_maybe(MU_COLOR_DEFAULT);
	fputs("\n", stdout);
}

static Mu::Result<void>
show_parts(const std::string& path, const Options& opts)
{
	auto msg_res{Message::make_from_path(path, message_options(opts.extract))};
	if (!msg_res)
		return Err(std::move(msg_res.error()));

	size_t index{};
	g_print("MIME-parts in this message:\n");
	for (auto&& part: msg_res->parts())
		show_part(part, ++index, !opts.nocolor);

	return Ok();
}

Mu::Result<void>
Mu::mu_cmd_extract(const Options& opts)
{
	if (opts.extract.parts.empty() &&
	    !opts.extract.save_attachments && !opts.extract.save_all &&
	    opts.extract.filename_rx.empty())
		return show_parts(opts.extract.message, opts); /* show, don't save */

	if (!mu_util_check_dir(opts.extract.targetdir.c_str(), FALSE, TRUE))
		return Err(Error::Code::File,
			   "target '%s' is not a writable directory",
			   opts.extract.targetdir.c_str());

	return save_parts(opts.extract.message, opts.extract.filename_rx, opts);
}
