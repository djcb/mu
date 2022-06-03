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
#include "mu-config.hh"
#include "utils/mu-util.h"
#include "utils/mu-utils.hh"
#include <message/mu-message.hh>
#include <regex>

using namespace Mu;


static Result<void>
save_part(const Message::Part& part, size_t idx, const MuConfig* opts)
{
	const auto targetdir = std::invoke([&]{
		auto tdir{std::string{opts->targetdir ? opts->targetdir : ""}};
		return tdir.empty() ? tdir : tdir + G_DIR_SEPARATOR_S;
	});
	const auto path{targetdir +
		part.cooked_filename().value_or(format("part-%zu", idx))};

	if (auto&& res{part.to_file(path, opts->overwrite)}; !res)
		return Err(res.error());

	if (opts->play) {
		GError *err{};
		if (auto res{mu_util_play(path.c_str(), &err)};
		    res != MU_OK)
			return Err(Error::Code::Play, &err, "playing '%s' failed",
				   path.c_str());
	}

	return Ok();
}

static Result<void>
save_parts(const std::string& path, Option<std::string>& filename_rx,
	   const MuConfig* opts)
{
	auto message{Message::make_from_path(path, mu_config_message_options(opts))};
	if (!message)
		return Err(std::move(message.error()));


	size_t partnum{}, saved_num{};
	const auto partnums = std::invoke([&]()->std::vector<size_t> {
			std::vector<size_t> nums;
			for (auto&& numstr : split(opts->parts ? opts->parts : "", ','))
				nums.emplace_back(
					static_cast<size_t>(::atoi(numstr.c_str())));
			return nums;
		});


	for (auto&& part: message->parts()) {

		++partnum;

		if (!opts->save_all) {

			if (!partnums.empty() &&
			    !seq_some(partnums, [&](auto&& num){return num==partnum;}))
				continue; // not a wanted partnum.

			if (filename_rx && (!part.raw_filename() ||
					    !std::regex_match(*part.raw_filename(),
							      std::regex{*filename_rx})))
				continue; // not a wanted pattern.
		}

		if (auto res = save_part(part, partnum, opts); !res)
			return res;

		++saved_num;
	}

	// if (saved_num == 0)
	//	return Err(Error::Code::File,
	//		   "no %s extracted from this message",
	//		   opts->save_attachments ? "attachments" : "parts");
	// else
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
show_parts(const char* path, const MuConfig* opts)
{
	//msgopts = mu_config_get_msg_options(opts);

	auto msg_res{Message::make_from_path(path, mu_config_message_options(opts))};
	if (!msg_res)
		return Err(std::move(msg_res.error()));

	/* TODO: update this for crypto */
	size_t index{};
	g_print("MIME-parts in this message:\n");
	for (auto&& part: msg_res->parts())
		show_part(part, ++index, !opts->nocolor);

	return Ok();
}

static Mu::Result<void>
check_params(const MuConfig* opts)
{
	size_t param_num;
	param_num = mu_config_param_num(opts);

	if (param_num < 2)
		return Err(Error::Code::InvalidArgument, "parameters missing");

	if (opts->save_attachments || opts->save_all)
		if (opts->parts || param_num == 3)
			return Err(Error::Code::User,
				   "--save-attachments and --save-all don't "
				   "accept a filename pattern or --parts");

	if (opts->save_attachments && opts->save_all)
		return Err(Error::Code::User,
			   "only one of --save-attachments and"
			   " --save-all is allowed");
	return Ok();
}


Mu::Result<void>
Mu::mu_cmd_extract(const MuConfig* opts)
{
	if (!opts ||  opts->cmd != MU_CONFIG_CMD_EXTRACT)
		return Err(Error::Code::Internal, "error in arguments");
	if (auto res = check_params(opts); !res)
		return Err(std::move(res.error()));

	if (!opts->params[2] && !opts->parts &&
	    !opts->save_attachments && !opts->save_all)
		return show_parts(opts->params[1], opts); /* show, don't save */

	if (!mu_util_check_dir(opts->targetdir, FALSE, TRUE))
		return Err(Error::Code::File,
			   "target '%s' is not a writable directory",
			   opts->targetdir);

	Option<std::string> pattern{};
	if (opts->params[2])
		pattern = opts->params[2];

	return save_parts(opts->params[1], pattern, opts);
}
