/*
** Copyright (C) 2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-cmd.hh"

#include "message/mu-message.hh"

#include <iostream>
#include <iomanip>

using namespace Mu;


#define VIEW_TERMINATOR '\f' /* form-feed */

using namespace Mu;

static Mu::Result<void>
view_msg_sexp(const Message& message, const Options& opts)
{
	::fputs(message.sexp().to_string().c_str(), stdout);
	::fputs("\n", stdout);

	return Ok();
}


static std::string /* return comma-sep'd list of attachments */
get_attach_str(const Message& message, const Options& opts)
{
	std::string str;
	seq_for_each(message.parts(), [&](auto&& part) {
		if (auto fname = part.raw_filename(); fname) {
			if (str.empty())
				str = fname.value();
			else
				str += ", " + fname.value();
		}
	});

	return str;
}

#define color_maybe(C)                          \
	do {                                    \
		if (color)                      \
			fputs((C), stdout);     \
	} while (0)

static void
print_field(const std::string& field, const std::string& val, bool color)
{
	if (val.empty())
		return;

	color_maybe(MU_COLOR_MAGENTA);
	fputs_encoded(field, stdout);
	color_maybe(MU_COLOR_DEFAULT);
	fputs(": ", stdout);

	color_maybe(MU_COLOR_GREEN);
	fputs_encoded(val, stdout);

	color_maybe(MU_COLOR_DEFAULT);
	fputs("\n", stdout);
}

/* a summary_len of 0 mean 'don't show summary, show body */
static void
body_or_summary(const Message& message, const Options& opts)
{
	const auto color{!opts.nocolor};
	using Format = Options::View::Format;

	std::string body, btype;
	switch (opts.view.format) {
	case Format::Plain:
		btype = "plain text";
		body = message.body_text().value_or("");
		break;
	case Format::Html:
		btype = "html";
		body = message.body_html().value_or("");
		break;
	default:
		throw std::range_error("unsupported format"); // bug
	}

	if (body.empty()) {
		if (any_of(message.flags() & Flags::Encrypted)) {
			color_maybe(MU_COLOR_CYAN);
			mu_println("[No {} body found; message does have encrypted parts]",
				   btype);
		} else {
			color_maybe(MU_COLOR_MAGENTA);
			mu_println("[No {} body found]", btype);
		}
		color_maybe(MU_COLOR_DEFAULT);
		return;
	}

	if (opts.view.summary_len) {
		const auto summ{summarize(body, *opts.view.summary_len)};
		print_field("Summary", summ, color);
	} else {
		mu_print_encoded("{}", body);
		if (!g_str_has_suffix(body.c_str(), "\n"))
			mu_println("");
	}
}

/* we ignore fields for now */
/* summary_len == 0 means "no summary */
static Mu::Result<void>
view_msg_plain(const Message& message, const Options& opts)
{
	const auto color{!opts.nocolor};

	print_field("From",    to_string(message.from()), color);
	print_field("To",      to_string(message.to()), color);
	print_field("Cc",      to_string(message.cc()), color);
	print_field("Bcc",     to_string(message.bcc()), color);
	print_field("Subject", message.subject(), color);

	if (auto&& date = message.date(); date != 0)
		print_field("Date", time_to_string("%c", date), color);

	print_field("Tags", join(message.tags(), ", "), color);

	print_field("Attachments",get_attach_str(message, opts), color);

	mu_println("");
	body_or_summary(message, opts);

	return Ok();
}

static Mu::Result<void>
handle_msg(const Message& message, const Options& opts)
{
	using Format = Options::View::Format;

	switch (opts.view.format) {
	case Format::Plain:
	case Format::Html:
		return view_msg_plain(message, opts);

	case Format::Sexp:
		return view_msg_sexp(message, opts);
	default:
		mu_critical("bug: should not be reached");
		return Err(Error::Code::Internal, "error");
	}
}

Mu::Result<void>
Mu::mu_cmd_view(const Options& opts)
{
	for (auto&& file: opts.view.files) {
		auto message{Message::make_from_path(
				file, message_options(opts.view))};
		if (!message)
			return Err(message.error());

		if (auto res = handle_msg(*message, opts); !res)
			return res;
		/* add a separator between two messages? */
		if (opts.view.terminate)
			mu_print("{}", VIEW_TERMINATOR);
	}

	// no files? read from stding
	if (opts.view.files.empty()) {
		const auto msgtxt = read_from_stdin();
		if (!msgtxt)
			return Err(msgtxt.error());
		auto message = Message::make_from_text(*msgtxt,{},
						       message_options(opts.view));
		if (!message)
			return Err(message.error());
		else
			return handle_msg(*message, opts);
	}
	return Ok();
}
