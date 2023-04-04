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

#include <iostream>
#include <iomanip>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

#include "mu-options.hh"
#include "mu-cmd.hh"
#include "mu-maildir.hh"
#include "mu-contacts-cache.hh"
#include "message/mu-message.hh"
#include "message/mu-mime-object.hh"

#include "utils/mu-error.hh"
#include "utils/mu-utils.hh"
#include "message/mu-message.hh"

#include <thirdparty/tabulate.hpp>

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
	gboolean    color;

	color = !opts.nocolor;

	const auto body{message.body_text()};
	if (!body || body->empty()) {
		if (any_of(message.flags() & Flags::Encrypted)) {
			color_maybe(MU_COLOR_CYAN);
			g_print("[No text body found; "
				"message has encrypted parts]\n");
		} else {
			color_maybe(MU_COLOR_MAGENTA);
			g_print("[No text body found]\n");
		}
		color_maybe(MU_COLOR_DEFAULT);
		return;
	}

	if (opts.view.summary_len) {
		const auto summ{summarize(body->c_str(), *opts.view.summary_len)};
		print_field("Summary", summ, color);
	} else {
		print_encoded("%s", body->c_str());
		if (!g_str_has_suffix(body->c_str(), "\n"))
			g_print("\n");
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
	body_or_summary(message, opts);

	return Ok();
}

static Mu::Result<void>
handle_msg(const std::string& fname, const Options& opts)
{
	using Format = Options::View::Format;

	auto message{Message::make_from_path(fname, message_options(opts.view))};
	if (!message)
		return Err(message.error());

	switch (opts.view.format) {
	case Format::Plain:
		return view_msg_plain(*message, opts);
	case Format::Sexp:
		return view_msg_sexp(*message, opts);
	default:
		g_critical("bug: should not be reached");
		return Err(Error::Code::Internal, "error");
	}
}

static Mu::Result<void>
cmd_view(const Options& opts)
{
	for (auto&& file: opts.view.files) {
		if (auto res = handle_msg(file, opts); !res)
			return res;
		/* add a separator between two messages? */
		if (opts.view.terminate)
			g_print("%c", VIEW_TERMINATOR);
	}

	return Ok();
}

static Mu::Result<void>
cmd_mkdir(const Options& opts)
{
	for (auto&& dir: opts.mkdir.dirs) {
		if (auto&& res =
		    maildir_mkdir(dir, opts.mkdir.mode); !res)
			return res;
	}

	return Ok();
}

static Result<void>
cmd_add(Mu::Store& store, const Options& opts)
{
	for (auto&& file: opts.add.files) {
		const auto docid{store.add_message(file)};
		if (!docid)
			return Err(docid.error());
		else
			g_debug("added message @ %s, docid=%u",
				file.c_str(), docid.value());
	}

	return Ok();
}

static Result<void>
cmd_remove(Mu::Store& store, const Options& opts)
{
	for (auto&& file: opts.remove.files) {
		const auto res = store.remove_message(file);
		if (!res)
			return Err(Error::Code::File, "failed to remove %s", file.c_str());
		else
			g_debug("removed message @ %s", file.c_str());
	}

	return Ok();
}


template <typename T>
static void
key_val(const Mu::MaybeAnsi& col, const std::string& key, T val)
{
	using Color = Mu::MaybeAnsi::Color;

	std::cout << col.fg(Color::BrightBlue) << std::left << std::setw(18) << key << col.reset()
		  << ": ";

	std::cout << col.fg(Color::Green) << val << col.reset() << "\n";
}


static void
print_signature(const Mu::MimeSignature& sig, const Options& opts)
{
	Mu::MaybeAnsi col{!opts.nocolor};

	const auto created{sig.created()};
	key_val(col, "created",
		created == 0 ? "unknown" :
		time_to_string("%c", sig.created()).c_str());

	const auto expires{sig.expires()};
	key_val(col, "expires", expires==0 ? "never" :
		time_to_string("%c", sig.expires()).c_str());

	const auto cert{sig.certificate()};
	key_val(col, "public-key algo",
		to_string_view_opt(cert.pubkey_algo()).value_or("unknown"));
	key_val(col, "digest algo",
		to_string_view_opt(cert.digest_algo()).value_or("unknown"));
	key_val(col, "id-validity",
		to_string_view_opt(cert.id_validity()).value_or("unknown"));
	key_val(col, "trust",
		to_string_view_opt(cert.trust()).value_or("unknown"));
	key_val(col, "issuer-serial", cert.issuer_serial().value_or("unknown"));
	key_val(col, "issuer-name", cert.issuer_name().value_or("unknown"));
	key_val(col, "finger-print", cert.fingerprint().value_or("unknown"));
	key_val(col, "key-id", cert.key_id().value_or("unknown"));
	key_val(col, "name", cert.name().value_or("unknown"));
	key_val(col, "user-id", cert.user_id().value_or("unknown"));
}


static bool
verify(const MimeMultipartSigned& sigpart, const Options& opts)
{
	using VFlags = MimeMultipartSigned::VerifyFlags;
	const auto vflags{opts.verify.auto_retrieve ?
		VFlags::EnableKeyserverLookups: VFlags::None};

	auto ctx{MimeCryptoContext::make_gpg()};
	if (!ctx)
		return false;

	const auto sigs{sigpart.verify(*ctx, vflags)};
	Mu::MaybeAnsi col{!opts.nocolor};

	if (!sigs || sigs->empty()) {

		if (!opts.quiet)
			g_print("cannot find signatures in part\n");

		return true;
	}

	bool valid{true};
	for (auto&& sig: *sigs) {

		const auto status{sig.status()};

		if (!opts.quiet)
			key_val(col, "status", to_string(status));

		if (opts.verbose)
			print_signature(sig, opts);

		if (none_of(sig.status() & MimeSignature::Status::Green))
			valid = false;
	}

	return valid;
}

static Mu::Result<void>
cmd_verify(const Options& opts)
{
	bool all_ok{true};
	const auto mopts = message_options(opts.verify);

	for (auto&& file: opts.verify.files) {

		auto message{Message::make_from_path(file, mopts)};
		if (!message)
			return Err(message.error());

		if (!opts.quiet && opts.verify.files.size() > 1)
			g_print("verifying %sn\n", file.c_str());

		if (none_of(message->flags() & Flags::Signed)) {
			if (!opts.quiet)
				g_print("%s: no signed parts found\n", file.c_str());
			continue;
		}

		bool verified{true}; /* innocent until proven guilty */
		for(auto&& part: message->parts()) {

			if (!part.is_signed())
				continue;

			const auto& mobj{part.mime_object()};
			if (!mobj.is_multipart_signed())
				continue;

			if (!verify(MimeMultipartSigned(mobj), opts))
				verified = false;
		}

		all_ok = all_ok && verified;
	}

	if (all_ok)
		return Ok();
	else
		return Err(Error::Code::UnverifiedSignature,
			   "failed to verify one or more signatures");
}

static Result<void>
cmd_info(const Mu::Store& store, const Options& opts)
{
	using namespace tabulate;

	if (!locale_workaround())
		return Err(Error::Code::User, "failed to find a working locale");

	auto colorify = [](Table& table) {
		for (auto&& row: table) {

			if (row.cells().size() < 2)
				continue;

			row.cells().at(0)->format().font_style({FontStyle::bold})
				.font_color(Color::green);
			row.cells().at(1)->format().font_color(Color::blue);
		}
	};

	auto tstamp = [](::time_t t)->std::string {
		if (t == 0)
			return "never";
		else
			return time_to_string("%c", t);

	};

	Table info;
	info.add_row({"maildir", store.properties().root_maildir});
	info.add_row({"database-path", store.properties().database_path});
	info.add_row({"schema-version", store.properties().schema_version});
	info.add_row({"max-message-size", format("%zu", store.properties().max_message_size)});
	info.add_row({"batch-size", format("%zu", store.properties().batch_size)});
	info.add_row({"created", tstamp(store.properties().created)});
	for (auto&& c : store.properties().personal_addresses)
			info.add_row({"personal-address", c});

	info.add_row({"messages in store", format("%zu", store.size())});
	info.add_row({"last-change", tstamp(store.statistics().last_change)});
	info.add_row({"last-index",  tstamp(store.statistics().last_index)});

	if (!opts.nocolor)
		colorify(info);

	std::cout << info << '\n';

	return Ok();
}

static Result<void>
cmd_init(const Options& opts)
{
	auto store = std::invoke([&]()->Result<Store> {

		/*
		 * reinit
		 */
		if (opts.init.reinit)
			return Store::make(opts.runtime_path(RuntimePath::XapianDb),
					   Store::Options::ReInit|Store::Options::Writable);
		/*
		 * full init
		 */

		/* not provided, nor could we find a good default */
		if (opts.init.maildir.empty())
			return Err(Error::Code::InvalidArgument,
				   "missing --maildir parameter and could "
				   "not determine default");

		Mu::Store::Config conf{};
		conf.max_message_size = opts.init.max_msg_size.value_or(0);
		conf.batch_size       = opts.init.batch_size.value_or(0);

		return Store::make_new(opts.runtime_path(RuntimePath::XapianDb),
				       opts.init.maildir, opts.init.my_addresses,  conf);
	});

	if (!store)
		return Err(store.error());

	if (!opts.quiet) {
		cmd_info(*store, opts);
		std::cout << "database "
			  << (opts.init.reinit ? "reinitialized" : "created")
			  << "; use the 'index' command to fill/update it.\n";
	}
	return Ok();
}

static Result<void>
cmd_find(const Options& opts)
{
	auto store{Store::make(opts.runtime_path(RuntimePath::XapianDb))};
	if (!store)
		return Err(store.error());
	else
		return mu_cmd_find(*store, opts);
}

static void
show_usage(void)
{
	g_print("usage: mu command [options] [parameters]\n");
	g_print("where command is one of index, find, cfind, view, mkdir, "
		"extract, add, remove, script, verify or server\n");
	g_print("see the mu, mu-<command> or mu-easy manpages for "
		"more information\n");
}


using ReadOnlyStoreFunc = std::function<Result<void>(const Store&, const Options&)>;
using WritableStoreFunc = std::function<Result<void>(Store&, const Options&)>;

static Result<void>
with_readonly_store(const ReadOnlyStoreFunc& func, const Options& opts)
{
	auto store{Store::make(opts.runtime_path(RuntimePath::XapianDb))};
	if (!store)
		return Err(store.error());

	return func(store.value(), opts);
}

static Result<void>
with_writable_store(const WritableStoreFunc func, const Options& opts)
{
	auto store{Store::make(opts.runtime_path(RuntimePath::XapianDb),
			       Store::Options::Writable)};
	if (!store)
		return Err(store.error());

	return func(store.value(), opts);
}

Result<void>
Mu::mu_cmd_execute(const Options& opts) try {

	if (!opts.sub_command)
		return Err(Error::Code::Internal, "missing subcommand");

	switch (*opts.sub_command) {
	case Options::SubCommand::Help:
		return Ok(); /* already handled in mu-options.cc */
	/*
	 * no store needed
	 */
	case Options::SubCommand::Fields:
		return mu_cmd_fields(opts);
	case Options::SubCommand::Mkdir:
		return cmd_mkdir(opts);
	case Options::SubCommand::Script:
		return mu_cmd_script(opts);
	case Options::SubCommand::View:
		return cmd_view(opts);
	case Options::SubCommand::Verify:
		return cmd_verify(opts);
	case Options::SubCommand::Extract:
		return mu_cmd_extract(opts);
	/*
	 * read-only store
	 */

	case Options::SubCommand::Cfind:
		return with_readonly_store(mu_cmd_cfind, opts);
	case Options::SubCommand::Find:
		return cmd_find(opts);
	case Options::SubCommand::Info:
		return with_readonly_store(cmd_info, opts);

	/* writable store */

	case Options::SubCommand::Add:
		return with_writable_store(cmd_add, opts);
	case Options::SubCommand::Remove:
		return with_writable_store(cmd_remove, opts);
	case Options::SubCommand::Index:
		return with_writable_store(mu_cmd_index, opts);

	/* commands instantiate store themselves */
	case Options::SubCommand::Init:
		return cmd_init(opts);
	case Options::SubCommand::Server:
		return mu_cmd_server(opts);

	default:
		show_usage();
		return Ok();
	}

} catch (const Mu::Error& er) {
	return Err(er);
} catch (const std::runtime_error& re) {
	return Err(Error::Code::Internal, "runtime-error: %s", re.what());
} catch (const std::exception& ex) {
	return Err(Error::Code::Internal, "error: %s", ex.what());
} catch (...) {
	return Err(Error::Code::Internal, "caught exception");
}
