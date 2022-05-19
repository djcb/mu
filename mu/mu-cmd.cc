/*
** Copyright (C) 2010-2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-config.hh"
#include "mu-cmd.hh"
#include "mu-maildir.hh"
#include "mu-contacts-cache.hh"
#include "mu-runtime.hh"
#include "message/mu-message.hh"
#include "message/mu-mime-object.hh"

#include "utils/mu-util.h"

#include "utils/mu-error.hh"
#include "utils/mu-utils.hh"
#include "message/mu-message.hh"

#include <thirdparty/tabulate.hpp>

#define VIEW_TERMINATOR '\f' /* form-feed */

using namespace Mu;

static Mu::Result<void>
view_msg_sexp(const Message& message, const MuConfig* opts)
{
	::fputs(message.to_sexp().to_sexp_string().c_str(), stdout);
	::fputs("\n", stdout);

	return Ok();
}


static std::string /* return comma-sep'd list of attachments */
get_attach_str(const Message& message, const MuConfig* opts)
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
	mu_util_fputs_encoded(field.c_str(), stdout);
	color_maybe(MU_COLOR_DEFAULT);
	fputs(": ", stdout);

	color_maybe(MU_COLOR_GREEN);
	mu_util_fputs_encoded(val.c_str(), stdout);

	color_maybe(MU_COLOR_DEFAULT);
	fputs("\n", stdout);
}

/* a summary_len of 0 mean 'don't show summary, show body */
static void
body_or_summary(const Message& message, const MuConfig* opts)
{
	gboolean    color;
	//int         my_opts = mu_config_get_msg_options(opts) | MU_MSG_OPTION_CONSOLE_PASSWORD;

	color = !opts->nocolor;

	const auto body{message.body_text()};
	if (!body || body->empty()) {
		if (any_of(message.flags() & Flags::Encrypted)) {
			color_maybe(MU_COLOR_CYAN);
			g_print("[No body found; "
				"message has encrypted parts]\n");
		} else {
			color_maybe(MU_COLOR_MAGENTA);
			g_print("[No body found]\n");
		}
		color_maybe(MU_COLOR_DEFAULT);
		return;
	}

	if (opts->summary_len != 0) {
		gchar* summ;
		summ = mu_str_summarize(body->c_str(), opts->summary_len);
		print_field("Summary", summ, color);
		g_free(summ);
	} else {
		mu_util_print_encoded("%s", body->c_str());
		if (!g_str_has_suffix(body->c_str(), "\n"))
			g_print("\n");
	}
}

/* we ignore fields for now */
/* summary_len == 0 means "no summary */
static Mu::Result<void>
view_msg_plain(const Message& message, const MuConfig* opts)
{
	const auto color{!opts->nocolor};

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
handle_msg(const std::string& fname, const MuConfig* opts)
{
	auto message{Message::make_from_path(fname, mu_config_message_options(opts))};
	if (!message)
		return Err(message.error());

	switch (opts->format) {
	case MU_CONFIG_FORMAT_PLAIN:
		return view_msg_plain(*message, opts);
	case MU_CONFIG_FORMAT_SEXP:
		return view_msg_sexp(*message, opts);
	default:
		g_critical("bug: should not be reached");
		return Err(Error::Code::Internal, "error");
	}
}

static Mu::Result<void>
view_params_valid(const MuConfig* opts)
{
	/* note: params[0] will be 'view' */
	if (!opts->params[0] || !opts->params[1])
		return Err(Error::Code::InvalidArgument, "error in parameters");

	switch (opts->format) {
	case MU_CONFIG_FORMAT_PLAIN:
	case MU_CONFIG_FORMAT_SEXP: break;
	default:
		return Err(Error::Code::InvalidArgument, "invalid output format");
	}

	return Ok();
}

static Mu::Result<void>
cmd_view(const MuConfig* opts)
{
	if (!opts || opts->cmd != Mu::MU_CONFIG_CMD_VIEW)
		return Err(Error::Code::InvalidArgument, "invalid parameters");
	if (auto res = view_params_valid(opts); !res)
		return res;

	for (auto i = 1; opts->params[i]; ++i) {
		if (auto res = handle_msg(opts->params[i], opts); !res)
			return res;
		/* add a separator between two messages? */
		if (opts->terminator)
			g_print("%c", VIEW_TERMINATOR);
	}

	return Ok();
}

static Mu::Result<void>
cmd_mkdir(const MuConfig* opts)
{
	int i;

	if (!opts->params[1])
		return Err(Error::Code::InvalidArgument,
			   "missing directory parameter");

	for (i = 1; opts->params[i]; ++i) {
		if (auto&& res =
		    maildir_mkdir(opts->params[i], opts->dirmode, FALSE); !res)
			return res;
	}

	return Ok();
}

static Result<void>
cmd_add(Mu::Store& store, const MuConfig* opts)
{
	/* note: params[0] will be 'add' */
	if (!opts->params[0] || !opts->params[1])
		return Err(Error::Code::InvalidArgument,
			   "expected some files to add");

	for (auto u = 1; opts->params[u]; ++u) {

		const auto docid{store.add_message(opts->params[u])};
		if (!docid)
			return Err(docid.error());
		else
			g_debug("added message @ %s, docid=%u",
				opts->params[u], docid.value());
	}

	return Ok();
}

static Result<void>
cmd_remove(Mu::Store& store, const MuConfig* opts)
{
	/* note: params[0] will be 'remove' */
	if (!opts->params[0] || !opts->params[1])
		return Err(Error::Code::InvalidArgument,
			   "expected some files to remove");

	for (auto u = 1; opts->params[u]; ++u) {

		const auto res = store.remove_message(opts->params[u]);
		if (!res)
			return Err(Error::Code::File, "failed to remove %s",
				   opts->params[u]);
		else
			g_debug("removed message @ %s", opts->params[u]);
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
print_signature(const Mu::MimeSignature& sig, const MuConfig *opts)
{
	Mu::MaybeAnsi col{!opts->nocolor};

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
verify(const MimeMultipartSigned& sigpart, const MuConfig *opts)
{
	using VFlags = MimeMultipartSigned::VerifyFlags;
	const auto vflags{opts->auto_retrieve ?
		VFlags::EnableKeyserverLookups: VFlags::None};

	auto ctx{MimeCryptoContext::make_gpg()};
	if (!ctx)
		return false;

	const auto sigs{sigpart.verify(*ctx, vflags)};
	Mu::MaybeAnsi col{!opts->nocolor};

	if (!sigs || sigs->empty()) {

		if (!opts->quiet)
			g_print("cannot find signatures in part\n");

		return true;
	}

	bool valid{true};
	for (auto&& sig: *sigs) {

		const auto status{sig.status()};

		if (!opts->quiet)
			key_val(col, "status", to_string(status));

		if (opts->verbose)
			print_signature(sig, opts);

		if (none_of(sig.status() & MimeSignature::Status::Green))
			valid = false;
	}

	return valid;
}

static Mu::Result<void>
cmd_verify(const MuConfig* opts)
{
	if (!opts || opts->cmd != MU_CONFIG_CMD_VERIFY)
		return Err(Error::Code::Internal, "error in parameters");

	if (!opts->params[1])
		return Err(Error::Code::InvalidArgument,
			   "missing message-file parameter");

	auto message{Message::make_from_path(opts->params[1],
					     mu_config_message_options(opts))};
	if (!message)
		return Err(message.error());


	if (none_of(message->flags() & Flags::Signed)) {
		if (!opts->quiet)
			g_print("no signed parts found\n");
		return Ok();
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

	if (verified)
		return Ok();
	else
		return Err(Error::Code::UnverifiedSignature,
			   "failed to verify one or more signatures");
}

static Result<void>
cmd_info(const Mu::Store& store, const MuConfig* opts)
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

	if (!opts->nocolor)
		colorify(info);

	std::cout << info << '\n';

	return Ok();
}

static Result<void>
cmd_init(const MuConfig* opts)
{
	/* not provided, nor could we find a good default */
	if (!opts->maildir)
		return Err(Error::Code::InvalidArgument,
			   "missing --maildir parameter and could "
			   "not determine default");

	if (opts->max_msg_size < 0)
		return Err(Error::Code::InvalidArgument,
			   "invalid value for max-message-size");
	else if (opts->batch_size < 0)
		return Err(Error::Code::InvalidArgument,
			   "invalid value for batch-size");

	Mu::Store::Config conf{};
	conf.max_message_size = opts->max_msg_size;
	conf.batch_size       = opts->batch_size;

	Mu::StringVec my_addrs;
	auto          addrs = opts->my_addresses;
	while (addrs && *addrs) {
		my_addrs.emplace_back(*addrs);
		++addrs;
	}

	auto store = Store::make_new(mu_runtime_path(MU_RUNTIME_PATH_XAPIANDB),
				     opts->maildir, my_addrs, conf);
	if (!store)
		return Err(store.error());

	if (!opts->quiet) {
		cmd_info(*store, opts);
		std::cout << "\nstore created; use the 'index' command to fill/update it.\n";
	}

	return Ok();
}

static Result<void>
cmd_find(const MuConfig* opts)
{
	auto store{Store::make(mu_runtime_path(MU_RUNTIME_PATH_XAPIANDB))};
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


using ReadOnlyStoreFunc = std::function<Result<void>(const Store&, const MuConfig*)>;
using WritableStoreFunc = std::function<Result<void>(Store&, const MuConfig*)>;

static Result<void>
with_readonly_store(const ReadOnlyStoreFunc& func, const MuConfig* opts)
{
	auto store{Store::make(mu_runtime_path(MU_RUNTIME_PATH_XAPIANDB))};
	if (!store)
		return Err(store.error());

	return func(store.value(), opts);
}

static Result<void>
with_writable_store(const WritableStoreFunc func, const MuConfig* opts)
{
	auto store{Store::make(mu_runtime_path(MU_RUNTIME_PATH_XAPIANDB),
			       Store::Options::Writable)};
	if (!store)
		return Err(store.error());

	return func(store.value(), opts);
}

Result<void>
Mu::mu_cmd_execute(const MuConfig* opts) try {

	if (!opts || !opts->params || !opts->params[0])
		return Err(Error::Code::InvalidArgument, "error in parameters");

	switch (opts->cmd) {
	case MU_CONFIG_CMD_HELP: /* already handled in mu-config.c */
		return Ok();

	/*
	 * no store needed
	 */
	case MU_CONFIG_CMD_FIELDS:
		return mu_cmd_fields(opts);
	case MU_CONFIG_CMD_MKDIR:
		return cmd_mkdir(opts);
	case MU_CONFIG_CMD_SCRIPT:
		return mu_cmd_script(opts);
	case MU_CONFIG_CMD_VIEW:
		return cmd_view(opts);
	case MU_CONFIG_CMD_VERIFY:
		return cmd_verify(opts);
	case MU_CONFIG_CMD_EXTRACT:
		return mu_cmd_extract(opts);
	/*
	 * read-only store
	 */

	case MU_CONFIG_CMD_CFIND:
		return with_readonly_store(mu_cmd_cfind, opts);
	case MU_CONFIG_CMD_FIND:
		return cmd_find(opts);
	case MU_CONFIG_CMD_INFO:
		return with_readonly_store(cmd_info, opts);

	/* writable store */

	case MU_CONFIG_CMD_ADD:
		return with_writable_store(cmd_add, opts);
	case MU_CONFIG_CMD_REMOVE:
		return with_writable_store(cmd_remove, opts);
	case MU_CONFIG_CMD_INDEX:
		return with_writable_store(mu_cmd_index, opts);

	/* commands instantiate store themselves */
	case MU_CONFIG_CMD_INIT:
		return cmd_init(opts);
	case MU_CONFIG_CMD_SERVER:
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
