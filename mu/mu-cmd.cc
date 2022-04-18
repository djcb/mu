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
#include "mu-msg.hh"
#include "mu-msg-part.hh"
#include "mu-cmd.hh"
#include "mu-maildir.hh"
#include "mu-contacts-cache.hh"
#include "mu-runtime.hh"
#include "message/mu-message.hh"
#include "message/mu-mime-object.hh"

#include "utils/mu-util.h"
#include "utils/mu-str.h"

#include "utils/mu-error.hh"
#include "utils/mu-utils.hh"

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

#define color_maybe(C)                                                                             \
	do {                                                                                       \
		if (color)                                                                         \
			fputs((C), stdout);                                                        \
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
	auto message{Message::make_from_path(mu_config_message_options(opts), fname)};
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

static MuError
cmd_mkdir(const MuConfig* opts, GError** err)
{
	int i;

	g_return_val_if_fail(opts, MU_ERROR_INTERNAL);
	g_return_val_if_fail(opts->cmd == MU_CONFIG_CMD_MKDIR, MU_ERROR_INTERNAL);

	if (!opts->params[1]) {
		mu_util_g_set_error(err, MU_ERROR_IN_PARAMETERS, "missing directory parameter");
		return MU_ERROR_IN_PARAMETERS;
	}

	for (i = 1; opts->params[i]; ++i) {
		if (auto&& res{mu_maildir_mkdir(opts->params[i],
						opts->dirmode, FALSE)}; !res) {
			g_set_error(err, MU_ERROR_DOMAIN, MU_ERROR_FILE,
				    "%s", res.error().what());
			return MU_ERROR_FILE_CANNOT_MKDIR;
		}
	}

	return MU_OK;
}

static gboolean
check_file_okay(const char* path, gboolean cmd_add)
{
	if (!g_path_is_absolute(path)) {
		g_printerr("path is not absolute: %s\n", path);
		return FALSE;
	}

	if (cmd_add && access(path, R_OK) != 0) {
		g_printerr("path is not readable: %s: %s\n", path, g_strerror(errno));
		return FALSE;
	}

	return TRUE;
}

typedef bool (*ForeachMsgFunc)(Mu::Store& store, const char* path, GError** err);

static MuError
foreach_msg_file(Mu::Store& store, const MuConfig* opts, ForeachMsgFunc foreach_func, GError** err)
{
	unsigned u;
	gboolean all_ok;

	/* note: params[0] will be 'add' */
	if (!opts->params[0] || !opts->params[1]) {
		g_print("usage: mu %s <file> [<files>]\n",
			opts->params[0] ? opts->params[0] : "<cmd>");
		mu_util_g_set_error(err, MU_ERROR_IN_PARAMETERS, "missing parameters");
		return MU_ERROR_IN_PARAMETERS;
	}

	for (u = 1, all_ok = TRUE; opts->params[u]; ++u) {
		const char* path;

		path = opts->params[u];

		if (!check_file_okay(path, TRUE)) {
			all_ok = FALSE;
			g_printerr("not a valid message file: %s\n", path);
			continue;
		}

		if (!foreach_func(store, path, err)) {
			all_ok = FALSE;
			g_printerr("error with %s: %s\n",
				   path,
				   (err && *err) ? (*err)->message : "something went wrong");
			g_clear_error(err);
			continue;
		}
	}

	if (!all_ok) {
		mu_util_g_set_error(err,
				    MU_ERROR_XAPIAN_STORE_FAILED,
				    "%s failed for some message(s)",
				    opts->params[0]);
		return MU_ERROR_XAPIAN_STORE_FAILED;
	}

	return MU_OK;
}

static bool
add_path_func(Mu::Store& store, const char* path, GError** err)
{
	const auto docid = store.add_message(path);
	g_debug("added message @ %s, docid=%u", path, docid);

	return true;
}

static MuError
cmd_add(Mu::Store& store, const MuConfig* opts, GError** err)
{
	g_return_val_if_fail(opts, MU_ERROR_INTERNAL);
	g_return_val_if_fail(opts->cmd == MU_CONFIG_CMD_ADD, MU_ERROR_INTERNAL);

	return foreach_msg_file(store, opts, add_path_func, err);
}

static bool
remove_path_func(Mu::Store& store, const char* path, GError** err)
{
	const auto res = store.remove_message(path);
	g_debug("removed %s (%s)", path, res ? "yes" : "no");

	return true;
}

static MuError
cmd_remove(Mu::Store& store, const MuConfig* opts, GError** err)
{
	g_return_val_if_fail(opts, MU_ERROR_INTERNAL);
	g_return_val_if_fail(opts->cmd == MU_CONFIG_CMD_REMOVE, MU_ERROR_INTERNAL);

	return foreach_msg_file(store, opts, remove_path_func, err);
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


static Mu::Result<MuError>
cmd_verify(const MuConfig* opts)
{
	if (!opts || opts->cmd != MU_CONFIG_CMD_VERIFY)
		return Err(Error::Code::Internal, "error in parameters");

	if (!opts->params[1])
		return Err(Error::Code::InvalidArgument,
			   "missing message-file parameter");

	auto message{Message::make_from_path(mu_config_message_options(opts),
					     opts->params[1])};
	if (!message)
		return Err(message.error());


	if (none_of(message->flags() & Flags::Signed)) {
		if (!opts->quiet)
			g_print("no signed parts found\n");
		return Ok(MU_ERROR);
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

	return Ok(verified ? MU_OK : MU_ERROR);
}

static MuError
cmd_info(const Mu::Store& store, const MuConfig* opts, GError** err)
{
	Mu::MaybeAnsi col{!opts->nocolor};

	key_val(col, "maildir", store.properties().root_maildir);
	key_val(col, "database-path", store.properties().database_path);
	key_val(col, "schema-version", store.properties().schema_version);
	key_val(col, "max-message-size", store.properties().max_message_size);
	key_val(col, "batch-size", store.properties().batch_size);
	key_val(col, "messages in store", store.size());

	const auto created{store.properties().created};
	const auto tstamp{::localtime(&created)};

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wformat-y2k"
	char tbuf[64];
	strftime(tbuf, sizeof(tbuf), "%c", tstamp);
#pragma GCC diagnostic pop

	key_val(col, "created", tbuf);

	const auto addrs{store.properties().personal_addresses};
	if (addrs.empty())
		key_val(col, "personal-address", "<none>");
	else
		for (auto&& c : addrs)
			key_val(col, "personal-address", c);

	return MU_OK;
}

static MuError
cmd_init(const MuConfig* opts, GError** err)
{
	/* not provided, nor could we find a good default */
	if (!opts->maildir) {
		mu_util_g_set_error(err,
				    MU_ERROR_IN_PARAMETERS,
				    "missing --maildir parameter and could "
				    "not determine default");
		return MU_ERROR_IN_PARAMETERS;
	}

	if (opts->max_msg_size < 0) {
		mu_util_g_set_error(err,
				    MU_ERROR_IN_PARAMETERS,
				    "invalid value for max-message-size");
		return MU_ERROR_IN_PARAMETERS;
	} else if (opts->batch_size < 0) {
		mu_util_g_set_error(err, MU_ERROR_IN_PARAMETERS, "invalid value for batch-size");
		return MU_ERROR_IN_PARAMETERS;
	}

	Mu::Store::Config conf{};
	conf.max_message_size = opts->max_msg_size;
	conf.batch_size       = opts->batch_size;

	Mu::StringVec my_addrs;
	auto          addrs = opts->my_addresses;
	while (addrs && *addrs) {
		my_addrs.emplace_back(*addrs);
		++addrs;
	}

	Mu::Store store(mu_runtime_path(MU_RUNTIME_PATH_XAPIANDB), opts->maildir, my_addrs, conf);
	if (!opts->quiet) {
		cmd_info(store, opts, NULL);
		std::cout << "\nstore created; use the 'index' command to fill/update it.\n";
	}

	return MU_OK;
}

static MuError
cmd_find(const MuConfig* opts, GError** err)
{
	Mu::Store store{mu_runtime_path(MU_RUNTIME_PATH_XAPIANDB), true /*readonly*/};

	return mu_cmd_find(store, opts, err);
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

typedef MuError (*readonly_store_func)(const Mu::Store&, const MuConfig*, GError** err);
typedef MuError (*writable_store_func)(Mu::Store&, const MuConfig*, GError** err);

static MuError
with_readonly_store(readonly_store_func func, const MuConfig* opts, GError** err)
{
	const Mu::Store store{mu_runtime_path(MU_RUNTIME_PATH_XAPIANDB), true /*readonly*/};
	return func(store, opts, err);
}

static MuError
with_writable_store(writable_store_func func, const MuConfig* opts, GError** err)
{
	Mu::Store store{mu_runtime_path(MU_RUNTIME_PATH_XAPIANDB), false /*!readonly*/};
	return func(store, opts, err);
}

static gboolean
check_params(const MuConfig* opts, GError** err)
{
	if (!opts->params || !opts->params[0]) { /* no command? */
		show_usage();
		mu_util_g_set_error(err, MU_ERROR_IN_PARAMETERS,
				    "error in parameters");
		return FALSE;
	}

	return TRUE;
}

MuError
Mu::mu_cmd_execute(const MuConfig* opts, GError** err) try {

	MuError merr;

	g_return_val_if_fail(opts, MU_ERROR_INTERNAL);
	if (!check_params(opts, err))
		return MU_G_ERROR_CODE(err);

	auto mu_error_from_result = [](auto&& result, GError **err) {
		if (!result) {
			result.error().fill_g_error(err);
			return MU_ERROR;
		} else
			return MU_OK;
	};

	switch (opts->cmd) {
		/* already handled in mu-config.c */
	case MU_CONFIG_CMD_HELP:
		return MU_OK;

	/*
	 * no store needed
	 */

	case MU_CONFIG_CMD_MKDIR: merr = cmd_mkdir(opts, err); break;
	case MU_CONFIG_CMD_SCRIPT: merr = mu_cmd_script(opts, err); break;
	case MU_CONFIG_CMD_VIEW:
		merr = mu_error_from_result(cmd_view(opts), err);
		break;
	case MU_CONFIG_CMD_VERIFY: {
		if (const auto res = cmd_verify(opts); !res) {
			res.error().fill_g_error(err);
			merr = MU_ERROR;
		} else
			merr = res.value();
		break;
	}

	case MU_CONFIG_CMD_EXTRACT:
		merr = mu_error_from_result(mu_cmd_extract(opts), err);
		break;
	/*
	 * read-only store
	 */

	case MU_CONFIG_CMD_CFIND: merr = with_readonly_store(mu_cmd_cfind, opts, err); break;
	case MU_CONFIG_CMD_FIND: merr = cmd_find(opts, err); break;
	case MU_CONFIG_CMD_INFO:
		merr = with_readonly_store(cmd_info, opts, err);
		break;

	/* writable store */

	case MU_CONFIG_CMD_ADD: merr = with_writable_store(cmd_add, opts, err); break;
	case MU_CONFIG_CMD_REMOVE: merr = with_writable_store(cmd_remove, opts, err); break;
	case MU_CONFIG_CMD_INDEX: merr = with_writable_store(mu_cmd_index, opts, err); break;

	/* commands instantiate store themselves */
	case MU_CONFIG_CMD_INIT: merr = cmd_init(opts, err); break;
	case MU_CONFIG_CMD_SERVER: merr = mu_cmd_server(opts, err); break;

	default: merr = MU_ERROR_IN_PARAMETERS; break;
	}

	return merr;

} catch (const Mu::Error& er) {
	g_set_error(err, MU_ERROR_DOMAIN, MU_ERROR, "%s", er.what());
	return MU_ERROR;
} catch (...) {
	g_set_error(err, MU_ERROR_DOMAIN, MU_ERROR, "%s", "caught exception");
	return MU_ERROR;
}
