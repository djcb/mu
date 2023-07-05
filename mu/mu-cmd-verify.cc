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

#include "config.h"
#include "mu-cmd.hh"

#include "message/mu-message.hh"
#include "message/mu-mime-object.hh"

#include <iostream>
#include <iomanip>

using namespace Mu;

template <typename T>
static void
key_val(const Mu::MaybeAnsi& col, const std::string& key, T val)
{
	using Color = Mu::MaybeAnsi::Color;

	mu_println("{}{:<18}{}: {}{}{}",
		   col.fg(Color::BrightBlue), key, col.reset(),
		   col.fg(Color::Green), val, col.reset());
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
			mu_println("cannot find signatures in part");

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


static bool
verify_message(const Message& message, const Options& opts, const std::string& name)
{
	if (none_of(message.flags() & Flags::Signed)) {
		if (!opts.quiet)
			mu_println("{}: no signed parts found", name);
		return false;
	}

	bool verified{true}; /* innocent until proven guilty */
	for(auto&& part: message.parts()) {

		if (!part.is_signed())
			continue;

		const auto& mobj{part.mime_object()};
		if (!mobj.is_multipart_signed())
			continue;

		if (!verify(MimeMultipartSigned(mobj), opts))
			verified = false;
	}

	return verified;
}



Mu::Result<void>
Mu::mu_cmd_verify(const Options& opts)
{
	bool all_ok{true};
	const auto mopts = message_options(opts.verify);

	for (auto&& file: opts.verify.files) {

		auto message{Message::make_from_path(file, mopts)};
		if (!message)
			return Err(message.error());

		if (!opts.quiet && opts.verify.files.size() > 1)
			mu_println("verifying {}", file);

		if (!verify_message(*message, opts, file))
			all_ok = false;
	}

	// when no messages provided, read from stdin
	if (opts.verify.files.empty()) {
		const auto msgtxt = read_from_stdin();
		if (!msgtxt)
			return Err(msgtxt.error());
		auto message{Message::make_from_text(*msgtxt, {}, mopts)};
		if (!message)
			return Err(message.error());

		all_ok = verify_message(*message, opts, "<stdin>");
	}

	if (all_ok)
		return Ok();
	else
		return Err(Error::Code::UnverifiedSignature,
			   "failed to verify one or more signatures");
}
