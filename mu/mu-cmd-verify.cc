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
		created == 0 ? std::string{"unknown"} :
		mu_format("{:%c}", mu_time(sig.created())));

	const auto expires{sig.expires()};
	key_val(col, "expires", expires==0 ? std::string{"never"} :
		mu_format("{:%c}", mu_time(sig.expires())));

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



#ifdef BUILD_TESTS
/*
 * Tests.
 *
 */

#include "utils/mu-test-utils.hh"

/* we can only test 'verify' if gpg is installed, and has djcb@djcbsoftware's key in the keyring */
static bool
verify_is_testable(void)
{
	auto gpg{program_in_path("gpg2")};
	if (!gpg) {
		mu_message("cannot find gpg2 in path");
		return false;
	}

	auto res{run_command({*gpg, "--list-keys", "DCC4A036"})}; /* djcb@djcbsoftware.nl's key */
	if (!res || res->exit_code != 0) {
		mu_message("key DCC4A036 not found");
		return false;
	}

	return true;
}

static void
test_mu_verify_good(void)
{
	if (!verify_is_testable()) {
		g_test_skip("cannot test verify");
		return;
	}

	auto res = run_command({MU_PROGRAM, "verify",
			join_paths(MU_TESTMAILDIR4, "signed!2,S")});
	assert_valid_result(res);
	g_assert_cmpuint(res->exit_code ,==, 0);
}

static void
test_mu_verify_bad(void)
{
	if (!verify_is_testable()) {
		g_test_skip("cannot test verify");
		return;
	}

	auto res = run_command({MU_PROGRAM, "verify",
			join_paths(MU_TESTMAILDIR4, "signed-bad!2,S")});
	assert_valid_result(res);
	g_assert_cmpuint(res->exit_code,==, 1);
}

int
main(int argc, char* argv[]) try {

	mu_test_init(&argc, &argv);

	g_test_add_func("/cmd/verify/good", test_mu_verify_good);
	g_test_add_func("/cmd/verify/bad", test_mu_verify_bad);

	return g_test_run();

} catch (const Error& e) {
	mu_printerrln("{}", e.what());
	return 1;
} catch (...) {
	mu_printerrln("caught exception");
	return 1;
}
#endif /*BUILD_TESTS*/
