/*
** Copyright (C) 2022-2025 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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


#include "mu-mime-object.hh"
#include "gmime/gmime-message.h"
#include "utils/mu-utils.hh"
#include "utils/mu-utils-file.hh"
#include <mutex>
#include <regex>
#include <fcntl.h>
#include <sys/stat.h>
#include <errno.h>

using namespace Mu;



/* note, we do the gmime initialization here rather than in mu-runtime, because this way
 * we don't need mu-runtime for simple cases -- such as our unit tests. Also note that we
 * need gmime init even for the doc backend, as we use the address parsing functions also
 * there. */

void
Mu::init_gmime(void)
{
	// fast path.
	static bool gmime_initialized = false;
	if (gmime_initialized)
		return;

	static std::mutex gmime_lock;
	std::lock_guard lock (gmime_lock);
	if (gmime_initialized)
		return; // already

	mu_debug("initializing gmime {}.{}.{}",
		gmime_major_version,
		gmime_minor_version,
		gmime_micro_version);

	g_mime_init();
	gmime_initialized = true;

	std::atexit([] {
		mu_debug("shutting down gmime");
		g_mime_shutdown();
		gmime_initialized = false;
	});
}


std::string
Mu::address_rfc2047(const Contact& contact)
{
	init_gmime();

	InternetAddress *addr =
		internet_address_mailbox_new(contact.name.c_str(),
					     contact.email.c_str());

	std::string encoded = to_string_gchar(
		internet_address_to_string(addr, {}, true));

	g_object_unref(addr);

	return encoded;
}


/*
 * MimeObject
 */

Option<std::string>
MimeObject::header(const std::string& hdr) const noexcept
{
	if (auto val{g_mime_object_get_header(self(), hdr.c_str())}; !val)
		return Nothing;
	else if (!g_utf8_validate(val, -1, {}))
		return utf8_clean(val);
	else
		return std::string{val};
}


std::vector<std::pair<std::string, std::string>>
MimeObject::headers() const noexcept
{
	GMimeHeaderList *lst;

	lst = g_mime_object_get_header_list(self()); /* _not_ owned */
	if (!lst)
		return {};

	std::vector<std::pair<std::string, std::string>> hdrs;
	const auto hdr_num{g_mime_header_list_get_count(lst)};

	for (int i = 0; i != hdr_num; ++i) {
		GMimeHeader *hdr{g_mime_header_list_get_header_at(lst, i)};
		if (!hdr) /* ^^^ _not_ owned */
			continue;
		const auto name{g_mime_header_get_name(hdr)};
		const auto val{g_mime_header_get_value(hdr)};
		if (!name || !val)
			continue;
		hdrs.emplace_back(name, val);
	}

	return hdrs;
}

Result<size_t>
MimeObject::write_to_stream(const MimeFormatOptions& f_opts,
			    MimeStream& stream) const
{
	auto written = g_mime_object_write_to_stream(self(), f_opts.get(),
						     GMIME_STREAM(stream.object()));
	if (written < 0)
		return Err(Error::Code::File, "failed to write mime-object to stream");
	else
		return Ok(static_cast<size_t>(written));
}

Result<size_t>
MimeObject::to_file(const std::string& path, bool overwrite) const noexcept
{
	GError *err{};
	auto strm{g_mime_stream_fs_open(path.c_str(),
					O_WRONLY | O_CREAT | O_TRUNC |(overwrite ? 0 : O_EXCL),
					S_IRUSR|S_IWUSR,
					&err)};
	if (!strm)
		return Err(Error::Code::File, &err, "failed to open '{}'", path);

	MimeStream stream{MimeStream::make_from_stream(strm)};
	return write_to_stream({}, stream);
}


Option<std::string>
MimeObject::to_string_opt() const noexcept
{
	auto stream{MimeStream::make_mem()};
	if (!stream) {
		mu_warning("failed to create mem stream");
		return Nothing;
	}

	const auto written = g_mime_object_write_to_stream(
		self(), {}, GMIME_STREAM(stream.object()));
	if (written < 0) {
		mu_warning("failed to write object to stream");
		return Nothing;
	}

	std::string buffer;
	buffer.resize(written + 1);
	stream.reset();

	auto bytes{g_mime_stream_read(GMIME_STREAM(stream.object()),
				      buffer.data(), written)};
	if (bytes < 0)
		return Nothing;

	buffer.data()[written]='\0';
	buffer.resize(written);

	return buffer;
}


/*
 * MimeCryptoContext
 */

Result<size_t>
MimeCryptoContext::import_keys(MimeStream& stream)
{
	GError *err{};
	auto res = g_mime_crypto_context_import_keys(
		self(), GMIME_STREAM(stream.object()), &err);

	if (res < 0)
		return Err(Error::Code::File, &err,
			   "error importing keys");

	return Ok(static_cast<size_t>(res));
}

void
MimeCryptoContext::set_request_password(PasswordRequestFunc pw_func)
{
	static auto request_func = pw_func;

	g_mime_crypto_context_set_request_password(
		self(),
		[](GMimeCryptoContext *ctx,
		   const char *user_id,
		   const char *prompt,
		   gboolean reprompt,
		   GMimeStream *response,
		   GError **err) -> gboolean {
			MimeStream mstream{MimeStream::make_from_stream(response)};

			auto res = request_func(MimeCryptoContext(ctx),
						std::string{user_id ? user_id : ""},
						std::string{prompt ? prompt : ""},
						!!reprompt,
						mstream);
			if (res)
				return TRUE;

			res.error().fill_g_error(err);
			return FALSE;
		});

}

Result<void>
MimeCryptoContext::setup_gpg_test(const std::string& testpath)
{
	/* setup clean environment for testing; inspired by gmime */

	g_setenv ("GNUPGHOME", join_paths(testpath, ".gnupg").c_str(), 1);

	/* disable environment variables that gpg-agent uses for pinentry */
	g_unsetenv ("DBUS_SESSION_BUS_ADDRESS");
	g_unsetenv ("DISPLAY");
	g_unsetenv ("GPG_TTY");

	if (g_mkdir_with_parents((testpath + "/.gnupg").c_str(), 0700) != 0)
		return Err(Error::Code::File,
			   "failed to create gnupg dir; err={}", errno);

	auto write_gpgfile=[&](const std::string& fname, const std::string& data)
		-> Result<void> {

		GError *err{};
		std::string path{mu_format("{}/{}", testpath, fname)};
		if (!g_file_set_contents(path.c_str(), data.c_str(), data.size(), &err))
			return Err(Error::Code::File, &err, "failed to write {}", path);
		else
			return Ok();
	};

	// some more elegant way?
	if (auto&& res = write_gpgfile("gpg.conf", "pinentry-mode loopback\n"); !res)
		return res;
	if (auto&& res = write_gpgfile("gpgsm.conf", "disable-crl-checks\n"))
		return res;

	return Ok();
}


/*
 * MimeMessage
 */



static Result<MimeMessage>
make_from_stream(GMimeStream* &&stream/*consume*/)
{
	init_gmime();
	GMimeParser *parser{g_mime_parser_new_with_stream(stream)};
	g_object_unref(stream);
	if (!parser)
		return Err(Error::Code::Message, "cannot create mime parser");

	GMimeMessage *gmime_msg{g_mime_parser_construct_message(parser, NULL)};
	g_object_unref(parser);
	if (!gmime_msg)
		return Err(Error::Code::Message, "message seems invalid");

	auto mime_msg{MimeMessage{std::move(G_OBJECT(gmime_msg))}};
	g_object_unref(gmime_msg);

	return Ok(std::move(mime_msg));
}

Result<MimeMessage>
MimeMessage::make_from_file(const std::string& path)
{
	GError* err{};
	init_gmime();
	if (auto&& stream{g_mime_stream_file_open(path.c_str(), "r", &err)}; !stream)
		return Err(Error::Code::Message, &err,
			   "failed to open stream for {}", path);
	else
		return make_from_stream(std::move(stream));
}

Result<MimeMessage>
MimeMessage::make_from_text(const std::string& text)
{
	init_gmime();
	if (auto&& stream{g_mime_stream_mem_new_with_buffer(
				text.c_str(), text.length())}; !stream)
		return Err(Error::Code::Message,
			   "failed to open stream for string");
	else
		return make_from_stream(std::move(stream));
}

Option<int64_t>
MimeMessage::date() const noexcept
{
	GDateTime *dt{g_mime_message_get_date(self())};
	if (!dt)
		return Nothing;
	else
		return g_date_time_to_unix(dt);
}

constexpr Option<GMimeAddressType>
address_type(Contact::Type ctype)
{
	switch(ctype) {
	case Contact::Type::Bcc:
		return GMIME_ADDRESS_TYPE_BCC;
	case Contact::Type::Cc:
		return GMIME_ADDRESS_TYPE_CC;
	case Contact::Type::From:
		return GMIME_ADDRESS_TYPE_FROM;
	case Contact::Type::To:
		return GMIME_ADDRESS_TYPE_TO;
	case Contact::Type::ReplyTo:
		return GMIME_ADDRESS_TYPE_REPLY_TO;
	case Contact::Type::Sender:
		return GMIME_ADDRESS_TYPE_SENDER;
	case Contact::Type::None:
	default:
		return Nothing;
	}
}

static Mu::Contacts
all_contacts(const MimeMessage& msg)
{
	Contacts contacts;

	for (auto&& cctype: {
			Contact::Type::Sender,
			Contact::Type::From,
			Contact::Type::ReplyTo,
			Contact::Type::To,
			Contact::Type::Cc,
			Contact::Type::Bcc
		}) {
		auto addrs{msg.contacts(cctype)};
		std::move(addrs.begin(), addrs.end(),
			  std::back_inserter(contacts));
	}

	return contacts;
}

Mu::Contacts
MimeMessage::contacts(Contact::Type ctype) const noexcept
{
	/* special case: get all */
	if (ctype == Contact::Type::None)
		return all_contacts(*this);

	const auto atype{address_type(ctype)};
	if (!atype)
		return {};

	auto addrs{g_mime_message_get_addresses(self(), *atype)};
	if (!addrs)
		return {};

	const auto msgtime{date().value_or(0)};

	Contacts contacts;
	auto lst_len{internet_address_list_length(addrs)};
	contacts.reserve(lst_len);
	for (auto i = 0; i != lst_len; ++i) {

		auto&& addr{internet_address_list_get_address(addrs, i)};
		const auto name{internet_address_get_name(addr)};

		if (G_UNLIKELY(!INTERNET_ADDRESS_IS_MAILBOX(addr)))
			continue;

		const auto email{internet_address_mailbox_get_addr (
				INTERNET_ADDRESS_MAILBOX(addr))};
		if (G_UNLIKELY(!email))
			continue;

		contacts.emplace_back(email, name ? name : "", ctype, msgtime);
	}

	return contacts;
}

/*
 * references() returns the concatenation of the References and In-Reply-To
 * message-ids (in that order). Duplicates are removed.
 *
 * The _first_ one in the list determines the thread-id for the message.
 */
std::vector<std::string>
MimeMessage::references() const noexcept
{
	// is ref already in the list? O(n) but with small n.
	auto is_dup = [](auto&& seq, const std::string& ref) {
		return seq_some(seq, [&](auto&& str) { return ref == str; });
	};

	auto on_blacklist = [](auto&& msgid) {
		// don't include empty message-ids
		if (!*msgid)
			return true;
		// this is bit ugly; protonmail injects fake References which
		// can otherwise screw up threading.
		if (g_str_has_suffix(msgid, "protonmail.internalid"))
			return true;
		/* ... */
		return false;
	};

	std::vector<std::string> refs;
	for (auto&& ref_header: { "References", "In-reply-to" }) {

		auto hdr{header(ref_header)};
		if (!hdr)
			continue;

		GMimeReferences *mime_refs{g_mime_references_parse({}, hdr->c_str())};
		refs.reserve(refs.size() + g_mime_references_length(mime_refs));

		for (auto i = 0; i != g_mime_references_length(mime_refs); ++i) {
			const auto msgid{g_mime_references_get_message_id(mime_refs, i)};
			if (msgid && !is_dup(refs, msgid) && !on_blacklist(msgid))
				refs.emplace_back(msgid);
		}
		g_mime_references_free(mime_refs);
	}

	return refs;
}

void
MimeMessage::for_each(const ForEachFunc& func) const noexcept
{
	struct CallbackData { const ForEachFunc& func; };
	CallbackData cbd{func};

	g_mime_message_foreach(
		self(),
		[] (GMimeObject *parent, GMimeObject *part, gpointer user_data) {
			auto cb_data{reinterpret_cast<CallbackData*>(user_data)};
			cb_data->func(MimeObject{parent}, MimeObject{part});
		}, &cbd);
}



/*
 * MimePart
 */
size_t
MimePart::size() const noexcept
{
	auto wrapper{g_mime_part_get_content(self())};
	if (!wrapper) {
		mu_warning("failed to get content wrapper");
		return 0;
	}

	auto stream{g_mime_data_wrapper_get_stream(wrapper)};
	if (!stream) {
		mu_warning("failed to get stream");
		return 0;
	}

	return static_cast<size_t>(g_mime_stream_length(stream));
}
Option<std::string>
MimePart::to_string() const noexcept
{
	/*
	 * easy case: text. this automatically handles conversion to utf-8.
	 */
	if (GMIME_IS_TEXT_PART(self())) {
		if (char* txt{g_mime_text_part_get_text(GMIME_TEXT_PART(self()))}; !txt)
			return Nothing;
		else
			return to_string_gchar(std::move(txt)/*consumes*/);
	}

	/*
	 * harder case: read from stream manually
	 */
	GMimeDataWrapper *wrapper{g_mime_part_get_content(self())};
	if (!wrapper) { /* this happens with invalid mails */
		mu_warning("failed to create data wrapper");
		return Nothing;
	}

	GMimeStream *stream{g_mime_stream_mem_new()};
	if (!stream) {
		mu_warning("failed to create mem stream");
		return Nothing;
	}

	ssize_t buflen{g_mime_data_wrapper_write_to_stream(wrapper, stream)};
	if (buflen <= 0) { /* empty buffer, not an error */
		g_object_unref(stream);
		return Nothing;
	}

	std::string buffer;
	buffer.resize(buflen + 1);
	g_mime_stream_reset(stream);

	auto bytes{g_mime_stream_read(stream, buffer.data(), buflen)};
	g_object_unref(stream);
	if (bytes < 0)
		return Nothing;

	buffer.resize(bytes + 1);

	return buffer;
}

Result<size_t>
MimePart::to_file(const std::string& path, bool overwrite) const noexcept
{
	MimeDataWrapper wrapper{g_mime_part_get_content(self())};
	if (!wrapper)  /* this happens with invalid mails */
		return Err(Error::Code::File, "failed to create data wrapper");

	GError *err{};
	auto strm{g_mime_stream_fs_open(path.c_str(),
					O_WRONLY | O_CREAT | O_TRUNC |(overwrite ? 0 : O_EXCL),
					S_IRUSR|S_IWUSR,
					&err)};
	if (!strm)
		return Err(Error::Code::File, &err, "failed to open '{}'", path);

	MimeStream stream{MimeStream::make_from_stream(strm)};
	ssize_t written{g_mime_data_wrapper_write_to_stream(
			GMIME_DATA_WRAPPER(wrapper.object()),
			GMIME_STREAM(stream.object()))};

	if (written < 0) {
		return Err(Error::Code::File, &err,
			   "failed to write to '{}'", path);
	}

	return Ok(static_cast<size_t>(written));
}

void
MimeMultipart::for_each(const ForEachFunc& func) const noexcept
{
	struct CallbackData { const ForEachFunc& func; };
	CallbackData cbd{func};

	g_mime_multipart_foreach(
		self(),
		[] (GMimeObject *parent, GMimeObject *part, gpointer user_data) {
			auto cb_data{reinterpret_cast<CallbackData*>(user_data)};
			cb_data->func(MimeObject{parent}, MimeObject{part});
		}, &cbd);
}


/*
 * we need to be able to pass a crypto-context to the verify(), but
 * g_mime_multipart_signed_verify() doesn't offer that anymore in GMime 3.x.
 *
 * So, add that by reimplementing it a bit (follow the upstream impl)
 */


static bool
mime_types_equal (const std::string& mime_type, const std::string& official_type)
{
	if (g_ascii_strcasecmp(mime_type.c_str(), official_type.c_str()) == 0)
		return true;

	const auto slash_pos = official_type.find("/");
	if (slash_pos == std::string::npos || slash_pos == 0)
		return false;

	/* If the official mime-type's subtype already begins with "x-", then there's
	 * nothing else to check. */
	const auto subtype{official_type.substr(slash_pos + 1)};
	if (g_ascii_strncasecmp (subtype.c_str(), "x-", 2) == 0)
		return false;
	const auto supertype{official_type.substr(0, slash_pos - 1)};
	const auto xtype{official_type.substr(0, slash_pos - 1) + "x-" + subtype};

	/* Check if the "x-" version of the official mime-type matches the
	 * supplied mime-type. For example, if the official mime-type is
	 * "application/pkcs7-signature", then we also want to match
	 * "application/x-pkcs7-signature". */
	return g_ascii_strcasecmp(mime_type.c_str(), xtype.c_str()) == 0;
}


/**
 * A bit of a monster, this impl.
 *
 * It's the transliteration of the g_mime_multipart_signed_verify() which
 * adds the feature of passing in the CryptoContext.
 *
 */
Result<std::vector<MimeSignature>>
MimeMultipartSigned::verify(const MimeCryptoContext& ctx, VerifyFlags vflags) const noexcept
{
	if (g_mime_multipart_get_count(GMIME_MULTIPART(self())) < 2)
		return Err(Error::Code::Crypto, "cannot verify, not enough subparts");

	const auto proto{content_type_parameter("protocol")};
	const auto sign_proto{ctx.signature_protocol()};

	if (!proto || !sign_proto || !mime_types_equal(*proto, *sign_proto))
		return Err(Error::Code::Crypto, "unsupported protocol {}",
			   proto.value_or("<unknown>"));

	const auto sig{signed_signature_part()};
	const auto content{signed_content_part()};
	if (!sig || !content)
		return Err(Error::Code::Crypto, "cannot find part");

	const auto sig_mime_type{sig->mime_type()};
	if (!sig || !mime_types_equal(sig_mime_type.value_or("<none>"), *sign_proto))
		return Err(Error::Code::Crypto, "failed to find matching signature part");

	MimeFormatOptions fopts{g_mime_format_options_new()};
	g_mime_format_options_set_newline_format(fopts.get(), GMIME_NEWLINE_FORMAT_DOS);

	MimeStream stream{MimeStream::make_mem()};
	if (auto&& res = content->write_to_stream(fopts, stream); !res)
		return Err(res.error());
	stream.reset();

	MimeDataWrapper wrapper{g_mime_part_get_content(GMIME_PART(sig->object()))};
	MimeStream sigstream{MimeStream::make_mem()};
	if (auto&& res = wrapper.write_to_stream(sigstream); !res)
		return Err(res.error());
	sigstream.reset();

	GError *err{};
	GMimeSignatureList *siglist{g_mime_crypto_context_verify(
			GMIME_CRYPTO_CONTEXT(ctx.object()),
			static_cast<GMimeVerifyFlags>(vflags),
			GMIME_STREAM(stream.object()),
			GMIME_STREAM(sigstream.object()),
			{},
			&err)};
	if (!siglist)
		return Err(Error::Code::Crypto, &err, "failed to verify");

	std::vector<MimeSignature> sigs;
	for (auto i = 0;
	     i != g_mime_signature_list_length(siglist); ++i) {
		GMimeSignature *msig = g_mime_signature_list_get_signature(siglist, i);
		sigs.emplace_back(MimeSignature(msig));
	}
	g_object_unref(siglist);

	return sigs;
}


std::vector<MimeCertificate>
MimeDecryptResult::recipients() const noexcept
{
	GMimeCertificateList *lst{g_mime_decrypt_result_get_recipients(self())};
	if (!lst)
		return {};

	std::vector<MimeCertificate> certs;
	for (int i = 0; i != g_mime_certificate_list_length(lst); ++i)
		certs.emplace_back(
			MimeCertificate(
				g_mime_certificate_list_get_certificate(lst, i)));

	return certs;
}

std::vector<MimeSignature>
MimeDecryptResult::signatures() const noexcept
{
	GMimeSignatureList *lst{g_mime_decrypt_result_get_signatures(self())};
	if (!lst)
		return {};

	std::vector<MimeSignature> sigs;
	for (auto i = 0; i != g_mime_signature_list_length(lst); ++i) {
		GMimeSignature *sig = g_mime_signature_list_get_signature(lst, i);
		sigs.emplace_back(MimeSignature(sig));
	}

	return sigs;
}
/**
 * Like verify, a bit of a monster, this impl.
 *
 * It's the transliteration of the g_mime_multipart_encrypted_decrypt() which
 * adds the feature of passing in the CryptoContext.
 *
 */

Mu::Result<MimeMultipartEncrypted::Decrypted>
MimeMultipartEncrypted::decrypt(const MimeCryptoContext& ctx, DecryptFlags dflags,
				const std::string& session_key) const noexcept
{
	if (g_mime_multipart_get_count(GMIME_MULTIPART(self())) < 2)
		return Err(Error::Code::Crypto, "cannot decrypted, not enough subparts");

	const auto proto{content_type_parameter("protocol")};
	const auto enc_proto{ctx.encryption_protocol()};

	if (!proto || !enc_proto || !mime_types_equal(*proto, *enc_proto))
		return Err(Error::Code::Crypto, "unsupported protocol {}",
			   proto.value_or("<unknown>"));

	const auto version{encrypted_version_part()};
	const auto encrypted{encrypted_content_part()};
	if (!version || !encrypted)
		return Err(Error::Code::Crypto, "cannot find part");

	if (!mime_types_equal(version->mime_type().value_or(""), proto.value()))
		return Err(Error::Code::Crypto,
			   "cannot decrypt; unexpected version content-type '{}' != '{}'",
			   version->mime_type().value_or(""), proto.value());

	if (!mime_types_equal(encrypted->mime_type().value_or(""),
			      "application/octet-stream"))
		return Err(Error::Code::Crypto,
			   "cannot decrypt; unexpected encrypted content-type '{}'",
			   encrypted->mime_type().value_or(""));

	const auto content{encrypted->content()};
	auto ciphertext{MimeStream::make_mem()};
	content.write_to_stream(ciphertext);
	ciphertext.reset();

	auto stream{MimeStream::make_mem()};
	auto filtered{MimeStream::make_filtered(stream)};
	auto filter{g_mime_filter_dos2unix_new(FALSE)};
	g_mime_stream_filter_add(GMIME_STREAM_FILTER(filtered.object()),
				 filter);
	g_object_unref(filter);

	GError *err{};
	GMimeDecryptResult *dres =
		g_mime_crypto_context_decrypt(GMIME_CRYPTO_CONTEXT(ctx.object()),
					      static_cast<GMimeDecryptFlags>(dflags),
					      session_key.empty() ?
					      NULL : session_key.c_str(),
					      GMIME_STREAM(ciphertext.object()),
					      GMIME_STREAM(filtered.object()),
					      &err);
	if (!dres)
		return Err(Error::Code::Crypto, &err, "decryption failed");

	filtered.flush();
	stream.reset();

	auto parser{g_mime_parser_new()};
	g_mime_parser_init_with_stream(parser, GMIME_STREAM(stream.object()));

	auto decrypted{g_mime_parser_construct_part(parser, NULL)};
	g_object_unref(parser);
	if (!decrypted) {
		g_object_unref(dres);
		return Err(Error::Code::Crypto, "failed to parse decrypted part");
	}

	Decrypted result = { MimeObject{decrypted}, MimeDecryptResult{dres} };

	g_object_unref(decrypted);
	g_object_unref(dres);

	return Ok(std::move(result));
}
