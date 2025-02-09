/*
** Copyright (C) 2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef MU_MIME_OBJECT_HH__
#define MU_MIME_OBJECT_HH__

#include <stdexcept>
#include <string>
#include <functional>
#include <array>
#include <vector>
#include <gmime/gmime.h>
#include "gmime/gmime-application-pkcs7-mime.h"
#include "gmime/gmime-crypto-context.h"
#include "utils/mu-option.hh"
#include "utils/mu-result.hh"
#include "utils/mu-utils.hh"
#include "mu-contact.hh"

namespace Mu {

/* non-GObject types */

using MimeFormatOptions = deletable_unique_ptr<GMimeFormatOptions, g_mime_format_options_free>;

/**
 * Initialize gmime (idempotent)
 *
 */
void init_gmime(void);


/**
 * Get a RFC2047-compatible address for the given contact
 *
 * @param contact a contact
 *
 * @return an address string
 */
std::string address_rfc2047(const Contact& contact);

class Object {
public:
	/**
	 * Default CTOR
	 *
	 */
	Object() noexcept: self_{}  {}

	/**
	 * Create an object from a GObject
	 *
	 * @param obj a gobject. A ref is added.
	 */
	Object(GObject* &&obj): self_{G_OBJECT(g_object_ref(obj))} {
		if (!G_IS_OBJECT(obj))
			throw std::runtime_error("not a g-object");
	}

	/**
	 * Copy CTOR
	 *
	 * @param other some other Object
	 */
	Object(const Object& other) noexcept { *this = other; }

	/**
	 * Move CTOR
	 *
	 * @param other some other Object
	 */
	Object(Object&& other) noexcept { *this = std::move(other); }

	/**
	 * operator=
	 *
	 * @param other copy some other object
	 *
	 * @return *this
	 */
	Object& operator=(const Object& other) noexcept {

		if (this != &other) {
			auto oldself = self_;
			self_ = other.self_ ?
				G_OBJECT(g_object_ref(other.self_)) : nullptr;
			if (oldself)
				g_object_unref(oldself);
		}
		return *this;
	}

	/**
	 * operator=
	 *
	 * @param other move some object object
	 *
	 * @return
	 */
	Object& operator=(Object&& other) noexcept {

		if (this != &other) {			auto oldself = self_;
			self_ = other.self_;
			other.self_ = nullptr;
			if (oldself)
				g_object_unref(oldself);
		}
		return *this;
	}

	/**
	 * DTOR
	 */
	virtual ~Object() {
		if (self_) {
			g_object_unref(self_);
		}
	}

	/**
	 * operator bool
	 *
	 * @return true if object wraps a GObject, false otherwise
	 */
	operator bool() const noexcept { return !!self_; }

	/**
	 * Get a ptr to the underlying GObject
	 *
	 * @return GObject or NULL
	 */
	GObject* object() const { return self_; }


	/**
	 * Unref the object
	 *
	 */
	void unref() noexcept {
		g_object_unref(self_);
	}


	/**
	 * Ref the object
	 *
	 */
	void ref() noexcept {
		g_object_ref(self_);
	}


private:
	mutable GObject *self_{};
};



/**
 * Thin wrapper around a GMimeContentType
 *
 */
struct MimeContentType: public Object {

	MimeContentType(GMimeContentType *ctype) : Object{G_OBJECT(ctype)} {
		if (!GMIME_IS_CONTENT_TYPE(self()))
			throw std::runtime_error("not a content-type");
	}
	std::string media_type() const noexcept {
		return g_mime_content_type_get_media_type(self());
	}
	std::string media_subtype() const noexcept {
		return g_mime_content_type_get_media_subtype(self());
	}

	Option<std::string> mime_type() const noexcept {
		return to_string_opt_gchar(g_mime_content_type_get_mime_type(self()));
	}

	bool is_type(const std::string& type, const std::string& subtype) const {
		return g_mime_content_type_is_type(self(), type.c_str(),
						   subtype.c_str());
	}

	Option<std::string> parameter(const std::string& name) const noexcept {
		const char *param{g_mime_content_type_get_parameter(self(), name.c_str())};
		if (!param || !param[0])
			return Nothing;
		else
			return Some(std::string{param});
	}

private:
	GMimeContentType* self() const {
		return reinterpret_cast<GMimeContentType*>(object());
	}
};





/**
 * Thin wrapper around a GMimeStream
 *
 */
struct MimeStream: public Object {

	ssize_t write(const char* buf, ::size_t size) {
		return g_mime_stream_write(self(), buf, size);
	}

	bool reset() {
		return g_mime_stream_reset(self()) < 0 ? false : true;
	}

	bool flush() {
		return g_mime_stream_flush(self()) < 0 ? false : true;
	}

	static MimeStream make_mem() {
		MimeStream mstream{g_mime_stream_mem_new()};
		mstream.unref(); /* remove extra ref */
		return mstream;
	}

	static MimeStream make_filtered(MimeStream& stream) {
		MimeStream mstream{g_mime_stream_filter_new(stream.self())};
		mstream.unref(); /* remove extra refs */
		return mstream;
	}

	static MimeStream make_from_stream(GMimeStream *strm) {
		MimeStream mstream{strm};
		mstream.unref(); /* remove extra ref */
		return mstream;
	}

private:
	MimeStream(GMimeStream *stream): Object(G_OBJECT(stream)) {
		if (!GMIME_IS_STREAM(self()))
			throw std::runtime_error("not a mime-stream");
	};

	GMimeStream* self() const {
		return reinterpret_cast<GMimeStream*>(object());
	}
};

template<typename S, typename T>
constexpr Option<std::string_view> to_string_view_opt(const S& seq, T t) {
	auto&& it = seq_find_if(seq, [&](auto&& item){return item.first == t;});
	if (it == seq.cend())
		return Nothing;
	else
		return it->second;
}


/**
 * Thin wrapper around a GMimeDataWrapper
 *
 */
struct MimeDataWrapper: public Object {
	MimeDataWrapper(GMimeDataWrapper *wrapper): Object(G_OBJECT(wrapper)) {
		if (!GMIME_IS_DATA_WRAPPER(self()))
			throw std::runtime_error("not a data-wrapper");
	};

	Result<size_t> write_to_stream(MimeStream& stream) const {
		if (auto&& res = g_mime_data_wrapper_write_to_stream(
			    self(), GMIME_STREAM(stream.object())) ; res < 0)
			return Err(Error::Code::Message, "failed to write to stream");
		else
			return Ok(static_cast<size_t>(res));
	}

private:
	GMimeDataWrapper* self() const {
		return reinterpret_cast<GMimeDataWrapper*>(object());
	}
};



/**
 * Thin wrapper around a GMimeCertifcate
 *
 */
struct MimeCertificate: public Object {
	MimeCertificate(GMimeCertificate *cert) : Object{G_OBJECT(cert)} {
		if (!GMIME_IS_CERTIFICATE(self()))
			throw std::runtime_error("not a certificate");
	}

	enum struct PubkeyAlgo {
		Default = GMIME_PUBKEY_ALGO_DEFAULT,
		Rsa	= GMIME_PUBKEY_ALGO_RSA,
		RsaE	= GMIME_PUBKEY_ALGO_RSA_E,
		RsaS	= GMIME_PUBKEY_ALGO_RSA_S,
		ElgE	= GMIME_PUBKEY_ALGO_ELG_E,
		Dsa	= GMIME_PUBKEY_ALGO_DSA,
		Ecc	= GMIME_PUBKEY_ALGO_ECC,
		Elg	= GMIME_PUBKEY_ALGO_ELG,
		EcDsa	= GMIME_PUBKEY_ALGO_ECDSA,
		EcDh	= GMIME_PUBKEY_ALGO_ECDH,
		EdDsa	= GMIME_PUBKEY_ALGO_EDDSA,
	};

	enum struct DigestAlgo {
		Default	     = GMIME_DIGEST_ALGO_DEFAULT,
		Md5	     = GMIME_DIGEST_ALGO_MD5,
		Sha1	     = GMIME_DIGEST_ALGO_SHA1,
		RipEmd160    = GMIME_DIGEST_ALGO_RIPEMD160,
		Md2	     = GMIME_DIGEST_ALGO_MD2,
		Tiger192     = GMIME_DIGEST_ALGO_TIGER192,
		Haval5160    = GMIME_DIGEST_ALGO_HAVAL5160,
		Sha256	     = GMIME_DIGEST_ALGO_SHA256,
		Sha384	     = GMIME_DIGEST_ALGO_SHA384,
		Sha512	     = GMIME_DIGEST_ALGO_SHA512,
		Sha224	     = GMIME_DIGEST_ALGO_SHA224,
		Md4	     = GMIME_DIGEST_ALGO_MD4,
		Crc32	     = GMIME_DIGEST_ALGO_CRC32,
		Crc32Rfc1510 = GMIME_DIGEST_ALGO_CRC32_RFC1510,
		Crc32Rfc2440 = GMIME_DIGEST_ALGO_CRC32_RFC2440,
	};

	enum struct Trust {
		Unknown	      = GMIME_TRUST_UNKNOWN,
		Undefined     = GMIME_TRUST_UNDEFINED,
		Never	      = GMIME_TRUST_NEVER,
		Marginal      = GMIME_TRUST_MARGINAL,
		TrustFull     = GMIME_TRUST_FULL,
		TrustUltimate = GMIME_TRUST_ULTIMATE,
	};

	enum struct Validity {
		Unknown	  = GMIME_VALIDITY_UNKNOWN,
		Undefined = GMIME_VALIDITY_UNDEFINED,
		Never	  = GMIME_VALIDITY_NEVER,
		Marginal  = GMIME_VALIDITY_MARGINAL,
		Full	  = GMIME_VALIDITY_FULL,
		Ultimate  = GMIME_VALIDITY_ULTIMATE,
	};

	PubkeyAlgo pubkey_algo() const {
		return static_cast<PubkeyAlgo>(
			g_mime_certificate_get_pubkey_algo(self()));
	}

	DigestAlgo digest_algo() const {
		return static_cast<DigestAlgo>(
			g_mime_certificate_get_digest_algo(self()));
	}

	Validity id_validity() const {
		return static_cast<Validity>(
			g_mime_certificate_get_id_validity(self()));
	}

	Trust trust() const {
		return static_cast<Trust>(
			g_mime_certificate_get_trust(self()));
	}

	Option<std::string> issuer_serial() const {
		return to_string_opt(g_mime_certificate_get_issuer_serial(self()));
	}
	Option<std::string> issuer_name() const {
		return to_string_opt(g_mime_certificate_get_issuer_name(self()));
	}

	Option<std::string> fingerprint() const {
		return to_string_opt(g_mime_certificate_get_fingerprint(self()));
	}

	Option<std::string> key_id() const {
		return to_string_opt(g_mime_certificate_get_key_id(self()));
	}


	Option<std::string> name() const {
		return to_string_opt(g_mime_certificate_get_name(self()));
	}

	Option<std::string> user_id() const {
		return to_string_opt(g_mime_certificate_get_user_id(self()));
	}

	Option<::time_t> created() const {
		if (auto t = g_mime_certificate_get_created(self()); t >= 0)
			return t;
		else
			return Nothing;
	}

	Option<::time_t> expires() const {
		if (auto t = g_mime_certificate_get_expires(self()); t >= 0)
			return t;
		else
			return Nothing;
	}

private:
	GMimeCertificate* self() const {
		return reinterpret_cast<GMimeCertificate*>(object());
	}
};

constexpr std::array<std::pair<MimeCertificate::PubkeyAlgo, std::string_view>, 11>
AllPubkeyAlgos = {{
		{ MimeCertificate::PubkeyAlgo::Default,	"default"},
		{ MimeCertificate::PubkeyAlgo::Rsa,	"rsa"},
		{ MimeCertificate::PubkeyAlgo::RsaE,	"rsa-encryption-only"},
		{ MimeCertificate::PubkeyAlgo::RsaS,	"rsa-signing-only"},
		{ MimeCertificate::PubkeyAlgo::ElgE,	"el-gamal-encryption-only"},
		{ MimeCertificate::PubkeyAlgo::Dsa,	"dsa"},
		{ MimeCertificate::PubkeyAlgo::Ecc,	"elliptic curve"},
		{ MimeCertificate::PubkeyAlgo::Elg,	"el-gamal"},
		{ MimeCertificate::PubkeyAlgo::EcDsa,	"elliptic-curve+dsa"},
		{ MimeCertificate::PubkeyAlgo::EcDh,	"elliptic-curve+diffie-helman"},
		{ MimeCertificate::PubkeyAlgo::EdDsa,	"elliptic-curve+dsa-2"}
		}};

constexpr Option<std::string_view> to_string_view_opt(MimeCertificate::PubkeyAlgo algo) {
	return to_string_view_opt(AllPubkeyAlgos, algo);
}

constexpr std::array<std::pair<MimeCertificate::DigestAlgo, std::string_view>, 15>
AllDigestAlgos = {{
		{ MimeCertificate::DigestAlgo::Default,		"default"},
		{ MimeCertificate::DigestAlgo::Md5,		"md5"},
		{ MimeCertificate::DigestAlgo::Sha1,		"sha1"},
		{ MimeCertificate::DigestAlgo::RipEmd160,	"ripemd-160"},
		{ MimeCertificate::DigestAlgo::Md2,		"md2"},
		{ MimeCertificate::DigestAlgo::Tiger192,	"tiger-192"},
		{ MimeCertificate::DigestAlgo::Haval5160,	"haval-5-160"},
		{ MimeCertificate::DigestAlgo::Sha256,		"sha-256"},
		{ MimeCertificate::DigestAlgo::Sha384,		"sha-384"},
		{ MimeCertificate::DigestAlgo::Sha512,		"sha-512"},
		{ MimeCertificate::DigestAlgo::Sha224,		"sha-224"},
		{ MimeCertificate::DigestAlgo::Md4,		"md4"},
		{ MimeCertificate::DigestAlgo::Crc32,		"crc32"},
		{ MimeCertificate::DigestAlgo::Crc32Rfc1510,	"crc32-rfc1510"},
		{ MimeCertificate::DigestAlgo::Crc32Rfc2440,	"crc32-rfc2440"},
	}};

constexpr Option<std::string_view> to_string_view_opt(MimeCertificate::DigestAlgo algo) {
	return to_string_view_opt(AllDigestAlgos, algo);
}

constexpr std::array<std::pair<MimeCertificate::Trust, std::string_view>, 6>
AllTrusts = {{
		{ MimeCertificate::Trust::Unknown,	"unknown" },
		{ MimeCertificate::Trust::Undefined,	"undefined" },
		{ MimeCertificate::Trust::Never,	"never" },
		{ MimeCertificate::Trust::Marginal,	"marginal" },
		{ MimeCertificate::Trust::TrustFull,	"trust-full" },
		{ MimeCertificate::Trust::TrustUltimate,"trust-ultimate" },
	}};

constexpr Option<std::string_view> to_string_view_opt(MimeCertificate::Trust trust) {
	return to_string_view_opt(AllTrusts, trust);
}

constexpr std::array<std::pair<MimeCertificate::Validity, std::string_view>, 6>
AllValidities = {{
		{ MimeCertificate::Validity::Unknown,	"unknown" },
		{ MimeCertificate::Validity::Undefined, "undefined" },
		{ MimeCertificate::Validity::Never,	"never" },
		{ MimeCertificate::Validity::Marginal,	"marginal" },
		{ MimeCertificate::Validity::Full,	"full" },
		{ MimeCertificate::Validity::Ultimate,	"ultimate" },
	}};

constexpr Option<std::string_view> to_string_view_opt(MimeCertificate::Validity val) {
	return to_string_view_opt(AllValidities, val);
}



/**
 * Thin wrapper around a GMimeSignature
 *
 */
struct MimeSignature: public Object {
	MimeSignature(GMimeSignature *sig) : Object{G_OBJECT(sig)} {
		if (!GMIME_IS_SIGNATURE(self()))
			throw std::runtime_error("not a signature");
	}

	/**
	 * Signature status
	 *
	 */
	enum struct Status {
		Valid	     = GMIME_SIGNATURE_STATUS_VALID,
		Green	     = GMIME_SIGNATURE_STATUS_GREEN,
		Red	     = GMIME_SIGNATURE_STATUS_RED,
		KeyRevoked   = GMIME_SIGNATURE_STATUS_KEY_REVOKED,
		KeyExpired   = GMIME_SIGNATURE_STATUS_KEY_EXPIRED,
		SigExpired   = GMIME_SIGNATURE_STATUS_SIG_EXPIRED,
		KeyMissing   = GMIME_SIGNATURE_STATUS_KEY_MISSING,
		CrlMissing   = GMIME_SIGNATURE_STATUS_CRL_MISSING,
		CrlTooOld    = GMIME_SIGNATURE_STATUS_CRL_TOO_OLD,
		BadPolicy    = GMIME_SIGNATURE_STATUS_BAD_POLICY,
		SysError     = GMIME_SIGNATURE_STATUS_SYS_ERROR,
		TofuConflict = GMIME_SIGNATURE_STATUS_TOFU_CONFLICT
	};

	Status status() const { return static_cast<Status>(
			g_mime_signature_get_status(self())); }

	::time_t created() const { return g_mime_signature_get_created(self()); }
	::time_t expires() const { return g_mime_signature_get_expires(self()); }


	const MimeCertificate certificate() const {
		return MimeCertificate{g_mime_signature_get_certificate(self())};
	}

private:
	GMimeSignature* self() const {
		return reinterpret_cast<GMimeSignature*>(object());
	}
};

constexpr std::array<std::pair<MimeSignature::Status, std::string_view>, 12>
AllMimeSignatureStatuses= {{
		{ MimeSignature::Status::Valid,         "valid" },
		{ MimeSignature::Status::Green,         "green" },
		{ MimeSignature::Status::Red,		"red" },
		{ MimeSignature::Status::KeyRevoked,	"key-revoked" },
		{ MimeSignature::Status::KeyExpired,	"key-expired" },
		{ MimeSignature::Status::SigExpired,	"sig-expired" },
		{ MimeSignature::Status::KeyMissing,	"key-missing" },
		{ MimeSignature::Status::CrlMissing,	"crl-missing" },
		{ MimeSignature::Status::CrlTooOld,	"crl-too-old" },
		{ MimeSignature::Status::BadPolicy,	"bad-policy" },
		{ MimeSignature::Status::SysError,	"sys-error" },
		{ MimeSignature::Status::TofuConflict,	"tofu-confict" },
	}};
MU_ENABLE_BITOPS(MimeSignature::Status);

static inline std::string to_string(MimeSignature::Status status) {
	std::string str;
	for (auto&& item: AllMimeSignatureStatuses) {
		if (none_of(item.first & status))
			continue;
		if (!str.empty())
			str += ", ";
		str += item.second;
	}
	if (str.empty())
		str = "none";

	return str;
}




/**
* Thin wrapper around a GMimeDecryptResult
 *
 */
struct MimeDecryptResult: public Object {
	MimeDecryptResult (GMimeDecryptResult *decres) : Object{G_OBJECT(decres)} {
		if (!GMIME_IS_DECRYPT_RESULT(self()))
			throw std::runtime_error("not a decrypt-result");
	}

	std::vector<MimeCertificate> recipients() const noexcept;
	std::vector<MimeSignature>   signatures() const noexcept;

	enum struct CipherAlgo {
		Default	    = GMIME_CIPHER_ALGO_DEFAULT,
		Idea	    = GMIME_CIPHER_ALGO_IDEA,
		Des3	    = GMIME_CIPHER_ALGO_3DES,
		Cast5	    = GMIME_CIPHER_ALGO_CAST5,
		Blowfish    = GMIME_CIPHER_ALGO_BLOWFISH,
		Aes	    = GMIME_CIPHER_ALGO_AES,
		Aes192	    = GMIME_CIPHER_ALGO_AES192,
		Aes256	    = GMIME_CIPHER_ALGO_AES256,
		TwoFish	    = GMIME_CIPHER_ALGO_TWOFISH,
		Camellia128 = GMIME_CIPHER_ALGO_CAMELLIA128,
		Camellia192 = GMIME_CIPHER_ALGO_CAMELLIA192,
		Camellia256 = GMIME_CIPHER_ALGO_CAMELLIA256
	};

	CipherAlgo cipher() const noexcept {
		return static_cast<CipherAlgo>(
			g_mime_decrypt_result_get_cipher(self()));
	}

	using DigestAlgo = MimeCertificate::DigestAlgo;
	DigestAlgo mdc() const noexcept {
		return static_cast<DigestAlgo>(
			g_mime_decrypt_result_get_mdc(self()));
	}

	Option<std::string> session_key() const noexcept {
		return to_string_opt(g_mime_decrypt_result_get_session_key(self()));
	}

private:
	GMimeDecryptResult* self() const {
		return reinterpret_cast<GMimeDecryptResult*>(object());
	}
};

constexpr std::array<std::pair<MimeDecryptResult::CipherAlgo, std::string_view>, 12>
AllCipherAlgos= {{
		{MimeDecryptResult::CipherAlgo::Default,	"default"},
		{MimeDecryptResult::CipherAlgo::Idea,		"idea"},
		{MimeDecryptResult::CipherAlgo::Des3,		"3des"},
		{MimeDecryptResult::CipherAlgo::Cast5,		"cast5"},
		{MimeDecryptResult::CipherAlgo::Blowfish,	"blowfish"},
		{MimeDecryptResult::CipherAlgo::Aes,		"aes"},
		{MimeDecryptResult::CipherAlgo::Aes192,		"aes192"},
		{MimeDecryptResult::CipherAlgo::Aes256,		"aes256"},
		{MimeDecryptResult::CipherAlgo::TwoFish,	"twofish"},
		{MimeDecryptResult::CipherAlgo::Camellia128,	"camellia128"},
		{MimeDecryptResult::CipherAlgo::Camellia192,	"camellia192"},
		{MimeDecryptResult::CipherAlgo::Camellia256,	"camellia256"},
	}};

constexpr Option<std::string_view> to_string_view_opt(MimeDecryptResult::CipherAlgo algo) {
	return to_string_view_opt(AllCipherAlgos, algo);
}


/**
 * Thin wrapper around a GMimeCryptoContext
 *
 */
struct MimeCryptoContext : public Object {

	/**
	 * Make a new PGP crypto context.
	 *
	 * For 'test-mode', pass a test-path; in this mode GPG will be setup
	 * in an isolated mode so it does not affect normal usage.
	 *
	 * @param testpath (for unit-tests) pass a path to an existing dir to
	 * create a pgp setup. For normal use, leave empty.
	 *
	 * @return A MimeCryptoContext or an error
	 */
	static Result<MimeCryptoContext>
	make_gpg(const std::string& testpath={}) try {
		if (!testpath.empty()) {
			if (auto&& res = setup_gpg_test(testpath); !res)
				return Err(res.error());
		}
		MimeCryptoContext ctx(g_mime_gpg_context_new());
		ctx.unref(); /* remove extra ref */
		return Ok(std::move(ctx));
	} catch (...) {
		return Err(Error::Code::Crypto, "failed to create crypto context");
	}

	static Result<MimeCryptoContext>
	make(const std::string& protocol) {
		auto ctx = g_mime_crypto_context_new(protocol.c_str());
		if (!ctx)
			return Err(Error::Code::Crypto,
				   "unsupported protocol {}", protocol);
		MimeCryptoContext mctx{ctx};
		mctx.unref(); /* remove extra ref */
		return Ok(std::move(mctx));
	}

	Option<std::string> encryption_protocol() const noexcept {
		return to_string_opt(g_mime_crypto_context_get_encryption_protocol(self()));
	}
	Option<std::string> signature_protocol() const noexcept {
		return to_string_opt(g_mime_crypto_context_get_signature_protocol(self()));
	}
	Option<std::string> key_exchange_protocol() const noexcept {
		return to_string_opt(g_mime_crypto_context_get_key_exchange_protocol(self()));
	}

	/**
	 * Imports a stream of keys/certificates contained within stream into
	 * the key/certificate database controlled by @this.
	 *
	 * @param stream
	 *
	 * @return number of keys imported, or an error.
	 */
	Result<size_t> import_keys(MimeStream& stream);

	/**
	 * Prototype for a request-password function.
	 *
	 * @param ctx the MimeCryptoContext making the request
	 * @param user_id the user_id of the password being requested
	 * @param prompt a string containing some helpful context for the prompt
	 * @param reprompt true if this password request is a reprompt due to a
	 * previously bad password response
	 * @param response a stream for the application to write the password to
	 * (followed by a newline '\n' character)
	 *
	 * @return nothing (Ok) or an error,
	 */
	using PasswordRequestFunc =
		std::function<Result<void>(
		const MimeCryptoContext& ctx,
			const std::string& user_id,
			const std::string& prompt,
			bool reprompt,
			MimeStream& response)>;
	/**
	 * Set a function to request a password.
	 *
	 * @param pw_func password function.
	 */
	void set_request_password(PasswordRequestFunc pw_func);


private:
	MimeCryptoContext(GMimeCryptoContext *ctx): Object{G_OBJECT(ctx)} {
		if (!GMIME_IS_CRYPTO_CONTEXT(self()))
			throw std::runtime_error("not a crypto-context");
	}

	static Result<void> setup_gpg_test(const std::string& testpath);

	GMimeCryptoContext* self() const {
		return reinterpret_cast<GMimeCryptoContext*>(object());
	}
};


/**
 * Thin wrapper around a GMimeObject
 *
 */
class MimeObject: public Object {
public:
	/**
	 * Construct a new MimeObject. Take a ref on the obj
	 *
	 * @param mime_part mime-part pointer
	 */
	MimeObject(const Object& obj): Object{obj}  {
		if (!GMIME_IS_OBJECT(self()))
			throw std::runtime_error("not a mime-object");
	}
	MimeObject(GMimeObject *mobj): Object{G_OBJECT(mobj)}  {
		if (mobj && !GMIME_IS_OBJECT(self()))
			throw std::runtime_error("not a mime-object");
	}

	/**
	 * Get a header from the MimeObject
	 *
	 * @param header the header to retrieve
	 *
	 * @return header value (UTF-8) or Nothing
	 */
	Option<std::string> header(const std::string& header) const noexcept;


	/**
	 * Get all headers as pairs of name, value
	 *
	 * @return all headers
	 */
	std::vector<std::pair<std::string, std::string>> headers() const noexcept;


	/**
	 * Get the content type
	 *
	 * @return  the content-type or Nothing
	 */
	Option<MimeContentType> content_type() const noexcept {
		auto ct{g_mime_object_get_content_type(self())};
		if (!ct)
			return Nothing;
		else
			return MimeContentType(ct);
	}

	Option<std::string> mime_type() const noexcept {
		if (auto ct = content_type(); !ct)
			return Nothing;
		else
			return ct->mime_type();
	}

	/**
	 * Get the content-type parameter
	 *
	 * @param param name of parameter
	 *
	 * @return the value of the parameter, or Nothing
	 */
	Option<std::string> content_type_parameter(const std::string& param) const noexcept {
		return Mu::to_string_opt(
			g_mime_object_get_content_type_parameter(self(), param.c_str()));
	}

	/**
	 * Write this MimeObject to some stream
	 *
	 * @param f_opts formatting options
	 * @param stream the stream
	 *
	 * @return the number or bytes written or an error
	 */
	Result<size_t> write_to_stream(const MimeFormatOptions& f_opts,
				       MimeStream& stream) const;
	/**
	 * Write the object to a string.
	 *
	 * @return
	 */
	Option<std::string> to_string_opt() const noexcept;

	/**
	 * Write object to a file
	 *
	 * @param path path to file
	 * @param overwrite if true, overwrite existing file, if it bqexists
	 *
	 * @return size of the wrtten file, or an error.
	 */
	Result<size_t> to_file(const std::string& path, bool overwrite) const noexcept;

	/*
	 * subtypes.
	 */

	/**
	 * Is this a MimePart?
	 *
	 * @return true or false
	 */
	bool is_part()         const { return GMIME_IS_PART(self()); }

	/**
	 * Is this a MimeMultiPart?
	 *
	 * @return true or false
	 */
	bool is_multipart()    const { return GMIME_IS_MULTIPART(self());}

	/**
	 * Is this a MimeMultiPart?
	 *
	 * @return true or false
	 */
	bool is_multipart_encrypted()    const {
		return GMIME_IS_MULTIPART_ENCRYPTED(self());
	}

	/**
	 * Is this a MimeMultiPart?
	 *
	 * @return true or false
	 */
	bool is_multipart_signed()    const {
		return GMIME_IS_MULTIPART_SIGNED(self());
	}

	/**
	 * Is this a MimeMessage?
	 *
	 * @return true or false
	 */
	bool is_message()      const { return GMIME_IS_MESSAGE(self());}

	/**
	 * Is this a MimeMessagePart?
	 *
	 * @return true orf alse
	 */
	bool is_message_part() const { return GMIME_IS_MESSAGE_PART(self());}

	/**
	 * Is this a MimeApplicationpkcs7Mime?
	 *
	 * @return true orf alse
	 */
	bool is_mime_application_pkcs7_mime() const {
		return GMIME_IS_APPLICATION_PKCS7_MIME(self());
	}

	/**
	 * Callback for for_each(). See GMimeObjectForEachFunc.
	 *
	 */
	using ForEachFunc = std::function<void(const MimeObject& parent,
		const MimeObject& part)>;

private:
	GMimeObject* self() const {
		return reinterpret_cast<GMimeObject*>(object());
	}
};


/**
 * Thin wrapper around a GMimeMessage
 *
 */
class MimeMessage: public MimeObject {
public:
	/**
	 * Construct a MimeMessage
	 *
	 * @param obj an Object of the right type
	 */
	MimeMessage(const Object& obj): MimeObject(obj) {
		if (!is_message())
			throw std::runtime_error("not a mime-message");
	}

	/**
	 * Get the top-level MIME-part or Nothing if there is none
	 *
	 * @return MIME-part of nothing
	 */
	Option<MimeObject> mime_part() const noexcept {
		auto obj = g_mime_message_get_mime_part(self());
		if (!obj)
			return Nothing;
		else
			return MimeObject{obj};
	}

	/**
	 * Make a MimeMessage from a file
	 *
	 * @param path path to the file
	 *
	 * @return a MimeMessage or an error.
	 */
	static Result<MimeMessage> make_from_file (const std::string& path);

	/**
	 * Make a MimeMessage from a string
	 *
	 * @param path path to the file
	 *
	 * @return a MimeMessage or an error.
	 */
	static Result<MimeMessage> make_from_text (const std::string& text);

	/**
	 * Get the contacts of a given type, or None for _all_
	 *
	 * @param ctype contact type
	 *
	 * @return contacts
	 */
	Contacts contacts(Contact::Type ctype) const noexcept;

	/**
	 * Gets the message-id if it exists, or nullopt otherwise.
	 *
	 * @return string or nullopt
	 */
	Option<std::string> message_id() const noexcept {
		return Mu::to_string_opt(g_mime_message_get_message_id(self()));
	}

	/**
	 * Gets the message-id if it exists, or nullopt otherwise.
	 *
	 * @return string or nullopt
	 */
	Option<std::string> subject() const noexcept {
		return Mu::to_string_opt(g_mime_message_get_subject(self()));
	}

	/**
	 * Gets the date if it exists, or nullopt otherwise.
	 *
	 * @return a time_t value (expressed as a 64-bit number) or nullopt
	 */
	Option<int64_t> date() const noexcept;


	/**
	 * Get the references for this message (including in-reply-to), in the
	 * order of older..newer; the first one would the oldest parent, and
	 * in-reply-to would be the last one (if any). These are de-duplicated,
	 * and known-fake references removed (see implementation)
	 *
	 * @return references.
	 */
	std::vector<std::string> references() const noexcept;


	/**
	 * Recursively apply func tol all parts of this message
	 *
	 * @param func a function
	 */
	void for_each(const ForEachFunc& func) const noexcept;

private:
	GMimeMessage* self() const {
		return reinterpret_cast<GMimeMessage*>(object());
	}
};

/**
 * Thin wrapper around a GMimePart.
 *
 */
class MimePart: public MimeObject {
public:
	/**
	 * Construct a MimePart
	 *
	 * @param obj an Object of the right type
	 */
	MimePart(const Object& obj): MimeObject(obj) {
		if (!is_part())
			throw std::runtime_error("not a mime-part");
	}

	/**
	 * Determines whether or not the part is an attachment based on the
	 * value of the Content-Disposition header.
	 *
	 * @return true or false
	 */
	bool is_attachment() const noexcept {
		return g_mime_part_is_attachment(self());
	}

	/**
	 * Gets the value of the Content-Description for this mime part
	 * if it exists, or nullopt otherwise.
	 *
	 * @return string or nullopt
	 */
	Option<std::string> content_description() const noexcept {
		return Mu::to_string_opt(g_mime_part_get_content_description(self()));
	}

	/**
	 * Gets the value of the Content-Id for this mime part
	 * if it exists, or nullopt otherwise.
	 *
	 * @return string or nullopt
	 */
	Option<std::string> content_id() const noexcept {
		return Mu::to_string_opt(g_mime_part_get_content_id(self()));
	}

	/**
	 * Gets the value of the Content-Md5 header for this mime part
	 * if it exists, or nullopt otherwise.
	 *
	 * @return string or nullopt
	 */
	Option<std::string> content_md5() const noexcept {
		return Mu::to_string_opt(g_mime_part_get_content_md5(self()));

	}

	/**
	 * Verify the content md5 for the specified mime part. Returns false if
	 * the mime part does not contain a Content-MD5.
	 *
	 * @return true or false
	 */
	bool verify_content_md5() const noexcept {
		return g_mime_part_verify_content_md5(self());
	}

	/**
	 * Gets the value of the Content-Location for this mime part if it
	 * exists, or nullopt otherwise.
	 *
	 * @return string or nullopt
	 */
	Option<std::string> content_location() const noexcept {
		return Mu::to_string_opt(g_mime_part_get_content_location(self()));
	}


	MimeDataWrapper content() const noexcept {
		return MimeDataWrapper{g_mime_part_get_content(self())};
	}

	/**
	 * Gets the filename for this mime part if it exists, or nullopt
	 * otherwise.
	 *
	 * @return string or nullopt
	 */
	Option<std::string> filename() const noexcept {
		return Mu::to_string_opt(g_mime_part_get_filename(self()));
	}

	/**
	 * Size of content, in bytes
	 *
	 * @return size
	 */
	size_t size() const noexcept;

	/**
	 * Get as UTF-8 string
	 *
	 * @return a string, or NULL.
	 */
	Option<std::string> to_string() const noexcept;

	/**
	 * Write part to a file
	 *
	 * @param path path to file
	 * @param overwrite if true, overwrite existing file, if it bqexists
	 *
	 * @return size of the wrtten file, or an error.
	 */
	Result<size_t> to_file(const std::string& path, bool overwrite)
		const noexcept;

	/**
	 * Types of Content Encoding.
	 *
	 */
	enum struct ContentEncoding {
		Default		= GMIME_CONTENT_ENCODING_DEFAULT,
		SevenBit	= GMIME_CONTENT_ENCODING_7BIT,
		EightBit	= GMIME_CONTENT_ENCODING_8BIT,
		Binary		= GMIME_CONTENT_ENCODING_BINARY,
		Base64		= GMIME_CONTENT_ENCODING_BASE64,
		QuotedPrintable = GMIME_CONTENT_ENCODING_QUOTEDPRINTABLE,
		UuEncode	= GMIME_CONTENT_ENCODING_UUENCODE
	};

	/**
	 * Gets the content encoding of the mime part.
	 *
	 * @return the content encoding
	 */
	ContentEncoding content_encoding() const noexcept {
		const auto enc{g_mime_part_get_content_encoding(self())};
		g_return_val_if_fail(enc <= GMIME_CONTENT_ENCODING_UUENCODE,
				     ContentEncoding::Default);
		return static_cast<ContentEncoding>(enc);
	}


	/**
	 * Types of OpenPGP data
	 *
	 */
	enum struct OpenPGPData {
		None	   = GMIME_OPENPGP_DATA_NONE,
		Encrypted  = GMIME_OPENPGP_DATA_ENCRYPTED,
		Signed	   = GMIME_OPENPGP_DATA_SIGNED,
		PublicKey  = GMIME_OPENPGP_DATA_PUBLIC_KEY,
		PrivateKey = GMIME_OPENPGP_DATA_PRIVATE_KEY,
	};

	/**
	 * Gets whether or not (and what type) of OpenPGP data is contained
	 *
	 * @return OpenGPGData
	 */
	OpenPGPData openpgp_data() const noexcept {
		const auto data{g_mime_part_get_openpgp_data(self())};
		g_return_val_if_fail(data <= GMIME_OPENPGP_DATA_PRIVATE_KEY,
				     OpenPGPData::None);
		return static_cast<OpenPGPData>(data);
	}

private:
	GMimePart* self() const {
		return reinterpret_cast<GMimePart*>(object());
	}
};



/**
 * Thin wrapper around a GMimeMessagePart.
 *
 */
class MimeMessagePart: public MimeObject {
public:
	/**
	 * Construct a MimeMessagePart
	 *
	 * @param obj an Object of the right type
	 */
	MimeMessagePart(const Object& obj): MimeObject(obj) {
		if (!is_message_part())
			throw std::runtime_error("not a mime-message-part");
	}

	/**
	 * Get the MimeMessage for this MimeMessagePart.
	 *
	 * @return the MimeMessage or Nothing
	 */
	Option<MimeMessage> get_message() const {
		auto msg{g_mime_message_part_get_message(self())};
		if (msg)
			return MimeMessage(Object(G_OBJECT(msg)));
		else
			return Nothing;
	}
private:
	GMimeMessagePart* self() const {
		return reinterpret_cast<GMimeMessagePart*>(object());
	}

};
/**
 * Thin wrapper around a GMimeApplicationPkcs7Mime
 *
 */
class MimeApplicationPkcs7Mime: public MimePart {
public:
	/**
	 * Construct a MimeApplicationPkcs7Mime
	 *
	 * @param obj an Object of the right type
	 */
	MimeApplicationPkcs7Mime(const Object& obj): MimePart(obj) {
		if (!is_mime_application_pkcs7_mime())
			throw std::runtime_error("not a mime-application-pkcs7-mime");
	}

	enum struct SecureMimeType {
		CompressedData = GMIME_SECURE_MIME_TYPE_COMPRESSED_DATA,
		EnvelopedData  = GMIME_SECURE_MIME_TYPE_ENVELOPED_DATA,
		SignedData     = GMIME_SECURE_MIME_TYPE_SIGNED_DATA,
		CertsOnly      = GMIME_SECURE_MIME_TYPE_CERTS_ONLY,
		Unknown	       = GMIME_SECURE_MIME_TYPE_UNKNOWN
	};

	SecureMimeType smime_type() const {
		return static_cast<SecureMimeType>(
			g_mime_application_pkcs7_mime_get_smime_type(self()));
	}

private:
	GMimeApplicationPkcs7Mime* self() const {
		return reinterpret_cast<GMimeApplicationPkcs7Mime*>(object());
	}
};


/**
 * Thin wrapper around a GMimeMultiPart
 *
 */
class MimeMultipart: public MimeObject {
public:
	/**
	 * Construct a MimeMultipart
	 *
	 * @param obj an Object of the right type
	 */
	MimeMultipart(const Object& obj): MimeObject(obj) {
		if (!is_multipart())
			throw std::runtime_error("not a mime-multipart");
	}

	Option<MimePart> signed_content_part() const {
		return part(GMIME_MULTIPART_SIGNED_CONTENT);
	}

	Option<MimePart> signed_signature_part() const {
		return part(GMIME_MULTIPART_SIGNED_SIGNATURE);
	}

	Option<MimePart> encrypted_version_part() const {
		return part(GMIME_MULTIPART_ENCRYPTED_VERSION);
	}

	Option<MimePart> encrypted_content_part() const {
		return part(GMIME_MULTIPART_ENCRYPTED_CONTENT);
	}

	/**
	 * Recursively apply func to all parts
	 *
	 * @param func a function
	 */
	void for_each(const ForEachFunc& func) const noexcept;

private:
	// Note: the part may not be available if the message was marked as
	// _signed_ or _encrypted_ because it contained a forwarded signed or
	// encrypted message.
	Option<MimePart> part(int index) const {
		if (auto&& p{g_mime_multipart_get_part(self() ,index)};
		    !GMIME_IS_PART(p))
			return Nothing;
		else
			return Some(MimeObject{p});
	}

	GMimeMultipart* self() const {
		return reinterpret_cast<GMimeMultipart*>(object());
	}
};


/**
 * Thin wrapper around a GMimeMultiPartEncrypted
 *
 */
class MimeMultipartEncrypted: public MimeMultipart {
public:
	/**
	 * Construct a MimeMultipartEncrypted
	 *
	 * @param obj an Object of the right type
	 */
	MimeMultipartEncrypted(const Object& obj): MimeMultipart(obj) {
		if (!is_multipart_encrypted())
			throw std::runtime_error("not a mime-multipart-encrypted");
	}

	enum struct DecryptFlags {
		None			      = GMIME_DECRYPT_NONE,
		ExportSessionKey	      = GMIME_DECRYPT_EXPORT_SESSION_KEY,
		NoVerify		      = GMIME_DECRYPT_NO_VERIFY,
		EnableKeyserverLookups	      = GMIME_DECRYPT_ENABLE_KEYSERVER_LOOKUPS,
		EnableOnlineCertificateChecks = GMIME_DECRYPT_ENABLE_ONLINE_CERTIFICATE_CHECKS
	};

	using Decrypted = std::pair<MimeObject, MimeDecryptResult>;
	Result<Decrypted> decrypt(const MimeCryptoContext& ctx,
				  DecryptFlags flags=DecryptFlags::None,
				  const std::string& session_key = {}) const noexcept;

private:
	GMimeMultipartEncrypted* self() const {
		return reinterpret_cast<GMimeMultipartEncrypted*>(object());
	}
};

MU_ENABLE_BITOPS(MimeMultipartEncrypted::DecryptFlags);


/**
 * Thin wrapper around a GMimeMultiPartSigned
 *
 */
class MimeMultipartSigned: public MimeMultipart {
public:
	/**
	 * Construct a MimeMultipartSigned
	 *
	 * @param obj an Object of the right type
	 */
	MimeMultipartSigned(const Object& obj): MimeMultipart(obj) {
		if (!is_multipart_signed())
			throw std::runtime_error("not a mime-multipart-signed");
	}

	enum struct VerifyFlags {
		None			      = GMIME_VERIFY_NONE,
		EnableKeyserverLookups	      = GMIME_VERIFY_ENABLE_KEYSERVER_LOOKUPS,
		EnableOnlineCertificateChecks = GMIME_VERIFY_ENABLE_ONLINE_CERTIFICATE_CHECKS
	};

	// Result<std::vector<MimeSignature>> verify(VerifyFlags vflags=VerifyFlags::None) const noexcept;

	Result<std::vector<MimeSignature>> verify(const MimeCryptoContext& ctx,
						  VerifyFlags vflags=VerifyFlags::None) const noexcept;

private:
	GMimeMultipartSigned* self() const {
		return reinterpret_cast<GMimeMultipartSigned*>(object());
	}
};


MU_ENABLE_BITOPS(MimeMultipartSigned::VerifyFlags);

} // namespace Mu


#endif /* MU_MIME_OBJECT_HH__ */
