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
#include <gmime/gmime.h>
#include "gmime/gmime-application-pkcs7-mime.h"
#include "utils/mu-option.hh"
#include "utils/mu-result.hh"
#include "mu-contact.hh"

namespace Mu {

/**
 * Initialize gmime (idempotent)
 *
 */
void init_gmime(void);

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
	Object(GObject* &&obj): self_{g_object_ref(obj)} {
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
			self_ = other.self_ ? g_object_ref(other.self_) : nullptr;
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

		if (this != &other) {
			auto oldself = self_;
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

protected:
	GObject* object() const { return self(); }

	static Option<std::string> maybe_string(const char *str) noexcept {
		if (!str)
			return Nothing;
		else
			return std::string(str);
	}

private:
	GObject *self() const { return self_; }
	mutable GObject *self_{};
};


struct MimeContentType: public Object {

	MimeContentType(GMimeContentType *ctype) : Object{G_OBJECT(ctype)} {
		if (!GMIME_IS_CONTENT_TYPE(self()))
			throw std::runtime_error("not a content-type");
	}
	std::string media_type() const {
		return g_mime_content_type_get_media_type(self());
	}
	std::string media_subtype() const {
		return g_mime_content_type_get_media_subtype(self());
	}
	bool is_type(const std::string& type, const std::string& subtype) const {
		return g_mime_content_type_is_type(self(), type.c_str(),
						   subtype.c_str());
	}
private:
	GMimeContentType* self() const {
		return reinterpret_cast<GMimeContentType*>(object());
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
		if (!GMIME_IS_OBJECT(self()))
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


	/**
	 * Write the object to a string.
	 *
	 * @return
	 */
	Option<std::string> object_to_string() const noexcept;

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
	static Result<MimeMessage> make_from_string (const std::string& text);


	/**
	 * Address types
	 *
	 */
	enum struct AddressType {
		Sender	= GMIME_ADDRESS_TYPE_SENDER,
		From	= GMIME_ADDRESS_TYPE_FROM,
		ReplyTo = GMIME_ADDRESS_TYPE_REPLY_TO,
		To	= GMIME_ADDRESS_TYPE_TO,
		Cc	= GMIME_ADDRESS_TYPE_CC,
		Bcc	= GMIME_ADDRESS_TYPE_BCC
	};

	Contacts addresses(AddressType atype) const noexcept;

	/**
	 * Gets the message-id if it exists, or nullopt otherwise.
	 *
	 * @return string or nullopt
	 */
	Option<std::string> message_id() const noexcept {
		return maybe_string(g_mime_message_get_message_id(self()));
	}


	/**
	 * Gets the message-id if it exists, or nullopt otherwise.
	 *
	 * @return string or nullopt
	 */
	Option<std::string> subject() const noexcept {
		return maybe_string(g_mime_message_get_subject(self()));
	}

	/**
	 * Gets the date if it exists, or nullopt otherwise.
	 *
	 * @return a time_t value (expressed as a 64-bit number) or nullopt
	 */
	Option<int64_t> date() const noexcept;


	/**
	 * Get the references for this message (including in-reply-to), in the
	 * order of older..newer; in-reply-to would be the last one.
	 *
	 * @return references.
	 */
	std::vector<std::string> references() const noexcept;


	/**
	 * Callback for for_each(). See GMimeObjectForEachFunc.
	 *
	 */
	using ForEachFunc = std::function<void(const MimeObject& parent,
					       const MimeObject& part)>;

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
		return maybe_string(g_mime_part_get_content_description(self()));
	}

	/**
	 * Gets the value of the Content-Id for this mime part
	 * if it exists, or nullopt otherwise.
	 *
	 * @return string or nullopt
	 */
	Option<std::string> content_id() const noexcept {
		return maybe_string(g_mime_part_get_content_id(self()));
	}

	/**
	 * Gets the value of the Content-Md5 header for this mime part
	 * if it exists, or nullopt otherwise.
	 *
	 * @return string or nullopt
	 */
	Option<std::string> content_md5() const noexcept {
		return maybe_string(g_mime_part_get_content_md5(self()));

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
		return maybe_string(g_mime_part_get_content_location(self()));
	}

	/**
	 * Gets the filename for this mime part if it exists, or nullopt
	 * otherwise.
	 *
	 * @return string or nullopt
	 */
	Option<std::string> filename() const noexcept {
		return maybe_string(g_mime_part_get_filename(self()));
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

private:
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

private:
	GMimeMultipartEncrypted* self() const {
		return reinterpret_cast<GMimeMultipartEncrypted*>(object());
	}
};


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

private:
	GMimeMultipartSigned* self() const {
		return reinterpret_cast<GMimeMultipartSigned*>(object());
	}
};

} // namespace Mu


#endif /* MU_MIME_OBJECT_HH__ */
