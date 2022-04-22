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


#include "mu-message.hh"
#include "gmime/gmime-references.h"
#include "gmime/gmime-stream-mem.h"
#include "mu-maildir.hh"

#include <array>
#include <optional>
#include <string>
#include <utils/mu-util.h>
#include <utils/mu-utils.hh>
#include <utils/mu-error.hh>

#include <atomic>
#include <mutex>
#include <cstdlib>

#include <glib.h>
#include <gmime/gmime.h>

#include "gmime/gmime-message.h"
#include "mu-mime-object.hh"

using namespace Mu;

struct Message::Private {
	Private(Message::Options options=Message::Options::None):
		opts{options} {}

	Message::Options                opts;
	Document			doc;
	mutable Option<MimeMessage>	mime_msg;

	Flags                           flags{};
	Option<std::string>		mailing_list;
	std::vector<std::string>        references;
	std::vector<Part>               parts;

	::time_t                        mtime{};

	/*
	 * we only need to index these, so we don't
	 * really need these copy if we re-arrange things
	 * a bit
	 */
	Option<std::string> body_txt;
	Option<std::string> body_html;
	Option<std::string> embedded;
};


static void fill_document(Message::Private& priv);

static Result<struct stat>
get_statbuf(const std::string& path)
{
	if (!g_path_is_absolute(path.c_str()))
		return Err(Error::Code::File, "path '%s' is not absolute",
			   path.c_str());
	if (::access(path.c_str(), R_OK) != 0)
		return Err(Error::Code::File, "file @ '%s' is not readable",
			   path.c_str());

	struct stat statbuf{};
	if (::stat(path.c_str(), &statbuf) < 0)
		return Err(Error::Code::File, "cannot stat %s: %s", path.c_str(),
			    g_strerror(errno));

	if (!S_ISREG(statbuf.st_mode))
		return Err(Error::Code::File, "not a regular file: %s", path.c_str());

	return Ok(std::move(statbuf));
}


Message::Message(Message::Options opts, const std::string& path,
		 const std::string& mdir):
	priv_{std::make_unique<Private>(opts)}
{

	const auto statbuf{get_statbuf(path)};
	if (!statbuf)
		throw statbuf.error();

	priv_->mtime = statbuf->st_mtime;

	init_gmime();
	if (auto msg{MimeMessage::make_from_file(path)}; !msg)
		throw msg.error();
	else
		priv_->mime_msg = std::move(msg.value());

	auto xpath{to_string_opt_gchar(g_canonicalize_filename(path.c_str(), NULL))};
	if (xpath)
		priv_->doc.add(Field::Id::Path, std::move(xpath.value()));

	if (!mdir.empty())
		priv_->doc.add(Field::Id::Maildir, mdir);

	priv_->doc.add(Field::Id::Size, static_cast<int64_t>(statbuf->st_size));

	// rest of the fields
	fill_document(*priv_);
}

Message::Message(Message::Options opts, const std::string& text, const std::string& path,
		 const std::string& mdir):
	priv_{std::make_unique<Private>(opts)}
{
	if (!path.empty()) {
		auto xpath{to_string_opt_gchar(g_canonicalize_filename(path.c_str(), {}))};
		if (xpath)
			priv_->doc.add(Field::Id::Path, std::move(xpath.value()));
	}

	if (!mdir.empty())
		priv_->doc.add(Field::Id::Maildir, mdir);

	priv_->doc.add(Field::Id::Size, static_cast<int64_t>(text.size()));

	init_gmime();
	if (auto msg{MimeMessage::make_from_text(text)}; !msg)
		throw msg.error();
	else
		priv_->mime_msg = std::move(msg.value());

	fill_document(*priv_);
}


Message::Message(Message&& other) noexcept
{
	*this = std::move(other);
}

Message&
Message::operator=(Message&& other) noexcept
{
	if (this != &other)
		priv_ = std::move(other.priv_);

	return *this;
}

Message::Message(Document&& doc):
	priv_{std::make_unique<Private>()}
{
	priv_->doc = std::move(doc);
}


Message::~Message() = default;

const Mu::Document&
Message::document() const
{
	return priv_->doc;
}

bool
Message::load_mime_message(bool reload) const
{
	if (priv_->mime_msg && !reload)
		return true;

	const auto path{document().string_value(Field::Id::Path)};
	if (auto mime_msg{MimeMessage::make_from_file(path)}; !mime_msg) {
		g_warning("failed to load '%s': %s",
			  path.c_str(), mime_msg.error().what());
		return false;
	} else {
		priv_->mime_msg = std::move(mime_msg.value());
		return true;
	}
}

void
Message::unload_mime_message() const
{
	priv_->mime_msg = Nothing;
}

bool
Message::has_mime_message() const
{
	return !!priv_->mime_msg;
}


static Priority
get_priority(const MimeMessage& mime_msg)
{
	constexpr std::array<std::pair<std::string_view, Priority>, 10>
		prio_alist = {{
			{"high",	Priority::High},
			{"1",		Priority::High},
			{"2",		Priority::High},

			{"normal",	Priority::Normal},
			{"3",		Priority::Normal},

			{"low",		Priority::Low},
			{"list",	Priority::Low},
			{"bulk",	Priority::Low},
			{"4",		Priority::Low},
			{"5",		Priority::Low}
		}};

	const auto opt_str = mime_msg.header("Precedence")
		.disjunction(mime_msg.header("X-Priority"))
		.disjunction(mime_msg.header("Importance"));

	if (!opt_str)
		return  Priority::Normal;

	const auto it = seq_find_if(prio_alist, [&](auto&& item) {
		return g_ascii_strncasecmp(item.first.data(), opt_str->c_str(),
					   item.first.size()) == 0; });

	return it == prio_alist.cend() ? Priority::Normal : it->second;
}


/* see: http://does-not-exist.org/mail-archives/mutt-dev/msg08249.html */
static std::vector<std::string>
get_tags(const MimeMessage& mime_msg)
{
	constexpr std::array<std::pair<const char*, char>, 3> tag_headers = {{
		{"X-Label", ' '}, {"X-Keywords", ','}, {"Keywords", ' '}
	}};

	std::vector<std::string> tags;
	seq_for_each(tag_headers, [&](auto&& item) {
		if (auto&& hdr{mime_msg.header(item.first)}; hdr) {

			auto lst = split(*hdr, item.second);
			tags.reserve(tags.size() + lst.size());
			tags.insert(tags.end(), lst.begin(), lst.end());
		}
	});

	return tags;
}

static Option<std::string>
get_mailing_list(const MimeMessage& mime_msg)
{
	char *dechdr, *res;
	const char *b, *e;

	const auto hdr{mime_msg.header("List-Id")};
	if (!hdr)
		return {};

	dechdr = g_mime_utils_header_decode_phrase(NULL, hdr->c_str());
	if (!dechdr)
		return {};

	e = NULL;
	b = ::strchr(dechdr, '<');
	if (b)
		e = strchr(b, '>');

	if (b && e)
		res = g_strndup(b + 1, e - b - 1);
	else
		res = g_strdup(dechdr);

	g_free(dechdr);

	return to_string_opt_gchar(std::move(res));
}

static bool /* heuristic */
looks_like_attachment(const MimeObject& parent,
		      const MimePart& part, const MimeContentType& ctype)
{
	constexpr std::array<std::pair<const char*, const char*>, 4> att_types = {{
			{"image", "*"},
			{"audio", "*"},
			{"application", "*"},
			{"application", "x-patch"}
		}};

	if (parent) { /* crypto multipart children are not considered attachments */
		if (const auto parent_ctype{parent.content_type()}; parent_ctype) {
			if (parent_ctype->is_type("multipart", "signed") ||
			    parent_ctype->is_type("multipart", "encrypted"))
				return false;
		}
	}

	/* we also consider patches, images, audio, and non-pgp-signature
	 * application attachments to be attachments... */
	if (ctype.is_type("*", "pgp-signature"))
		return false; /* don't consider as a signature */

	if (ctype.is_type("text", "*") &&
	    (ctype.is_type("*", "plain") || ctype.is_type("*", "html")))
		return false; /* not a signature */

	/* if not one of those special types, consider it any attachment
	 * if it says so */
	if (part.is_attachment())
		return true;

	const auto it = seq_find_if(att_types, [&](auto&& item){
		return ctype.is_type(item.first, item.second);
	});
	return it != att_types.cend(); /* if found, it's an attachment */
}

static void
accumulate_text(const MimePart& part, Message::Private& info,
		const MimeContentType& ctype)
{
	if (!ctype.is_type("text", "*"))
		return; /* not a text type */

	auto append = [](Option<std::string>& str, Option<std::string> app) {
		if (!str)
			str = app;
		else if (app)
			str.value() += app.value();
	};

	if (part.is_attachment())
		append(info.embedded, part.to_string());
	else if (ctype.is_type("text", "plain"))
		append(info.body_txt, part.to_string());
	else if (ctype.is_type("text", "html"))
		append(info.body_html, part.to_string());
}

static void
process_part(const MimeObject& parent, const MimePart& part,
	     Message::Private& info)
{
	const auto ctype{part.content_type()};
	if (!ctype)
		return;

	if (looks_like_attachment(parent, part, *ctype))
		info.flags |= Flags::HasAttachment;

	// if there are text parts, gather.
	accumulate_text(part, info, *ctype);
}


static void
handle_object(const MimeObject& parent,
	      const MimeObject& obj, Message::Private& info);


static void
handle_encrypted(const MimeMultipartEncrypted& part, Message::Private& info)
{
	if (!any_of(info.opts & Message::Options::Decrypt)) {
		/* just added to the list */
		info.parts.emplace_back(part);
		return;
	}

	const auto proto{part.content_type_parameter("protocol").value_or("unknown")};
	const auto ctx = MimeCryptoContext::make(proto);
	if (!ctx) {
		g_warning("failed to create context for protocol <%s>",
			  proto.c_str());
		return;
	}

	auto res{part.decrypt(*ctx)};
	if (!res) {
		g_warning("failed to decrypt: %s", res.error().what());
		return;
	}

	if (res->first.is_multipart()) {
		MimeMultipart{res->first}.for_each(
			[&](auto&& parent, auto&& child_obj) {
				handle_object(parent, child_obj, info);
			});

	} else
		handle_object(part, res->first, info);
}


static void
handle_object(const MimeObject& parent,
	      const MimeObject& obj, Message::Private& info)
{
	/* if it's an encrypted part we should decrypt, recurse */
	if (obj.is_multipart_encrypted())
		handle_encrypted(MimeMultipartEncrypted{obj}, info);
	else if (obj.is_part() ||
		 obj.is_message_part() ||
		 obj.is_multipart_signed() ||
		 obj.is_multipart_encrypted())
		info.parts.emplace_back(obj);

	if (obj.is_part())
		process_part(parent, obj, info);

	if (obj.is_multipart_signed())
		info.flags |= Flags::Signed;
	else if (obj.is_multipart_encrypted()) {
		/* FIXME: An encrypted part might be signed at the same time.
		 *        In that case the signed flag is lost. */
		info.flags |= Flags::Encrypted;
	} else if (obj.is_mime_application_pkcs7_mime()) {
		MimeApplicationPkcs7Mime smime(obj);
		switch (smime.smime_type()) {
		case Mu::MimeApplicationPkcs7Mime::SecureMimeType::SignedData:
			info.flags |= Flags::Signed;
			break;
		case Mu::MimeApplicationPkcs7Mime::SecureMimeType::EnvelopedData:
			info.flags |= Flags::Encrypted;
			break;
		default:
			break;
		}
	}
}

/**
 * This message -- recursively walk through message, and initialize some
 * other values that depend on another.
 *
 * @param mime_msg
 * @param path
 * @param info
 */
static void
process_message(const MimeMessage& mime_msg, const std::string& path,
		Message::Private& info)
{
	/* only have file-flags when there's a path. */
	if (!path.empty()) {
		info.flags = flags_from_path(path).value_or(Flags::None);
		/* pseudo-flag --> unread means either NEW or NOT SEEN, just
		 * for searching convenience */
		if (any_of(info.flags & Flags::New) || none_of(info.flags & Flags::Seen))
			info.flags |= Flags::Unread;
	}

	// parts
	mime_msg.for_each([&](auto&& parent, auto&& child_obj) {
		handle_object(parent, child_obj, info);
	});

	// get the mailing here, and use it do update flags, too.
	info.mailing_list = get_mailing_list(mime_msg);
	if (info.mailing_list)
		info.flags |= Flags::MailingList;
}

static Mu::Result<std::string>
calculate_sha256(const std::string& path)
{
	g_autoptr(GChecksum) checksum{g_checksum_new(G_CHECKSUM_SHA256)};

	FILE *file{::fopen(path.c_str(), "r")};
	if (!file)
		return Err(Error{Error::Code::File, "failed to open %s: %s",
					path.c_str(), ::strerror(errno)});

	std::array<uint8_t, 4096> buf{};
	while (true) {
		const auto n = ::fread(buf.data(), 1, buf.size(), file);
		if (n == 0)
			break;
		g_checksum_update(checksum, buf.data(), n);
	}

	bool has_err = ::ferror(file) != 0;
	::fclose(file);

	if (has_err)
		return Err(Error{Error::Code::File, "failed to read %s", path.c_str()});

	return Ok(g_checksum_get_string(checksum));
}

/**
 * Get a fake-message-id for a message without one.
 *
 * @param path message path
 *
 * @return a fake message-id
 */
static std::string
fake_message_id(const std::string& path)
{
	constexpr auto mu_suffix{"@mu.id"};

	// not a very good message-id, only for testing.
	if (path.empty() || ::access(path.c_str(), R_OK) != 0)
		return format("%08x%s", g_str_hash(path.c_str()), mu_suffix);
	if (const auto sha256_res{calculate_sha256(path)}; !sha256_res)
		return format("%08x%s", g_str_hash(path.c_str()), mu_suffix);
	else
		return format("%s%s", sha256_res.value().c_str(), mu_suffix);
}


static void
fill_document(Message::Private& priv)
{
	/* hunt & gather info from message tree */
	Document& doc{priv.doc};
	MimeMessage& mime_msg{priv.mime_msg.value()};

	const auto path{doc.string_value(Field::Id::Path)};
	const auto refs{mime_msg.references()};
	const auto message_id{mime_msg.message_id().value_or(fake_message_id(path))};

	process_message(mime_msg, path, priv);

	field_for_each([&](auto&& field) {
		/* insist on expliclity handling each */
#pragma GCC diagnostic push
#pragma GCC diagnostic error "-Wswitch"
		using AddrType = MimeMessage::AddressType;
		switch(field.id) {
		case Field::Id::Bcc:
			doc.add(field.id, mime_msg.addresses(AddrType::Bcc));
			break;
		case Field::Id::BodyHtml:
			doc.add(field.id, priv.body_html);
			break;
		case Field::Id::BodyText:
			doc.add(field.id, priv.body_txt);
			break;
		case Field::Id::Cc:
			doc.add(field.id, mime_msg.addresses(AddrType::Cc));
			break;
		case Field::Id::Date:
			doc.add(field.id, mime_msg.date());
			break;
		case Field::Id::EmbeddedText:
			doc.add(field.id, priv.embedded);
			break;
		case Field::Id::File:
			for (auto&& part: priv.parts)
				doc.add(field.id, part.raw_filename());
			break;
		case Field::Id::Flags:
			doc.add(priv.flags);
			break;
		case Field::Id::From:
			doc.add(field.id, mime_msg.addresses(AddrType::From));
			break;
		case Field::Id::Maildir: /* already */
			break;
		case Field::Id::MailingList:
			doc.add(field.id, priv.mailing_list);
			break;
		case Field::Id::MessageId:
			doc.add(field.id, message_id);
			break;
		case Field::Id::Mime:
			for (auto&& part: priv.parts)
				doc.add(field.id, part.mime_type());
			break;
		case Field::Id::Path: /* already */
			break;
		case Field::Id::Priority:
			doc.add(get_priority(mime_msg));
			break;
		case Field::Id::References:
			doc.add(field.id, refs);
			break;
		case Field::Id::Size: /* already */
			break;
		case Field::Id::Subject:
			doc.add(field.id, mime_msg.subject());
			break;
		case Field::Id::Tags:
			doc.add(field.id, get_tags(mime_msg));
			break;
		case Field::Id::ThreadId:
			// either the oldest reference, or otherwise the message id
			doc.add(field.id, refs.empty() ? message_id : refs.at(0));
			break;
		case Field::Id::To:
			doc.add(field.id, mime_msg.addresses(AddrType::To));
			break;
		case Field::Id::Uid:
			doc.add(field.id, path); // just a synonym for now.
			break;
		case Field::Id::_count_:
			break;
		}
#pragma GCC diagnostic pop

	});
}

Option<std::string>
Message::header(const std::string& header_field) const
{
	if (!load_mime_message())
		return Nothing;

	return priv_->mime_msg->header(header_field);
}

Option<std::string>
Message::body_text() const
{
	if (!load_mime_message())
		return {};

	return priv_->body_txt;
}

Option<std::string>
Message::body_html() const
{
	if (!load_mime_message())
		return {};

	return priv_->body_html;
}

Contacts
Message::all_contacts() const
{
	Contacts contacts;

	if (!load_mime_message())
		return contacts; /* empty */

	for (auto&& ctype: {
			MimeMessage::AddressType::Sender,
			MimeMessage::AddressType::From,
			MimeMessage::AddressType::ReplyTo,
			MimeMessage::AddressType::To,
			MimeMessage::AddressType::Cc,
			MimeMessage::AddressType::Bcc}) {
		auto addrs{priv_->mime_msg->addresses(ctype)};
		std::move(addrs.begin(), addrs.end(), std::back_inserter(contacts));
	}

	return contacts;
}

const std::vector<Message::Part>&
Message::parts() const
{
	if (!load_mime_message()) {
		static std::vector<Message::Part> empty;
		return empty;
	}

	return priv_->parts;
}

::time_t
Message::mtime() const
{
	if (!load_mime_message())
		return 0;

	return priv_->mtime;
}


