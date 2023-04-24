/*
** Copyright (C) 2022-2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <string>
#include <regex>
#include <utils/mu-utils.hh>
#include <utils/mu-error.hh>
#include <utils/mu-option.hh>

#include <atomic>
#include <mutex>
#include <cstdlib>

#include <glib.h>
#include <glib/gstdio.h>
#include <gmime/gmime.h>

#include "gmime/gmime-message.h"
#include "mu-mime-object.hh"

using namespace Mu;

struct Message::Private {
	Private(Message::Options options): opts{options} {}
	Private(Message::Options options, Xapian::Document&& xdoc):
		opts{options}, doc{std::move(xdoc)} {}

	Message::Options                opts;
	Document			doc;
	mutable Option<MimeMessage>	mime_msg;

	Flags                           flags{};
	Option<std::string>		mailing_list;
	std::vector<Part>               parts;

	::time_t                        ctime{};

	std::string                     cache_path;
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
get_statbuf(const std::string& path, Message::Options opts = Message::Options::None)
{
	if (none_of(opts & Message::Options::AllowRelativePath) &&
	    !g_path_is_absolute(path.c_str()))
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


Message::Message(const std::string& path,  Message::Options opts):
	priv_{std::make_unique<Private>(opts)}
{
	const auto statbuf{get_statbuf(path, opts)};
	if (!statbuf)
		throw statbuf.error();

	priv_->ctime = statbuf->st_ctime;

	init_gmime();
	if (auto msg{MimeMessage::make_from_file(path)}; !msg)
		throw msg.error();
	else
		priv_->mime_msg = std::move(msg.value());

	auto xpath{to_string_opt_gchar(g_canonicalize_filename(path.c_str(), NULL))};
	if (xpath)
		priv_->doc.add(Field::Id::Path, std::move(xpath.value()));

	priv_->doc.add(Field::Id::Size, static_cast<int64_t>(statbuf->st_size));

	// rest of the fields
	fill_document(*priv_);
}

Message::Message(const std::string& text, const std::string& path,
		 Message::Options opts):
	priv_{std::make_unique<Private>(opts)}
{
	if (text.empty())
		throw Error{Error::Code::InvalidArgument, "text must not be empty"};

	if (!path.empty()) {
		auto xpath{to_string_opt_gchar(g_canonicalize_filename(path.c_str(), {}))};
		if (xpath)
			priv_->doc.add(Field::Id::Path, std::move(xpath.value()));
	}

	priv_->ctime = ::time({});

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

Message::Message(Xapian::Document&& doc):
	priv_{std::make_unique<Private>(Message::Options::None, std::move(doc))}
{}


Message::~Message() = default;

const Mu::Document&
Message::document() const
{
	return priv_->doc;
}


unsigned
Message::docid() const
{
	return priv_->doc.xapian_document().get_docid();
}


const Mu::Sexp&
Message::sexp() const
{
	return priv_->doc.sexp();
}

Result<void>
Message::set_maildir(const std::string& maildir)
{
	/* sanity check a little bit */
	if (maildir.empty() ||
	    maildir.at(0) != '/' ||
	    (maildir.size() > 1 && maildir.at(maildir.length()-1) == '/'))
		return Err(Error::Code::Message,
			   "'%s' is not a valid maildir", maildir.c_str());

	const auto path{document().string_value(Field::Id::Path)};
	if (path == maildir || path.find(maildir) == std::string::npos)
		return Err(Error::Code::Message,
			   "'%s' is not a valid maildir for message @ %s",
			   maildir.c_str(), path.c_str());

	priv_->doc.remove(Field::Id::Maildir);
	priv_->doc.add(Field::Id::Maildir, maildir);

	return Ok();
}

void
Message::set_flags(Flags flags)
{
	priv_->doc.remove(Field::Id::Flags);
	priv_->doc.add(flags);
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
		fill_document(*priv_);
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
extract_tags(const MimeMessage& mime_msg)
{
	constexpr std::array<std::pair<const char*, char>, 3> tag_headers = {{
		{"X-Label", ' '}, {"X-Keywords", ','}, {"Keywords", ' '}
	}};

	std::vector<std::string> tags;
	seq_for_each(tag_headers, [&](auto&& item) {
		if (auto&& hdr = mime_msg.header(item.first); hdr) {
			for (auto&& tagval : split(*hdr, item.second)) {
				tagval.erase(0, tagval.find_first_not_of(' '));
				tagval.erase(tagval.find_last_not_of(' ')+1);
				tags.emplace_back(std::move(tagval));
			}
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

static void
append_text(Option<std::string>& str, Option<std::string> app)
{
	if (!str)
		str = app;
	else if (app)
		str.value() += app.value();
}

static void
accumulate_text(const MimePart& part, Message::Private& info,
		const MimeContentType& ctype)
{
	if (!ctype.is_type("text", "*"))
		return; /* not a text type */

	if (part.is_attachment())
		append_text(info.embedded, part.to_string());
	else if (ctype.is_type("text", "plain"))
		append_text(info.body_txt, part.to_string());
	else if (ctype.is_type("text", "html"))
		append_text(info.body_html, part.to_string());
}


static bool /* heuristic */
looks_like_attachment(const MimeObject& parent, const MessagePart& mpart)
{
	if (parent) { /* crypto multipart children are not considered attachments */
		if (const auto parent_ctype{parent.content_type()}; parent_ctype) {
			if (parent_ctype->is_type("multipart", "signed") ||
			    parent_ctype->is_type("multipart", "encrypted"))
				return false;
		}
	}

	return mpart.looks_like_attachment();
}


static void
process_part(const MimeObject& parent, const MimePart& part,
	     Message::Private& info, const MessagePart& mpart)
{
	const auto ctype{part.content_type()};
	if (!ctype)
		return;

	// flag as calendar, if not already
	if (none_of(info.flags & Flags::Calendar) &&
	    ctype->is_type("text", "calendar"))
		info.flags |= Flags::Calendar;

	// flag as attachment, if not already.
	if (none_of(info.flags & Flags::HasAttachment) &&
	    looks_like_attachment(parent, mpart))
		info.flags |= Flags::HasAttachment;

	// if there are text parts, gather.
	accumulate_text(part, info, *ctype);
}


static void
process_message_part(const MimeMessagePart& msg_part,
		     Message::Private& info)
{
	auto submsg{msg_part.get_message()};
	if (!submsg)
		return;

	submsg->for_each([&](auto&& parent, auto&& child_obj) {

		/* XXX: we only handle one level */

		if (!child_obj.is_part())
			return;

		const auto ctype{child_obj.content_type()};
		if (!ctype || !ctype->is_type("text", "*"))
			return;

		append_text(info.embedded, MimePart{child_obj}.to_string());
	});
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
		process_part(parent, obj, info, info.parts.back());
	else if (obj.is_message_part())
		process_message_part(obj, info);
	else if (obj.is_multipart_signed())
		info.flags |= Flags::Signed;
	else if (obj.is_multipart_encrypted()) {
		/* FIXME: An encrypted part might be signed at the same time.
		 *        In that case the signed flag is lost. */
		info.flags |= Flags::Encrypted;
	} else if (obj.is_mime_application_pkcs7_mime()) {
		MimeApplicationPkcs7Mime smime(obj);
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wswitch-enum"
		// CompressedData, CertsOnly, Unknown
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
#pragma GCC diagnostic pop
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

/* many of the doc.add(fiels ....) automatically update the sexp-list as well;
 * however, there are some _extra_ values in the sexp-list that are not
 * based on a field. So we add them here.
 */



static void
doc_add_list_post(Document& doc, const MimeMessage& mime_msg)
{
	/* some mailing lists do not set the reply-to; see pull #1278. So for
	 * those cases, check the List-Post address and use that instead */

	GMatchInfo* minfo;
	GRegex*     rx;
	const auto list_post{mime_msg.header("List-Post")};
	if (!list_post)
		return;

	rx = g_regex_new("<?mailto:([a-z0-9!@#$%&'*+-/=?^_`{|}~]+)>?",
			 G_REGEX_CASELESS, (GRegexMatchFlags)0, {});
	g_return_if_fail(rx);

	Contacts contacts;
	if (g_regex_match(rx, list_post->c_str(), (GRegexMatchFlags)0, &minfo)) {
		auto    address = (char*)g_match_info_fetch(minfo, 1);
		contacts.push_back(Contact(address));
		g_free(address);
	}

	g_match_info_free(minfo);
	g_regex_unref(rx);

	doc.add_extra_contacts(":list-post", contacts);
}

static void
doc_add_reply_to(Document& doc, const MimeMessage& mime_msg)
{
	doc.add_extra_contacts(":reply-to", mime_msg.contacts(Contact::Type::ReplyTo));
}

static void
fill_document(Message::Private& priv)
{
	/* hunt & gather info from message tree */
	Document& doc{priv.doc};
	MimeMessage& mime_msg{priv.mime_msg.value()};

	const auto path{doc.string_value(Field::Id::Path)};
	const auto refs{mime_msg.references()};
	const auto& raw_message_id = mime_msg.message_id();
	const auto message_id = raw_message_id.has_value() && !raw_message_id->empty()
	    ? *raw_message_id
	    : fake_message_id(path);

	process_message(mime_msg, path, priv);

	doc_add_list_post(doc, mime_msg); /* only in sexp */
	doc_add_reply_to(doc, mime_msg);  /* only in sexp */

	field_for_each([&](auto&& field) {
		/* insist on expliclity handling each */
#pragma GCC diagnostic push
#pragma GCC diagnostic error "-Wswitch"
		switch(field.id) {
		case Field::Id::Bcc:
			doc.add(field.id, mime_msg.contacts(Contact::Type::Bcc));
			break;
		case Field::Id::BodyText:
			doc.add(field.id, priv.body_txt);

			break;
		case Field::Id::Cc:
			doc.add(field.id, mime_msg.contacts(Contact::Type::Cc));
			break;
		case Field::Id::Changed:
			doc.add(field.id, priv.ctime);
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
			doc.add(field.id, mime_msg.contacts(Contact::Type::From));
			break;
		case Field::Id::Maildir: /* already */
			break;
		case Field::Id::MailingList:
			doc.add(field.id, priv.mailing_list);
			break;
		case Field::Id::MessageId:
			doc.add(field.id, message_id);
			break;
		case Field::Id::MimeType:
			for (auto&& part: priv.parts)
				doc.add(field.id, part.mime_type());
			break;
		case Field::Id::Path: /* already */
			break;
		case Field::Id::Priority:
			doc.add(get_priority(mime_msg));
			break;
		case Field::Id::References:
			if (!refs.empty())
				doc.add(field.id, refs);
			break;
		case Field::Id::Size: /* already */
			break;
		case Field::Id::Subject:
			doc.add(field.id, mime_msg.subject().map(remove_ctrl));
			break;
		case Field::Id::Tags:
			if (auto&& tags{extract_tags(mime_msg)}; !tags.empty())
				doc.add(field.id, tags);
			break;
		case Field::Id::ThreadId:
			// either the oldest reference, or otherwise the message id
			doc.add(field.id, refs.empty() ? message_id : refs.at(0));
			break;
		case Field::Id::To:
			doc.add(field.id, mime_msg.contacts(Contact::Type::To));
			break;
			/* internal fields */
		case Field::Id::XBodyHtml:
			doc.add(field.id, priv.body_html);
			break;
		/* LCOV_EXCL_START */
		case Field::Id::_count_:
		default:
			break;
		/* LCOV_EXCL_STOP */
		}
#pragma GCC diagnostic pop

	});
}

Option<std::string>
Message::header(const std::string& header_field) const
{
	load_mime_message();
	return priv_->mime_msg->header(header_field);
}

Option<std::string>
Message::body_text() const
{
	load_mime_message();
	return priv_->body_txt;
}

Option<std::string>
Message::body_html() const
{
	load_mime_message();
	return priv_->body_html;
}

Contacts
Message::all_contacts() const
{
	Contacts contacts;

	if (!load_mime_message())
		return contacts; /* empty */

	return priv_->mime_msg->contacts(Contact::Type::None); /* get all types */
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

Result<std::string>
Message::cache_path(Option<size_t> index) const
{
	/* create tmpdir for this message, if needed */
	if (priv_->cache_path.empty()) {
		GError *err{};
		auto tpath{to_string_opt_gchar(g_dir_make_tmp("mu-cache-XXXXXX", &err))};
		if (!tpath)
			return Err(Error::Code::File, &err, "failed to create temp dir");

		priv_->cache_path = std::move(tpath.value());
	}

	if (index) {
		GError *err{};
		auto tpath = format("%s/%zu", priv_->cache_path.c_str(), *index);
		if (g_mkdir(tpath.c_str(), 0700) != 0)
			return Err(Error::Code::File, &err,
				   "failed to create cache dir '%s'; err=%d",
				   tpath.c_str(), errno);
		return Ok(std::move(tpath));
	} else

		return Ok(std::string{priv_->cache_path});
}

// for now this only remove stray '/' at the end
std::string
Message::sanitize_maildir(const std::string& mdir)
{
	if (mdir.size() > 1 && mdir.at(mdir.length()-1) == '/')
		return mdir.substr(0, mdir.length() - 1);
	else
		return mdir;
}

Result<void>
Message::update_after_move(const std::string& new_path,
			   const std::string& new_maildir,
			   Flags new_flags)
{
	if (auto statbuf{get_statbuf(new_path)}; !statbuf)
		return Err(statbuf.error());
	else
		priv_->ctime = statbuf->st_ctime;

	priv_->doc.remove(Field::Id::Path);
	priv_->doc.remove(Field::Id::Changed);

	priv_->doc.add(Field::Id::Path, new_path);
	priv_->doc.add(Field::Id::Changed, priv_->ctime);

	set_flags(new_flags);

	if (const auto res = set_maildir(sanitize_maildir(new_maildir)); !res)
		return res;

	return Ok();
}
