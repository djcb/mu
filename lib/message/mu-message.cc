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
#include "mu-msg-priv.hh"

using namespace Mu;

struct Message::Private {

	Document			doc;
	mutable Option<MimeMessage>	mime_msg;

	Flags                           flags{};
	Option<std::string>		mailing_list;
	std::vector<std::string>        references;
	std::vector<Part>               parts;

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

Message::Message(const std::string& path, const std::string& mdir):
	priv_{std::make_unique<Private>()}
{
	if (!g_path_is_absolute(path.c_str()))
		throw Error(Error::Code::File, "path '%s' is not absolute", path.c_str());

	if (::access(path.c_str(), R_OK) != 0)
		throw Error(Error::Code::File, "file @ '%s' is not readable", path.c_str());

	struct stat statbuf{};
	if (::stat(path.c_str(), &statbuf) < 0)
		throw Error(Error::Code::File, "cannot stat %s: %s", path.c_str(),
			    g_strerror(errno));

	if (!S_ISREG(statbuf.st_mode))
		throw Error(Error::Code::File, "not a regular file: %s", path.c_str());

	init_gmime();
	if (auto msg{MimeMessage::make_from_file(path)}; !msg)
		throw msg.error();
	else
		priv_->mime_msg = std::move(msg.value());

	priv_->doc.add(Field::Id::Path,
		       Mu::from_gchars(g_canonicalize_filename(path.c_str(), NULL)));
	priv_->doc.add(Field::Id::Maildir, mdir);
	priv_->doc.add(Field::Id::Size, static_cast<int64_t>(statbuf.st_size));

	// rest of the fields
	fill_document(*priv_);
}

Message::Message(const std::string& text):
	priv_{std::make_unique<Private>()}
{
	priv_->doc.add(Field::Id::Size, static_cast<int64_t>(text.size()));
	priv_->doc.add(Field::Id::Path, "");

	init_gmime();
	if (auto msg{MimeMessage::make_from_string(text)}; !msg)
		throw msg.error();
	else
		priv_->mime_msg = std::move(msg.value());

	fill_document(*priv_);
}

Message::Message(Message&& msg)	       = default;
Message::Message(Document& doc):
	priv_{std::make_unique<Private>()}
{
	priv_->doc = doc;
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
		if (auto hdr{mime_msg.header(item.first)}; hdr) {
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

	return from_gchars(std::move(res));
}

static bool /* heuristic */
looks_like_attachment(const MimePart& part, const Option<MimeContentType>& ctype)
{
	constexpr std::array<std::pair<const char*, const char*>, 4> att_types = {{
			{"image", "*"},
			{"audio", "*"},
			{"application", "*"},
			{"application", "x-patch"}
		}};

	if (part.is_attachment()) /* explicity set as attachment */
		return true;
	else if (!ctype)
		return false;

	/* we also consider patches, images, audio, and non-pgp-signature
	 * application attachments to be attachments... */
	if (ctype->is_type("*", "pgp-signature"))
		return false; /* don't consider as a signature */

	if (ctype->is_type("text", "*") &&
	    (ctype->is_type("*", "plain") || ctype->is_type("*", "html")))
		return false; /* not a signature */

	const auto it = seq_find_if(att_types, [&](auto&& item){
		return ctype->is_type(item.first, item.second);
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
process_part(const MimePart& part, Message::Private& info)
{
	const auto ctype{part.content_type()};
	if (!ctype)
		return;

	if (looks_like_attachment(part, ctype))
		info.flags |= Flags::HasAttachment;

	// if there are text parts, gather.
	accumulate_text(part, info, *ctype);

	//MimePart mypart(part);
	info.parts.emplace_back(part);
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
	info.flags = Flags::None; //mu_maildir_flags_from_path(path).value_or(Flags::None);

	/* pseudo-flag --> unread means either NEW or NOT SEEN, just
	 * for searching convenience */
	if (any_of(info.flags & Flags::New) || none_of(info.flags & Flags::Seen))
		info.flags |= Flags::Unread;

	// parts
	mime_msg.for_each([&](auto&& parent, auto&& part) {

		if (part.is_part())
			process_part(part, info);

		if (part.is_multipart_signed())
			info.flags |= Flags::Signed;
		else if (part.is_multipart_encrypted()) {
			/* FIXME: An encrypted part might be signed at the same time.
			 *        In that case the signed flag is lost. */
			info.flags |= Flags::Encrypted;
		} else if (part.is_mime_application_pkcs7_mime()) {
			MimeApplicationPkcs7Mime smime(part);
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


static std::string
fake_message_id(const std::string path)
{
	constexpr auto mu_suffix{"@mu.id"};

	if (path.empty())
		return  format("12345@%s", mu_suffix);
	else if (const auto sha256_res{calculate_sha256(path)}; !sha256_res) {
		g_warning("failed to get sha-256: %s", sha256_res.error().what());
		// fallback... not a very good message-id, but should
		// not happen in practice.
		return format("%08x%s", g_str_hash(path.c_str()), mu_suffix);
	} else
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


/*
 *  tests
 */

#ifdef BUILD_TESTS

/*
 * test message 1
 */

static void
test_message_mailing_list()
{
	constexpr const char *test_message_1 =
R"(Return-Path: <sqlite-dev-bounces@sqlite.org>
X-Original-To: xxxx@localhost
Delivered-To: xxxx@localhost
Received: from mindcrime (localhost [127.0.0.1])
  by mail.xxxxsoftware.nl (Postfix) with ESMTP id 32F276963F
  for <xxxx@localhost>; Mon,  4 Aug 2008 21:49:34 +0300 (EEST)
Message-Id: <83B5AF40-DBFA-4578-A043-04C80276E195@sqlabs.net>
From: anon@example.com
To: sqlite-dev@sqlite.org
Mime-Version: 1.0 (Apple Message framework v926)
Date: Mon, 4 Aug 2008 11:40:49 +0200
X-Mailer: Apple Mail (2.926)
Subject: [sqlite-dev] VM optimization inside sqlite3VdbeExec
Precedence: list
Reply-To: sqlite-dev@sqlite.org
List-Id: <sqlite-dev.sqlite.org>
Content-Type: text/plain; charset="us-ascii"
Content-Transfer-Encoding: 7bit
Sender: sqlite-dev-bounces@sqlite.org
Content-Length: 639

Inside sqlite3VdbeExec there is a very big switch statement.
In order to increase performance with few modifications to the
original code, why not use this technique ?
http://docs.freebsd.org/info/gcc/gcc.info.Labels_as_Values.html

With a properly defined "instructions" array, instead of the switch
statement you can use something like:
goto * instructions[pOp->opcode];
)";
	auto message{Message::make_from_string(test_message_1)};
	g_assert_true(!!message);

	g_assert_true(message->bcc().empty());

	g_assert_true(!message->body_html());
	assert_equal(message->body_text().value_or(""),
R"(Inside sqlite3VdbeExec there is a very big switch statement.
In order to increase performance with few modifications to the
original code, why not use this technique ?
http://docs.freebsd.org/info/gcc/gcc.info.Labels_as_Values.html

With a properly defined "instructions" array, instead of the switch
statement you can use something like:
goto * instructions[pOp->opcode];
)");
	g_assert_true(message->cc().empty());
	g_assert_cmpuint(message->date(), ==, 1217842849);
	g_assert_true(message->flags() == (Flags::MailingList | Flags::Unread));

	const auto from{message->from()};
	g_assert_cmpuint(from.size(),==,1);
	assert_equal(from.at(0).name, "");
	assert_equal(from.at(0).email, "anon@example.com");

	assert_equal(message->mailing_list(), "sqlite-dev.sqlite.org");
	assert_equal(message->message_id(),
		     "83B5AF40-DBFA-4578-A043-04C80276E195@sqlabs.net");

	g_assert_true(message->path().empty());
	g_assert_true(message->priority() == Priority::Low);
	g_assert_cmpuint(message->size(),==,::strlen(test_message_1));

	g_assert_true(message->references().empty());

	assert_equal(message->subject(),
		     "[sqlite-dev] VM optimization inside sqlite3VdbeExec");

	const auto to{message->to()};
	g_assert_cmpuint(to.size(),==,1);
	assert_equal(to.at(0).name, "");
	assert_equal(to.at(0).email, "sqlite-dev@sqlite.org");

	assert_equal(message->header("X-Mailer").value_or(""), "Apple Mail (2.926)");

	auto all_contacts{message->all_contacts()};
	g_assert_cmpuint(all_contacts.size(), ==, 4);
	seq_sort(all_contacts, [](auto&& c1, auto&& c2){return c1.email < c2.email; });
	assert_equal(all_contacts[0].email, "anon@example.com");
	assert_equal(all_contacts[1].email, "sqlite-dev-bounces@sqlite.org");
	assert_equal(all_contacts[2].email, "sqlite-dev@sqlite.org");
	assert_equal(all_contacts[3].email, "sqlite-dev@sqlite.org");
}


static void
test_message_attachments(void)
{
	constexpr const char* msg_text =
R"(Return-Path: <foo@example.com>
Received: from pop.gmail.com [256.85.129.309]
	by evergrey with POP3 (fetchmail-6.4.29)
	for <djcb@localhost> (single-drop); Thu, 24 Mar 2022 20:12:40 +0200 (EET)
Sender: "Foo, Example" <foo@example.com>
User-agent: mu4e 1.7.10; emacs 29.0.50
From: "Foo Example" <foo@example.com>
To: bar@example.com
Subject: =?utf-8?B?w6R0dMOkY2htZcOxdHM=?=
Date: Thu, 24 Mar 2022 20:04:39 +0200
Organization: ACME Inc.
Message-ID: <87a6dfw7bg.fsf@example.com>
MIME-Version: 1.0
Content-Type: multipart/mixed; boundary="=-=-="

--=-=-=
Content-Type: text/plain

Hello,
--=-=-=
Content-Type: image/jpeg
Content-Disposition: attachment; filename=file-01.bin
Content-Transfer-Encoding: base64
Content-Description: test file 1

MDAwAQID
--=-=-=
Content-Type: audio/ogg
Content-Disposition: inline; filename=/tmp/file-02.bin
Content-Transfer-Encoding: base64

MDA0BQYH
--=-=-=
Content-Type: text/plain

World!
--=-=-=--
)";

	auto message{Message::make_from_string(msg_text)};
	g_assert_true(!!message);

	g_assert_true(message->bcc().empty());
	g_assert_true(!message->body_html());
	assert_equal(message->body_text().value_or(""),
R"(Hello,World!)");

	g_assert_true(message->cc().empty());
	g_assert_cmpuint(message->date(), ==, 1648145079);
	g_assert_true(message->flags() == (Flags::HasAttachment|Flags::Unread));

	const auto from{message->from()};
	g_assert_cmpuint(from.size(),==,1);
	assert_equal(from.at(0).name, "Foo Example");
	assert_equal(from.at(0).email, "foo@example.com");

	g_assert_true(message->path().empty());
	g_assert_true(message->priority() == Priority::Normal);
	g_assert_cmpuint(message->size(),==,::strlen(msg_text));

	assert_equal(message->subject(), "ättächmeñts");

	g_assert_cmpuint(message->parts().size(),==,4);
	{
		auto&& part{message->parts().at(0)};
		g_assert_false(!!part.raw_filename());
		assert_equal(part.mime_type().value(), "text/plain");
		assert_equal(part.to_string().value(), "Hello,");
	}
	{
		auto&& part{message->parts().at(1)};
		assert_equal(part.raw_filename().value(), "file-01.bin");
		assert_equal(part.mime_type().value(), "image/jpeg");
		// file consist of 6 bytes "000" 0x01,0x02.0x03.
		assert_equal(part.to_string().value(), "000\001\002\003");
	}
	{
		auto&& part{message->parts().at(2)};
		assert_equal(part.raw_filename().value(), "/tmp/file-02.bin");
		assert_equal(part.cooked_filename().value(), "tmp-file-02.bin");
		assert_equal(part.mime_type().value(), "audio/ogg");
		// file consist of the string "004" followed by 0x5,0x6,0x7.
		assert_equal(part.to_string().value(), "004\005\006\007");
	}
	{
		auto&& part{message->parts().at(3)};
		g_assert_false(!!part.raw_filename());
		g_assert_true(!!part.mime_type());
		assert_equal(part.mime_type().value(), "text/plain");
		assert_equal(part.to_string().value(), "World!");
	}
}

int
main(int argc, char* argv[])
{
	g_test_init(&argc, &argv, NULL);

	g_test_add_func("/message/message/mailing-list",
			test_message_mailing_list);
	g_test_add_func("/message/message/attachments",
			test_message_attachments);

	return g_test_run();
}

#endif /*BUILD_TESTS*/
