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
#include "utils/mu-test-utils.hh"
#include "mu-message.hh"
#include "mu-mime-object.hh"
#include <glib.h>
#include <regex>

using namespace Mu;

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
	auto message{Message::make_from_text(
			test_message_1,
			"/home/test/Maildir/inbox/cur/1649279256.107710_1.evergrey:2,S")};
	g_assert_true(!!message);
	assert_equal(message->path(),
		     "/home/test/Maildir/inbox/cur/1649279256.107710_1.evergrey:2,S");
	g_assert_true(message->maildir().empty());

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
	g_assert_true(message->flags() == (Flags::MailingList | Flags::Seen));

	const auto from{message->from()};
	g_assert_cmpuint(from.size(),==,1);
	assert_equal(from.at(0).name, "");
	assert_equal(from.at(0).email, "anon@example.com");

	assert_equal(message->mailing_list(), "sqlite-dev.sqlite.org");
	assert_equal(message->message_id(),
		     "83B5AF40-DBFA-4578-A043-04C80276E195@sqlabs.net");

	g_assert_true(message->priority() == Priority::Low);
	g_assert_cmpuint(message->size(),==,::strlen(test_message_1));

	/* text-based message use time({}) as their changed-time */
	g_assert_cmpuint(::time({}) - message->changed(), >=, 0);
	g_assert_cmpuint(::time({}) - message->changed(), <=, 2);

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
User-agent: mu4e 1.7.11; emacs 29.0.50
From: "Foo Example" <foo@example.com>
To: bar@example.com
Subject: =?utf-8?B?w6R0dMOkY2htZcOxdHM=?=
Date: Thu, 24 Mar 2022 20:04:39 +0200
Organization: ACME Inc.
Message-Id: <3144HPOJ0VC77.3H1XTAG2AMTLH@"@WILSONB.COM>
MIME-Version: 1.0
Content-Type: multipart/mixed; boundary="=-=-="

--=-=-=
Content-Type: text/plain

Hello,
--=-=-=
Content-Type: image/jpeg
Content-Disposition: attachment; filename=file-01.bin
Content-Transfer-Encoding: base64

AAECAw==
--=-=-=
Content-Type: audio/ogg
Content-Disposition: inline; filename=/tmp/file-02.bin
Content-Transfer-Encoding: base64

BAUGBw==
--=-=-=
Content-Type: message/rfc822
Content-Disposition: attachment;
 filename="message.eml"

From: "Fnorb" <fnorb@example.com>
To: Bob <bob@example.com>
Subject: news for you
Date: Mon, 28 Mar 2022 22:53:26 +0300

Attached message!

--=-=-=
Content-Type: text/plain

World!
--=-=-=--
)";

	auto message{Message::make_from_text(msg_text)};
	g_assert_true(!!message);
	g_assert_true(message->has_mime_message());
	g_assert_true(message->path().empty());

	g_assert_true(message->bcc().empty());
	g_assert_true(!message->body_html());
	assert_equal(message->body_text().value_or(""), R"(Hello,World!)");

	g_assert_true(message->cc().empty());
	g_assert_cmpuint(message->date(), ==, 1648145079);
	/* no Flags::Unread since it's a message without path */
	g_assert_true(message->flags() == (Flags::HasAttachment));

	const auto from{message->from()};
	g_assert_cmpuint(from.size(),==,1);
	assert_equal(from.at(0).name, "Foo Example");
	assert_equal(from.at(0).email, "foo@example.com");

	// problem case: https://github.com/djcb/mu/issues/2232o
	assert_equal(message->message_id(),
		"3144HPOJ0VC77.3H1XTAG2AMTLH@\"@WILSONB.COM");

	g_assert_true(message->path().empty());
	g_assert_true(message->priority() == Priority::Normal);
	g_assert_cmpuint(message->size(),==,::strlen(msg_text));

	/* text-based message use time({}) as their changed-time */
	g_assert_cmpuint(::time({}) - message->changed(), >=, 0);
	g_assert_cmpuint(::time({}) - message->changed(), <=, 2);

	assert_equal(message->subject(), "ättächmeñts");

	const auto cache_path{message->cache_path()};
	g_assert_true(!!cache_path);

	g_assert_cmpuint(message->parts().size(),==,5);
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
		// file consists of 4 bytes 0...3
		g_assert_cmpuint(part.to_string()->at(0), ==, 0);
		g_assert_cmpuint(part.to_string()->at(1), ==, 1);
		g_assert_cmpuint(part.to_string()->at(2), ==, 2);
		g_assert_cmpuint(part.to_string()->at(3), ==, 3);
	}
	{
		auto&& part{message->parts().at(2)};
		assert_equal(part.raw_filename().value(), "/tmp/file-02.bin");
		assert_equal(part.cooked_filename().value(), "file-02.bin");
		assert_equal(part.mime_type().value(), "audio/ogg");
		// file consistso of 4 bytes 4..7
		assert_equal(part.to_string().value(), "\004\005\006\007");
		const auto fpath{*cache_path + part.cooked_filename().value()};
		const auto res = part.to_file(fpath, true);

		g_assert_cmpuint(*res,==,4);
		g_assert_cmpuint(::access(fpath.c_str(), R_OK), ==, 0);
	}

	{
		auto&& part{message->parts().at(3)};
		g_assert_true(part.mime_type() == "message/rfc822");

		const auto fname{*cache_path + "/msgpart"};
		g_assert_cmpuint(part.to_file(fname, true).value_or(123), ==, 139);
		g_assert_true(::access(fname.c_str(), F_OK) == 0);
	}

	{
		auto&& part{message->parts().at(4)};
		g_assert_false(!!part.raw_filename());
		g_assert_true(!!part.mime_type());
		assert_equal(part.mime_type().value(), "text/plain");
		assert_equal(part.to_string().value(), "World!");
	}
}


/*
 * some test keys.
 */

constexpr std::string_view  pub_key =
R"(-----BEGIN PGP PUBLIC KEY BLOCK-----

mDMEYlbaNhYJKwYBBAHaRw8BAQdAEgxZnlN3mIwqV89zchjFlEby8OgrbrkT+yRN
hQhc+A+0LU11IFRlc3QgKG11IHRlc3Rpbmcga2V5KSA8bXVAZGpjYnNvZnR3YXJl
Lm5sPoiUBBMWCgA8FiEE/HZRT+2bPjARz29Cw7FsU49t3vAFAmJW2jYCGwMFCwkI
BwIDIgIBBhUKCQgLAgQWAgMBAh4HAheAAAoJEMOxbFOPbd7wJ2kBAIGmUDWYEPtn
qYTwhZIdZtTa4KJ3UdtTqey9AnxJ9mzAAQDRJOoVppj5wW2xRhgYP+ysN2iBUYGE
MhahOcNgxodbCLg4BGJW2jYSCisGAQQBl1UBBQEBB0D4Sp+GTVre7Cx5a8D3SwLJ
/bRAVGDwqI7PL9B/cMmCTwMBCAeIeAQYFgoAIBYhBPx2UU/tmz4wEc9vQsOxbFOP
bd7wBQJiVto2AhsMAAoJEMOxbFOPbd7w1tYA+wdfYCcwOP0QoNZZz2Yk12YkDk2R
FsRrZZpb0GKC/a2VAP4qFceeSegcUCBTQaoeFE9vq9XiUVOO98QI8r9C8QwvBw==
=jM/g
-----END PGP PUBLIC KEY BLOCK-----
)";

constexpr std::string_view priv_key = // "test1234"
R"(-----BEGIN PGP PRIVATE KEY BLOCK-----

lIYEYlbaNhYJKwYBBAHaRw8BAQdAEgxZnlN3mIwqV89zchjFlEby8OgrbrkT+yRN
hQhc+A/+BwMCz6T2uBpk6a7/rXyE7C1bRbGjP6YSFcyRFz8VRV3Xlm7z6rdbdKZr
8R15AtLvXA4DOK5GiZRB2VbIxi8B9CtZ9qQx6YbQPkAmRzISGAjECrQtTXUgVGVz
dCAobXUgdGVzdGluZyBrZXkpIDxtdUBkamNic29mdHdhcmUubmw+iJQEExYKADwW
IQT8dlFP7Zs+MBHPb0LDsWxTj23e8AUCYlbaNgIbAwULCQgHAgMiAgEGFQoJCAsC
BBYCAwECHgcCF4AACgkQw7FsU49t3vAnaQEAgaZQNZgQ+2ephPCFkh1m1NrgondR
21Op7L0CfEn2bMABANEk6hWmmPnBbbFGGBg/7Kw3aIFRgYQyFqE5w2DGh1sInIsE
YlbaNhIKKwYBBAGXVQEFAQEHQPhKn4ZNWt7sLHlrwPdLAsn9tEBUYPCojs8v0H9w
yYJPAwEIB/4HAwI9MZDWcsoiJ/9oV5DRiAedeo3Ta/1M+aKfeNV36Ch1VGLwQF3E
V77qIrJlsT8CwOZHWUksUBENvG3ak3vd84awHHaHoTmoFwtISfvQrFK0iHgEGBYK
ACAWIQT8dlFP7Zs+MBHPb0LDsWxTj23e8AUCYlbaNgIbDAAKCRDDsWxTj23e8NbW
APsHX2AnMDj9EKDWWc9mJNdmJA5NkRbEa2WaW9Bigv2tlQD+KhXHnknoHFAgU0Gq
HhRPb6vV4lFTjvfECPK/QvEMLwc=
=w1Nc
-----END PGP PRIVATE KEY BLOCK-----
)";


static void
test_message_signed(void)
{
	constexpr const char *msgtext =
R"(Return-Path: <diggler@gmail.com>
From: Mu Test <mu@djcbsoftware.nl>
To: Mu Test <mu@djcbsoftware.nl>
Subject: boo
Date: Wed, 13 Apr 2022 17:19:08 +0300
Message-ID: <878rs9ysin.fsf@djcbsoftware.nl>
MIME-Version: 1.0
Content-Type: multipart/signed; boundary="=-=-=";
	micalg=pgp-sha512; protocol="application/pgp-signature"

--=-=-=
Content-Type: text/plain

Sapperdeflap

--=-=-=
Content-Type: application/pgp-signature; name="signature.asc"

-----BEGIN PGP SIGNATURE-----

iIkEARYKADEWIQT8dlFP7Zs+MBHPb0LDsWxTj23e8AUCYlbcLhMcbXVAZGpjYnNv
ZnR3YXJlLm5sAAoJEMOxbFOPbd7waIkA/jK1oY7OL8vrDoubNYxamy8HHmwtvO01
Q46aYjxe0As6AP90bcAZ3dcn5RcTJaM0UhZssguawZ+tnriD3+5DPkMMCg==
=e32+
-----END PGP SIGNATURE-----
--=-=-=--
)";
	TempDir tempdir;
	auto ctx{MimeCryptoContext::make_gpg(tempdir.path())};
	g_assert_true(!!ctx);

	auto stream{MimeStream::make_mem()};
	stream.write(pub_key.data(), pub_key.size());
	stream.reset();

	auto imported = ctx->import_keys(stream);
	g_assert_cmpuint(*imported, ==, 1);

	auto message{Message::make_from_text(
			msgtext,
			"/home/test/Maildir/inbox/cur/1649279777.107710_1.mindcrime:2,RS")};
	g_assert_true(!!message);

	g_assert_true(message->bcc().empty());
	assert_equal(message->body_text().value_or(""), "Sapperdeflap\n");
	g_assert_true(message->flags() == (Flags::Signed|Flags::Seen|Flags::Replied));

	size_t n{};
	for (auto&& part: message->parts()) {
		if (!part.is_signed())
			continue;

		const auto& mobj{part.mime_object()};
		if (!mobj.is_multipart_signed())
			continue;

		const auto mpart{MimeMultipartSigned(mobj)};
		const auto sigs{mpart.verify(*ctx)};
		if (!sigs)
			mu_warning("{}", sigs.error().what());

		g_assert_true(!!sigs);
		g_assert_cmpuint(sigs->size(), ==, 1);
		++n;
	}

	g_assert_cmpuint(n, ==, 1);
}


static void
test_message_signed_encrypted(void)
{
	constexpr const char *msgtext =
R"(From: "Mu Test" <mu@djcbsoftware.nl>
To: mu@djcbsoftware.nl
Subject: encrypted and signed
Date: Wed, 13 Apr 2022 17:32:30 +0300
Message-ID: <87lew9xddt.fsf@djcbsoftware.nl>
MIME-Version: 1.0
Content-Type: multipart/encrypted; boundary="=-=-=";
	protocol="application/pgp-encrypted"

--=-=-=
Content-Type: application/pgp-encrypted

Version: 1

--=-=-=
Content-Type: application/octet-stream

-----BEGIN PGP MESSAGE-----

hF4DeEerj6WhdZASAQdAKdZwmugAlQA8c06Q5iQw4rwSADgfEWBTWlI6tDw7hEAw
0qSSeeQbA802qjG5TesaDVbFoPp1gOESt67HkJBABj9niwZLnjbzVRXKFoPTYabu
1MBWAQkCEO6kS0N73XQeJ9+nDkUacRX6sSgVM0j+nRdCGcrCQ8MOfLd9KUUBxpXy
r/rIBMpZGOIpKJnoZ2x75VsQIp/ADHLe9zzXVe0tkahXJqvLo26w3gn4NSEIEDp6
4T/zMZImqGrENaixNmRiRSAnwPkLt95qJGOIqYhuW3X6hMRZyU4zDNwkAvnK+2Fv
Wjd+EmiFzh5tvCmPOSj556YFMV7UpFWO9VznXX/T5+f4i+95Lsm9Uotv/SiNtNQG
DPU3wiL347SzmPFXckknjlzSzDL1XbdbHdmoJs0uNnbaZxRwhkuTYbLHdpBZrBgR
C0bdoCx44QVU8HaZ2x91h3GoM/0q5bqM/rvCauwbokiJgAUrznecNPY=
=Ado7
-----END PGP MESSAGE-----
--=-=-=--
)";
	TempDir tempdir;
	auto ctx{MimeCryptoContext::make_gpg(tempdir.path())};
	g_assert_true(!!ctx);

	/// test1234
	// ctx->set_request_password([](const MimeCryptoContext& ctx,
	//			     const std::string& user_id,
	//			     const std::string& prompt,
	//			     bool reprompt,
	//			     MimeStream& response)->Result<void> {
	//				  return Err(Error::Code::Internal, "boo");
	//				  //return Ok();
	//			  });

	{
		auto stream{MimeStream::make_mem()};
		stream.write(priv_key.data(), priv_key.size());
		stream.write(pub_key.data(), pub_key.size());
		stream.reset();


		g_assert_cmpint(ctx->import_keys(stream).value_or(-1),==,1);
	}

	auto message{Message::make_from_text(
			msgtext,
			"/home/test/Maildir/inbox/cur/1649279888.107710_1.mindcrime:2,FS")};
	g_assert_true(!!message);
	g_assert_true(message->flags() == (Flags::Encrypted|Flags::Seen|Flags::Flagged));

	size_t n{};
	for (auto&& part: message->parts()) {

		if (!part.is_encrypted())
			continue;

		g_assert_false(!!part.content_description());
		g_assert_false(part.is_attachment());
		g_assert_cmpuint(part.size(),==,0);

		const auto& mobj{part.mime_object()};
		if (!mobj.is_multipart_encrypted())
			continue;

		/* FIXME: make this work without user having to
		 * type password */

		// const auto mpart{MimeMultipartEncrypted(mobj)};
		// const auto decres = mpart.decrypt(*ctx);
		// assert_valid_result(decres);

		++n;
	}

	g_assert_cmpuint(n, ==, 1);
}


static void
test_message_multipart_mixed_rfc822(void)
{
	constexpr const char *msgtext =
R"(Content-Type: multipart/mixed;
	boundary="Multipart_Tue_Sep__2_15:42:35_2014-1"

--Multipart_Tue_Sep__2_15:42:35_2014-1
Content-Type: message/rfc822
)";
	auto message{Message::make_from_text(msgtext)};
	g_assert_true(!!message);
	//g_assert_true(message->sexp().empty());
}


static void
test_message_detect_attachment(void)
{
	constexpr const char *msgtext =
R"(From: "DUCK, Donald" <donald@example.com>
Date: Tue, 3 May 2022 10:26:26 +0300
Message-ID: <SADKLAJCLKDJLAS-xheQjE__+hS-3tff=pTYpMUyGiJwNGF_DA@mail.gmail.com>
Subject: =?Windows-1252?Q?Purkuty=F6urakka?=
To: Hello <moika@example.com>
Cc: =?iso-8859-1?q?M=FCller=2C?= Mickey <Mickey.Mueller@example.com>
Content-Type: multipart/mixed; boundary="000000000000e687ed05de166d71"

--000000000000e687ed05de166d71
Content-Type: multipart/alternative; boundary="000000000000e687eb05de166d6f"

--000000000000e687eb05de166d6f
Content-Type: text/plain; charset="UTF-8"
Content-Transfer-Encoding: quoted-printable

fyi

---------- Forwarded message ---------
From: Fooish Bar <foobar@example.com>
Date: Tue, 3 May 2022 at 08:59
Subject: Ty=C3=B6t
To: "DUCK, Donald" <donald@example.com>

Moi,

--

--000000000000e687eb05de166d6f
Content-Type: text/html; charset="UTF-8"
Content-Transfer-Encoding: quoted-printable

abc

--000000000000e687eb05de166d6f--
--000000000000e687ed05de166d71
Content-Type: application/pdf;
	name="test1.pdf"
Content-Disposition: attachment;
	filename="test2.pdf"
Content-Transfer-Encoding: base64
Content-ID: <18088cfd4bc5517c6321>
X-Attachment-Id: 18088cfd4bc5517c6321

JVBERi0xLjcKJeLjz9MKNyAwIG9iago8PCAvVHlwZSAvUGFnZSAvUGFyZW50IDEgMCBSIC9MYXN0
TW9kaWZpZWQgKEQ6MjAyMjA1MDMwODU3MzYrMDMnMDAnKSAvUmVzb3VyY2VzIDIgMCBSIC9NZWRp
cmVmCjM1NjE4CiUlRU9GCg==
--000000000000e687ed05de166d71--
)";
	auto message{Message::make_from_text(msgtext)};
	g_assert_true(!!message);

	g_assert_true(message->path().empty());

	/* https://groups.google.com/g/mu-discuss/c/kCtrlxMXBjo */
	g_assert_cmpuint(message->cc().size(),==, 1);
	assert_equal(message->cc().at(0).email, "Mickey.Mueller@example.com");
	assert_equal(message->cc().at(0).name, "Müller, Mickey");
	assert_equal(message->cc().at(0).display_name(), "\"Müller, Mickey\" <Mickey.Mueller@example.com>");

	g_assert_true(message->bcc().empty());
	assert_equal(message->subject(), "Purkutyöurakka");
	assert_equal(message->body_html().value_or(""), "abc\n");
	assert_equal(message->body_text().value_or(""),
		     R"(fyi

---------- Forwarded message ---------
From: Fooish Bar <foobar@example.com>
Date: Tue, 3 May 2022 at 08:59
Subject: Työt
To: "DUCK, Donald" <donald@example.com>

Moi,

--
)");
	g_assert_cmpuint(message->date(), ==, 1651562786);
	g_assert_true(message->flags() == (Flags::HasAttachment));

	g_assert_cmpuint(message->parts().size(), ==, 3);

	for (auto&& part: message->parts())
		g_info("%s %s",
		       part.is_attachment() ? "yes" : "no",
		       part.mime_type().value_or("boo").c_str());
}


static void
test_message_calendar(void)
{
	constexpr const char *msgtext =
R"(MIME-Version: 1.0
From: William <william@example.com>
To: Billy <billy@example.com>
Date: Thu, 9 Jan 2014 11:09:34 +0100
Subject: Invitation: HELLO, @ Thu 9 Jan 2014 08:30 - 09:30
 (william@example.com)
Thread-Topic: Invitation: HELLO, @ Thu 9 Jan 2014 08:30 - 09:30
 (william@example.com)
Thread-Index: Ac8NIuske7OtG01VRpukb/bHE7SVHg==
Message-ID: <001a11c3440066ee0b04ef86cea8@google.com>
Accept-Language: en-US
Content-Language: en-US
X-MS-Exchange-Organization-AuthAs: Anonymous
X-MS-Has-Attach: yes
Content-Type: multipart/mixed;
	boundary="_004_001a11c3440066ee0b04ef86cea8googlecom_"

--_004_001a11c3440066ee0b04ef86cea8googlecom_
Content-Type: multipart/alternative;
	boundary="_002_001a11c3440066ee0b04ef86cea8googlecom_"

--_002_001a11c3440066ee0b04ef86cea8googlecom_
Content-Type: text/html; charset="utf-8"
Content-Transfer-Encoding: base64

PGh0bWw+DQo8aGVhZD4NCjxtZXRhIGh0dHAtZXF1aXY9IkNvbnRlbnQtVHlwZSIgY29udGVudD0i
dGV4dC9odG1sOyBjaGFyc2V0PXV0Zi04Ij4NCjxtZXRhIG5hbWU9IkdlbmVyYXRvciIgY29udGVu
dD0iTWljcm9zb2Z0IEV4Y2hhbmdlIFNlcnZlciI+DQo8IS0tIGNvbnZlcnRlZCBmcm9tIHJ0ZiAt
LT4NCjxzdHlsZT48IS0tIC5FbWFpbFF1b3RlIHsgbWFyZ2luLWxlZnQ6IDFwdDsgcGFkZGluZy1s
ZWZ0OiA0cHQ7IGJvcmRlci1sZWZ0OiAjODAwMDAwIDJweCBzb2xpZDsgfSAtLT48L3N0eWxlPg0K
PC9oZWFkPg0KPGJvZHk+DQo8Zm9udCBmYWNlPSJUaW1lcyBOZXcgUm9tYW4iIHNpemU9IjMiPjxh
IG5hbWU9IkJNX0JFR0lOIj48L2E+DQo8dGFibGUgYm9yZGVyPSIxIiB3aWR0aD0iNzM0IiBzdHls
ZT0iYm9yZGVyOjEgc29saWQ7IGJvcmRlci1jb2xsYXBzZTpjb2xsYXBzZTsgbWFyZ2luLWxlZnQ6
IDJwdDsgIj4NCjx0cj4NCjx0ZD48Zm9udCBzaXplPSIxIj48YSBocmVmPSJodHRwczovL3d3dy5n
b29nbGUuY29tL2NhbGVuZGFyL2V2ZW50P2FjdGlvbj1WSUVXJmFtcDtlaWQ9YzNOemNXUXhjRGxs
Ym1VeU0ySnZNbWsyYjNOeU56ZG5jRzhnWkdwallrQmthbU5pYzI5bWRIZGhjbVV1Ym13JmFtcDt0
b2s9TWpZamQybHNiR2xoYlhOZlpESXdRR2RzYjJKdmJXRnBiQzVqYjIxak16YzJZVGhrWW1Ga016
QTJaRFV3TldVMlltWXhOamRqTm1ZMVlUVXhObUpqTWpFNU4yWTMmYW1wO2N0ej1BbWVyaWNhL1Nh
b19QYXVsbyZhbXA7aGw9ZW5fR0IiPjxmb250IGNvbG9yPSIjMjIwMENDIj48dT5tb3JlDQpkZXRh
aWxzIMK7PC91PjwvZm9udD48L2E+PGJyPg0KDQo8ZGl2IHN0eWxlPSJtYXJnaW4tYm90dG9tOiAx
NHB0OyAiPjxmb250IGZhY2U9IkFyaWFsLCBzYW5zLXNlcmlmIiBzaXplPSIyIiBjb2xvcj0iIzIy
MjIyMiI+PGI+SEVMTE8sPC9iPjwvZm9udD48L2Rpdj4NCjxkaXY+PGZvbnQgc2l6ZT0iMSIgY29s
b3I9IiMyMjIyMjIiPjxicj4NCg0KSSBBTSBERVNNT05EIFdJTExJQU1TIEFORCBNWSBMSVRUTEUg
U0lTVEVSIElTIEdMT1JJQSwgT1VSIEZBVEhFUiBPV05TIEEgTElNSVRFRCBPRiBDT0NPQSBBTkQg
R09MRCBCVVNJTkVTUyBJTiBSRVBVQkxJUVVFIERVIENPTkdPLiBBRlRFUiBISVMgVFJJUCBUTyBD
T1RFIERJVk9JUkUgVE8gTkVHT1RJQVRFIE9OIENPQ09BIEFORCBHT0xEIEJVU0lORVNTIEhFIFdB
TlRFRCBUTyBJTlZFU1QgSU4gQUJST0FELiA8L2ZvbnQ+PC9kaXY+DQo8ZGl2IHN0eWxlPSJtYXJn
aW4tdG9wOiAxNHB0OyBtYXJnaW4tYm90dG9tOiAxNHB0OyAiPjxmb250IHNpemU9IjMiPk9ORSBX
RUVLIEhFIENBTUUgQkFDSyBGUk9NIEhJUyBUUklQIFRPIEFCSURKQU4gSEUgSEFEIEEgTU9UT1Ig
QUNDSURFTlQgV0lUSCBPVVIgTU9USEVSIFdISUNIIE9VUiBNT1RIRVIgRElFRCBJTlNUQU5UTFkg
QlVUIE9VUiBGQVRIRVIgRElFRCBBRlRFUiBGSVZFIERBWVMgSU4gQSBQUklWQVRFIEhPU1BJVEFM
IElOIE9VUiBDT1VOVFJZLg0KSVQgV0FTIExJS0UgT1VSIEZBVEhFUiBLTkVXIEhFIFdBUyBHT0lO
RyBUTyBESUUgTUFZIEhJUyBHRU5UTEUgU09VTCBSRVNUIElOIFBSRUZFQ1QgUEVBQ0UuIDwvZm9u
dD48L2Rpdj4NCjxkaXYgc3R5bGU9Im1hcmdpbi10b3A6IDE0cHQ7IG1hcmdpbi1ib3R0b206IDE0
cHQ7ICI+PGZvbnQgc2l6ZT0iMyI+SEUgRElTQ0xPU0VEIFRPIE1FIEFTIFRIRSBPTkxZIFNPTiBU
SEFUIEhFIERFUE9TSVRFRCBUSEUgU1VNIE9GIChVU0QgJCAxMCw1MDAsMDAwKSBJTlRPIEEgQkFO
SyBJTiBBQklESkFOIFRIQVQgVEhFIE1PTkVZIFdBUyBNRUFOVCBGT1IgSElTIENPQ09BIEFORCBH
T0xEIEJVU0lORVNTIEhFIFdBTlRFRCBUTyBFU1RBQkxJU0ggSU4NCkFCUk9BRC5XRSBBUkUgU09M
SUNJVElORyBGT1IgWU9VUiBIRUxQIFRPIFRSQU5TRkVSIFRISVMgTU9ORVkgSU5UTyBZT1VSIEFD
Q09VTlQgSU4gWU9VUiBDT1VOVFJZIEZPUiBPVVIgSU5WRVNUTUVOVC4gPC9mb250PjwvZGl2Pg0K
PGRpdiBzdHlsZT0ibWFyZ2luLXRvcDogMTRwdDsgbWFyZ2luLWJvdHRvbTogMTRwdDsgIj48Zm9u
dCBzaXplPSIzIj5QTEVBU0UgRk9SIFNFQ1VSSVRZIFJFQVNPTlMsSSBBRFZJQ0UgWU9VIFJFUExZ
IFVTIFRIUk9VR0ggT1VSIFBSSVZBVEUgRU1BSUw6IDxhIGhyZWY9Im1haWx0bzp3aWxsaWFtc2Rl
c21vbmQxMDdAeWFob28uY29tLnZuIj48Zm9udCBjb2xvcj0iIzAwMDBGRiI+PHU+d2lsbGlhbXNk
ZXNtb25kMTA3QHlhaG9vLmNvbS52bjwvdT48L2ZvbnQ+PC9hPg0KRk9SIE1PUkUgREVUQUlMUy4g
PC9mb250PjwvZGl2Pg0KPGRpdiBzdHlsZT0ibWFyZ2luLXRvcDogMTRwdDsgbWFyZ2luLWJvdHRv
bTogMTRwdDsgIj48Zm9udCBzaXplPSIzIj5SRUdBUkRTLiA8L2ZvbnQ+PC9kaXY+DQo8ZGl2IHN0
eWxlPSJtYXJnaW4tdG9wOiAxNHB0OyBtYXJnaW4tYm90dG9tOiAxNHB0OyAiPjxmb250IHNpemU9
IjMiPkRFU01PTkQgL0dMT1JJQSBXSUxMSUFNUy48L2ZvbnQ+PC9kaXY+DQo8ZGl2Pjxmb250IHNp
emU9IjMiIGNvbG9yPSIjMjIyMjIyIj4mbmJzcDs8L2ZvbnQ+PC9kaXY+DQo8ZGl2Pjxmb250IHNp
emU9IjMiIGNvbG9yPSIjMjIyMjIyIj4mbmJzcDs8L2ZvbnQ+PC9kaXY+DQo8ZGl2Pjxmb250IHNp
emU9IjMiIGNvbG9yPSIjMjIyMjIyIj4mbmJzcDs8L2ZvbnQ+PC9kaXY+DQo8ZGl2Pjxmb250IHNp
emU9IjMiIGNvbG9yPSIjMjIyMjIyIj4mbmJzcDs8L2ZvbnQ+PC9kaXY+DQo8dGFibGUgYm9yZGVy
PSIxIiB3aWR0aD0iNzM0IiBzdHlsZT0iYm9yZGVyOjEgc29saWQ7IGJvcmRlci1jb2xsYXBzZTpj
b2xsYXBzZTsgbWFyZ2luLWxlZnQ6IDJwdDsgIj4NCjxjb2wgd2lkdGg9IjM2NSI+DQo8Y29sIHdp
ZHRoPSIzNjkiPg0KPHRyPg0KPHRkPjxmb250IHNpemU9IjMiPjxpPldoZW48L2k+PC9mb250Pjwv
dGQ+DQo8dGQ+PGZvbnQgZmFjZT0iQXJpYWwsIHNhbnMtc2VyaWYiIHNpemU9IjEiIGNvbG9yPSIj
MjIyMjIyIj5UaHUgOSBKYW4gMjAxNCAwODozMCDigJMgMDk6MzAgPGZvbnQgY29sb3I9IiM4ODg4
ODgiPlNhbyBQYXVsbzwvZm9udD48L2ZvbnQ+PC90ZD4NCjwvdHI+DQo8dHI+DQo8dGQ+PGZvbnQg
c2l6ZT0iMyI+PGk+Q2FsZW5kYXI8L2k+PC9mb250PjwvdGQ+DQo8dGQ+PGZvbnQgZmFjZT0iQXJp
YWwsIHNhbnMtc2VyaWYiIHNpemU9IjEiIGNvbG9yPSIjMjIyMjIyIj53aWxsaWFtc19kMjBAZ2xv
Ym9tYWlsLmNvbTwvZm9udD48L3RkPg0KPC90cj4NCjx0cj4NCjx0ZD48Zm9udCBzaXplPSIzIj48
aT5XaG88L2k+PC9mb250PjwvdGQ+DQo8dGQ+PGZvbnQgZmFjZT0iQXJpYWwsIHNhbnMtc2VyaWYi
IHNpemU9IjEiIGNvbG9yPSIjMjIyMjIyIj4oR3Vlc3QgbGlzdCBoYXMgYmVlbiBoaWRkZW4gYXQg
b3JnYW5pc2VyJ3MgcmVxdWVzdCk8L2ZvbnQ+PC90ZD4NCjwvdHI+DQo8L3RhYmxlPg0KPGRpdiBz
dHlsZT0ibWFyZ2luLWJvdHRvbTogMTRwdDsgIj48Zm9udCBzaXplPSIxIiBjb2xvcj0iIzg4ODg4
OCI+R29pbmc/Jm5ic3A7Jm5ic3A7IDxhIGhyZWY9Imh0dHBzOi8vd3d3Lmdvb2dsZS5jb20vY2Fs
ZW5kYXIvZXZlbnQ/YWN0aW9uPVJFU1BPTkQmYW1wO2VpZD1jM056Y1dReGNEbGxibVV5TTJKdk1t
azJiM055TnpkbmNHOGdaR3BqWWtCa2FtTmljMjltZEhkaGNtVXVibXcmYW1wO3JzdD0xJmFtcDt0
b2s9TWpZamQybHNiR2xoYlhOZlpESXdRR2RzYjJKdmJXRnBiQzVqYjIxak16YzJZVGhrWW1Ga016
QTJaRFV3TldVMlltWXhOamRqTm1ZMVlUVXhObUpqTWpFNU4yWTMmYW1wO2N0ej1BbWVyaWNhL1Nh
b19QYXVsbyZhbXA7aGw9ZW5fR0IiPjxmb250IGNvbG9yPSIjMjIwMENDIj48dT48Yj5ZZXM8L2I+
PC91PjwvZm9udD48L2E+PGZvbnQgY29sb3I9IiMyMjIyMjIiPjxiPg0KLSA8L2I+PC9mb250Pjxh
IGhyZWY9Imh0dHBzOi8vd3d3Lmdvb2dsZS5jb20vY2FsZW5kYXIvZXZlbnQ/YWN0aW9uPVJFU1BP
TkQmYW1wO2VpZD1jM056Y1dReGNEbGxibVV5TTJKdk1tazJiM055TnpkbmNHOGdaR3BqWWtCa2Ft
TmljMjltZEhkaGNtVXVibXcmYW1wO3JzdD0zJmFtcDt0b2s9TWpZamQybHNiR2xoYlhOZlpESXdR
R2RzYjJKdmJXRnBiQzVqYjIxak16YzJZVGhrWW1Ga016QTJaRFV3TldVMlltWXhOamRqTm1ZMVlU
VXhObUpqTWpFNU4yWTMmYW1wO2N0ej1BbWVyaWNhL1Nhb19QYXVsbyZhbXA7aGw9ZW5fR0IiPjxm
b250IGNvbG9yPSIjMjIwMENDIj48dT48Yj5NYXliZTwvYj48L3U+PC9mb250PjwvYT48Zm9udCBj
b2xvcj0iIzIyMjIyMiI+PGI+DQotIDwvYj48L2ZvbnQ+PGEgaHJlZj0iaHR0cHM6Ly93d3cuZ29v
Z2xlLmNvbS9jYWxlbmRhci9ldmVudD9hY3Rpb249UkVTUE9ORCZhbXA7ZWlkPWMzTnpjV1F4Y0Rs
bGJtVXlNMkp2TW1rMmIzTnlOemRuY0c4Z1pHcGpZa0JrYW1OaWMyOW1kSGRoY21VdWJtdyZhbXA7
cnN0PTImYW1wO3Rvaz1NallqZDJsc2JHbGhiWE5mWkRJd1FHZHNiMkp2YldGcGJDNWpiMjFqTXpj
MllUaGtZbUZrTXpBMlpEVXdOV1UyWW1ZeE5qZGpObVkxWVRVeE5tSmpNakU1TjJZMyZhbXA7Y3R6
PUFtZXJpY2EvU2FvX1BhdWxvJmFtcDtobD1lbl9HQiI+PGZvbnQgY29sb3I9IiMyMjAwQ0MiPjx1
PjxiPk5vPC9iPjwvdT48L2ZvbnQ+PC9hPjxmb250IGNvbG9yPSIjMjIyMjIyIj4mbmJzcDsmbmJz
cDsmbmJzcDsNCjwvZm9udD48YSBocmVmPSJodHRwczovL3d3dy5nb29nbGUuY29tL2NhbGVuZGFy
L2V2ZW50P2FjdGlvbj1WSUVXJmFtcDtlaWQ9YzNOemNXUXhjRGxsYm1VeU0ySnZNbWsyYjNOeU56
ZG5jRzhnWkdwallrQmthbU5pYzI5bWRIZGhjbVV1Ym13JmFtcDt0b2s9TWpZamQybHNiR2xoYlhO
ZlpESXdRR2RzYjJKdmJXRnBiQzVqYjIxak16YzJZVGhrWW1Ga016QTJaRFV3TldVMlltWXhOamRq
Tm1ZMVlUVXhObUpqTWpFNU4yWTMmYW1wO2N0ej1BbWVyaWNhL1Nhb19QYXVsbyZhbXA7aGw9ZW5f
R0IiPjxmb250IGNvbG9yPSIjMjIwMENDIj48dT5tb3JlDQpvcHRpb25zIMK7PC91PjwvZm9udD48
L2E+PC9mb250PjwvZGl2Pg0KPC9mb250PjwvdGQ+DQo8L3RyPg0KPHRyPg0KPHRkIHN0eWxlPSJi
YWNrZ3JvdW5kLWNvbG9yOiAjRjZGNkY2OyAiPjxmb250IHNpemU9IjMiPkludml0YXRpb24gZnJv
bSA8YSBocmVmPSJodHRwczovL3d3dy5nb29nbGUuY29tL2NhbGVuZGFyLyI+PGZvbnQgY29sb3I9
IiMwMDAwRkYiPjx1Pkdvb2dsZSBDYWxlbmRhcjwvdT48L2ZvbnQ+PC9hPg0KPGRpdiBzdHlsZT0i
bWFyZ2luLXRvcDogMTRwdDsgbWFyZ2luLWJvdHRvbTogMTRwdDsgIj48Zm9udCBzaXplPSIzIj5Z
b3UgYXJlIHJlY2VpdmluZyB0aGlzIGNvdXJ0ZXN5IGVtYWlsIGF0IHRoZSBhY2NvdW50IGRqY2JA
ZGpjYnNvZnR3YXJlLm5sIGJlY2F1c2UgeW91IGFyZSBhbiBhdHRlbmRlZSBvZiB0aGlzIGV2ZW50
LjwvZm9udD48L2Rpdj4NCjxkaXYgc3R5bGU9Im1hcmdpbi10b3A6IDE0cHQ7IG1hcmdpbi1ib3R0
b206IDE0cHQ7ICI+PGZvbnQgc2l6ZT0iMyI+VG8gc3RvcCByZWNlaXZpbmcgZnV0dXJlIG5vdGlm
aWNhdGlvbnMgZm9yIHRoaXMgZXZlbnQsIGRlY2xpbmUgdGhpcyBldmVudC4gQWx0ZXJuYXRpdmVs
eSwgeW91IGNhbiBzaWduIHVwIGZvciBhIEdvb2dsZSBhY2NvdW50IGF0DQo8YSBocmVmPSJodHRw
czovL3d3dy5nb29nbGUuY29tL2NhbGVuZGFyLyI+aHR0cHM6Ly93d3cuZ29vZ2xlLmNvbS9jYWxl
bmRhci88L2E+IGFuZCBjb250cm9sIHlvdXIgbm90aWZpY2F0aW9uIHNldHRpbmdzIGZvciB5b3Vy
IGVudGlyZSBjYWxlbmRhci48L2ZvbnQ+PC9kaXY+DQo8L2ZvbnQ+PC90ZD4NCjwvdHI+DQo8L3Rh
YmxlPg0KPC9mb250Pg0KPC9ib2R5Pg0KPC9odG1sPg0K

--_002_001a11c3440066ee0b04ef86cea8googlecom_
Content-Type: text/calendar; charset="UTF-8"; method=REQUEST
Content-Transfer-Encoding: 7bit

BEGIN:VCALENDAR
PRODID:-//Google Inc//Google Calendar 70.9054//EN
VERSION:2.0
CALSCALE:GREGORIAN
METHOD:REQUEST
BEGIN:VEVENT
DTSTART:20140109T103000Z
DTEND:20140109T113000Z
DTSTAMP:20140109T100934Z
ORGANIZER;CN=William:mailto:william@example.com
UID:sssqd1p9ene23bo2i6osr77gpo@google.com
ATTENDEE;CUTYPE=INDIVIDUAL;ROLE=REQ-PARTICIPANT;PARTSTAT=NEEDS-ACTION;RSVP=
 TRUE;CN=billy@example.com;X-NUM-GUESTS=0:mailto:billy@example.com
CREATED:20140109T100932Z
DESCRIPTION:\nI AM DESMOND WILLIAMS AND MY LITTLE SISTER IS GLORIA\, OUR FA
 THER OWNS A LIMITED OF COCOA AND GOLD BUSINESS IN REPUBLIQUE DU CONGO. AFTE
 R HIS TRIP TO COTE DIVOIRE TO NEGOTIATE ON COCOA AND GOLD BUSINESS HE WANTE
 D TO INVEST IN ABROAD. \n\nONE WEEK HE CAME BACK FROM HIS TRIP TO ABIDJAN H
 E HAD A MOTOR ACCIDENT WITH OUR MOTHER WHICH OUR MOTHER DIED INSTANTLY BUT
 OUR FATHER DIED AFTER FIVE DAYS IN A PRIVATE HOSPITAL IN OUR COUNTRY. IT WA
 S LIKE OUR FATHER KNEW HE WAS GOING TO DIE MAY HIS GENTLE SOUL REST IN PREF
 ECT PEACE. \n\nHE DISCLOSED TO ME AS THE ONLY SON THAT HE DEPOSITED THE SUM
  OF (USD $ 10\,500\,000) INTO A BANK IN ABIDJAN THAT THE MONEY WAS MEANT FO
 R HIS COCOA AND GOLD BUSINESS HE WANTED TO ESTABLISH IN ABROAD.WE ARE SOLIC
 ITING FOR YOUR HELP TO TRANSFER THIS MONEY INTO YOUR ACCOUNT IN YOUR COUNTR
 Y FOR OUR INVESTMENT. \n\nPLEASE FOR SECURITY REASONS\,I ADVICE YOU REPLY U
 S THROUGH OUR PRIVATE EMAIL FOR MORE DETAI
 LS. \n\nREGARDS. \n\nDESMOND /GLORIA WILLIAMS.\nView your event at http://w
 ww.google.com/calendar/event?action=VIEW&eid=c3NzcWQxcDllbmUyM2JvMmk2b3NyNz
 dncG8gZGpjYkBkamNic29mdHdhcmUubmw&tok=MjYjd2lsbGlhbXNfZDIwQGdsb2JvbWFpbC5jb
 21jMzc2YThkYmFkMzA2ZDUwNWU2YmYxNjdjNmY1YTUxNmJjMjE5N2Y3&ctz=America/Sao_Pau
 lo&hl=en_GB.
LAST-MODIFIED:20140109T100932Z
LOCATION:
SEQUENCE:0
STATUS:CONFIRMED
SUMMARY:HELLO\,
TRANSP:OPAQUE
END:VEVENT
END:VCALENDAR

--_002_001a11c3440066ee0b04ef86cea8googlecom_--

--_004_001a11c3440066ee0b04ef86cea8googlecom_
Content-Type: application/ics; name="invite.ics"
Content-Description: invite.ics
Content-Disposition: attachment; filename="invite.ics"; size=2029;
	creation-date="Thu, 09 Jan 2014 10:09:44 GMT";
	modification-date="Thu, 09 Jan 2014 10:09:44 GMT"
Content-Transfer-Encoding: base64

QkVHSU46VkNBTEVOREFSDQpQUk9ESUQ6LS8vR29vZ2xlIEluYy8vR29vZ2xlIENhbGVuZGFyIDcw
LjkwNTQvL0VODQpWRVJTSU9OOjIuMA0KQ0FMU0NBTEU6R1JFR09SSUFODQpNRVRIT0Q6UkVRVUVT
VA0KQkVHSU46VkVWRU5UDQpEVFNUQVJUOjIwMTQwMTA5VDEwMzAwMFoNCkRURU5EOjIwMTQwMTA5
VDExMzAwMFoNCkRUU1RBTVA6MjAxNDAxMDlUMTAwOTM0Wg0KT1JHQU5JWkVSO0NOPVdpbGxpYW1z
IFdpbGxpYW1zOm1haWx0bzp3aWxsaWFtc19kMjBAZ2xvYm9tYWlsLmNvbQ0KVUlEOnNzc3FkMXA5
ZW5lMjNibzJpNm9zcjc3Z3BvQGdvb2dsZS5jb20NCkFUVEVOREVFO0NVVFlQRT1JTkRJVklEVUFM
O1JPTEU9UkVRLVBBUlRJQ0lQQU5UO1BBUlRTVEFUPU5FRURTLUFDVElPTjtSU1ZQPQ0KIFRSVUU7
Q049ZGpjYkBkamNic29mdHdhcmUubmw7WC1OVU0tR1VFU1RTPTA6bWFpbHRvOmRqY2JAZGpjYnNv
ZnR3YXJlLm5sDQpDUkVBVEVEOjIwMTQwMTA5VDEwMDkzMloNCkRFU0NSSVBUSU9OOlxuSSBBTSBE
RVNNT05EIFdJTExJQU1TIEFORCBNWSBMSVRUTEUgU0lTVEVSIElTIEdMT1JJQVwsIE9VUiBGQQ0K
IFRIRVIgT1dOUyBBIExJTUlURUQgT0YgQ09DT0EgQU5EIEdPTEQgQlVTSU5FU1MgSU4gUkVQVUJM
SVFVRSBEVSBDT05HTy4gQUZURQ0KIFIgSElTIFRSSVAgVE8gQ09URSBESVZPSVJFIFRPIE5FR09U
SUFURSBPTiBDT0NPQSBBTkQgR09MRCBCVVNJTkVTUyBIRSBXQU5URQ0KIEQgVE8gSU5WRVNUIElO
IEFCUk9BRC4gXG5cbk9ORSBXRUVLIEhFIENBTUUgQkFDSyBGUk9NIEhJUyBUUklQIFRPIEFCSURK
QU4gSA0KIEUgSEFEIEEgTU9UT1IgQUNDSURFTlQgV0lUSCBPVVIgTU9USEVSIFdISUNIIE9VUiBN
T1RIRVIgRElFRCBJTlNUQU5UTFkgQlVUIA0KIE9VUiBGQVRIRVIgRElFRCBBRlRFUiBGSVZFIERB
WVMgSU4gQSBQUklWQVRFIEhPU1BJVEFMIElOIE9VUiBDT1VOVFJZLiBJVCBXQQ0KIFMgTElLRSBP
VVIgRkFUSEVSIEtORVcgSEUgV0FTIEdPSU5HIFRPIERJRSBNQVkgSElTIEdFTlRMRSBTT1VMIFJF
U1QgSU4gUFJFRg0KIEVDVCBQRUFDRS4gXG5cbkhFIERJU0NMT1NFRCBUTyBNRSBBUyBUSEUgT05M
WSBTT04gVEhBVCBIRSBERVBPU0lURUQgVEhFIFNVTQ0KICBPRiAoVVNEICQgMTBcLDUwMFwsMDAw
KSBJTlRPIEEgQkFOSyBJTiBBQklESkFOIFRIQVQgVEhFIE1PTkVZIFdBUyBNRUFOVCBGTw0KIFIg
SElTIENPQ09BIEFORCBHT0xEIEJVU0lORVNTIEhFIFdBTlRFRCBUTyBFU1RBQkxJU0ggSU4gQUJS
T0FELldFIEFSRSBTT0xJQw0KIElUSU5HIEZPUiBZT1VSIEhFTFAgVE8gVFJBTlNGRVIgVEhJUyBN
T05FWSBJTlRPIFlPVVIgQUNDT1VOVCBJTiBZT1VSIENPVU5UUg0KIFkgRk9SIE9VUiBJTlZFU1RN
RU5ULiBcblxuUExFQVNFIEZPUiBTRUNVUklUWSBSRUFTT05TXCxJIEFEVklDRSBZT1UgUkVQTFkg
VQ0KIFMgVEhST1VHSCBPVVIgUFJJVkFURSBFTUFJTDogd2lsbGlhbXNkZXNtb25kMTA3QHlhaG9v
LmNvbS52biBGT1IgTU9SRSBERVRBSQ0KIExTLiBcblxuUkVHQVJEUy4gXG5cbkRFU01PTkQgL0dM
T1JJQSBXSUxMSUFNUy5cblZpZXcgeW91ciBldmVudCBhdCBodHRwOi8vdw0KIHd3Lmdvb2dsZS5j
b20vY2FsZW5kYXIvZXZlbnQ/YWN0aW9uPVZJRVcmZWlkPWMzTnpjV1F4Y0RsbGJtVXlNMkp2TW1r
MmIzTnlOeg0KIGRuY0c4Z1pHcGpZa0JrYW1OaWMyOW1kSGRoY21VdWJtdyZ0b2s9TWpZamQybHNi
R2xoYlhOZlpESXdRR2RzYjJKdmJXRnBiQzVqYg0KIDIxak16YzJZVGhrWW1Ga016QTJaRFV3TldV
MlltWXhOamRqTm1ZMVlUVXhObUpqTWpFNU4yWTMmY3R6PUFtZXJpY2EvU2FvX1BhdQ0KIGxvJmhs
PWVuX0dCLg0KTEFTVC1NT0RJRklFRDoyMDE0MDEwOVQxMDA5MzJaDQpMT0NBVElPTjoNClNFUVVF
TkNFOjANClNUQVRVUzpDT05GSVJNRUQNClNVTU1BUlk6SEVMTE9cLA0KVFJBTlNQOk9QQVFVRQ0K
RU5EOlZFVkVOVA0KRU5EOlZDQUxFTkRBUg0K

--_004_001a11c3440066ee0b04ef86cea8googlecom_--

)";
	auto message{Message::make_from_text(
			msgtext,
			"/home/test/Maildir/inbox/cur/162342449279256.107710_1.evergrey:2,PSp")};
	g_assert_true(!!message);
	assert_equal(message->subject(),
		     "Invitation: HELLO, @ Thu 9 Jan 2014 08:30 - 09:30 (william@example.com)");
	g_assert_true(message->flags() == (Flags::Passed|Flags::Seen|
					   Flags::HasAttachment|Flags::Calendar));
	g_assert_cmpuint(message->body_html().value_or("").find("DETAILS"), ==, 2271);
}


static void
test_message_references()
{
	constexpr auto msgtext =
R"(Content-Transfer-Encoding: quoted-printable
Content-Type: text/plain; charset=utf-8
References: <YuvYh1JbE3v+abd5@kili>
 <90a760c4-6e88-07b4-1f20-8b10414e49aa@arm.com>
 <T4CDWjUrgtI5n4mh1JEdW6RLYzqbPE9-yDrhEVwDM22WX-198fBwcnLd-4_xR1gvsVSHQps9fp_pZevTF0ZmaA==@protonmail.internalid>
To: "Robin Murphy" <robin.murphy@arm.com>
Reply-To: "Dan Carpenter" <dan.carpenter@oracle.com>
From: "Dan Carpenter" <dan.carpenter@oracle.com>
Subject: Re: [PATCH] iommu/omap: fix buffer overflow in debugfs
List-Id: <kernel-janitors.vger.kernel.org>
Date: Fri, 5 Aug 2022 09:37:02 +0300
In-Reply-To: <90a760c4-6e88-07b4-1f20-8b10414e49aa@arm.com>
Precedence: bulk
Message-Id: <20220805063702.GH3438@kadam>

On Thu, Aug 04, 2022 at 05:31:39PM +0100, Robin Murphy wrote:
> On 04/08/2022 3:32 pm, Dan Carpenter wrote:
> > There are two issues here:
)";
	auto message{Message::make_from_text(
			msgtext,
			"/home/test/Maildir/inbox/cur/162342449279256.88888_1.evergrey:2,S")};
	g_assert_true(!!message);
	assert_equal(message->subject(),
		     "Re: [PATCH] iommu/omap: fix buffer overflow in debugfs");
	g_assert_true(message->priority() == Priority::Low);

	/*
	 * "90a760c4-6e88-07b4-1f20-8b10414e49aa@arm.com" is seen both in
	 * references and in-reply-to; in the de-duplication, the first one wins.
	 */
	std::vector<std::string> expected_refs = {
		"YuvYh1JbE3v+abd5@kili",
		"90a760c4-6e88-07b4-1f20-8b10414e49aa@arm.com",
		/* protonmail.internalid is fake and removed */
		// "T4CDWjUrgtI5n4mh1JEdW6RLYzqbPE9-yDrhEVwDM22WX-198fBwcnLd-4_"
		// "xR1gvsVSHQps9fp_pZevTF0ZmaA==@protonmail.internalid"
	};

	assert_equal_seq_str(expected_refs, message->references());
}


static void
test_message_outlook_body()
{
	constexpr auto msgtext =
R"x(Received: from vu-ex2.activedir.vu.lt (172.16.159.219) by
 vu-ex1.activedir.vu.lt (172.16.159.218) with Microsoft SMTP Server
 (version=TLS1_2, cipher=TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384) id 15.2.1118.9
 via Mailbox Transport; Fri, 27 May 2022 11:40:05 +0300
Received: from vu-ex2.activedir.vu.lt (172.16.159.219) by
 vu-ex2.activedir.vu.lt (172.16.159.219) with Microsoft SMTP Server
 (version=TLS1_2, cipher=TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384) id
 15.2.1118.9; Fri, 27 May 2022 11:40:05 +0300
Received: from vu-ex2.activedir.vu.lt ([172.16.159.219]) by
 vu-ex2.activedir.vu.lt ([172.16.159.219]) with mapi id 15.02.1118.009; Fri,
 27 May 2022 11:40:05 +0300
From: =?windows-1257?Q?XXXXXXXXXX= <XXXXXXXXXX>
To:  <XXXXXXXXXX@XXXXXXXXXX.com>
Subject: =?windows-1257?Q?Pra=F0ymas?=
Thread-Topic: =?windows-1257?Q?Pra=F0ymas?=
Thread-Index: AQHYcaRi3ejPSLxkl0uTFDto7z2OcA==
Date: Fri, 27 May 2022 11:40:05 +0300
Message-ID: <5c2cd378af634e929a6cc69da1e66b9d@XX.vu.lt>
Accept-Language: en-US, lt-LT
Content-Language: en-US
X-MS-Has-Attach:
Content-Type: text/html; charset="windows-1257"
Content-Transfer-Encoding: quoted-printable
MIME-Version: 1.0
X-TUID: 1vFQ9RPwwg/u

<html>
<head>
<meta http-equiv=3D"Content-Type" content=3D"text/html; charset=3Dwindows-1=
257">
<style type=3D"text/css" style=3D"display:none;"><!-- P {margin-top:0;margi=
n-bottom:0;} --></style>
</head>
<body dir=3D"ltr">
<div id=3D"divtagdefaultwrapper" style=3D"font-size:12pt;color:#000000;font=
-family:Calibri,Helvetica,sans-serif;" dir=3D"ltr">
<p>Laba diena visiems,</p>
<p>Trumpai.</p>
<p>D=EBl leidimo ar neleidimo ginti darb=E0: ed=EBstytojo paskyroje spaud=
=FEiate ikon=E0 &quot;ra=F0to darbai&quot;, atidar=E6 susiraskite =E1ra=F0=
=E0 &quot;tvirtinti / netvirtinti&quot;, pa=FEym=EBkite vien=E0 i=F0 j=F8.&=
nbsp;</p>
<p><br>
</p>
<p>=D0=E1 darb=E0 privalu atlikti, kad paskui nekilt=F8 problem=F8 studentu=
i =E1vedant =E1vertinim=E0.</p>
<p><br>
</p>
<p>Jei neleid=FEiate ginti darbo, pra=F0au informuoti mane ir komisijos sek=
retori=F8.&nbsp;&nbsp;</p>
<p><br>
</p>
<p>Vis=E0 tolesn=E6 informacij=E0 atsi=F8siu artimiausiu metu (stengsiuosi =
=F0iandien vakare).</p>
<p><br>
</p>
<p>Pagarbiai.</p>
<p><br>
</p>
<p><br>
</p>
<div id=3D"Signature">
<div id=3D"divtagdefaultwrapper" dir=3D"ltr" style=3D"font-family: Calibri,=
 Helvetica, sans-serif, EmojiFont, &quot;Apple Color Emoji&quot;, &quot;Seg=
oe UI Emoji&quot;, NotoColorEmoji, &quot;Segoe UI Symbol&quot;, &quot;Andro=
id Emoji&quot;, EmojiSymbols;">
<p style=3D"color:rgb(0,0,0); font-size:12pt"><br>
</p>
<p style=3D"color:rgb(0,0,0); font-size:12pt"><br>
</p>
<p style=3D"color:rgb(0,0,0); font-size:12pt"><br>
</p>
<p style=3D"color:rgb(0,0,0); font-size:12pt"><span style=3D"font-size:10pt=
; background-color:rgb(255,255,255); color:rgb(0,111,201)"><br>
</span></p>
<p style=3D"color:rgb(0,0,0); font-size:12pt"><span style=3D"font-size:10pt=
; background-color:rgb(255,255,255); color:rgb(0,111,201)">XXXXXXXXXX</span></p>
<p style=3D""><font color=3D"#006fc9"><span style=3D"font-size:13.3333px"><=
/span></font></p>
<span style=3D"font-size:10pt; background-color:rgb(255,255,255); color:rgb=
(0,111,201); font-size:10pt"></span>
<p style=3D""><font color=3D"#006fc9"><span style=3D"font-size:13.3333px">XXXXXXXXXX</span></font></p>
<p style=3D""><font color=3D"#006fc9"><span style=3D"font-size:13.3333px">XXXXXXXXXX</span></font></p>
<p style=3D""><font color=3D"#006fc9"><span style=3D"font-size:13.3333px">XXXXXXXXXX</span></font></p>
<p style=3D""><br>
</p>
<p style=3D""><br>
</p>
</div>
</div>
</div>
</body>
</html>
)x";
	g_test_bug("2349");

	auto message{Message::make_from_text(
			msgtext,
			"/home/test/Maildir/inbox/cur/162342449279256.77777_1.evergrey:2,S")};
	g_assert_true(!!message);

	assert_equal(message->subject(), "Prašymas");
	g_assert_true(message->priority() == Priority::Normal);

	g_assert_false(!!message->body_text());
	g_assert_true(!!message->body_html());
	g_assert_cmpuint(message->body_html()->find("<p>Pagarbiai.</p>"), ==, 935);
}


static void
test_message_message_id()
{
	constexpr const auto msg1 =
R"(From: "Mu Test" <mu@djcbsoftware.nl>
To: mu@djcbsoftware.nl
Message-ID: <87lew9xddt.fsf@djcbsoftware.nl>

abc
)";

	constexpr const auto msg2 =
R"(From: "Mu Test" <mu@djcbsoftware.nl>
To: mu@djcbsoftware.nl

abc
)";

	constexpr const auto msg3 =
R"(From: "Mu Test" <mu@djcbsoftware.nl>
To: mu@djcbsoftware.nl
Message-ID:

abc
)";
	const auto m1{Message::make_from_text(msg1, "/foo/cur/m123:2,S")};
	assert_valid_result(m1);

	const auto m2{Message::make_from_text(msg2, "/foo/cur/m456:2,S")};
	assert_valid_result(m2);
	const auto m3{Message::make_from_text(msg3, "/foo/cur/m789:2,S")};
	assert_valid_result(m3);

	assert_equal(m1->message_id(), "87lew9xddt.fsf@djcbsoftware.nl");

	/* both with absent and empty message-id, generate "random" fake one,
	 * which must end in @mu.id */
	const auto id2{m2->message_id()};
	const auto id3{m3->message_id()};

	g_assert_true(g_str_has_suffix(id2.c_str(), "@mu.id"));
	g_assert_true(g_str_has_suffix(id3.c_str(), "@mu.id"));
}


static void
test_message_fail ()
{
	{
		const auto msg = Message::make_from_path("/root/non-existent-path-12345");
		g_assert_false(!!msg);
	}

	{
		const auto msg = Message::make_from_text("", "");
		g_assert_false(!!msg);
	}
}

static void
test_message_sanitize_maildir()
{
	assert_equal(Message::sanitize_maildir("/"), "/");
	assert_equal(Message::sanitize_maildir("/foo/bar"), "/foo/bar");
	assert_equal(Message::sanitize_maildir("/foo/bar/cuux/"), "/foo/bar/cuux");
}

static void
test_message_subject_with_newline()
{
constexpr const auto txt =
R"(To: foo@example.com
Subject: =?utf-8?q?Le_poids_=C3=A9conomique_de_la_chasse_:_=0A=0Ala_dette_cach?= =?utf-8?q?=C3=A9e_de_la_chasse_!?=
Date: Mon, 24 Apr 2023 07:32:43 +0000

Hello!
)";
	g_test_bug("2477");

	const auto msg{Message::make_from_text(txt, "/foo/cur/m123:2,S")};
	assert_valid_result(msg);

	assert_equal(msg->subject(), // newlines are filtered-out
		     "Le poids économique de la chasse : la dette cachée de la chasse !");
	assert_equal(msg->header("Subject").value_or(""),
		     "Le poids économique de la chasse : \n\nla dette cachée de la chasse !");
	g_assert_true(none_of(msg->flags() & Flags::MailingList));
}

static void
test_message_list_unsubscribe()
{
	constexpr const auto txt =
R"(From: "Mu Test" <mu@djcbsoftware.nl>
To: mu@djcbsoftware.nl
Subject: Test
Message-ID: <87lew9xddt.fsf@djcbsoftware.nl>
List-Unsubscribe: <mailto:unsubscribe-T7BC8RRQMK-booking-email-9@booking.com>

abcdef
)";
	const auto msg{Message::make_from_text(txt, "/xxx/m123:2,S")};
	assert_valid_result(msg);

	assert_equal(msg->mailing_list(), "");
	g_assert_true(any_of(msg->flags() & Flags::MailingList));
}

int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	g_test_add_func("/message/message/mailing-list",
			test_message_mailing_list);
	g_test_add_func("/message/message/attachments",
			test_message_attachments);
	g_test_add_func("/message/message/signed",
			test_message_signed);
	g_test_add_func("/message/message/signed-encrypted",
			test_message_signed_encrypted);
	g_test_add_func("/message/message/multipart-mixed-rfc822",
			test_message_multipart_mixed_rfc822);
	g_test_add_func("/message/message/detect-attachment",
			test_message_detect_attachment);
	g_test_add_func("/message/message/calendar",
			test_message_calendar);
	g_test_add_func("/message/message/references",
			test_message_references);
	g_test_add_func("/message/message/outlook-body",
			test_message_outlook_body);
	g_test_add_func("/message/message/message-id",
			test_message_message_id);
	g_test_add_func("/message/message/subject-with-newline",
			test_message_subject_with_newline);
	g_test_add_func("/message/message/fail",
			test_message_fail);
	g_test_add_func("/message/message/sanitize-maildir",
			test_message_sanitize_maildir);
	g_test_add_func("/message/message/message-list-unsubscribe",
			test_message_list_unsubscribe);

	return g_test_run();
}
