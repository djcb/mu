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
#include "mu-mime-object.hh"
#include <glib.h>

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
		assert_equal(part.cooked_filename().value(), "tmp-file-02.bin");
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
			g_warning("%s", sigs.error().what());

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

	g_assert_true(message->cached_sexp().empty());
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

	g_assert_true(message->cc().empty());
	g_assert_cmpuint(message->date(), ==, 1651562786);
	g_assert_true(message->flags() == (Flags::HasAttachment));

	g_assert_cmpuint(message->parts().size(), ==, 3);

	for (auto&& part: message->parts())
		g_info("%s %s",
		       part.is_attachment() ? "yes" : "no",
		       part.mime_type().value_or("boo").c_str());

}


int
main(int argc, char* argv[])
{
	g_test_init(&argc, &argv, NULL);

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

	return g_test_run();
}
