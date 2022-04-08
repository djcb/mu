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
			"/home/test/Maildir/inbox/cur/1649279256.107710_1.evergrey:2,S",
			"/inbox")};
	g_assert_true(!!message);

	assert_equal(message->path(),
		     "/home/test/Maildir/inbox/cur/1649279256.107710_1.evergrey:2,S");
	assert_equal(message->maildir(), "/inbox");

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

	g_assert_true(message->path().empty());
	g_assert_true(message->priority() == Priority::Normal);
	g_assert_cmpuint(message->size(),==,::strlen(msg_text));

	assert_equal(message->subject(), "ättächmeñts");

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

constexpr const char* pub_key =
R"(-----BEGIN PGP PUBLIC KEY BLOCK-----

mDMEYkycYxYJKwYBBAHaRw8BAQdAiE6rRtXjh1u8ZNVB02k1d3divvp0qrifJSe2
/vcCLDm0HE11IFRlc3QgPG11QGRqY2Jzb2Z0d2FyZS5ubD6IlAQTFgoAPAIbAwUL
CQgHAgMiAgEGFQoJCAsCBBYCAwECHgcCF4AWIQT0f3WQZJn/X54Jqiz74qC0xbNs
qgUCYkysVAAKCRD74qC0xbNsquJ0AP95o557wp8GoCn4Fn6RA4mX8QOVv9lGrGRu
D42pFNyFmgD+LinWJR973YEGaGSamGwjc0vKY8nXiuY21noN+RMBYQm4OARiTJxj
EgorBgEEAZdVAQUBAQdAdfnXbQAAgQ/2zDKh1kn0EeTZnzEgC0y+VCz+VQOhXDMD
AQgHiHgEGBYKACACGwwWIQT0f3WQZJn/X54Jqiz74qC0xbNsqgUCYkysvgAKCRD7
4qC0xbNsqsQOAPkBi4cDuf0Yk6PmDb10ARuL4E8plQTO8Ehqp/+O5JeIFQD/f3mi
KTUVweCNFi/1aZ/ViQ4umui3RTmCi+M91A7bRQg=
=3Xa7
-----END PGP PUBLIC KEY BLOCK-----)";

constexpr const char* priv_key =
R"(-----BEGIN PGP PRIVATE KEY BLOCK-----

lIYEYkycYxYJKwYBBAHaRw8BAQdAiE6rRtXjh1u8ZNVB02k1d3divvp0qrifJSe2
/vcCLDn+BwMCMFZr+icelQr/nHyufC4ON2PZG1WTURyap1CAXvV8Jgg8KAtG2olp
Ftp22lSko5JL791GuWe5SnQaIT2I0FNVYPJiuwtcoLxT6vCutam4GLQcTXUgVGVz
dCA8bXVAZGpjYnNvZnR3YXJlLm5sPoiUBBMWCgA8AhsDBQsJCAcCAyICAQYVCgkI
CwIEFgIDAQIeBwIXgBYhBPR/dZBkmf9fngmqLPvioLTFs2yqBQJiTKxUAAoJEPvi
oLTFs2yq4nQA/3mjnnvCnwagKfgWfpEDiZfxA5W/2UasZG4PjakU3IWaAP4uKdYl
H3vdgQZoZJqYbCNzS8pjydeK5jbWeg35EwFhCZyLBGJMnGMSCisGAQQBl1UBBQEB
B0B1+ddtAACBD/bMMqHWSfQR5NmfMSALTL5ULP5VA6FcMwMBCAf+BwMCEaDNUrOs
FLX/HOOPlvFb4zh7IkWYnpCRX1HEWheJIlhYAtzS/EU+Ebc11ricUleyM3mKIeYb
st5PE8NNcm40ep3RtBwNNMt/TGht4/iLfIh4BBgWCgAgAhsMFiEE9H91kGSZ/1+e
Caos++KgtMWzbKoFAmJMrL4ACgkQ++KgtMWzbKrEDgD5AYuHA7n9GJOj5g29dAEb
i+BPKZUEzvBIaqf/juSXiBUA/395oik1FcHgjRYv9Wmf1YkOLprot0U5govjPdQO
20UI
=hlnL
-----END PGP PRIVATE KEY BLOCK-----)";

static void
test_message_signed(void)
{
	constexpr const char *msgtext =
R"(From: Mu Test <mu@djcbsoftware.nl>
To: boo@example.com
Subject: object
Date: Thu, 07 Apr 2022 00:04:26 +0300
Message-ID: <87bkxdyl8i.fsf@djcbsoftware.nl>
MIME-Version: 1.0
Content-Type: multipart/signed; boundary="=-=-=";
	micalg=pgp-sha512; protocol="application/pgp-signature"

--=-=-=
Content-Type: text/plain

Sapperdeflap

--=-=-=
Content-Type: application/pgp-signature; name="signature.asc"

-----BEGIN PGP SIGNATURE-----

iIkEARYKADEWIQT0f3WQZJn/X54Jqiz74qC0xbNsqgUCYk4BBRMcbXVAZGpjYnNv
ZnR3YXJlLm5sAAoJEPvioLTFs2yqhuwBANzT0Lrex/1ohZ5t3GrAfykkbZPZUHDW
1fhWrQ9GIP+8AQCqlgXEteQjQC0VLPNuV4Iz1wOq/e+Hn0KEBNr230v9AQ==
=PeYV
-----END PGP SIGNATURE-----
--=-=-=--
)";
	auto message{Message::make_from_text(
			msgtext,
			"/home/test/Maildir/inbox/cur/1649279777.107710_1.mindcrime:2,RS",
			"/inbox")};
	g_assert_true(!!message);

	g_assert_true(message->bcc().empty());
	assert_equal(message->body_text().value_or(""), "Sapperdeflap\n");
	g_assert_true(message->flags() == (Flags::Signed|Flags::Seen|Flags::Replied));
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

	return g_test_run();
}
