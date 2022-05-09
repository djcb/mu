/*
** Copyright (C) 2008-2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <glib.h>
#include <stdlib.h>
#include <thread>
#include <unistd.h>
#include <time.h>
#include <fstream>

#include <locale.h>

#include "test-mu-common.hh"
#include "mu-store.hh"
#include "utils/mu-result.hh"
#include <utils/mu-utils.hh>
#include "mu-maildir.hh"

using namespace Mu;

static std::string MuTestMaildir  = Mu::canonicalize_filename(MU_TESTMAILDIR, "/");
static std::string MuTestMaildir2 = Mu::canonicalize_filename(MU_TESTMAILDIR2, "/");

static void
test_store_ctor_dtor()
{
	TempDir tempdir;
	auto store{Store::make_new(tempdir.path(), "/tmp", {}, {})};
	assert_valid_result(store);

	g_assert_true(store->empty());
	g_assert_cmpuint(0, ==, store->size());

	g_assert_cmpstr(MU_STORE_SCHEMA_VERSION, ==,
			store->properties().schema_version.c_str());
}

static void
test_store_add_count_remove()
{
	TempDir tempdir{false};

	auto store{Store::make_new(tempdir.path() + "/xapian", MuTestMaildir, {}, {})};
	assert_valid_result(store);

	const auto msgpath{MuTestMaildir + "/cur/1283599333.1840_11.cthulhu!2,"};
	const auto id1 = store->add_message(msgpath);
	assert_valid_result(id1);
	store->commit();

	g_assert_cmpuint(store->size(), ==, 1);
	g_assert_true(store->contains_message(msgpath));

	g_assert_true(store->contains_message(msgpath));

	const auto id2 = store->add_message(MuTestMaildir2 + "/bar/cur/mail3");
	g_assert_false(!!id2); // wrong maildir.
	store->commit();

	const auto msg3path{MuTestMaildir + "/cur/1252168370_3.14675.cthulhu!2,S"};
	const auto id3 = store->add_message(msg3path);
	assert_valid_result(id3);

	g_assert_cmpuint(store->size(), ==, 2);
	g_assert_true(store->contains_message(msg3path));

	store->remove_message(id1.value());
	g_assert_cmpuint(store->size(), ==, 1);
	g_assert_false(
	    store->contains_message(MuTestMaildir + "/cur/1283599333.1840_11.cthulhu!2,"));

	store->remove_message(msg3path);
	g_assert_true(store->empty());
	g_assert_false(store->contains_message(msg3path));
}


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
Subject: Capybaras United
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
	TempDir tempdir;
	auto store{Store::make_new(tempdir.path(), "/home/test/Maildir", {}, {})};
	assert_valid_result(store);

	const auto msgpath{"/home/test/Maildir/inbox/cur/1649279256.107710_1.evergrey:2,S"};
	auto message{Message::make_from_text(test_message_1, msgpath)};
	assert_valid_result(message);

	const auto docid = store->add_message(*message);
	assert_valid_result(docid);
	g_assert_cmpuint(store->size(),==, 1);

	auto msg2{store->find_message(*docid)};
	g_assert_true(!!msg2);
	assert_equal(message->path(), msg2->path());

	g_assert_true(store->contains_message(message->path()));

	const auto qr = store->run_query("to:sqlite-dev@sqlite.org");
	g_assert_true(!!qr);
	g_assert_cmpuint(qr->size(), ==, 1);
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
X-label: @NextActions operation:mindcrime Queensrÿche
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

	TempDir tempdir;
	auto store{Store::make_new(tempdir.path(), "/home/test/Maildir", {}, {})};
	assert_valid_result(store);

	auto message{Message::make_from_text(
			msg_text,
			"/home/test/Maildir/inbox/cur/1649279256.abcde_1.evergrey:2,S")};
	assert_valid_result(message);

	const auto docid = store->add_message(*message);
	assert_valid_result(docid);
	store->commit();

	auto msg2{store->find_message(*docid)};
	g_assert_true(!!msg2);
	assert_equal(message->path(), msg2->path());

	g_assert_true(store->contains_message(message->path()));

	// for (auto&& term = msg2->document().xapian_document().termlist_begin();
	//      term != msg2->document().xapian_document().termlist_end(); ++term)
	//	g_message(">>> %s", (*term).c_str());
}


static void
test_index_move()
{
	using namespace std::chrono_literals;

	 const std::string msg_text =
R"(From: Valentine Michael Smith <mike@example.com>
To: Raul Endymion <raul@example.com>
Cc: emacs-devel@gnu.org
Subject: Re: multi-eq hash tables
Date: Tue, 03 May 2022 20:58:02 +0200
Message-ID: <87h766tzzz.fsf@gnus.org>
MIME-Version: 1.0
Content-Type: text/plain
Precedence: list
List-Id: "Emacs development discussions." <emacs-devel.gnu.org>
List-Post: <mailto:emacs-devel@gnu.org>

Raul Endymion <raul@example.com> writes:

> Maybe we should introduce something like:
>
>     (define-hash-table-test shallow-equal
>       (lambda (x1 x2) (while (and (consp x1) (consp x2) (eql (car x1) (car x2)))
>                         (setq x1 (cdr x1)) (setq x2 (cdr x2)))
>                       (equal x1 x2)))
>       ...)

Yes, that would be excellent.
)";

	 TempDir tempdir2;

	 { // create a message file.
		 const auto res1 = maildir_mkdir(tempdir2.path() + "/Maildir/a");
		 assert_valid_result(res1);

		 std::ofstream output{tempdir2.path() + "/Maildir/a/new/msg"};
		 output.write(msg_text.c_str(), msg_text.size());
		 output.close();
		 g_assert_true(output.good());
	 }

	 // Index it into a store.
	 TempDir tempdir;
	 auto store{Store::make_new(tempdir.path(), tempdir2.path() + "/Maildir", {}, {})};
	 assert_valid_result(store);

	 store->indexer().start({});
	 size_t n{};
	 while (store->indexer().is_running()) {
		 std::this_thread::sleep_for(100ms);
		 g_assert_cmpuint(n++,<=,25);
	 }
	 g_assert_true(!store->indexer().is_running());
	 const auto& prog{store->indexer().progress()};
	 g_assert_cmpuint(prog.updated,==,1);
	 g_assert_cmpuint(store->size(), ==, 1);
	 g_assert_false(store->empty());

	 // Find the message
	 auto qr = store->run_query("path:" + tempdir2.path() + "/Maildir/a/new/msg");
	 assert_valid_result(qr);
	 g_assert_cmpuint(qr->size(),==,1);

	 const auto msg = qr->begin().message();
	 g_assert_true(!!msg);

	 // Check the message
	 const auto oldpath{msg->path()};
	 assert_equal(msg->subject(), "Re: multi-eq hash tables");
	 g_assert_true(msg->docid() != 0);
	 g_debug("%s", msg->to_sexp().to_sexp_string().c_str());

	 // Move the message from new->cur
	 std::this_thread::sleep_for(1s); /* ctime should change */
	 const auto msg3 = store->move_message(msg->docid(), {}, Flags::Seen);
	 assert_valid_result(msg3);
	 assert_equal(msg3->maildir(), "/a");
	 assert_equal(msg3->path(), tempdir2.path() + "/Maildir/a/cur/msg:2,S");
	 g_assert_true(::access(msg3->path().c_str(), R_OK)==0);
	 g_assert_false(::access(oldpath.c_str(), R_OK)==0);

	 g_debug("%s", msg3->to_sexp().to_sexp_string().c_str());
	 g_assert_cmpuint(store->size(), ==, 1);
}



int
main(int argc, char* argv[])
{
	g_test_init(&argc, &argv, NULL);

	g_test_add_func("/store/ctor-dtor", test_store_ctor_dtor);
	g_test_add_func("/store/add-count-remove", test_store_add_count_remove);
	g_test_add_func("/store/message/mailing-list",
			test_message_mailing_list);
	g_test_add_func("/store/message/attachments",
			test_message_attachments);
	g_test_add_func("/store/index/move",
			test_index_move);

	if (!g_test_verbose())
		g_log_set_handler(
			NULL,
			(GLogLevelFlags)(G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL |
					 G_LOG_FLAG_RECURSION),
			(GLogFunc)black_hole,
			NULL);

	return g_test_run();
}
