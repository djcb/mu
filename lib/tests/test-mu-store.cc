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
#include <array>
#include <unistd.h>
#include <time.h>
#include <fstream>

#include <locale.h>

#include "utils/mu-test-utils.hh"
#include "mu-store.hh"
#include "utils/mu-result.hh"
#include <utils/mu-utils.hh>
#include <utils/mu-utils-file.hh>
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
test_store_reinit()
{
	TempDir tempdir;
	{
		Store::Config conf{};
		conf.max_message_size = 1234567;
		conf.batch_size	      = 7654321;

		StringVec my_addresses{ "foo@example.com", "bar@example.com" };

		auto store{Store::make_new(tempdir.path(), MuTestMaildir, my_addresses, conf)};
		assert_valid_result(store);

		g_assert_true(store->empty());
		g_assert_cmpuint(0, ==, store->size());

		g_assert_cmpstr(MU_STORE_SCHEMA_VERSION, ==,
				store->properties().schema_version.c_str());

		const auto msgpath{MuTestMaildir + "/cur/1283599333.1840_11.cthulhu!2,"};
		const auto id = store->add_message(msgpath);
		assert_valid_result(id);
		g_assert_true(store->contains_message(msgpath));
		g_assert_cmpuint(store->size(), ==, 1);
	}

	//now let's reinitialize it.
	{
		auto store{Store::make(tempdir.path(),
				       Store::Options::Writable|Store::Options::ReInit)};

		assert_valid_result(store);
		g_assert_true(store->empty());

		assert_equal(store->properties().database_path, tempdir.path());
		g_assert_cmpuint(store->properties().batch_size,==,7654321);
		g_assert_cmpuint(store->properties().max_message_size,==,1234567);

		const auto addrs{store->properties().personal_addresses};
		g_assert_cmpuint(addrs.size(),==,2);
		g_assert_true(seq_some(addrs, [](auto&& a){return a=="foo@example.com";}));
		g_assert_true(seq_some(addrs, [](auto&& a){return a=="bar@example.com";}));

		const auto msgpath{MuTestMaildir + "/cur/1283599333.1840_11.cthulhu!2,"};
		const auto id = store->add_message(msgpath);
		assert_valid_result(id);
		g_assert_true(store->contains_message(msgpath));
		g_assert_cmpuint(store->size(), ==, 1);
	}
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

	/* ensure 'update' dtrt, i.e., nothing. */
	const auto docid2 = store->update_message(*message, *docid);
	assert_valid_result(docid2);
	g_assert_cmpuint(store->size(),==, 1);
	g_assert_cmpuint(*docid,==,*docid2);

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

	const auto stats{store->statistics()};
	g_assert_cmpuint(stats.size,==,store->size());
	g_assert_cmpuint(stats.last_index,==,0);
	g_assert_cmpuint(stats.last_change,>=,::time({}));
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
	 g_debug("%s", msg->sexp().to_string().c_str());

	 // Move the message from new->cur
	 std::this_thread::sleep_for(1s); /* ctime should change */
	 const auto msgs3 = store->move_message(msg->docid(), {}, Flags::Seen);
	 assert_valid_result(msgs3);
	 g_assert_true(msgs3->size() == 1);
	 const auto& msg3{msgs3->at(0).second};
	 assert_equal(msg3.maildir(), "/a");
	 assert_equal(msg3.path(), tempdir2.path() + "/Maildir/a/cur/msg:2,S");
	 g_assert_true(::access(msg3.path().c_str(), R_OK)==0);
	 g_assert_false(::access(oldpath.c_str(), R_OK)==0);

	 g_debug("%s", msg3.sexp().to_string().c_str());
	 g_assert_cmpuint(store->size(), ==, 1);
}



static void
test_store_move_dups()
{
	 const std::string msg_text =
R"(From: Valentine Michael Smith <mike@example.com>
To: Raul Endymion <raul@example.com>
Subject: Re: multi-eq hash tables
Date: Tue, 03 May 2022 20:58:02 +0200
Message-ID: <87h766tzzz.fsf@gnus.org>

Yes, that would be excellent.
)";
	 TempDir tempdir2;

	 // create a message file + dups
	 const auto res1 = maildir_mkdir(tempdir2.path() + "/Maildir/a");
	 assert_valid_result(res1);
	 const auto res2 = maildir_mkdir(tempdir2.path() + "/Maildir/b");
	 assert_valid_result(res2);

	 auto msg1_path = tempdir2.path() + "/Maildir/a/new/msg123";
	 auto msg2_path = tempdir2.path() + "/Maildir/a/cur/msgabc:2,S";
	 auto msg3_path = tempdir2.path() + "/Maildir/b/cur/msgdef:2,RS";

	 TempDir tempdir;
	 auto store{Store::make_new(tempdir.path(), tempdir2.path() + "/Maildir", {}, {})};
	 assert_valid_result(store);

	 std::vector<Store::Id> ids;
	 for (auto&& p: {msg1_path, msg2_path, msg3_path}) {
			 std::ofstream output{p};
			 output.write(msg_text.c_str(), msg_text.size());
			 output.close();
			 auto res = store->add_message(p);
			 assert_valid_result(res);
			 ids.emplace_back(*res);
	 }
	 g_assert_cmpuint(store->size(), ==, 3);

	 // mark main message (+ dups) as seen
	 auto mres = store->move_message(ids.at(0), {},
					 Flags::Seen | Flags::Flagged | Flags::Passed,
					 Store::MoveOptions::DupFlags);
	 assert_valid_result(mres);
	 // al three dups should have been updated
	 g_assert_cmpuint(mres->size(), ==, 3);
	 // first should be the  original
	 g_assert_cmpuint(mres->at(0).first, ==, ids.at(0));
	 { // Message 1
		 const Message& msg = mres->at(0).second;
		 assert_equal(msg.path(), tempdir2.path() + "/Maildir/a/cur/msg123:2,FPS");
		 g_assert_true(msg.flags() == (Flags::Seen|Flags::Flagged|Flags::Passed));
	 }
	 // note: Seen and Passed should be added to msg2/3, but Flagged shouldn't
	 // msg3 should loose its R flag.

	 auto check_msg2 = [&](const Message& msg) {
		 assert_equal(msg.path(), tempdir2.path() + "/Maildir/a/cur/msgabc:2,PS");
	 };

	 auto check_msg3 = [&](const Message& msg) {
		 assert_equal(msg.path(), tempdir2.path() + "/Maildir/b/cur/msgdef:2,PS");
	 };

	 if (mres->at(1).first == ids.at(1)) {
		 check_msg2(mres->at(1).second);
		 check_msg3(mres->at(2).second);
	 } else  {
		 check_msg2(mres->at(2).second);
		 check_msg3(mres->at(1).second);
	 }
}


static void
test_store_fail()
{
	{
		const auto store = Store::make("/root/non-existent-path/12345");
		g_assert_false(!!store);
	}

	{
		const auto store = Store::make_new("/../../root/non-existent-path/12345",
						   "/../../root/non-existent-path/54321",
						   {}, {});
		g_assert_false(!!store);
	}
}

int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	g_test_add_func("/store/ctor-dtor", test_store_ctor_dtor);
	g_test_add_func("/store/reinit", test_store_reinit);
	g_test_add_func("/store/add-count-remove", test_store_add_count_remove);
	g_test_add_func("/store/message/mailing-list",
			test_message_mailing_list);
	g_test_add_func("/store/message/attachments",
			test_message_attachments);
	g_test_add_func("/store/index/index-move", test_index_move);
	g_test_add_func("/store/index/move-dups", test_store_move_dups);
	g_test_add_func("/store/index/fail", test_store_fail);

	return g_test_run();
}
