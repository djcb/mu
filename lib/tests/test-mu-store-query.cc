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

#include <array>
#include <thread>
#include <string>
#include <string_view>
#include <fstream>
#include <unordered_map>

#include <mu-store.hh>
#include <utils/mu-utils.hh>
#include <message/mu-message.hh>

using namespace Mu;


/// map of some (unique) path-tail to the message-text
using TestMap = std::unordered_map<std::string, std::string>;

static Store
make_test_store(const std::string& test_path, const TestMap& test_map,
		const StringVec &personal_addresses)
{
	std::string maildir = test_path + "/Maildir";

	/* write messages to disk */
	for (auto&& item: test_map) {

		const auto msgpath = maildir + "/" + item.first;

		/* create the directory for the message */
		auto dir = to_string_gchar(g_path_get_dirname(msgpath.c_str()));
		if (g_test_verbose())
			g_message("create message dir %s", dir.c_str());

		g_assert_cmpuint(g_mkdir_with_parents(dir.c_str(), 0700), ==, 0);

		/* write the file */
		std::ofstream stream(msgpath);
		stream.write(item.second.data(), item.second.size());
		g_assert_true(stream.good());
		stream.close();
	}

	/* make the store */
	auto store = Store::make_new(test_path, maildir, personal_addresses, {});
	assert_valid_result(store);

	/* index the messages */
	auto res = store->indexer().start({});
	g_assert_true(res);
	while(store->indexer().is_running()) {
		using namespace std::chrono_literals;
		std::this_thread::sleep_for(100ms);
	}
	g_assert_true(!store->empty());
	g_assert_cmpuint(store->size(),==,test_map.size());

	/* and we have a fully-ready store */
	return std::move(store.value());
}


static void
test_simple()
{
	const TestMap test_msgs = {{

// "sqlite-msg" "Simple mailing list message.
{
"basic/cur/sqlite-msg:2,S",
R"(Return-Path: <sqlite-dev-bounces@sqlite.org>
X-Original-To: xxxx@localhost
Delivered-To: xxxx@localhost
Received: from mindcrime (localhost [127.0.0.1])
  by mail.xxxxsoftware.nl (Postfix) with ESMTP id 32F276963F
  for <xxxx@localhost>; Mon,  4 Aug 2008 21:49:34 +0300 (EEST)
Message-Id: <83B5AF40-DBFA-4578-A043-04C80276E195@sqlabs.net>
From: "Foo Example" <foo@example.com>
To: sqlite-dev@sqlite.org
Cc: "Bank of America" <bank@example.com>
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

Inside sqlite3VdbeExec there is a very big switch statement.
In order to increase performance with few modifications to the
original code, why not use this technique ?
http://docs.freebsd.org/info/gcc/gcc.info.Labels_as_Values.html

With a properly defined "instructions" array, instead of the switch
statement you can use something like:
goto * instructions[pOp->opcode];

I said: "Aujourd'hui!"
)"},
}};
	TempDir tdir;
	auto store{make_test_store(tdir.path(), test_msgs, {})};

	// matches
	for (auto&& expr: {
			"Inside",
			"from:foo@example.com",
			"from:Foo",
			"from:\"Foo Example\"",
			"from:/Foo.*Example/",
			"recip:\"Bank Of America\"",
			"date:2008-08-01..2008-09-01",
			"prio:low",
			"to:sqlite-dev@sqlite.org",
			"list:sqlite-dev.sqlite.org",
			"aujourd'hui",
		}) {

		if (g_test_verbose())
			g_message("query: '%s'", expr);
		auto qr = store.run_query(expr);
		assert_valid_result(qr);
		g_assert_false(qr->empty());
		g_assert_cmpuint(qr->size(), ==, 1);
	}

	auto qr = store.run_query("statement");
	assert_valid_result(qr);
	g_assert_false(qr->empty());
	g_assert_cmpuint(qr->size(), ==, 1);

	assert_equal(qr->begin().subject().value_or(""),
		     "[sqlite-dev] VM optimization inside sqlite3VdbeExec");
	g_assert_true(qr->begin().references().empty());
	//g_assert_cmpuint(qr->begin().date().value_or(0), ==, 123454);
}

static void
test_spam_address_components()
{
	const TestMap test_msgs = {{

// "sqlite-msg" "Simple mailing list message.
{
"spam/cur/spam-msg:2,S",
R"(Message-Id: <abcde@foo.bar>
From: "Foo Example" <bar@example.com>
To: example@example.com
Subject: ***SPAM*** this is a test

Boo!
)"},
}};
	TempDir tdir;
	auto store{make_test_store(tdir.path(), test_msgs, {})};

	g_test_bug("2278");
	g_test_bug("2281");

	// matches both
	for (auto&& expr: {
			"SPAM",
			"spam",
			"/.*SPAM.*/",
			"subject:SPAM",
			"from:bar@example.com",
			"subject:\\*\\*\\*SPAM\\*\\*\\*",
			"bar",
			"example.com"
		}) {

		if (g_test_verbose())
			g_message("query: '%s'", expr);
		auto qr = store.run_query(expr);
		assert_valid_result(qr);
		g_assert_false(qr->empty());
		g_assert_cmpuint(qr->size(), ==, 1);
	}
}

int
main(int argc, char* argv[])
{
	g_test_init(&argc, &argv, NULL);

	g_test_bug_base("https://github.com/djcb/mu/issues/");

	g_test_add_func("/store/query/simple",       test_simple);
	g_test_add_func("/store/query/spam-address-components",
			test_spam_address_components);

	return g_test_run();
}
