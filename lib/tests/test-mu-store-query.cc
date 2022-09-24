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
#include <utils/mu-test-utils.hh>
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
Bcc: Aku Ankka <donald.duck@duckstad.nl>
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
			"cc:bank@example.com",
			"cc:bank",
			"cc:america",
			"bcc:donald.duck@duckstad.nl",
			"bcc:donald.duck",
			"bcc:duckstad.nl",
			"bcc:aku",
			"bcc:ankka",
			"bcc:\"aku ankka\"",
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


static void
test_dups_related()
{
	const TestMap test_msgs = {{
/* parent */
{
"inbox/cur/msg1:2,S",
R"(Message-Id: <abcde@foo.bar>
From: "Foo Example" <bar@example.com>
Date: Sat, 06 Aug 2022 11:01:54 -0700
To: example@example.com
Subject: test1

Parent
)"},
/* child (dup vv) */
{
"boo/cur/msg2:1,S",
R"(Message-Id: <edcba@foo.bar>
In-Reply-To: <abcde@foo.bar>
From: "Foo Example" <bar@example.com>
Date: Sat, 06 Aug 2022 13:01:54 -0700
To: example@example.com
Subject: Re: test1

Child
)"},
/* child (dup ^^) */
{
"inbox/cur/msg2:1,S",
R"(Message-Id: <edcba@foo.bar>
In-Reply-To: <abcde@foo.bar>
From: "Foo Example" <bar@example.com>
Date: Sat, 06 Aug 2022 14:01:54 -0700
To: example@example.com
Subject: Re: test1

Child
)"},
}};
	TempDir tdir;
	auto store{make_test_store(tdir.path(), test_msgs, {})};
	{
		// direct matches
		auto qr = store.run_query("test1", Field::Id::Date,
					  QueryFlags::None);
		g_assert_true(!!qr);
		g_assert_false(qr->empty());
		g_assert_cmpuint(qr->size(), ==, 3);
	}

	{
		// skip duplicate messages; which one is skipped is arbitrary.
		auto qr = store.run_query("test1", Field::Id::Date,
					  QueryFlags::SkipDuplicates);
		g_assert_true(!!qr);
		g_assert_false(qr->empty());
		g_assert_cmpuint(qr->size(), ==, 2);
	}

	{
		// no related
		auto qr = store.run_query("Parent", Field::Id::Date);
		g_assert_true(!!qr);
		g_assert_false(qr->empty());
		g_assert_cmpuint(qr->size(), ==, 1);
	}

	{
		// find related messages
		auto qr = store.run_query("Parent", Field::Id::Date,
					  QueryFlags::IncludeRelated);
		g_assert_true(!!qr);
		g_assert_false(qr->empty());
		g_assert_cmpuint(qr->size(), ==, 3);
	}

	{
		// find related messages, skip dups. the leader message
		// should _not_ be skipped.
		auto qr = store.run_query("test1 AND maildir:/inbox",
					  Field::Id::Date,
					  QueryFlags::IncludeRelated|
					  QueryFlags::SkipDuplicates);
		g_assert_true(!!qr);
		g_assert_false(qr->empty());
		g_assert_cmpuint(qr->size(), ==, 2);

		// ie the /boo is to be skipped, since it's not in the leader
		// set.
		for (auto&& m: *qr)
			assert_equal(m.message()->maildir(), "/inbox");
	}

	{
		// find related messages, find parent from child.
		auto qr = store.run_query("Child and maildir:/inbox",
					  Field::Id::Date,
					  QueryFlags::IncludeRelated);
		g_assert_true(!!qr);
		g_assert_false(qr->empty());
		g_assert_cmpuint(qr->size(), ==, 3);

	}

	{
		// find related messages, find parent from child.
		// leader message wins
		auto qr = store.run_query("Child and maildir:/inbox",
					  Field::Id::Date,
					  QueryFlags::IncludeRelated|
					  QueryFlags::SkipDuplicates|
					  QueryFlags::Descending);
		g_assert_true(!!qr);
		g_assert_false(qr->empty());
		g_assert_cmpuint(qr->size(), ==, 2);

		// ie the /boo is to be skipped, since it's not in the leader
		// set.
		for (auto&& m: *qr)
			assert_equal(m.message()->maildir(), "/inbox");
	}
}


static void
test_related_missing_root()
{
	const TestMap test_msgs = {{
{
"inbox/cur/msg1:2,S",
R"(Content-Type: text/plain; charset=utf-8
References:  <EZrZOnVCsYfFcX3Ls0VFoRnJdCGV4GM5YtO739l-iOB2ADNH7cIJWb0DaO5Of3BWDUEKq18Rz3a7rNoI96bNwQ==@protonmail.internalid>
To: "Joerg Roedel" <joro@8bytes.org>, "Suman Anna" <s-anna@ti.com>
Reply-To: "Dan Carpenter" <dan.carpenter@oracle.com>
From: "Dan Carpenter" <dan.carpenter@oracle.com>
Subject: [PATCH] iommu/omap: fix buffer overflow in debugfs
Date: Thu, 4 Aug 2022 17:32:39 +0300
Message-Id: <YuvYh1JbE3v+abd5@kili>
List-Id: <kernel-janitors.vger.kernel.org>
Precedence: bulk

There are two issues here:
)"},
{
"inbox/cur/msg2:2,S",
R"(Content-Transfer-Encoding: quoted-printable
Content-Type: text/plain; charset=utf-8
References: <YuvYh1JbE3v+abd5@kili>
 <9pEUi_xoxa7NskF7EK_qfrlgjXzGsyw9K7cMfYbo-KI6fnyVMKTpc8E2Fu94V8xedd7cMpn0LlBrr9klBMflpw==@protonmail.internalid>
Reply-To: "Laurent Pinchart" <laurent.pinchart@ideasonboard.com>
From: "Laurent Pinchart" <laurent.pinchart@ideasonboard.com>
Subject: Re: [PATCH] iommu/omap: fix buffer overflow in debugfs
List-Id: <kernel-janitors.vger.kernel.org>
Message-Id: <YuvzKJM66k+ZPD9c@pendragon.ideasonboard.com>
Precedence: bulk
In-Reply-To: <YuvYh1JbE3v+abd5@kili>

Hi Dan,

Thank you for the patch.
)"},
{
"inbox/cur/msg3:2,S",
R"(Content-Transfer-Encoding: quoted-printable
Content-Type: text/plain; charset=utf-8
References: <YuvYh1JbE3v+abd5@kili>
 <G6TStg8J52Q-uSMTR7wRQdPeloxpZMiEQT_F8_JIDYM25eEPeHGgrNKO0fuO78MiQgD9Mz4BDtsZlZgmPKFe4Q==@protonmail.internalid>
To: "Dan Carpenter" <dan.carpenter@oracle.com>, "Joerg Roedel"
 <joro@8bytes.org>, "Suman Anna" <s-anna@ti.com>
Reply-To: "Robin Murphy" <robin.murphy@arm.com>
From: "Robin Murphy" <robin.murphy@arm.com>
Subject: Re: [PATCH] iommu/omap: fix buffer overflow in debugfs
List-Id: <kernel-janitors.vger.kernel.org>
Message-Id: <90a760c4-6e88-07b4-1f20-8b10414e49aa@arm.com>
Precedence: bulk
In-Reply-To: <YuvYh1JbE3v+abd5@kili>
Date: Thu, 4 Aug 2022 17:31:39 +0100

On 04/08/2022 3:32 pm, Dan Carpenter wrote:
> There are two issues here:
)"},
{
"inbox/new/msg4",
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
)"},
}};
	TempDir tdir;
	auto store{make_test_store(tdir.path(), test_msgs, {})};
	{
		auto qr = store.run_query("fix buffer overflow in debugfs",
					  Field::Id::Date, QueryFlags::IncludeRelated);
		g_assert_true(!!qr);
		g_assert_cmpuint(qr->size(), ==, 4);
	}

	{
		auto qr = store.run_query("fix buffer overflow in debugfs and flag:unread",
					  Field::Id::Date, QueryFlags::None);
		g_assert_true(!!qr);
		g_assert_cmpuint(qr->size(), ==, 1);
		assert_equal(qr->begin().message_id().value_or(""), "20220805063702.GH3438@kadam");
		assert_equal(qr->begin().thread_id().value_or(""), "YuvYh1JbE3v+abd5@kili");
	}

	{
		/* this one failed earlier, because the 'protonmail' id is the
		 * first reference, which means it does _not_ have the same
		 * thread-id as the rest; however, we filter these
		 * fake-message-ids now.*/
		g_test_bug("2312");

		auto qr = store.run_query("fix buffer overflow in debugfs and flag:unread",
					  Field::Id::Date, QueryFlags::IncludeRelated);
		g_assert_true(!!qr);
		g_assert_cmpuint(qr->size(), ==, 4);
	}
}


static void
test_body_matricula()
{
	const TestMap test_msgs = {{
{
"basic/cur/matricula-msg:2,S",
R"(From: XXX <XX@XX.com>
Subject:
 =?iso-8859-1?Q?EF_-_Pago_matr=EDcula_de_la_matr=EDcula_de_inscripci=F3n_a?=
Date: Thu, 4 Aug 2022 14:29:41 +0000
Message-ID:
 <VE1PR03MB5471882920DE08CFE44D97A0FE9F9@VE1PR03MB5471.eurprd03.prod.outlook.com>
Accept-Language: es-AR, es-ES, en-US
Content-Language: es-AR
X-MS-Has-Attach: yes
Content-Type: multipart/mixed;
	boundary="_004_VE1PR03MB5471882920DE08CFE44D97A0FE9F9VE1PR03MB5471eurp_"
MIME-Version: 1.0
X-OriginatorOrg: ef.com
X-MS-Exchange-CrossTenant-AuthAs: Internal
X-MS-Exchange-CrossTenant-AuthSource: VE1PR03MB5471.eurprd03.prod.outlook.com

--_004_VE1PR03MB5471882920DE08CFE44D97A0FE9F9VE1PR03MB5471eurp_
Content-Type: multipart/alternative;
	boundary="_000_VE1PR03MB5471882920DE08CFE44D97A0FE9F9VE1PR03MB5471eurp_"

--_000_VE1PR03MB5471882920DE08CFE44D97A0FE9F9VE1PR03MB5471eurp_
Content-Type: text/plain; charset="iso-8859-1"
Content-Transfer-Encoding: quoted-printable

Buenas tardes Familia,


Espero que est=E9n muy bien.



Ya cargamos en sistema su pre inscripci=F3n para el curso


Quedamos atentos ante cualquier consulta que surja.

Saludos,
)"},
}};
	TempDir tdir;
	auto store{make_test_store(tdir.path(), test_msgs, {})};

	/* i.e., non-utf8 text parts were not converted */
	g_test_bug("2333");

	// matches
	for (auto&& expr: {
			"subject:matrícula",
			"subject:matricula",
			"body:atentos",
			"body:inscripción"
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
	mu_test_init(&argc, &argv);

	g_test_bug_base("https://github.com/djcb/mu/issues/");

	g_test_add_func("/store/query/simple", test_simple);
	g_test_add_func("/store/query/spam-address-components",
			test_spam_address_components);
	g_test_add_func("/store/query/dups-related",
			test_dups_related);
	g_test_add_func("/store/query/related-missing-root",
			test_related_missing_root);
	g_test_add_func("/store/query/body-matricula",
			test_body_matricula);

	return g_test_run();
}
