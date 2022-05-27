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
#include <glib.h>
#include <string>
#include <thread>
#include <vector>
#include <iostream>
#include <regex>
#include <fstream>

#include <utils/mu-utils.hh>
#include <mu-store.hh>
#include "mu-maildir.hh"

using namespace Mu;

constexpr auto test_msg = R"(Date: Sat, 21 May 2022 07:55:16 -0700
From: "Quinn @ID@" <q@ID@@example.com>
To: test@ID@.example.com
Message-ID: <abcdefg@ID@@example.com>
In-Reply-To: <314151592@ID@@example.com>
References: <2718281828@ID@@example.com>
Subject: Test Message @ID@
Mime-Version: 1.0
Content-Type: multipart/alternative;
 boundary="--==_mimepart_6288fd545cc61_3421d2c8149243";
 charset=UTF-8
Content-Transfer-Encoding: 7bit

----==_mimepart_6288fd545cc61_3421d2c8149243
Content-Type: text/plain;
 charset=UTF-8
Content-Transfer-Encoding: 7bit

Just some random text.

--
Reply to this email directly or view it on GitHub:
https://github.com/djcb/mu/pull/2262#issuecomment-1133647407
You are receiving this because you are subscribed to this thread.

Message ID: <djcb/mu/pull/2262/c1133647407@github.com>
----==_mimepart_6288fd545cc61_3421d2c8149243
Content-Type: text/html;
 charset=UTF-8
Content-Transfer-Encoding: 7bit

<p></p>
Some random <b>text</b> as html
----==_mimepart_6288fd545cc61_3421d2c8149243--
)";


static std::string
message(const std::regex& rx, size_t id)
{
	char buf[16];
	::snprintf(buf, sizeof(buf), "%zu", id);
	return std::regex_replace(test_msg, rx, buf);
}


static void
setup(size_t num_maildirs, size_t num_messages)
{
	/* create toplevel */
	auto top_maildir = std::string{BENCH_MAILDIRS};
	int res = g_mkdir_with_parents(top_maildir.c_str(), 0700);
	g_assert_cmpuint(res,==, 0);

	/* create maildirs */
	for (size_t i = 0; i != num_maildirs; ++i) {
		const auto mdir = format("%s/maildir-%zu", top_maildir.c_str(), i);
		auto res = maildir_mkdir(mdir);
		g_assert(!!res);
	}
	const auto rx = std::regex("@ID@");
	/* create messages */
	for (size_t n = 0; n != num_messages; ++n) {
		auto mpath = format("%s/maildir-%zu/cur/msg-%zu:2,S",
				    top_maildir.c_str(),
				    n % num_maildirs,
				    n);
		std::ofstream stream(mpath);
		auto msg = message(rx, n);
		stream.write(msg.c_str(), msg.size());
		g_assert_true(stream.good());
	}
}

static void
tear_down()
{
	/* ugly */
	GError *err{};
	const auto cmd{format("/bin/rm -rf '%s' '%s'", BENCH_MAILDIRS, BENCH_STORE)};
	if (!g_spawn_command_line_sync(cmd.c_str(), NULL, NULL, NULL, &err)) {
		g_warning("error: %s\n", err ? err->message : "?");
		g_clear_error(&err);
	}
}

int
main(int argc, char *argv[])
{
	using namespace std::chrono_literals;
	using Clock = std::chrono::steady_clock;

	constexpr size_t NumMaildirs=20;
	constexpr size_t NumMessages=5000;

	setup(NumMaildirs, NumMessages);

	auto start = Clock::now();

	{
		auto store{Store::make_new(BENCH_STORE, BENCH_MAILDIRS, {}, {})};
		g_assert_true(!!store);
		auto res = store->indexer().start({});
		g_assert_true(res);
		while(store->indexer().is_running()) {
			std::this_thread::sleep_for(100ms);
		}
		g_assert_cmpuint(store->size(),==,NumMessages);
	}
	const auto elapsed = Clock::now() - start;
	std::cout << "indexed " << NumMessages << " messages in "
		  << NumMaildirs << " maildirs in "
		  << to_ms(elapsed) << "ms; "
		  << to_us(elapsed) / NumMessages << "us/msg\n";

	tear_down();

	return 0;
}
