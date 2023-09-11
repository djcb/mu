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
#include <config.h>

#include <vector>
#include <glib.h>

#include <iostream>
#include <sstream>
#include <unistd.h>

#include "mu-store.hh"
#include "mu-query.hh"
#include "utils/mu-result.hh"
#include "utils/mu-utils.hh"
#include "utils/mu-test-utils.hh"

using namespace Mu;

static void
test_query()
{
	allow_warnings();
	TempDir temp_dir;

	auto store = Store::make_new(temp_dir.path(), std::string{MU_TESTMAILDIR});
	assert_valid_result(store);

	auto&& idx{store->indexer()};
	g_assert_true(idx.start(Indexer::Config{}));
	while (idx.is_running()) {
		g_usleep(1000);
	}

	auto dump_matches = [](const QueryResults& res) {
		size_t n{};
		for (auto&& item : res) {
			if (g_test_verbose()) {
				std::cout << item.query_match() << '\n';
				mu_debug("{:02d} {} {}",
					 ++n,
					 item.path().value_or("<none>"),
					 item.message_id().value_or("<none>"));
			}
		}
	};

	g_assert_cmpuint(store->size(), ==, 19);

	{
		const auto res = store->run_query("", {}, QueryFlags::None);
		g_assert_true(!!res);
		g_assert_cmpuint(res->size(), ==, 19);
		dump_matches(*res);

		g_assert_cmpuint(store->count_query(""), ==, 19);

	}

	{
		const auto res = store->run_query("", Field::Id::Path, QueryFlags::None, 11);
		g_assert_true(!!res);
		g_assert_cmpuint(res->size(), ==, 11);
		dump_matches(*res);
	}
}

int
main(int argc, char* argv[]) try {

	mu_test_init(&argc, &argv);

	g_test_add_func("/query", test_query);

	return g_test_run();

} catch (const std::runtime_error& re) {
	std::cerr << re.what() << "\n";
	return 1;
} catch (...) {
	std::cerr << "caught exception\n";
	return 1;
}
