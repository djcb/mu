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

#include "mu-config.hh"

using namespace Mu;

constexpr /*static*/ bool
validate_props()
{
	size_t id{0};
	for (auto&& prop: Config::properties) {

		// ids must match
		if (static_cast<size_t>(prop.id) != id)
			return false;
		++id;
	}

	return true;
}

#ifdef BUILD_TESTS
#define static_assert g_assert_true
#endif /*BUILD_TESTS*/

[[maybe_unused]]
static void
test_props()
{
	static_assert(validate_props());
}

#ifdef BUILD_TESTS
/*
 * Tests.
 *
 */

#include "utils/mu-test-utils.hh"

static void
test_basic()
{
	MemDb db;
	Config conf_db{db};

	g_assert_false(conf_db.read_only());

	using Id = Config::Id;

	{
		const auto rmd = conf_db.get<Id::RootMaildir>();
		g_assert_true(rmd.empty());
	}

	{
		auto res = conf_db.set<Id::RootMaildir>("/home/djcb/Maildir");
		assert_valid_result(res);

		const auto rmd = conf_db.get<Id::RootMaildir>();
		assert_equal(rmd, "/home/djcb/Maildir");
	}

	{
		g_assert_true(Config::property<Id::BatchSize>().default_val == "50000");
		g_assert_cmpuint(conf_db.get<Id::BatchSize>(),==,50000);

		assert_valid_result(conf_db.set<Id::BatchSize>(123456));
		g_assert_cmpuint(conf_db.get<Id::BatchSize>(),==,123456);
	}


	{
		MemDb db2;
		Config conf_db2{db2};

		g_assert_cmpuint(conf_db2.get<Id::BatchSize>(),==,50000);
		g_assert_true(conf_db2.get<Id::RootMaildir>().empty());

		// BatchSize is configurable; RootMaildir is not.
		conf_db2.import_configurable(conf_db);

		g_assert_cmpuint(conf_db2.get<Id::BatchSize>(),==,123456);
		g_assert_true(conf_db2.get<Id::RootMaildir>().empty());
	}
}

static void
test_read_only()
{
	MemDb db{true/*read-only*/};
	Config conf_db{db};

	auto res = conf_db.set<Config::Id::MaxMessageSize>(12345);
	g_assert_false(!!res);
}

int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	g_test_add_func("/config-db/props", test_props);
	g_test_add_func("/config-db/basic", test_basic);
	g_test_add_func("/config-db/read-only", test_read_only);

	return g_test_run();
}
#endif /*BUILD_TESTS*/
