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

#include "mu-query-macros.hh"

#include <glib.h>
#include <unordered_map>

#include "utils/mu-utils.hh"

using namespace Mu;

constexpr auto MU_BOOKMARK_GROUP = "mu";

struct QueryMacros::Private {
	Private(const Config& conf): conf_{conf} {}


	Result<void> import_key_file(GKeyFile *kfile);

	const Config& conf_;
	std::unordered_map<std::string, std::string> macros_{};
};

Result<void>
QueryMacros::Private::import_key_file(GKeyFile *kfile)
{
	if (!kfile)
		return Err(Error::Code::InvalidArgument, "invalid key-file");

	GError *err{};
	size_t num{};
	gchar **keys{g_key_file_get_keys(kfile, MU_BOOKMARK_GROUP, &num, &err)};
	if (!keys)
		return Err(Error::Code::File, &err/*cons*/,"failed to read keys");

	for (auto key = keys; key && *key; ++key) {

		auto rawval{g_key_file_get_string(kfile, MU_BOOKMARK_GROUP, *key, &err)};
		if (!rawval) {
			g_strfreev(keys);
			return Err(Error::Code::File, &err/*cons*/,"failed to read key '{}'", *key);
		}

		auto val{to_string_gchar(std::move(rawval))};
		macros_.erase(val); // we want to replace
		macros_.emplace(std::string(*key), std::move(val));
		++num;
	}

	g_strfreev(keys);
	mu_debug("imported {} query macro(s); total {}", num, macros_.size());
	return Ok();
}

QueryMacros::QueryMacros(const Config& conf):
	priv_{std::make_unique<Private>(conf)} {}

QueryMacros::~QueryMacros() = default;

Result<void>
QueryMacros::load_bookmarks(const std::string& path)
{
	GError *err{};
	GKeyFile *kfile{g_key_file_new()};
	if (!g_key_file_load_from_file(kfile, path.c_str(), G_KEY_FILE_NONE, &err)) {
		g_key_file_unref(kfile);
		return Err(Error::Code::File, &err/*cons*/,
			   "failed to read bookmarks from {}", path);
	}

	auto&& res = priv_->import_key_file(kfile);
	g_key_file_unref(kfile);

	return res;
}

Option<std::string>
QueryMacros::find_macro(const std::string& name) const
{
	if (const auto it{priv_->macros_.find(name)}; it != priv_->macros_.end())
		return it->second;
	else
		return Nothing;
}


#ifdef BUILD_TESTS
/*
 * Tests.
 *
 */

#include "utils/mu-test-utils.hh"
#include "utils/mu-utils-file.hh"

static void
test_bookmarks()
{
	MemDb db;
	Config conf_db{db};
	QueryMacros qm{conf_db};

	TempDir tdir{};
	const auto bmfile{join_paths(tdir.path(), "bookmarks.ini")};
	std::ofstream os{bmfile};

	mu_println(os, "# test\n"
		   "[mu]\n"
		   "foo=subject:bar");
	os.close();

	auto res = qm.load_bookmarks(bmfile);
	assert_valid_result(res);

	assert_equal(qm.find_macro("foo").value_or(""), "subject:bar");
	assert_equal(qm.find_macro("bar").value_or("nope"), "nope");
}


static void
test_bookmarks_fail()
{

	MemDb db;
	Config conf_db{db};
	QueryMacros qm{conf_db};

	auto res = qm.load_bookmarks("/foo/bar/non-existent");
	g_assert_false(!!res);
}


int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	g_test_add_func("/query/macros/bookmarks", test_bookmarks);
	g_test_add_func("/query/macros/bookmarks-fail", test_bookmarks_fail);

	return g_test_run();
}
#endif /*BUILD_TESTS*/
