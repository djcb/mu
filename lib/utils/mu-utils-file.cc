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

#include "config.h"

#include "mu-utils.hh"
#include "mu-utils-file.hh"

#include <sys/stat.h>

#include <glib.h>
#include <gio/gio.h>

#ifdef HAVE_WORDEXP_H
#include <wordexp.h>
#endif /*HAVE_WORDEXP_H*/

using namespace Mu;


Mu::Option<std::string>
Mu::program_in_path(const std::string& name)
{
	if (char *path = g_find_program_in_path(name.c_str()); path)
		return to_string_gchar(std::move(path)/*consumes*/);
	else
		return Nothing;
}

/*
 * Set the child to a group leader to avoid being killed when the
 * parent group is killed.
 */
static void
maybe_setsid (G_GNUC_UNUSED gpointer user_data)
{
#if HAVE_SETSID
	setsid();
#endif /*HAVE_SETSID*/
}

Mu::Result<void>
Mu::play (const std::string& path)
{
	/* check nativity */
	GFile	*gf	   = g_file_new_for_path(path.c_str());
	auto	 is_native = g_file_is_native(gf);
	g_object_unref(gf);
	if (!is_native)
		return Err(Error::Code::File, "'%s' is not a native file", path.c_str());

	const char *prog{g_getenv ("MU_PLAY_PROGRAM")};
	if (!prog) {
#ifdef __APPLE__
		prog = "open";
#else
		prog = "xdg-open";
#endif /*!__APPLE__*/
	}

	const auto program_path{program_in_path(prog)};
	if (!program_path)
		return Err(Error::Code::File, "cannot find '%s' in path", prog);

	const gchar *argv[3]{};
	argv[0] = program_path->c_str();
	argv[1] = path.c_str();
	argv[2] = nullptr;

	GError *err{};
	if (!g_spawn_async ({}, (gchar**)&argv, {}, G_SPAWN_SEARCH_PATH, maybe_setsid,
			    {}, {}, &err))
		return Err(Error::Code::File, &err/*consumes*/, "failed to open '%s' with '%s'",
			   path. c_str(), program_path->c_str());

	return Ok();
}


bool
Mu::check_dir (const std::string& path, bool readable, bool writeable)
{
	const auto mode = F_OK | (readable ? R_OK : 0) | (writeable ? W_OK : 0);

	if (::access (path.c_str(), mode) != 0)
		return false;

	struct stat statbuf{};
	if (::stat (path.c_str(), &statbuf) != 0)
		return false;

	return S_ISDIR(statbuf.st_mode) ? true : false;
}

uint8_t
Mu::determine_dtype (const std::string& path, bool use_lstat)
{
	int res;
	struct stat statbuf{};

	if (use_lstat)
		res = ::lstat(path.c_str(), &statbuf);
	else
		res = ::stat(path.c_str(), &statbuf);

	if (res != 0) {
		g_warning ("%sstat failed on %s: %s",
			   use_lstat ? "l" : "", path.c_str(), g_strerror(errno));
		return DT_UNKNOWN;
	}

	/* we only care about dirs, regular files and links */
	if (S_ISREG (statbuf.st_mode))
		return DT_REG;
	else if (S_ISDIR (statbuf.st_mode))
		return DT_DIR;
	else if (S_ISLNK (statbuf.st_mode))
		return DT_LNK;

	return DT_UNKNOWN;
}

std::string
Mu::canonicalize_filename(const std::string& path, const std::string& relative_to)
{
	auto str{to_string_opt_gchar(
		g_canonicalize_filename(
			path.c_str(),
			relative_to.empty() ? nullptr : relative_to.c_str())).value()};

	// remove trailing '/'... is this needed?
	if (str[str.length()-1] == G_DIR_SEPARATOR)
		str.erase(str.length() - 1);

	return str;
}


std::string
Mu::runtime_path(Mu::RuntimePath path, const std::string& muhome)
{
	auto [mu_cache, mu_config] =
		std::invoke([&]()->std::pair<std::string, std::string> {
			if (muhome.empty())
				return { join_paths(g_get_user_cache_dir(), "mu"),
					 join_paths(g_get_user_config_dir(), "mu")};
			else
				return { muhome, muhome };
	});

	switch (path) {
	case Mu::RuntimePath::Cache:
		return mu_cache;
	case Mu::RuntimePath::XapianDb:
		return join_paths(mu_cache, "xapian");
	case Mu::RuntimePath::LogFile:
		return join_paths(mu_cache, "mu.log");
	case Mu::RuntimePath::Bookmarks:
		return join_paths(mu_config, "bookmarks");
	case Mu::RuntimePath::Config:
		return mu_config;
	case Mu::RuntimePath::Scripts:
		return join_paths(mu_config, "scripts");
	default:
		throw std::logic_error("unknown path");
	}
}


Result<std::string>
Mu::expand_path(const std::string& str)
{
#ifndef HAVE_WORDEXP_H
	return Ok(std::string{str});
#else
	int res;
	wordexp_t result;
	memset(&result, 0, sizeof(result));

	res = wordexp(str.c_str(), &result, 0);
	if (res != 0 || result.we_wordc == 0)
		return Err(Error::Code::File, "cannot expand '%s'; err=%d", str.c_str(), res);

	std::string expanded{result.we_wordv[0]};
	wordfree(&result);

	return Ok(std::move(expanded));

#endif /*HAVE_WORDEXP_H*/
}



#ifdef BUILD_TESTS

/*
 * Tests.
 *
 */

#include <glib/gstdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "utils/mu-test-utils.hh"

static void
test_check_dir_01(void)
{
	if (g_access("/usr/bin", F_OK) == 0) {
		g_assert_cmpuint(
		    check_dir("/usr/bin", true, false) == true,
		    ==,
		    g_access("/usr/bin", R_OK) == 0);
	}
}

static void
test_check_dir_02(void)
{
	if (g_access("/tmp", F_OK) == 0) {
		g_assert_cmpuint(
		    check_dir("/tmp", false, true) == true,
		    ==,
		    g_access("/tmp", W_OK) == 0);
	}
}

static void
test_check_dir_03(void)
{
	if (g_access(".", F_OK) == 0) {
		g_assert_cmpuint(
		    check_dir(".", true, true) == true,
		    ==,
		    g_access(".", W_OK | R_OK) == 0);
	}
}

static void
test_check_dir_04(void)
{
	/* not a dir, so it must be false */
	g_assert_cmpuint(
	    check_dir("test-util.c", true, true),
	    ==,
	    false);
}

static void
test_determine_dtype_with_lstat(void)
{
	g_assert_cmpuint(
		determine_dtype(MU_TESTMAILDIR, true), ==, DT_DIR);
	g_assert_cmpuint(
		determine_dtype(MU_TESTMAILDIR2, true), ==, DT_DIR);
	g_assert_cmpuint(
		determine_dtype(MU_TESTMAILDIR2 "/Foo/cur/mail5", true),
		==, DT_REG);
}


static void
test_program_in_path(void)
{
	g_assert_true(!!program_in_path("ls"));
}

static void
test_join_paths()
{

	assert_equal(join_paths(), "");
	assert_equal(join_paths("a"), "a");
	assert_equal(join_paths("a", "b"), "a/b");
	assert_equal(join_paths("/a/b///c/d//", "e"), "/a/b/c/d/e");
}



int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	/* check_dir */
	g_test_add_func("/utils/check-dir-01",
			test_check_dir_01);
	g_test_add_func("/utils/check-dir-02",
			test_check_dir_02);
	g_test_add_func("/utils/check-dir-03",
			test_check_dir_03);
	g_test_add_func("/utils/check-dir-04",
			test_check_dir_04);
	g_test_add_func("/utils/determine-dtype-with-lstat",
			test_determine_dtype_with_lstat);
	g_test_add_func("/utils/program-in-path",
			test_program_in_path);
	g_test_add_func("/utils/join-paths",
			test_join_paths);

	return g_test_run();
}

#endif /*BUILD_TESTS*/
