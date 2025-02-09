/*
** Copyright (C) 2023-2024 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <sys/wait.h>

#include <glib.h>
#include <gio/gio.h>
#include <gio/gunixinputstream.h>

#ifdef HAVE_WORDEXP_H
#include <wordexp.h>
#endif /*HAVE_WORDEXP_H*/

using namespace Mu;


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
		mu_warning ("{}stat failed on {}: {}",
			   use_lstat ? "l" : "", path, g_strerror(errno));
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
Mu::basename(const std::string& path)
{
	return to_string_gchar(g_path_get_basename(path.c_str()));
}

std::string
Mu::dirname(const std::string& path)
{
	return to_string_gchar(g_path_get_dirname(path.c_str()));
}

Result<std::string>
Mu::make_temp_dir()
{
	GError *err{};
	if (auto tmpdir{g_dir_make_tmp("mu-tmp-XXXXXX", &err)}; !tmpdir)
		return Err(Error::Code::File, &err,
			   "failed to create temporary directory");
	else
		return Ok(to_string_gchar(std::move(tmpdir)));
}


Result<void>
Mu::remove_directory(const std::string& path)
{
	/* ugly */
	GError *err{};
	const auto cmd{mu_format("/bin/rm -rf '{}'", path)};
	if (!g_spawn_command_line_sync(cmd.c_str(), NULL,
				       NULL, NULL, &err))
		return Err(Error::Code::File, &err, "failed to remove {}", path);
	else
		return Ok();
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
		/*LCOV_EXCL_START*/
	default:
		throw std::logic_error("unknown path");
		/*LCOV_EXCL_STOP*/
	}
}

/* LCOV_EXCL_START*/
static gpointer
cancel_wait(gpointer data)
{
	guint timeout, deadline;
	GCancellable *cancel;

	cancel	 = (GCancellable*)data;
	timeout	 = GPOINTER_TO_UINT(g_object_get_data(G_OBJECT(cancel), "timeout"));
	deadline = g_get_monotonic_time() + 1000 * timeout;

	while (g_get_monotonic_time() < deadline && !g_cancellable_is_cancelled(cancel)) {
		g_usleep(50 * 1000); /* 50 ms */
		g_thread_yield();
	}

	g_cancellable_cancel(cancel);

	return NULL;
}

static void
cancel_wait_free(gpointer data)
{
	GThread *thread;
	GCancellable *cancel;

	cancel	 = (GCancellable*)data;
	thread   = (GThread*)g_object_get_data(G_OBJECT(cancel), "thread");

	g_cancellable_cancel(cancel);
	g_thread_join(thread);
}

GCancellable*
Mu::g_cancellable_new_with_timeout(guint timeout)
{
	GCancellable *cancel;

	cancel = g_cancellable_new();

	g_object_set_data(G_OBJECT(cancel), "timeout", GUINT_TO_POINTER(timeout));
	g_object_set_data(G_OBJECT(cancel), "thread",
			  g_thread_new("cancel-wait", cancel_wait, cancel));
	g_object_set_data_full(G_OBJECT(cancel), "cancel", cancel, cancel_wait_free);

	return cancel;
}
/* LCOV_EXCL_STOP*/

/* LCOV_EXCL_START*/
Result<std::string>
Mu::read_from_stdin()
{
	g_autoptr(GOutputStream) outmem = g_memory_output_stream_new_resizable();
	g_autoptr(GInputStream) input = g_unix_input_stream_new(STDIN_FILENO, TRUE);
	//g_autoptr(GCancellable) cancel{maybe_cancellable_timeout(timeout)};

	GError *err{};
	auto bytes = g_output_stream_splice(outmem, input,
					    static_cast<GOutputStreamSpliceFlags>
					    (G_OUTPUT_STREAM_SPLICE_CLOSE_SOURCE |
					    G_OUTPUT_STREAM_SPLICE_CLOSE_TARGET),
					    {}, &err);

	if (bytes < 0)
		return Err(Error::Code::File, &err, "error reading from pipe");

	return Ok(std::string{
			static_cast<const char*>(g_memory_output_stream_get_data(
							 G_MEMORY_OUTPUT_STREAM(outmem))),
			g_memory_output_stream_get_size(G_MEMORY_OUTPUT_STREAM(outmem))});
}
/* LCOV_EXCL_STOP*/


/*
 * Set the child to a group leader to avoid being killed when the
 * parent group is killed.
 */
/*LCOV_EXCL_START*/
static void
maybe_setsid (G_GNUC_UNUSED gpointer user_data)
{
#if HAVE_SETSID
	setsid();
#endif /*HAVE_SETSID*/
}
/*LCOV_EXCL_STOP*/

Result<Mu::CommandOutput>
Mu::run_command(std::initializer_list<std::string> args, bool try_setsid)
{
	std::vector<char*> argvec{};
	for (auto&& arg: args)
		argvec.push_back(g_strdup(arg.c_str()));
	argvec.push_back({});

	{
		std::vector<std::string> qargs{};
		for(auto&& arg: args)
			qargs.emplace_back("'" + arg + "'");
		mu_debug("run-command: {}", fmt::join(qargs, "  "));
	}

	GError *err{};
	int wait_status{};
	gchar *std_out{}, *std_err{};
	auto res = g_spawn_sync({},
				static_cast<char**>(argvec.data()),
				{},
				(GSpawnFlags)(G_SPAWN_SEARCH_PATH),
				try_setsid ? maybe_setsid : nullptr, {},
				&std_out, &std_err, &wait_status, &err);

	for (auto& a: argvec)
		g_free(a);

	if (!res)
		return Err(Error::Code::File, &err, "failed to execute command");
	else
		return Ok(Mu::CommandOutput{
				WEXITSTATUS(wait_status),
				to_string_gchar(std::move(std_out/*consumed*/)),
				to_string_gchar(std::move(std_err/*consumed*/))});
}

Result<Mu::CommandOutput>
Mu::run_command0(std::initializer_list<std::string> args, bool try_setsid)
{
	if (auto&& res{run_command(args, try_setsid)}; !res)
		return res;
	else if (res->exit_code != 0)
		return Err(Error::Code::File, "command returned {}: {}",
			   res->exit_code,
			   res->standard_err.empty() ?
			   std::string{"something went wrong"}:
			   res->standard_err);
	else
		return Ok(std::move(*res));
}


Mu::Option<std::string>
Mu::program_in_path(const std::string& name)
{
	if (char *path = g_find_program_in_path(name.c_str()); path)
		return to_string_gchar(std::move(path)/*consumes*/);
	else
		return Nothing;
}


/* LCOV_EXCL_START*/
constexpr auto default_open_program =
#ifdef __APPLE__
		"open"
#else
		"xdg-open"
#endif /*!__APPLE__*/
	;

Mu::Result<void>
Mu::play (const std::string& path)
{
	/* check nativity */
	GFile	*gf	   = g_file_new_for_path(path.c_str());
	auto	 is_native = g_file_is_native(gf);
	g_object_unref(gf);
	if (!is_native)
		return Err(Error::Code::File, "'{}' is not a native file", path);

	auto mpp{g_getenv ("MU_PLAY_PROGRAM")};
	const std::string prog{mpp ? mpp : default_open_program};

	const auto program_path{program_in_path(prog)};
	if (!program_path)
		return Err(Error::Code::File, "cannot find '{}' in path", prog);
	else if (auto&& res{run_command({*program_path, path}, true/*try-setsid*/)}; !res)
		return Err(std::move(res.error()));
	else
		return Ok();
}
/* LCOV_EXCL_STOP*/

Result<std::string>
expand_path_real(const std::string& str)
{
#ifndef HAVE_WORDEXP_H
	return Ok(std::string{str});
#else
	int res;
	wordexp_t result{};

	res = wordexp(str.c_str(), &result, 0);
	if (res != 0)
		return Err(Error::Code::File, "cannot expand {}; err={}", str, res);
	else if (auto&n = result.we_wordc; n != 1) {
		wordfree(&result);
		return Err(Error::Code::File,
			   "expected 1 expansion, but got {} for '{}'", n, str);
	}

	std::string expanded{result.we_wordv[0]};
	wordfree(&result);

	return Ok(std::move(expanded));

#endif /*HAVE_WORDEXP_H*/
}


Result<std::string>
Mu::expand_path(const std::string& str)
{
	if (auto&& res{expand_path_real(str)}; res)
		return res;

	// failed... try quoting.
	auto qstr{to_string_gchar(g_shell_quote(str.c_str()))};
	return expand_path_real(qstr);
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

static void
test_runtime_paths()
{
	TempDir tdir;

	assert_equal(runtime_path(RuntimePath::Cache, tdir.path()), tdir.path());
	assert_equal(runtime_path(RuntimePath::XapianDb, tdir.path()),
		     join_paths(tdir.path(), "xapian"));
	assert_equal(runtime_path(RuntimePath::Bookmarks, tdir.path()),
		     join_paths(tdir.path(), "bookmarks"));
	assert_equal(runtime_path(RuntimePath::Config, tdir.path()), tdir.path());
	assert_equal(runtime_path(RuntimePath::Scripts, tdir.path()),join_paths(tdir.path(), "scripts"));
}


static void
test_expand_path()
{
	g_assert_true(!!g_get_home_dir());
	const std::string homedir{g_get_home_dir()};

	assert_equal(join_paths(homedir, "boo"), expand_path("~/boo").value());
}

int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	/* check_dir */
	g_test_add_func("/utils/file/check-dir-01", test_check_dir_01);
	g_test_add_func("/utils/file/check-dir-02", test_check_dir_02);
	g_test_add_func("/utils/file/check-dir-03", test_check_dir_03);
	g_test_add_func("/utils/file/check-dir-04", test_check_dir_04);
	g_test_add_func("/utils/file/determine-dtype-with-lstat",
			test_determine_dtype_with_lstat);
	g_test_add_func("/utils/file/program-in-path", test_program_in_path);
	g_test_add_func("/utils/file/join-paths", test_join_paths);
	g_test_add_func("/utils/file/runtime-paths", test_runtime_paths);
	g_test_add_func("/utils/file/expand-path", test_expand_path);

	return g_test_run();
}

#endif /*BUILD_TESTS*/
