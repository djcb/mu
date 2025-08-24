/*
** Copyright (C) 2025 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-scm.hh"

#include <thread>
#include <unistd.h>
#include <errno.h>

#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include "mu-utils.hh"

#include "mu-scm-types.hh"

#ifdef HAVE_PTHREAD_SETNAME_NP
#include <pthread.h>
#endif

using namespace Mu;
using namespace Mu::Scm;

namespace {
SCM mu_mod; // The mu module
}

/**
 * Create a plist for the relevant option items
 *
 * @param opts
 */
static void
init_options(const Options& opts)
{
	SCM scm_opts = alist_add(SCM_EOL,
				 make_symbol("verbose"), opts.verbose,
				 make_symbol("debug"), opts.debug,
				 make_symbol("quiet"), opts.quiet);

	if (opts.muhome.empty())
		scm_opts = alist_add(scm_opts, make_symbol("mu-home"), SCM_BOOL_F);
	else
		scm_opts = alist_add(scm_opts, make_symbol("mu-home"), opts.muhome);

	scm_c_define("%options", scm_opts);
}

static void
init_misc()
{
	scm_define(make_symbol("level-critical"), to_scm(G_LOG_LEVEL_CRITICAL));
	scm_define(make_symbol("level-warning"), to_scm(G_LOG_LEVEL_WARNING));
	scm_define(make_symbol("level-info"), to_scm(G_LOG_LEVEL_INFO));
	scm_define(make_symbol("level-debug"), to_scm(G_LOG_LEVEL_DEBUG));
}

static SCM
subr_cc_log(SCM level_scm, SCM str_scm) try {
	constexpr auto func{"cc-log"};

	const auto level{static_cast<GLogLevelFlags>(from_scm<int>(level_scm, func, 1))};
	if (level != G_LOG_LEVEL_CRITICAL && level != G_LOG_LEVEL_WARNING &&
		level != G_LOG_LEVEL_INFO && level != G_LOG_LEVEL_DEBUG)
		throw ScmError{ScmError::Id::WrongType, func, 1, level_scm, "level"};

	const auto str{from_scm<std::string>(str_scm, func, 2)};

	g_log("mu-scm", level, "%s", str.c_str());

	return SCM_UNSPECIFIED;

} catch (const ScmError& err) {
	err.throw_scm();
}

static void
init_subrs()
{
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-function-type"
	scm_c_define_gsubr("cc-log", 2/*req*/, 0/*opt*/, 0/*rst*/,
			   reinterpret_cast<scm_t_subr>(subr_cc_log));
#pragma GCC diagnostic pop
}



static const Result<std::string>
make_mu_scm_path(const std::string& fname) {

	const std::string dir = []() {
		if (const char *altpath{::getenv("MU_SCM_DIR")}; altpath)
			return altpath;
		else
			return MU_SCM_DIR;
	}();

	auto fpath{join_paths(dir, fname)};
	if (::access(fpath.c_str(), R_OK) != 0)
		return Err(Error::Code::File, "cannot read {}: {}",
			   fpath, ::strerror(errno));
	else
		return Ok(std::move(fpath));
}

namespace {
std::string mu_scm_path;
std::string mu_scm_repl_path;
std::string mu_scm_socket_path;
constexpr auto SOCKET_PATH_ENV = "MU_SCM_SOCKET_PATH";
using StrVec = std::vector<std::string>;
StrVec scm_args;
std::thread scm_worker;
}

static Result<void>
prepare_run(const Mu::Options& opts)
{
	// do a checks _before_ entering guile, so we get a bit more civilized
	// error message.
	if (const auto path = make_mu_scm_path("mu-scm.scm"); path)
		mu_scm_path = *path;
	else
		return Err(path.error());

	if (const auto path = make_mu_scm_path("mu-scm-repl.scm"); path)
		mu_scm_repl_path = *path;
	else
		return Err(path.error());

	if (opts.scm.script_path) {
		const auto path{opts.scm.script_path->c_str()};
		if (const auto res = ::access(path, R_OK); res != 0) {
			return Err(Error::Code::InvalidArgument,
				   "cannot read '{}': {}", path, ::strerror(errno));
		}
	}

	return Ok();
}

static void
prepare_script(const Options& opts, StrVec& args)
{
	static std::string cmd; // keep alive

	// XXX: couldn't get another combination of -l/-s/-e/-c to work
	// a) invokes `main' with arguments, and
	// b) exits (rather than drop to a shell)
	// but, what works is to manually specify (main ....)
	cmd = "(main " + quote(*opts.scm.script_path);
	for (const auto& scriptarg : opts.scm.params)
		cmd += " " + quote(scriptarg);
	cmd += ")";

	args.emplace_back("-l");
	args.emplace_back(*opts.scm.script_path);
	args.emplace_back("-c");
	args.emplace_back(cmd);
}

static void
maybe_remove_socket_path()
{
	struct stat statbuf{};
	const auto sock{mu_scm_socket_path};

	// opportunistic, so no real warnings, but be careful deleting!

	if (const int res = ::stat(sock.c_str(), &statbuf); res != 0) {
		mu_debug("can't stat '{}'; err={}", sock, -res);
	} else if ((statbuf.st_mode & S_IFMT) != S_IFSOCK) {
		mu_debug("{} is not a socket", sock);
	} else if (const int ulres = ::unlink(sock.c_str()); ulres != 0) {
		mu_debug("failed to unlink '{}'; err={}", sock, -ulres);
	} else {
		mu_debug("unlinked {}", sock);
	}
}

static void
prepare_shell(const Options& opts, StrVec& args)
{
	// drop us into an interactive shell/repl or start listening on a domain socket.
	if (opts.scm.listen && opts.scm.socket_path) {
		mu_scm_socket_path = *opts.scm.socket_path;
		g_setenv(SOCKET_PATH_ENV, mu_scm_socket_path.c_str(), 1);
		mu_info("setting up socket-path {}", mu_scm_socket_path);
		::atexit(maybe_remove_socket_path); //opportunistic cleanup
	}
	else
		g_unsetenv(SOCKET_PATH_ENV);

	args.emplace_back("--no-auto-compile");
	args.emplace_back("-l");
	args.emplace_back(mu_scm_repl_path);
}


struct ModMuData { const Mu::Store& store; const Mu::Options& opts; };

static void
init_module_mu(void* data)
{
	const ModMuData& conf{*reinterpret_cast<ModMuData*>(data)};

	init_options(conf.opts);
	init_misc();
	init_subrs();

	init_store(conf.store);
	init_message();
	init_mime();
}

static void
run_scm(const Mu::Store& store, const Mu::Options& opts)
{
	static ModMuData mu_data{store, opts};

	scm_boot_guile(0, {},
		       [](auto _data, auto _argc, auto _argv) {
			       mu_mod = scm_c_define_module ("mu", init_module_mu, &mu_data);
		std::vector<char*> args;
		std::transform(scm_args.begin(),
			       scm_args.end(), std::back_inserter(args),
			       [&](const std::string& strarg){
				       /* ahem...*/
				       return const_cast<char*>(strarg.c_str());
		});
		scm_shell(args.size(), args.data());

	}, {}); // never returns.
}

Result<void>
Mu::Scm::run(const Mu::Store& store, const Mu::Options& opts, bool blocking)
{
	if (const auto res = prepare_run(opts); !res)
		return Err(res.error());

	scm_args = {"mu", "-l", mu_scm_path};

	// do env stuff _before_ starting guile / threads.
	if (opts.scm.script_path)
		prepare_script(opts, scm_args);
	else
		prepare_shell(opts, scm_args);

	// in the non-blocking case, we start guile in a
	// background thread; otherwise it will block.
	if (!blocking) {
		auto worker = std::thread([&](){
#ifdef HAVE_PTHREAD_SETNAME_NP
			pthread_setname_np(pthread_self(), "mu-scm");
#endif /*HAVE_PTHREAD_SETNAME_NP*/
			run_scm(store, opts);
		});
		worker.detach();
	} else
		run_scm(store, opts);

	return Ok();

}


#ifdef BUILD_TESTS

/*
 * Tests.
 *
 */
#include <config.h>
#include <mu-store.hh>
#include "utils/mu-test-utils.hh"

static void
test_scm_script()
{
	TempDir tempdir{};
	const auto MuTestMaildir{ Mu::canonicalize_filename(MU_TESTMAILDIR, "/")};

	::setenv("MU_TESTTEMPDIR", tempdir.path().c_str(), 1);

	auto store{Store::make_new(tempdir.path(), MuTestMaildir)};
	assert_valid_result(store);

	{
		const auto res = store->indexer().start({}, true/*block*/);
		g_assert_true(res);
	}

	// add some label for testing
	{
		auto res = store->run_query("optimization");
		const Labels::DeltaLabelVec labels{*Labels::parse_delta_label("+performance")};
		assert_valid_result(res);
		g_assert_cmpuint(res->size(), ==, 4);
		for (auto& it: *res) {
			auto msg{it.message()};
			g_assert_true(!!msg);
			const auto updateres{store->update_labels(*msg, labels)};
			assert_valid_result(updateres);
		}
	}

	Mu::Options opts{};
	opts.scm.script_path = join_paths(MU_SCM_SRCDIR, "mu-scm-test.scm");

	{
		const auto res = Mu::Scm::run(*store, opts, false /*blocks*/);
		assert_valid_result(res);
	}
}

int
main(int argc, char* argv[])
{
	::setenv("MU_SCM_DIR", MU_SCM_SRCDIR, 1);
	::setenv("MU_TESTDATADIR", MU_TESTDATADIR, 1);

	mu_test_init(&argc, &argv);

	g_test_add_func("/scm/script", test_scm_script);

	return g_test_run();
}

#endif /*BUILD_TESTS*/
