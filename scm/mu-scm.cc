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

#include "mu-scm.hh"

#include <unistd.h>
#include <errno.h>

#include "mu-utils.hh"
#include "config.h"

#include "mu-scm-types.hh"

using namespace Mu;
using namespace Mu::Scm;

namespace {
static const Mu::Scm::Config *config{};
static SCM mu_mod; // The mu module
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
init_module_mu(void* _data)
{
	init_options(config->options);
	init_store(config->store);
	init_message();
	init_mime();
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
static std::string mu_scm_path;
static std::string mu_scm_shell_path;
}

static Result<void>
prepare_run(const Mu::Scm::Config& conf)
{
	if (config)
		return Err(Error{Error::Code::AccessDenied,
				"already prepared"});
	config = &conf;

	// do a checks _before_ entering guile, so we get a bit more civilized
	// error message.

	if (const auto path = make_mu_scm_path("mu-scm.scm"); path)
		mu_scm_path = *path;
	else
		return Err(path.error());

	if (const auto path = make_mu_scm_path("mu-scm-shell.scm"); path)
		mu_scm_shell_path = *path;
	else
		return Err(path.error());


	if (config->options.scm.script_path) {
		const auto path{config->options.scm.script_path->c_str()};
		if (const auto res = ::access(path, R_OK); res != 0) {
			return Err(Error::Code::InvalidArgument,
				   "cannot read '{}': {}", path, ::strerror(errno));
		}
	}

	return Ok();
}

Result<void>
Mu::Scm::run(const Mu::Scm::Config& conf) {

	if (const auto res = prepare_run(conf); !res)
		return Err(res.error());

	scm_boot_guile(0, {}, [](void *data, int argc, char **argv) {
		mu_mod = scm_c_define_module ("mu", init_module_mu, {});

		std::vector<const char*> args {
			"mu",
			"-l", mu_scm_path.c_str(),
		};
		std::string cmd;
		const auto opts{config->options.scm};
		// if a script-path was specified, run a script
		if (opts.script_path) {
			// XXX: couldn't get another combination of -l/-s/-e/-c to work
			// a) invokes `main' with arguments, and
			// b) exits (rather than drop to a shell)
			// but, what works is to manually specify (main ....)
			cmd = "(main " + quote(*opts.script_path);
			for (const auto& scriptarg : opts.params)
				cmd += " " + quote(scriptarg);
			cmd += ")";
			for (const auto& arg: {
					"-l", opts.script_path->c_str(),
					"-c", cmd.c_str()})
				args.emplace_back(arg);
		} else {
			// otherwise, drop us into an interactive shell/repl (and
			// shell spec)
			args.emplace_back("-l");
			args.emplace_back(mu_scm_shell_path.c_str());
		}
		/* ahem...*/
		scm_shell(std::size(args), const_cast<char**>(args.data()));
	}, {}); // never returns.

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

	Mu::Options opts{};
	opts.scm.script_path = join_paths(MU_SCM_SRCDIR, "mu-scm-test.scm");

	Mu::Scm::Config scm_conf {
		.store = *store,
		.options = opts
	};

	{
		const auto res = Mu::Scm::run(scm_conf);
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
