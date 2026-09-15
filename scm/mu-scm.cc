/*
** Copyright (C) 2025-2026 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <mutex>
#include <memory>
#include <functional>
#include <optional>
#include <unistd.h>
#include <errno.h>

#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include "mu-utils.hh"

#include "mu-scm-types.hh"

#include "mu-config.hh"

#ifdef HAVE_PTHREAD_SETNAME_NP
#include <pthread.h>
#endif

using namespace Mu;
using namespace Mu::Scm;

namespace {
SCM mu_mod; // The mu module
}

/**
 * Create an alist for the relevant option items (i.e., command-line parameters)
 *
 * @param opts
 */
static void
init_options(const Options& opts)
{
	SCM scm_opts = alist_add(SCM_EOL,
				 make_symbol("verbose?"), opts.verbose,
				 make_symbol("debug?"), opts.debug,
				 make_symbol("quiet?"), opts.quiet);

	if (opts.muhome.empty())
		scm_opts = alist_add(scm_opts, make_symbol("mu-home"), SCM_BOOL_F);
	else
		scm_opts = alist_add(scm_opts, make_symbol("mu-home"), opts.muhome);


	scm_c_define("%options", scm_reverse_x(scm_opts, SCM_EOL));
}

/**
 * Create an alist for the relevant system-configuration parameters
 */
static void
init_configuration()
{
	SCM conf{SCM_EOL};

	for (const auto& prop: Config::properties) {

		if (none_of(prop.flags & Property::Flags::System))
			continue;

	        const auto val{Config::default_value(prop)};
		if (!val)
			continue;

		switch(prop.type) {
		case Config::Type::Boolean:
			conf = alist_add(conf, make_symbol(mu_format("{}?", prop.name, "?")),
					 Config::decode<Config::Type::Boolean>(*val));
			break;
		case Config::Type::String:
			conf = alist_add(conf, make_symbol(prop.name), *val);
			break;
		default:
			// other types are not used here yet.
			break;
		}
        }

	scm_c_define("%configuration", scm_reverse_x(conf, SCM_EOL));
}

static void
init_fields_info()
{
	SCM fields_scm{SCM_EOL};

	const auto search_type = [&](const Field& field)->SCM {
		if (field.is_boolean_term())
			return make_symbol("boolean");
		else if (field.is_phrasable_term())
			return make_symbol("phrase");
		else if (field.is_contact())
			return make_symbol("contact");
		else if (field.is_range())
			return make_symbol("range");
		else
			return SCM_BOOL_F;
	};

	field_for_each([&](const auto& field) {
		SCM field_scm = alist_add(SCM_EOL,
					  make_symbol("field"), make_symbol(field.name),
					  make_symbol("name"),   field.name,
					  make_symbol("shortcut"),
					  field.shortcut ? to_scm(field.shortcut) : SCM_BOOL_F,
					  make_symbol("value?"), field.is_value(),
					  make_symbol("search-type"), search_type(field));
		fields_scm = scm_cons(scm_reverse_x(field_scm, SCM_EOL), fields_scm);
	});

	scm_c_define("%fields", scm_reverse_x(fields_scm, SCM_EOL));
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
}

/**
 * Resolve the paths to the mu-scm Scheme sources; idempotent.
 *
 * Done as a check _before_ entering guile, so we get a bit more civilized
 * error message in case something's missing.
 */
static Result<void>
prepare_paths()
{
	if (!mu_scm_path.empty())
		return Ok(); // already resolved.

	if (const auto path = make_mu_scm_path("mu-scm.scm"); path)
		mu_scm_path = *path;
	else
		return Err(path.error());

	if (const auto path = make_mu_scm_path("mu-scm-repl.scm"); path)
		mu_scm_repl_path = *path;
	else
		return Err(path.error());

	return Ok();
}

namespace {

/**
 * Run fn (some code that calls into Scheme) and catch any Scm-conditions
 * Call from Guile-mode, i.e. from within a function invoked via
 * scm_with_guile().
 */
struct GuardedBody   { const std::function<void()>& fn; };
struct GuardedCatch  { bool errored{}; std::string message; };

SCM
guarded_body(void *data)
{
	reinterpret_cast<GuardedBody*>(data)->fn();
	return SCM_UNSPECIFIED;
}

std::string
scm_to_display_string(SCM obj)
{
	SCM port{scm_open_output_string()};
	scm_display(obj, port);
	size_t len{};
	auto *cstr{scm_to_utf8_stringn(scm_get_output_string(port), &len)};
	std::string str{cstr, len};
	::free(cstr);
	return str;
}

std::string
format_condition(SCM key, SCM args)
{
	// Attempt to massage guile error into something printable.
	if (scm_is_true(scm_list_p(args)) && scm_is_true(scm_eq_p(scm_length(args), scm_from_int(4))) &&
	    scm_is_string(scm_cadr(args))) {
		const SCM formatted{scm_simple_format(SCM_BOOL_F, scm_cadr(args), scm_caddr(args))};
		return scm_to_display_string(key) + ": " + scm_to_display_string(formatted);
	}

	return scm_to_display_string(key) + ": " + scm_to_display_string(args);
}

SCM
guarded_handler(void *data, SCM key, SCM args)
{
	// (exit ...)/(quit ...) work by throwing a 'quit condition, handle
	// here.
	if (scm_is_eq(key, scm_from_utf8_symbol("quit"))) {
		const SCM code_scm = scm_is_pair(args) ? scm_car(args) : SCM_UNDEFINED;
		const auto code = scm_is_integer(code_scm) ? scm_to_int(code_scm) : 0;
		scm_flush_all_ports();
		::exit(code);
	}

	auto& caught{*reinterpret_cast<GuardedCatch*>(data)};
	caught.errored = true;
	caught.message = format_condition(key, args);

	return SCM_BOOL_F;
}

Result<void>
guarded(const std::function<void()>& fn)
{
	GuardedBody  body{fn};
	GuardedCatch caught{};

	scm_c_catch(SCM_BOOL_T,
		   guarded_body, &body,
		   guarded_handler, &caught,
		   nullptr, nullptr);

	if (caught.errored)
		return Err(Error::Code::Script, "{}", caught.message);

	return Ok();
}

} // namespace

static void
maybe_remove_socket_path()
{
	struct stat statbuf{};
	const auto sock{mu_scm_socket_path};

	// opportunistic, so no real warnings, but be careful deleting!

	if (::stat(sock.c_str(), &statbuf) != 0) {
		mu_debug("can't stat '{}': {}", sock, ::strerror(errno));
	} else if ((statbuf.st_mode & S_IFMT) != S_IFSOCK) {
		mu_debug("{} is not a socket", sock);
	} else if (::unlink(sock.c_str()) != 0) {
		mu_debug("failed to unlink '{}': {}", sock, ::strerror(errno));
	} else {
		mu_debug("unlinked {}", sock);
	}
}

struct ModMuData { const Mu::Store& store; const Mu::Options& opts; };

static void
init_module_mu(void* data)
{
	const ModMuData& conf{*reinterpret_cast<ModMuData*>(data)};

	init_options(conf.opts);
	init_configuration();
	init_fields_info();

	init_misc();
	init_subrs();

	init_store(conf.store);
	init_message();
	init_mime();
}

namespace {

std::once_flag                guile_boot_once;
std::optional<Result<void>>   guile_boot_result;

void*
boot_trampoline(void *data)
{
	auto& mu_data{*reinterpret_cast<ModMuData*>(data)};

	guile_boot_result.emplace(guarded([&]{
		mu_mod = scm_c_define_module("mu", init_module_mu, &mu_data);
		scm_c_primitive_load(mu_scm_path.c_str());
	}));

	return nullptr;
}

} // namespace

/**
 * Boot Guile and define/load the "mu" module, exactly once per process
 * MT-safe / idempotent; returns to caller.
 */
static Result<void>
ensure_booted(const Mu::Store& store, const Mu::Options& opts)
{
	if (const auto res = prepare_paths(); !res)
		return Err(res.error());

	static ModMuData mu_data{store, opts};

	std::call_once(guile_boot_once, [&]{
		scm_with_guile(boot_trampoline, &mu_data);
	});

	return *guile_boot_result;
}

namespace {

struct ReplData { std::vector<std::string> args; };

void*
run_repl_trampoline(void *data)
{
	auto& rd{*reinterpret_cast<ReplData*>(data)};

	scm_set_current_module(mu_mod);

	std::vector<char*> argv;
	argv.reserve(rd.args.size());
	std::ranges::transform(rd.args, std::back_inserter(argv),
			       [](const std::string& arg){
				       /* ahem...*/
				       return const_cast<char*>(arg.c_str());
			       });
	scm_shell(static_cast<int>(argv.size()), argv.data());

	return nullptr;
}

} // namespace

Result<void>
Mu::Scm::run_repl(const Mu::Store& store, const Mu::Options& opts,
		  const std::string& socket_path)
{
	if (const auto res = ensure_booted(store, opts); !res)
		return Err(res.error());

	auto rd{std::make_shared<ReplData>()};
	rd->args = {"mu", "--no-auto-compile", "-l", mu_scm_repl_path};

	if (!socket_path.empty()) {
		mu_scm_socket_path = socket_path;
		g_setenv(SOCKET_PATH_ENV, mu_scm_socket_path.c_str(), 1);
		mu_info("setting up socket-path {}", mu_scm_socket_path);
		::atexit(maybe_remove_socket_path); //opportunistic cleanup

		// if a socket-path is provided, run in a background thread
		// and offer a REPL on a Unix domain socket on said socket_path.
		// `rd` is kept alive by the shared_ptr captured in the thread.
		auto worker = std::thread([rd](){
			set_thread_name("mu-scm");
			scm_with_guile(run_repl_trampoline, rd.get());
		});
		worker.detach();
	} else { // otherwise, a normal, interactive shell
		g_unsetenv(SOCKET_PATH_ENV);
		scm_with_guile(run_repl_trampoline, rd.get());
	}

	return Ok();
}

namespace {

struct ScriptData {
	const std::string&          script_path;
	const StringVec&            params;
	bool                        run_main;
	std::optional<Result<void>> result;
};

void*
run_script_trampoline(void *data)
{
	auto& sd{*reinterpret_cast<ScriptData*>(data)};

	sd.result.emplace(guarded([&]{

		scm_set_current_module(mu_mod);
		scm_c_primitive_load(sd.script_path.c_str());

		if (!sd.run_main)
			return;

		const SCM main_proc{scm_variable_ref(scm_c_lookup("main"))};

		StringVec args{sd.script_path};
		args.insert(args.end(), sd.params.begin(), sd.params.end());

		scm_apply_0(main_proc, to_scm(args));
	}));

	return nullptr;
}

} // namespace

Result<void>
Mu::Scm::run_script(const Mu::Store& store, const Mu::Options& opts,
		    const std::string& script_path, bool run_main)
{
	if (script_path.empty())
		return Err(Error::Code::InvalidArgument, "missing script path");

	if (const auto res = ::access(script_path.c_str(), R_OK); res != 0)
		return Err(Error::Code::InvalidArgument,
			   "cannot read '{}': {}", script_path, ::strerror(errno));

	if (const auto res = ensure_booted(store, opts); !res)
		return Err(res.error());

	ScriptData sd{script_path, opts.scm.params, run_main, {}};
	scm_with_guile(run_script_trampoline, &sd);

	return *sd.result;
}

namespace {

struct EvalData {
	const std::string&          expr;
	std::optional<Result<void>> result;
};

void*
run_eval_trampoline(void *data)
{
	auto& ed{*reinterpret_cast<EvalData*>(data)};

	ed.result.emplace(guarded([&]{
		scm_c_eval_string_in_module(ed.expr.c_str(), mu_mod);
	}));

	return nullptr;
}

} // namespace

Result<void>
Mu::Scm::run_eval(const Mu::Store& store, const Mu::Options& opts, const std::string& expr)
{
	if (const auto res = ensure_booted(store, opts); !res)
		return Err(res.error());

	EvalData ed{expr, {}};
	scm_with_guile(run_eval_trampoline, &ed);

	return *ed.result;
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

	MemDb mdb;
	Config conf{mdb};
	assert_valid_result(conf.set<Config::Id::PersonalAddresses>(std::vector<std::string>{"user@example.com"}));

	auto store{Store::make_new(tempdir.path(), MuTestMaildir, conf)};
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
	{
		const auto script_path{join_paths(MU_SCM_SRCDIR, "mu-scm-test.scm")};
		const auto res = Mu::Scm::run_script(*store, opts, script_path, true/*run main*/);
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
