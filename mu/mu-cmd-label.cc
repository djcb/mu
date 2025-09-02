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

#include "mu-cmd.hh"

#include <algorithm>
#include <string_view>

#include "mu-store.hh"
#include "message/mu-message.hh"
#include "message/mu-labels.hh"

using namespace Mu;
using namespace Labels;

static Result<void>
label_update(Mu::Store& store, const Options& opts)
{
	// First get our list of parsed delta-label, and ensure they
	// are valid.
	DeltaLabelVec deltas{};
	for (auto&& delta_label : opts.label.delta_labels) {
	  if (const auto res = parse_delta_label(delta_label); !res)
		  return Err(Error{Error::Code::InvalidArgument,
				   "invalid delta-label '{}': {}", delta_label,
				   res.error().what()});
	  else
		  deltas.emplace_back(std::move(*res));
	}

	if (!opts.label.query)
		return Err(Error{Error::Code::Query,
				 "missing query"});

	// now run the query and apply the deltas to each.
	const auto query{*opts.label.query};
	auto results{store.run_query(query)};
	if (!results)
		return Err(Error{Error::Code::Query,
				 "failed to run query '{}': {}", query, *results.error().what()});

	// seems we got some results... let's apply to each
	size_t n{};
	const auto labelstr{join(opts.label.delta_labels, " ")};
	for (auto&& result : *results) {
		if (auto &&msg{result.message()}; msg) {

			if (opts.label.dry_run || opts.verbose)
				mu_println("labels: apply {} to {}", labelstr, msg->path());

			if (!opts.label.dry_run) {
				store.update_labels(*msg, deltas);
			}
			++n;
		}
	}

	if (opts.verbose || opts.label.dry_run)
		mu_println("labels: {}updated {} message(s)",
			   opts.label.dry_run ? "would have " : "", n);

	return Ok();
}

static Result<void>
label_clear(Mu::Store& store, const Options& opts)
{
	if (!opts.label.query)
		return Err(Error{Error::Code::Query,
				 "missing query"});

	const auto query{*opts.label.query};
	auto results{store.run_query(query)};
	if (!results)
		return Err(Error{Error::Code::Query,
				 "failed to run query '{}': {}", query, *results.error().what()});

	size_t n{};
	for (auto&& result : *results) {
		if (auto &&msg{result.message()}; msg) {

			if (opts.label.dry_run || opts.verbose)
				mu_println("labels: clear all from {}", msg->path());

			if (!opts.label.dry_run) {
				store.clear_labels(*msg);
			}
			++n;
		}
	}

	if (opts.verbose || opts.label.dry_run)
		mu_println("labels: {}cleared {} message(s)",
			   opts.label.dry_run ? "would have " : "", n);

	return Ok();
}

static Result<void>
label_list(Mu::Store& store, const Options& opts)
{
	if (opts.label.restore) {
		if (!opts.quiet)
			mu_println("labels: restoring list from store...");
		if (const auto res = store.restore_label_map(); !res)
			return res;
	}

	for (const auto& [label, n]: store.label_map())
		if (opts.verbose)
			mu_println("{}: {}", label, n);
		else
			mu_println("{}", label);

	return Ok();
}

static Result<void>
label_export(const Mu::Store& store, const Options& opts)
{
	const auto res = export_labels(store, "", opts.label.file);
	if (!res)
		return Err(res.error());

	if (!opts.quiet)
		mu_println("written {}", *res);

	return Ok();
}

static Result<void>
label_import(Mu::Store& store, const Options& opts)
{
	// sanity check, should be caught during arg parsing
	if (!opts.label.file)
		return Err(Error{Error::Code::InvalidArgument,
				"missing input file"});

	return Mu::import_labels(store, *opts.label.file,
				     opts.label.dry_run, opts.quiet, opts.verbose);
}

Result<void>
Mu::mu_cmd_label(Mu::Store &store, const Options &opts)
{
	switch (opts.label.sub) {
	case Options::Label::Sub::List:
		return label_list(store, opts);
	case Options::Label::Sub::Update:
		return label_update(store, opts);
	case Options::Label::Sub::Clear:
		return label_clear(store, opts);
	case Options::Label::Sub::Export:
		return label_export(store, opts);
	case Options::Label::Sub::Import:
		return label_import(store, opts);

	default:
		return Err(Error{Error::Code::Internal,
				"invalid sub-command"});
	}
}

#ifdef BUILD_TESTS

/*
 * Tests.
 *
 */
#include <config.h>
#include "utils/mu-test-utils.hh"


static std::string test_mu_home;

static void
test_mu_label_update()
{
	{
		const auto res = run_command({MU_PROGRAM,
			"label", "update", "subject:abc",
			"--labels", "+foo,-bar",
			"--muhome", test_mu_home});
		assert_valid_result(res);
		g_assert_cmpuint(res->exit_code,==,0);
	}

	{
		const auto res = run_command({MU_PROGRAM,
				"find",  "label:foo",
				"--muhome", test_mu_home,});
		assert_valid_result(res);
		g_assert_cmpuint(res->exit_code,==,0);
		g_assert_cmpuint(count_nl(res->standard_out), ==, 2);
	}

	{
		const auto res = run_command({MU_PROGRAM,
				"find", "label:bar",
				 "--muhome", test_mu_home,});
		assert_valid_result(res);
		g_assert_cmpuint(res->exit_code,==,2/*not found*/);
	}

	{
		const auto res = run_command({MU_PROGRAM,
			"label", "update",
			"subject:abc",
			"--labels", "-foo,+bar",
			"--muhome", test_mu_home});
		assert_valid_result(res);
		g_assert_cmpuint(res->exit_code,==,0);
	}

	{
		const auto res = run_command({MU_PROGRAM,
				"find",  "label:foo",
				"--muhome", test_mu_home,});
		assert_valid_result(res);
		g_assert_cmpuint(res->exit_code,==,2/*not found*/);
	}

	{
		const auto res = run_command({MU_PROGRAM,
				"find",  "label:bar",
				"--muhome", test_mu_home});
		assert_valid_result(res);
		g_assert_cmpuint(res->exit_code,==,0);
		g_assert_cmpuint(count_nl(res->standard_out), ==, 2);
	}
}

static void
test_mu_label_clear()
{
	{
		const auto res = run_command({MU_PROGRAM,
			"label", "update", "subject:abc",
			"--labels", "+foo",
			"--muhome", test_mu_home});
		assert_valid_result(res);
		g_assert_cmpuint(res->exit_code,==,0);
	}

	{
		const auto res = run_command({MU_PROGRAM,
				"find",  "label:foo",
				"--muhome", test_mu_home});
		assert_valid_result(res);
		g_assert_cmpuint(res->exit_code,==,0);
		g_assert_cmpuint(count_nl(res->standard_out), ==, 2);
	}
	{
		const auto res = run_command({MU_PROGRAM,
			"label", "clear", "subject:abc",
			"--muhome", test_mu_home});
		assert_valid_result(res);
		g_assert_cmpuint(res->exit_code,==,0);
	}

	{
		const auto res = run_command({MU_PROGRAM,
				"find",  "label:foo",
				"--muhome", test_mu_home});
		assert_valid_result(res);
		g_assert_cmpuint(res->exit_code,==,2/*not found*/);
		g_assert_cmpuint(count_nl(res->standard_out), ==, 0);
	}
}


static void
test_mu_label_list()
{
	{
		const auto res = run_command({MU_PROGRAM,
				"label", "update", "subject:abc",
				"--labels", "+foo,-bar,+cuux,+fnorb",
				"--muhome", test_mu_home});
		assert_valid_result(res);
		g_assert_cmpuint(res->exit_code,==,0);
	}

	{
		const auto res = run_command({MU_PROGRAM,
				"label", "update", "subject:abc",
				"--labels", "-cuux",
				"--muhome", test_mu_home});
		assert_valid_result(res);
		g_assert_cmpuint(res->exit_code,==,0);
	}


	{
		const auto res = run_command({MU_PROGRAM,
				"label", "list",
				"--muhome", test_mu_home});
		assert_valid_result(res);
		g_assert_cmpuint(res->exit_code,==,0);
		g_assert_cmpuint(count_nl(res->standard_out), ==, 2);
		// foo & fnorb
	}
}

static void
test_mu_label_export_import()
{
	TempDir temp_dir{};
	const auto exportfile{join_paths(temp_dir.path(), "export.txt")};

	// ensure  there are some labels (from previous test)
	{
		const auto res = run_command({MU_PROGRAM,
				"label", "list",
				"--muhome", test_mu_home});
		assert_valid_result(res);
		g_assert_cmpuint(res->exit_code,==,0);
		g_assert_cmpuint(count_nl(res->standard_out), ==, 2);
		// foo & fnorb
	}

	// export the current labels; they're from the previous test
	// fnorb,foo
	{
		const auto res = run_command({MU_PROGRAM,
				"label", "export", exportfile,
				"--muhome", test_mu_home});
		assert_valid_result(res);
		g_assert_cmpuint(res->exit_code,==,0);
	}

	// now, re-init / index the store
	{
		auto res = run_command({MU_PROGRAM, "--quiet", "init",
				"--muhome", test_mu_home, "--reinit"});
		assert_valid_result(res);

		auto res2 = run_command({MU_PROGRAM, "--quiet", "index",
				"--muhome", test_mu_home});
		assert_valid_result(res2);
	}

	// ensure the labels are gone.
	{
		const auto res = run_command({MU_PROGRAM,
				"label", "list",
				"--muhome", test_mu_home});
		assert_valid_result(res);
		g_assert_cmpuint(res->exit_code,==,0);
		g_assert_cmpuint(count_nl(res->standard_out), ==, 0);
	}

	// import the labels
	{
		const auto res = run_command({MU_PROGRAM,
				"label", "import", exportfile,
				"--muhome", test_mu_home});
		assert_valid_result(res);
		g_assert_cmpuint(res->exit_code,==,0);
	}

	// ensure the label are back
	{
		const auto res = run_command({MU_PROGRAM,
				"label", "list",
				"--muhome", test_mu_home});
		assert_valid_result(res);
		g_assert_cmpuint(res->exit_code,==,0);
		g_assert_cmpuint(count_nl(res->standard_out), ==, 2);
	}
}




int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	TempDir temp_dir{};
	{
		test_mu_home = temp_dir.path();

		auto res1 = run_command({MU_PROGRAM, "--quiet", "init",
				"--muhome", test_mu_home, "--maildir" , MU_TESTMAILDIR2});
		assert_valid_result(res1);

		auto res2 = run_command({MU_PROGRAM, "--quiet", "index",
				"--muhome", test_mu_home});
		assert_valid_result(res2);
	}

	g_test_add_func("/cmd/label/update", test_mu_label_update);
	g_test_add_func("/cmd/label/clear",  test_mu_label_clear);
	g_test_add_func("/cmd/label/list",   test_mu_label_list);
	g_test_add_func("/cmd/label/export-import",   test_mu_label_export_import);


	return g_test_run();
}

#endif /*BUILD_TESTS*/
