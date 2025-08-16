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

#include "mu-store-labels.hh"
#include "mu-store.hh"
#include "message/mu-labels.hh"

using namespace Mu;

namespace {
constexpr std::string_view path_key = "path:";
constexpr std::string_view message_id_key = "message-id:";
constexpr std::string_view labels_key = "labels:";
}

using OutputPair = std::pair<std::ofstream, std::string>;

static Result<OutputPair>
export_output(Option<std::string> path)
{
	const auto now_t{::time({})};
	const auto now_tm{::localtime(&now_t)};

	const auto now{mu_format("{:%F-%T}", *now_tm)};
	auto fname = path.value_or(mu_format("mu-export-{}.txt", now));

	auto output{std::ofstream{fname, std::ios::out}};
	if (!output.good())
		return Err(Error{Error::Code::File,
				"failed pen '{}' for writing", fname});

	mu_println(output, ";; version:0 @ {}\n", now);

	return Ok(OutputPair{std::move(output), std::move(fname)});
}

Result<std::string>
Mu::export_labels(const Store& store, const std::string& query, Option<std::string> path)
{
	const auto results{store.run_query(query)};
	if (!results)
		return Err(Error{Error::Code::Query,
				 "failed to run query '{}': {}",
				 query, *results.error().what()});

	auto output_res = export_output(path);
	if (!output_res)
		return Err(std::move(output_res.error()));

	auto&[output, output_path] = *output_res;

	for (auto&& result : *results) {
		if (auto &&msg{result.message()}; msg) {
			if (const auto labels{msg->labels()}; !labels.empty()) {
				mu_print(output,
					 "{}{}\n"
					 "{}{}\n"
					 "{}{}\n\n",
					 path_key,  msg->path(),
					 message_id_key, msg->message_id(),
					 labels_key, join(labels,','));
			}
		}
	}

	return Ok(std::move(output_path));
}

static void
log_import(bool quiet, bool verbose, const std::string& msg, bool is_err=false)
{
	if (is_err)
		mu_debug("{}", msg);
	else
		mu_warning("{}", msg);

	if (is_err && !quiet)
		mu_printerrln("{}", msg);
	else if (verbose)
		mu_println("{}", msg);
}

static void
log_import_err(bool quiet, bool verbose, const std::string& msg)
{
	log_import(quiet, verbose, msg, true);
}


static Result<QueryResults>
log_import_get_matching(Mu::Store& store, const std::string& query, int max=1)
{
	if (auto qres = store.run_query(query, {}, {}, max); !qres)
		return Err(std::move(qres.error()));
	else if (qres->empty())
		return Err(Error{Error::Code::Query,
				"no matching messages for {}", query});
	else
		return Ok(std::move(*qres));
}


static void
import_labels_for_message(Mu::Store& store,
			  bool dry_run, bool quiet, bool verbose,
			  const std::string& path, const std::string& msgid,
			  const std::vector<std::string> labels)
{
	using namespace Labels;

	Labels::DeltaLabelVec delta_labels{};
	std::transform(labels.begin(), labels.end(),
		       std::back_inserter(delta_labels),
		       [](const auto& label) {
			       return DeltaLabel{Delta::Add, label}; });

	const auto qres = [&]()->Result<QueryResults>{
		// plan A: match by path
		if (auto qres_a{log_import_get_matching(store, "path:" + path)}; !qres_a) {
			log_import_err(quiet, verbose, mu_format("failed to find by path: {}; try with message-id",
				       qres_a.error().what()));
			// plan B: try the message-id
			return log_import_get_matching(store, "msgid:" + msgid, -1/*all matching*/);
		} else
			return qres_a;
	}();

	// neither plan a or b worked? we have to give up...
	if (!qres) {
		log_import_err(quiet, verbose, qres.error().what());
		return;
	}

	// we have match(es)!
	for (auto&& item: *qres) {
		auto msg{*item.message()};
		if (dry_run )
			mu_println("labels: would apply label '{}' to {}", join(labels, ","), path);
		else if (const auto res = store.update_labels(msg, delta_labels); !res)
			log_import_err(quiet, verbose,
				       mu_format("failed to update labels for {}: {}",
						 msg.path(), res.error().what()));
		else
			log_import(quiet, verbose,
				   mu_format("applied labels {} to {}", join(labels, ","), path));
	}
}

Result<void>
Mu::import_labels(Mu::Store& store, const std::string& path, bool dry_run, bool quiet, bool verbose)
{
	auto input{std::ifstream{path, std::ios::in}};
	if (!input.good())
		return Err(Error{Error::Code::File,
				"failed to open '{}' for reading",
				path});

	std::string line;
	std::string current_path, current_msgid;
	std::vector<std::string> current_labels;

	while (std::getline(input, line)) {

		if (line.find(path_key) ==  0)
			current_path = line.substr(path_key.length());
		else if (line.find(message_id_key) == 0)
			current_msgid = line.substr(message_id_key.length());
		else if (line.find(labels_key) == 0) {
			current_labels = split(line.substr(labels_key.length()), ',');
			if (!current_labels.empty())
				import_labels_for_message(store, dry_run, quiet, verbose,
							current_path, current_msgid,
							current_labels);
			current_path.clear();
			current_msgid.clear();
			current_labels.clear();
		}
		// ignore anything else.
	}

	return Ok();
}
