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

#include <istream>
#include <sstream>

#include "mu-store.hh"

using namespace Mu;

namespace {
constexpr std::string_view	path_key       = "path:";
constexpr std::string_view	message_id_key = "message-id:";
constexpr std::string_view	labels_key     = "labels:";
}

using OutputPair = std::pair<std::ofstream, std::string>;

static Result<OutputPair>
export_output(Option<std::string> path)
{
	const auto now_t{::time({})};
	const auto now_tm{::localtime(&now_t)};
	const auto now{mu_format("{:%F-%T}", *now_tm)};

	// if path is not specified, use a generated file name (in pwd)
	// if path is specified but ends in '/', use the generated file in that
	// directory (must exist)
	// otherwise, use the path.
	auto fname = [&]() {
		const auto default_fname{mu_format("mu-export-{}.txt", now)};
		if (!path || path->empty())
			return default_fname;
		else if (path->at(path->length() - 1) == '/')
			return *path + default_fname;
		else
			return *path;
	}();

	auto output{std::ofstream{fname, std::ios::out}};
	if (!output.good())
		return Err(Error{Error::Code::File,
				"failed to open '{}' for writing", fname});

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

namespace Levels {
using Level = size_t;
constexpr Level quiet = 1 << 0;
constexpr Level verbose = 1 << 1;
constexpr Level error = 1 << 0;
}

using Level = Levels::Level;

template<typename...T>
static void output(Level level,
		   fmt::format_string<T...> frm, T&&... args) noexcept {

	GLogLevelFlags lflags = level & Levels::error ?
		G_LOG_LEVEL_WARNING : G_LOG_LEVEL_DEBUG;
	mu_log(lflags, frm, std::forward<T>(args)...);

	if ((level & Levels::error))
		mu_printerrln(frm, std::forward<T>(args)... );
	else if ((level & Levels::verbose))
		mu_println(frm, std::forward<T>(args)... );

}

static Result<QueryResults>
import_get_matching(Mu::Store& store, const std::string& query, int max=1)
{
	if (auto qres = store.run_query(query, {}, {}, max); !qres)
		return Err(std::move(qres.error()));
	else if (qres->empty())
		return Err(Error{Error::Code::Query,
				"no match for or '{}'", query});
	else
		return Ok(std::move(*qres));
}


static void
import_labels_for_message(Mu::Store& store, bool dry_run, Level level,
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
		if (auto qres_a{import_get_matching(store, "path:" + path)}; !qres_a) {
			output(level, "path '{}' does not match; try message-id",
			       qres_a.error().what());
			// plan B: try the message-id
			auto qres_b{import_get_matching(store, "msgid:" + msgid, -1/*all matching*/)};
			if (!qres_b) { // plan-B failed too?
				output(level | Levels::error,
				       "import failed: cannot find message by path '{}' or message-id '{}'",
				       path, msgid);
			}
			return qres_b;
		} else
			return qres_a;
	}();

	// neither plan a or b worked? we have to give up...
	if (!qres) {
		return;
	}

	mu_println("{} matches", qres->size());

	// we have match(es)!
	for (auto&& item: *qres) {
		auto msg{*item.message()};
		if (dry_run )
			output(level, "{}: would have applied labels {}", msg.path(), join(labels, ","));
		else if (const auto res = store.update_labels(msg, delta_labels); !res)
			output(level | Levels::error, "failed to update labels for {}: {}",
			       msg.path(), res.error().what());
		else
			output(level, "{}: applied labels {}", msg.path(), join(labels, ","));
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

	const Level level{(quiet ? Levels::quiet : 0) | (verbose ? Levels::verbose : 0)};

	while (std::getline(input, line)) {

		if (line.find(path_key) ==  0)
			current_path = line.substr(path_key.length());
		else if (line.find(message_id_key) == 0)
			current_msgid = line.substr(message_id_key.length());
		else if (line.find(labels_key) == 0) {
			current_labels = split(line.substr(labels_key.length()), ',');
			if (!current_labels.empty())
				import_labels_for_message(store, dry_run, level,
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
