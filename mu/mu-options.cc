/*
** Copyright (C) 2022-2026 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

/**
 * @brief Command-line handling
 *
 * Here we implement mu's command-line parsing based on the CLI11 library.
 *
 * At the time of writing, that library seems to be the best based on the criteria
 * that it supports the features we need and is available as a header-only
 * include.
 *
 * CLI11 can do quite a bit, and we're only scratching the surface here,
 * plan is to slowly improve things.
 */

#include <config.h>
#include <algorithm>
#include <stdexcept>
#include <array>
#include <unordered_map>
#include <iostream>
#include <string_view>
#include <unistd.h>

#include <utils/mu-utils.hh>
#include <utils/mu-utils-file.hh>
#include <utils/mu-error.hh>
#include "utils/mu-test-utils.hh"
#include "mu-options.hh"
#include "mu-script.hh"

#ifdef USE_EMBEDDED_CLI11
#include "CLI11.hpp"
#else
#include "CLI/CLI.hpp"
#endif

using namespace Mu;

/*
 * helpers
 */
namespace {
/**
 * Element of an array of associated pair elements -- like an alist
 * entry; make the array with std::to_array so its size is deduced.
 */
template<typename T1, typename T2>
      using AssocPair = std::pair<T1, T2>;

/**
  * Get the first value of the pair where the second element is @param s.
  *
  * @param p AssocPairs
  * @param s some second pair value
  *
  * @return the matching first pair value, or Nothing if not found.
  */
template<typename P>
constexpr Option<typename P::value_type::first_type>
to_first(const P& p, typename P::value_type::second_type s)
{
	for (const auto& item: p)
		if (item.second == s)
			return item.first;
	return Nothing;
}

/**
  * Get the second value of the pair where the first element is @param f.
  *
  * @param p AssocPairs
  * @param f some first pair value
  *
  * @return the matching second pair value, or Nothing if not found.
  */
template<typename P>
constexpr Option<typename P::value_type::second_type>
to_second(const P& p, typename P::value_type::first_type f)
{
	for (const auto& item: p)
		if (item.first == f)
			return item.second;
	return Nothing;
}

/**
 * Options-specific pair type that maps some enum to a <name, description> pair
 */
template<typename T>
using InfoPair = AssocPair<T, std::pair<std::string_view, std::string_view>>;

/**
 * Get map from string->type
 */
template<typename IE>
std::unordered_map<std::string, typename IE::value_type::first_type>
options_map(const IE& ie)
{
	std::unordered_map<std::string, typename IE::value_type::first_type> map;
	for (auto&& item : ie)
		map.emplace(std::string{item.second.first}, item.first);

	return map;
}

/**
 * Get the list of option names (shortnames), in declaration order
 */
template<typename IE>
std::vector<std::string>
options_names(const IE& ie)
{
	std::vector<std::string> names;
	names.reserve(ie.size());
	for (auto&& item : ie)
		names.emplace_back(item.second.first);

	return names;
}

/**
 * Friendly error messages for some options (see add_choice_option,
 * add_number_option), used for rewriting CLI11's rather terse errors.
 */
struct FriendlyError {
	const CLI::App		*sub;	 /**< subcommand owning the option */
	const CLI::Option	*opt;	 /**< the option */
	std::string		 errmsg; /**< friendly error message */
};
std::vector<FriendlyError> friendly_errors;

/**
 * Get a friendly error message for some error.
 *
 * @param err a parse error
 *
 * @return the error message
 */
std::string
friendly_error(const CLI::ParseError& err)
{
	const std::string what{err.what()};
	for (const auto& info: friendly_errors) {
		if (!info.sub->parsed())
			continue;
		const auto name{info.opt->get_name()};
		// the CLI11 messages we know how to improve upon
		for (const auto pat: { ": 1 required",	    // no value given
				       ": requires one of", // bad choice value
				       ": Value " })	    // bad numeric value
			if (what.starts_with(name + pat))
				return info.errmsg;
	}
	return what;
}

/**
 * Add an option to sub that takes its value from a fixed set of choices.
 *
 * Like CLI11's CheckedTransformer, but with friendlier help and error
 * messages, e.g.
 *   error: --format requires one of { plain, links }; default is plain
 *
 * @param sub command to add the option to
 * @param name option name(s), e.g. "--format,-o"
 * @param value target for the parsed value
 * @param help help text; the choices and default are appended
 * @param type_name name for the value in the help, e.g. "<format>"
 * @param choices map from choice-name to value
 * @param choice_names the choice names; displayed in alphabetical order
 * @param default_name name of the default choice; must occur in @p choices
 *
 * @return the newly added option
 */
template<typename T>
CLI::Option*
add_choice_option(CLI::App& sub, const std::string& name, T& value,
		  const std::string& help, const std::string& type_name,
		  std::unordered_map<std::string, T> choices,
		  std::vector<std::string> choice_names,
		  const std::string& default_name)
{
	std::ranges::sort(choice_names);
	const auto choices_str{"{ " + join(choice_names, ", ") + " }"};

	auto opt = sub.add_option(name, value,
				  mu_format("{}; one of {}; default is {}",
					    help, choices_str, default_name))
		->type_name(type_name)
		->default_val(choices.at(default_name))
		->default_str(default_name);

	// add the transform only after default_val(), so the default does not
	// go through the transform (CLI11 applies it at default_val() time).
	opt->transform([choices = std::move(choices),
			errmsg = mu_format("requires one of {}; default is {}",
					   choices_str, default_name)]
		       (const std::string& val) -> std::string {
		if (const auto it = choices.find(val); it != choices.end())
			return std::to_string(
				static_cast<std::underlying_type_t<T>>(it->second));
		// normally rewritten by friendly_choice_error()
		throw CLI::ValidationError{errmsg};
	});

	friendly_errors.push_back(
		{&sub, opt, mu_format("{} requires one of {}; default is {}",
				      opt->get_name(), choices_str, default_name)});
	return opt;
}

/** The kind of number a numeric option accepts */
enum struct Numeric { Positive, NonNegative };

/**
 * Add an option to @p sub that takes a numeric value.
 *
 * Like CLI11's PositiveNumber/NonNegativeNumber checks, but with
 * friendlier error messages, e.g.
 *   error: --maxnum requires a positive number
 *
 * @param sub command to add the option to
 * @param name option name(s), e.g. "--maxnum,-n"
 * @param value target for the parsed value
 * @param help help text
 * @param type_name name for the value in the help, e.g. "<number>"
 * @param kind the kind of number the option accepts
 *
 * @return the newly added option
 */
template<typename T>
CLI::Option*
add_number_option(CLI::App& sub, const std::string& name, T& value,
		  const std::string& help, const std::string& type_name,
		  Numeric kind)
{
	const auto positive{kind == Numeric::Positive};

	auto opt = sub.add_option(name, value, help)
		->type_name(type_name)
		->check(positive ? CLI::PositiveNumber : CLI::NonNegativeNumber);

	friendly_errors.push_back(
		{&sub, opt, mu_format("{} requires a {} number", opt->get_name(),
				      positive ? "positive" : "non-negative")});
	return opt;
}

// transformers

// Expand the path using wordexp
const std::function ExpandPath = [](const std::string& path)->std::string {
	if (auto&& res{expand_path(path)}; !res)
		throw CLI::ValidationError{res.error().what()};
	else
		return res.value();
};

// Canonicalize path
const std::function CanonicalizePath = [](const std::string& path)->std::string {
	return canonicalize_filename(path);
};

} // end of anonymous namespace

/*
 * common
 */

template<typename T>
static void
sub_crypto(CLI::App& sub, T& opts)
{
	sub.add_flag("--auto-retrieve,-r", opts.auto_retrieve,
		     "Attempt to automatically retrieve online keys");
	sub.add_flag("--decrypt", opts.decrypt,
		     "Attempt to decrypt");
}

static void add_muhome_option(CLI::App& sub, Options& opts)
{
	sub.add_option("--muhome",
		       opts.muhome, "Specify alternative mu directory")
		->envname("MUHOME")
		->type_name("<dir>")
		->transform(ExpandPath, "expand muhome path");
}

/*
 * subcommands
 */

static void
sub_add(CLI::App& sub, Options& opts)
{
	sub.add_option("files", opts.add.files,
		       "Path(s) to message files(s)")
		->required();
}

static void
sub_cfind(CLI::App& sub, Options& opts)
{
	using Format = Options::Cfind::Format;
	static constexpr auto FormatInfos = std::to_array<InfoPair<Format>>({
			{ Format::Plain,           {"plain", "Plain output"} },
			{ Format::MuttAlias,       {"mutt-alias", "Mutt alias"} },
			{ Format::MuttAddressBook, {"mutt-ab", "Mutt address book"}},
			{ Format::Wanderlust,      {"wl", "Wanderlust"}},
			{ Format::OrgContact,      {"org-contact", "org-contact"}},
			{ Format::Bbdb,            {"bbdb", "Emacs BBDB"}},
			{ Format::Csv,             {"csv", "comma-separated values"}},
			{ Format::Json,            {"json", "format as json array"}},
		});

	add_choice_option(sub, "--format,-o", opts.cfind.format,
			  "Output format", "<format>",
			  options_map(FormatInfos), options_names(FormatInfos),
			  "plain");

	sub.add_option("pattern", opts.cfind.rx_pattern,
		       "Regular expression pattern to match");
	sub.add_flag("--personal,-p", opts.cfind.personal,
		       "Only show 'personal' contacts");
	add_number_option(sub, "--after", opts.cfind.after,
			  "Only show results after some timestamps",
			  "<time_t>", Numeric::NonNegative);
	add_number_option(sub, "--maxnum,-n", opts.cfind.maxnum,
			  "Maximum number of results",
			  "<number>", Numeric::Positive);
}


static void
sub_extract(CLI::App& sub, Options& opts)
{
	sub_crypto(sub, opts.extract);

	sub.add_flag("--save-attachments,-a", opts.extract.save_attachments,
		     "Save all attachments");
	sub.add_flag("--save-all", opts.extract.save_all, "Save all MIME parts")
		->excludes("--save-attachments");
	sub.add_flag("--overwrite", opts.extract.overwrite,
		     "Overwrite existing files");
	sub.add_flag("--play", opts.extract.play,
		     "Attempt to open the extracted parts");
	sub.add_option("--parts", opts.extract.parts,
		       "Save specific parts (comma-sep'd list)")
		->type_name("<parts>")->delimiter(',');
	sub.add_option("--target-dir", opts.extract.targetdir,
		       "Target directory for saving")
		->type_name("<dir>")
		->transform(ExpandPath, "expand target path")
		->default_str("<current>")
		->default_val(".");
	sub.add_flag("--uncooked,-u", opts.extract.uncooked,
		       "Avoid massaging extracted file-names");
	// optional; otherwise use standard-input
	sub.add_option("message-path", opts.extract.message,
		       "Path to message file")
		->type_name("<message-path>");

	sub.add_option("--matches", opts.extract.filename_rx,
		       "Regular expression for files to save")
		->type_name("<filename-rx>")
		->excludes("--parts")
		->excludes("--save-attachments")
		->excludes("--save-all");

	// backward compat: filename-rx as non-option
	sub.add_option("filename-rx", opts.extract.filename_rx,
		       "Regular expression for files to save")
		->type_name("<filename-rx>")
		->excludes("--parts")
		->excludes("--save-attachments")
		->excludes("--matches")
		->excludes("--save-all");
}

static void
sub_fields(CLI::App& sub, Options& opts)
{
	// nothing to do.
}

static void
sub_find(CLI::App& sub, Options& opts)
{
	using Format = Options::Find::Format;
	static constexpr auto FormatInfos = std::to_array<InfoPair<Format>>({
			{ Format::Plain,
			  {"plain", "Plain output"}
			},
			{ Format::Links,
			  {"links", "Maildir with symbolic links"}
			},
			{ Format::Xml,
			  {"xml", "XML"}
			},
			{ Format::Sexp,
			  {"sexp", "S-expressions"}
			},
			{ Format::Json,
			  {"json", "JSON"}
			},
			{ Format::Json2,
			  {"json2", "more idiomatic JSON"}
			},
		});

	sub.add_flag("--threads,-t", opts.find.threads,
		     "Show message threads");
	sub.add_flag("--skip-dups,-u", opts.find.skip_dups,
		     "Show only one of messages with same message-id");
	sub.add_flag("--include-related,-r", opts.find.include_related,
		     "Include related messages in results");
	sub.add_flag("--analyze,-a", opts.find.analyze,
		     "Analyze the query");

	add_choice_option(sub, "--format,-o", opts.find.format,
			  "Output format", "<format>",
			  options_map(FormatInfos), options_names(FormatInfos),
			  "plain");

	add_number_option(sub, "--maxnum,-n", opts.find.maxnum,
			  "Maximum number of results",
			  "<number>", Numeric::Positive);

	sub.add_option("--fields,-f", opts.find.fields,
		       "Fields to display")
		->default_val("d f s");

	std::unordered_map<std::string, Field::Id> smap;
	std::vector<std::string> snames;
	field_for_each([&](auto&& field){
		if (field.is_sortable()) {
			smap.emplace(std::string(field.name), field.id);
			smap.emplace(std::string(1, field.shortcut), field.id);
			snames.emplace_back(mu_format("{}|{}", field.name,
						      field.shortcut));
		}
	});
	add_choice_option(sub, "--sortfield,-s", opts.find.sortfield,
			  "Field to sort the results by", "<field>",
			  std::move(smap), std::move(snames), "date");

	sub.add_flag("--reverse,-z", opts.find.reverse,
		     "Sort in descending order");

	sub.add_option("--bookmark,-b", opts.find.bookmark,
		       "Use bookmarked query")
		->type_name("<bookmark>");

	sub.add_flag("--clearlinks", opts.find.clearlinks,
		     "Clear old links first");
	sub.add_option("--linksdir", opts.find.linksdir,
		       "Target directory for symlinks")
		->type_name("<dir>")
		->transform(ExpandPath, "expand linksdir path");

	add_number_option(sub, "--after", opts.find.after,
			  "Only show messages whose message file was changed "
			  "after some timestamp",
			  "<time_t>", Numeric::NonNegative);

	add_number_option(sub, "--summary-len", opts.find.summary_len,
			  "Use up to so many lines for the summary",
			  "<lines>", Numeric::Positive);

	sub.add_option("--exec", opts.find.exec,
		       "Command to execute on message file")
		->type_name("<command>");

	sub.add_option("query", opts.find.query,
		       "Search query pattern(s)")
		->type_name("<query>");
}

static void
sub_help(CLI::App& sub, Options& opts)
{
	sub.add_option("command", opts.help.command,
		       "Command to request help for")
		->type_name("<command>");
}

static void
sub_index(CLI::App& sub, Options& opts)
{
	sub.add_flag("--lazy-check", opts.index.lazycheck,
		       "Skip based on dir-timestamps");
	sub.add_flag("--nocleanup", opts.index.nocleanup,
		       "Don't clean up database after indexing");
	sub.add_flag("--reindex", opts.index.reindex,
		     "Perform a complete reindexing");
}

static void
sub_info(CLI::App& sub, Options& opts)
{
	sub.add_option("topic", opts.info.topic,
		       "Information topic")
		->type_name("<topic>") ;
}

static void
sub_init(CLI::App& sub, Options& opts)
{
	const auto default_mdir = std::invoke([]()->std::string {
		if (const auto mdir_env{::getenv("MAILDIR")}; mdir_env)
			return mdir_env;
		else if (const auto mdir_home = ::join_paths(g_get_home_dir(), "Maildir");
			 check_dir(mdir_home))
			return mdir_home;
		else
			return {};
	});

	sub.add_option("--maildir,-m", opts.init.maildir, "Root maildir")
		->type_name("<maildir>")
		->default_val(default_mdir)
		->transform(ExpandPath, "expand maildir path");
	// don't attempt to canonicalize; in bash this breaks together with
	// expand path.
	sub.add_option("--personal-address,--my-address",
		       opts.init.personal_addresses,
		       "Personal e-mail address or regexp (can be used multiple times)")
		->type_name("<address>");
	sub.add_option("--ignored-address", opts.init.ignored_addresses,
		       "Ignored e-mail address or regexp")
		->type_name("<address>");

	sub.add_option("--max-message-size", opts.init.max_msg_size,
		       "Maximum allowed message size in bytes");
	sub.add_option("--batch-size", opts.init.batch_size,
		       "Maximum size of database transaction");
	sub.add_flag("--support-ngrams", opts.init.support_ngrams,
		     "Support CJK n-grams for querying/indexing");
	sub.add_flag("--reinit", opts.init.reinit,
		       "Re-initialize database with current settings")
		->excludes("--maildir")
		->excludes("--personal-address")
		->excludes("--my-address")
		->excludes("--ignored-address")
		->excludes("--max-message-size")
		->excludes("--batch-size")
		->excludes("--support-ngrams");
}

static void
sub_labels(CLI::App& sub, Options& opts)
{
	sub.require_subcommand(0, 1);

	auto update{sub.add_subcommand("update", "update labels")};
	update->add_option("--labels", opts.labels.delta_labels,
		       "One or more comma-separated +label,-label")
		->delimiter(',')
		->type_name("<delta-label>")
		->required();
	update->add_flag("-n,--dry-run", opts.labels.dry_run,
			 "Output what would change without changing anything");
	update->add_option("query", opts.labels.query, "Query for messages to update")
		->required();
	add_muhome_option(*update, opts);

	auto clear = sub.add_subcommand("clear", "clear all labels from matched messages");
	clear ->add_option("query", opts.labels.query, "Query for messages to clear of labels")
		->required();
	clear->add_flag("-n,--dry-run", opts.labels.dry_run,
		     "Output what would change without changing anything");
	add_muhome_option(*clear, opts);

	[[maybe_unused]] auto list = sub.add_subcommand("list", "list labels in the store");
	add_muhome_option(*list, opts);

	[[maybe_unused]] auto restore_list = sub.add_subcommand(
		"restore-list", "restore the labels cache");
	add_muhome_option(*restore_list, opts);

	[[maybe_unused]] auto exportsub = sub.add_subcommand("export", "export labels to a file");
	add_muhome_option(*exportsub, opts);
	exportsub->add_option("output", opts.labels.file, "File to export labels to")
		->type_name("<file>");

	auto importsub = sub.add_subcommand("import", "import labels from a file");
	importsub->add_flag("-n,--dry-run", opts.labels.dry_run,
			    "Output what would change without changing anything");
	importsub->add_option("input", opts.labels.file, "File with labels to import")
		->required()
		->type_name("<file>");
	add_muhome_option(*importsub, opts);

	sub.final_callback([&](){
		if (sub.got_subcommand("list")) {
			opts.labels.sub = Options::Labels::Sub::List;
			opts.labels.read_only = true;
		} else if (sub.got_subcommand("restore-list")) {
			opts.labels.sub = Options::Labels::Sub::RestoreList;
			opts.labels.read_only  = false;/*opts.labels.dry_run*/
		} else if (sub.got_subcommand("clear")) {
			opts.labels.sub = Options::Labels::Sub::Clear;
			opts.labels.read_only = opts.labels.dry_run;
		} else if (sub.got_subcommand("update")){
			opts.labels.sub = Options::Labels::Sub::Update;
			opts.labels.read_only = opts.labels.dry_run;
		} else if (sub.got_subcommand("export")){
			opts.labels.sub = Options::Labels::Sub::Export;
			opts.labels.read_only = true;
		} else if (sub.got_subcommand("import")){
			opts.labels.sub = Options::Labels::Sub::Import;
			opts.labels.read_only = opts.labels.dry_run;
		} else {
			mu_println("{}", sub.help());
			opts.labels.read_only = true;
		}
	});
}

static void
sub_mkdir(CLI::App& sub, Options& opts)
{
	sub.add_option("--mode", opts.mkdir.mode, "Set the access mode (octal)")
		->default_val(0755)
		->type_name("<mode>");

	sub.add_option("dirs", opts.mkdir.dirs, "Path to directory/ies")
		->type_name("<dir>")
		->required();
}

static void
sub_move(CLI::App& sub, Options& opts)
{
	sub.add_flag("--change-name", opts.move.change_name,
		     "Change name of target file");
	sub.add_flag("--update-dups", opts.move.update_dups,
		     "Update duplicate messages too");
	sub.add_flag("--dry-run,-n", opts.move.dry_run,
		     "Print target name, but do not change anything");

	sub.add_option("--flags", opts.move.flags, "Target flags")
		->type_name("<flags>");

	sub.add_option("source", opts.move.src, "Message file to move")
		->type_name("<message-path>")
		->transform(ExpandPath, "expand source path")
		->required();
	sub.add_option("destination", opts.move.dest,
		       "Destination maildir")
		->type_name("<maildir>");
}

static void
sub_remove(CLI::App& sub, Options& opts)
{
	sub.add_option("files", opts.remove.files,
		       "Paths to message files to remove")
		->type_name("<files>");
}

static std::string
concoct_socket_path()
{
	return join_paths(g_get_user_runtime_dir(),
			  mu_format("mu-scm-{}.sock", ::getpid()));
}

static void
add_listen_flag(CLI::App& sub, Options& opts)
{
	sub.add_flag("--listen", opts.scm.listen,
		     "Start SCM REPL on a domain socket");
	sub.callback([&opts]{
		if (opts.scm.listen)
			opts.scm.socket_path = concoct_socket_path();
	});
}

static void
sub_server(CLI::App& sub, Options& opts)
{
	sub.add_flag("--commands", opts.server.commands,
		       "List available commands");
	sub.add_option("--eval", opts.server.eval,
		       "Evaluate mu server expression")
		->excludes("--commands");
	sub.add_flag("--allow-temp-file", opts.server.allow_temp_file,
		     "Allow for the temp-file optimization")
		->excludes("--commands");

#if BUILD_SCM
	add_listen_flag(sub, opts);
#endif/*BUILD_SCM*/
}

static void
sub_scm(CLI::App& sub, Options& opts)
{
	add_listen_flag(sub, opts);

	sub.add_option("script-path", opts.scm.script_path, "Path to script")
		->type_name("<path>")
		->excludes("--listen");
	sub.add_option("script-args", opts.scm.params, "Parameters for script")
		->type_name("<parameters>");
	sub.add_option("--eval", opts.scm.eval, "Expression to evaluate")
		->excludes("--listen")
		->excludes("script-path");
}

static void
sub_verify(CLI::App& sub, Options& opts)
{
	sub_crypto(sub, opts.verify);

	// optional; otherwise use standard-input
	sub.add_option("message-paths", opts.verify.files,
		       "Message files to verify")
		->type_name("<message-path>");
}

static void
sub_view(CLI::App& sub, Options& opts)
{
	using Format = Options::View::Format;
	static constexpr auto FormatInfos = std::to_array<InfoPair<Format>>({
			{ Format::Plain,
			  {"plain", "Plain output"}
			},
			{ Format::Html,
			  {"html", "Plain output with HTML body"}
			},
			{ Format::Sexp,
			  {"sexp", "S-expressions"}
			},
		});

	add_choice_option(sub, "--format,-o", opts.view.format,
			  "Output format", "<format>",
			  options_map(FormatInfos), options_names(FormatInfos),
			  "plain");

	sub_crypto(sub, opts.view);

	add_number_option(sub, "--summary-len", opts.view.summary_len,
			  "Use up to so many lines for the summary",
			  "<lines>", Numeric::Positive);

	sub.add_flag("--terminate", opts.view.terminate,
		     "Insert form-feed after each message");

	// optional; otherwise use standard-input
	sub.add_option("message-paths", opts.view.files,
		       "Message files to view")
		->type_name("<message-path>");
}

using	SubCommand = Options::SubCommand;
using   Category   = Options::Category;

struct CommandInfo {
	Category		category;
	std::string_view	name;
	std::string_view	help;

	// std::function is not constexp-friendly
	using setup_func_t = void(*)(CLI::App&, Options&);
	setup_func_t setup_func{};
};

static constexpr auto SubCommandInfos =
	std::to_array<AssocPair<SubCommand, CommandInfo>>({
		{ SubCommand::Add,
		  { Category::NeedsWritableStore,
		    "add", "Add messages to the database", sub_add}
		},
		{ SubCommand::Cfind,
		  { Category::NeedsReadOnlyStore,
		    "cfind", "Find contacts matching some pattern", sub_cfind}
		},
		{ SubCommand::Extract,
		  {Category::None,
		  "extract", "Extract attachments and other MIME-parts", sub_extract}
		},
		{ SubCommand::Fields,
		  {Category::None,
		  "fields", "Superseded by 'mu info'", sub_fields}
		},
		{ SubCommand::Find,
		  {Category::NeedsReadOnlyStore,
		  "find", "Find messages matching some query", sub_find }
		},
		{ SubCommand::Help,
		  {Category::None,
		  "help", "Show help information", sub_help }
		},
		{ SubCommand::Index,
		  {Category::NeedsWritableStore,
		  "index", "Scan maildirs and store information", sub_index }
		},
		{ SubCommand::Info,
		  {Category::NeedsReadOnlyStore,
		  "info", "Show information about mu", sub_info }
		},
		{ SubCommand::Init,
		  {Category::NeedsWritableStore,
		  "init", "Initialize the mu database", sub_init }
		},
		{ SubCommand::Labels,
		  {Category::None, // note Store handled on sub-subcommmands
		  "labels", "Manage message labels", sub_labels }
		},
		{ SubCommand::Mkdir,
		  {Category::None,
		  "mkdir", "Create a new Maildir", sub_mkdir }
		},
		{ SubCommand::Move,
		  {Category::NeedsWritableStore,
		   "move", "Move a message or change its flags", sub_move }
		},
		{ SubCommand::Remove,
		  {Category::NeedsWritableStore,
		  "remove", "Remove message from file-system and database", sub_remove }
		},
		{ SubCommand::Scm,
		  {Category::NeedsReadOnlyStore,
		   "scm", "Start Guile/Scheme shell or run script",sub_scm}
		},
		{ SubCommand::Script,
		  // Note: SubCommand::Script is special; there's no literal
		  // "script" subcommand, there are subcommands for all the scripts.
		  {Category::None,
		   "script", "Invoke a script", {}}
		},
		{ SubCommand::Server,
		  {Category::NeedsWritableStore,
		  "server", "Start a mu server (for mu4e)", sub_server}
		},
		{ SubCommand::Verify,
		  {Category::None,
		  "verify", "Verify cryptographic signatures", sub_verify}
		},
		{ SubCommand::View,
		  {Category::None,
		  "view", "View specific messages", sub_view}
		},
	});
static_assert(SubCommandInfos.size() == Options::SubCommandNum,
	      "SubCommandInfos must have an entry for each subcommand");

static ScriptInfos
add_scripts(CLI::App& app, Options& opts)
{
#ifndef BUILD_GUILE
	return {};
#else
	 ScriptPaths paths = { MU_SCRIPTS_DIR };
	 auto scriptinfos{script_infos(paths)};
	 for (auto&& script: scriptinfos) {
		 auto&& sub = app.add_subcommand(script.name)->group("Scripts")
			 ->description(script.oneline);
		 sub->add_option("params", opts.script.params,
				 "Parameter to script")
			 ->type_name("<params>");
	 }

	 return scriptinfos;
#endif /*BUILD_GUILE*/
}

static Result<Options>
show_manpage(Options& opts, const std::string& name)
{
	const auto manprog{program_in_path("man")};
	if (!manprog)
		return Err(Error::Code::Command,
			   "cannot find 'man' program");

	GError* err{};
	const auto cmd{mu_format("{} {}", *manprog, shell_quote(name))};
	// run_command0 doesn't work here.
	int wait_status{};
	auto res = g_spawn_command_line_sync(cmd.c_str(), {}, {},
					     &wait_status, &err);
	if (!res || !g_spawn_check_wait_status(wait_status, &err))
		return Err(Error::Code::Command, &err,
			   "error running man command");

	return Ok(std::move(opts));
}

static Result<Options>
cmd_help(const CLI::App& app, Options& opts)
{
	if (opts.help.command.empty()) {
		mu_println("{}", app.help());
		return Ok(std::move(opts));
	}

	for (auto&& item: SubCommandInfos) {
		if (item.second.name == opts.help.command)
			return show_manpage(opts, "mu-" + opts.help.command);
	}

	for (auto&& item: {"query", "easy"})
		if (item == opts.help.command)
			return show_manpage(opts, "mu-" + opts.help.command);

	return Err(Error::Code::Command,
		   "no help available for '{}'", opts.help.command);
}

bool
Options::default_no_color()
{
	static const auto no_color =
		!::isatty(::fileno(stdout)) ||
		!::isatty(::fileno(stderr)) ||
		::getenv("NO_COLOR") != NULL;

	return no_color;
}

static void
add_global_options(CLI::App& cli, Options& opts)
{
	opts.nocolor = Options::default_no_color();

	cli.add_flag("-q,--quiet", opts.quiet, "Hide non-essential output");
	cli.add_flag("-v,--verbose", opts.verbose, "Show verbose output");
	cli.add_flag("--log-stderr", opts.log_stderr, "Log to stderr")
		->group(""/*always hide*/);
	cli.add_flag("--nocolor", opts.nocolor, "Don't show ANSI colors")
		->default_val(Options::default_no_color())
		->default_str(Options::default_no_color() ? "<true>" : "<false>");
	cli.add_flag("-d,--debug", opts.debug, "Run in debug mode")
		->group(""/*always hide*/);
}

Result<Options>
Options::make(int argc, char *argv[])
{
	Options opts{};
	CLI::App app{"mu mail indexer/searcher " PACKAGE_VERSION, "mu"};

	friendly_errors.clear(); // entries refer to the previous app, if any.

	app.description(R"(mu mail indexer/searcher
Copyright (C) 2008-2025 Dirk-Jan C. Binnema

License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.)");

	app.set_version_flag("-V,--version", PACKAGE_VERSION);
	app.set_help_flag("-h,--help", "Show help information");
	app.set_help_all_flag("--help-all");
	app.require_subcommand(0, 1);

	add_global_options(app, opts);

	/*
	 * subcommands
	 *
	 * we keep around a map of the subcommand pointers, so we can
	 * easily find the chosen one (if any) later.
	 */
	for (auto&& cmdinfo: SubCommandInfos) {
		//const auto cmdtype = cmdinfo.first;
		const auto name{std::string{cmdinfo.second.name}};
		const auto help{std::string{cmdinfo.second.help}};
		const auto setup{cmdinfo.second.setup_func};
		const auto cat{category(cmdinfo.first)};

		if (!setup)
			continue;

		auto sub = app.add_subcommand(name, help);
		setup(*sub, opts);

		/* allow global options _after_ subcommand as well;
		 * this is for backward compat with the older
		 * command-line parsing */
		sub->fallthrough(true);

		/* store commands get the '--muhome' parameter as well */
		if (cat == Category::NeedsReadOnlyStore ||
		    cat == Category::NeedsWritableStore)
			add_muhome_option(*sub, opts);
	}

	/* add scripts (if supported) as semi-subcommands as well */
	const auto scripts = add_scripts(app, opts);

	try {
		app.parse(argc, argv);

		// find the chosen sub command,  if any.
		for (auto&& cmdinfo: SubCommandInfos) {
			if (cmdinfo.first == SubCommand::Script)
				continue; // not a _real_ subcommand.
			const auto name{std::string{cmdinfo.second.name}};
			if (app.got_subcommand(name)) {
				opts.sub_command = cmdinfo.first;
			}
		}

		// otherwise, perhaps it's a script?
		if (!opts.sub_command) {
			for (auto&& info: scripts) { // find the chosen script, if any.
				if (app.got_subcommand(info.name)) {
					opts.sub_command = SubCommand::Script;
					opts.script.name = info.name;
				}
			}
		}

		// if nothing else, try "help"
		if (opts.sub_command.value_or(SubCommand::Help) == SubCommand::Help)
			return cmd_help(app, opts);

	} catch (const CLI::CallForHelp& cfh) {
		mu_println("{}", app.help());
	} catch (const CLI::CallForAllHelp& cfah) {
		mu_println("{}", app.help("", CLI::AppFormatMode::All));
	} catch (const CLI::CallForVersion&) {
		mu_println("version {}", PACKAGE_VERSION);
	} catch (const CLI::ExtrasError& xe) {
		/* a first unexpected non-option argument without any subcommand
		 * is (likely) a mistyped or unknown command */
		if (const auto extras{app.remaining()};
		    app.get_subcommands().empty() && !extras.empty() &&
		    !extras.front().starts_with('-'))
			return Err(Error::Code::InvalidArgument,
				   "'{}' is not a mu command. See 'mu --help'",
				   extras.front());
		return Err(Error::Code::InvalidArgument, "{}", xe.what());
	} catch (const CLI::ParseError& pe) {
		return Err(Error::Code::InvalidArgument, "{}",
			   friendly_error(pe));
	}  catch (...) {
		return Err(Error::Code::Internal, "error parsing arguments");
	}

	return Ok(std::move(opts));
}

Category
Options::category(Options::SubCommand sub)
{
	for (auto&& item: SubCommandInfos)
		if (item.first == sub)
			return item.second.category;

	return Category::None;
}

/*
 * trust but verify
 */

static constexpr bool
validate_subcommand_ids()
{
	size_t val{};
	for (auto& cmd: Options::SubCommands)
		if (static_cast<size_t>(cmd) != val++)
			return false;

	for (auto u = 0U; u != SubCommandInfos.size(); ++u)
		if (static_cast<size_t>(SubCommandInfos.at(u).first) != u)
			return false;
	return true;
}

/*
 * tests... also build as runtime-tests, so we can get coverage info
 */
#ifdef BUILD_TESTS
#define static_assert g_assert_true
#endif /*BUILD_TESTS*/

[[maybe_unused]]
static void
test_ids()
{
	static_assert(validate_subcommand_ids());
}

#ifdef BUILD_TESTS

enum struct TestEnum { A, B, C };
constexpr auto test_epairs =
	std::to_array<AssocPair<TestEnum, std::string_view>>({
	{TestEnum::A, "a"},
	{TestEnum::B, "b"},
	{TestEnum::C, "c"},
});

static constexpr Option<std::string_view>
to_name(TestEnum te)
{
	return to_second(test_epairs, te);
}

static constexpr Option<TestEnum>
to_type(std::string_view name)
{
	return to_first(test_epairs, name);

}

static void
test_enum_pairs(void)
{
	assert_equal(to_name(TestEnum::A).value(), "a");
	g_assert_true(to_type("c").value() ==  TestEnum::C);
}

static Result<Options>
test_make_options(std::vector<std::string> args)
{
	std::vector<char*> argv;
	argv.reserve(args.size());
	for (auto& arg: args)
		argv.push_back(arg.data());

	return Options::make(static_cast<int>(argv.size()), argv.data());
}

static void
test_choice_option(void)
{
	// an explicit value
	const auto explicit_fmt =
		test_make_options({"mu", "find", "--format", "sexp", "x"});
	g_assert_true(!!explicit_fmt);
	g_assert_true(explicit_fmt->sub_command == Options::SubCommand::Find);
	g_assert_true(explicit_fmt->find.format == Options::Find::Format::Sexp);

	// the default
	const auto default_fmt = test_make_options({"mu", "find", "x"});
	g_assert_true(!!default_fmt);
	g_assert_true(default_fmt->find.format == Options::Find::Format::Plain);
	g_assert_true(default_fmt->find.sortfield == Field::Id::Date);
}

static void
test_choice_option_errors(void)
{
	constexpr auto errmsg =
		"--format requires one of { json, json2, links, plain, sexp, xml }; "
		"default is plain";

	// bad value
	const auto bad = test_make_options({"mu", "find", "--format=nope", "x"});
	g_assert_false(!!bad);
	assert_equal(bad.error().what(), errmsg);

	// no value
	const auto missing = test_make_options({"mu", "find", "x", "--format"});
	g_assert_false(!!missing);
	assert_equal(missing.error().what(), errmsg);

	// another subcommand's --format gets its own choices
	const auto view = test_make_options({"mu", "view", "--format=nope", "x"});
	g_assert_false(!!view);
	assert_equal(view.error().what(),
		     "--format requires one of { html, plain, sexp }; "
		     "default is plain");
}

static void
test_sortfield_option(void)
{
	// by name and by shortcut
	const auto by_name =
		test_make_options({"mu", "find", "--sortfield=subject", "x"});
	g_assert_true(!!by_name);
	g_assert_true(by_name->find.sortfield == Field::Id::Subject);

	const auto by_shortcut = test_make_options({"mu", "find", "-s", "s", "x"});
	g_assert_true(!!by_shortcut);
	g_assert_true(by_shortcut->find.sortfield == Field::Id::Subject);

	const auto bad = test_make_options({"mu", "find", "-s", "nope", "x"});
	g_assert_false(!!bad);
	g_assert_true(std::string{bad.error().what()}
		      .starts_with("--sortfield requires one of {"));
}

static void
test_number_option(void)
{
	// valid values; --after=0 is allowed (non-negative)
	const auto ok = test_make_options({"mu", "find", "--maxnum=10",
			"--after=0", "x"});
	g_assert_true(!!ok);
	g_assert_cmpuint(ok->find.maxnum.value(), ==, 10);
	g_assert_cmpuint(ok->find.after.value(), ==, 0);

	// bad values
	for (auto&& val: {"-5", "abc"}) {
		const auto bad = test_make_options({"mu", "find",
				mu_format("--maxnum={}", val), "x"});
		g_assert_false(!!bad);
		assert_equal(bad.error().what(),
			     "--maxnum requires a positive number");
	}

	const auto negative = test_make_options({"mu", "find", "--after=-1", "x"});
	g_assert_false(!!negative);
	assert_equal(negative.error().what(),
		     "--after requires a non-negative number");
}

static void
test_unknown_command(void)
{
	constexpr auto errmsg = "'flimflam' is not a mu command. See 'mu --help'";

	const auto unknown = test_make_options({"mu", "flimflam"});
	g_assert_false(!!unknown);
	assert_equal(unknown.error().what(), errmsg);

	// also with a global option in front
	const auto with_opt = test_make_options({"mu", "--quiet", "flimflam"});
	g_assert_false(!!with_opt);
	assert_equal(with_opt.error().what(), errmsg);

	// an unknown _option_ keeps CLI11's message
	const auto unknown_opt = test_make_options({"mu", "--flimflam"});
	g_assert_false(!!unknown_opt);
	g_assert_true(std::string{unknown_opt.error().what()}
		      .find("--flimflam") != std::string::npos);
	g_assert_true(std::string{unknown_opt.error().what()}
		      .find("is not a mu command") == std::string::npos);

	// extra arguments to a real subcommand are not rewritten
	const auto sub_extra = test_make_options({"mu", "index", "extra"});
	g_assert_false(!!sub_extra);
	g_assert_true(std::string{sub_extra.error().what()}
		      .find("is not a mu command") == std::string::npos);
}

int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	g_test_add_func("/options/ids", test_ids);
	g_test_add_func("/option/enum-pairs", test_enum_pairs);
	g_test_add_func("/options/choice", test_choice_option);
	g_test_add_func("/options/choice-errors", test_choice_option_errors);
	g_test_add_func("/options/sortfield", test_sortfield_option);
	g_test_add_func("/options/number", test_number_option);
	g_test_add_func("/options/unknown-command", test_unknown_command);

	return g_test_run();
}
#endif /*BUILD_TESTS*/
