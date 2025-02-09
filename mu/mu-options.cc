/*
** Copyright (C) 2022-2024 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
 * Here we implement mu's command-line parsing based on the CLI11 library. At
 * the time of writing, that library seems to be the best based on the criteria
 * that it supports the features we need and is available as a header-only
 * include.
 *
 * CLI11 can do quite a bit, and we're only scratching the surface here,
 * plan is to slowly improve things.
 *
 * - we do quite a bit of sanity-checking, but the errors are a rather terse
 * - the docs could be improved, e.g., `mu find --help` and --format/--sortfield
 *
 */


#include <config.h>
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

#include <thirdparty/CLI11.hpp>

using namespace Mu;


/*
 * helpers
 */



/**
 * array of associated pair elements -- like an alist
 * but based on std::array and thus can be constexpr
 */
template<typename T1, typename T2, std::size_t N>
      using AssocPairs = std::array<std::pair<T1, T2>, N>;


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
 * Options-specific array-bases type that maps some enum to a <name, description> pair
 */
template<typename T, std::size_t N>
using InfoEnum = AssocPairs<T, std::pair<std::string_view, std::string_view>, N>;

/**
 * Get the name (shortname) for some InfoEnum, based on the enum
 *
 * @param ie an InfoEnum
 * @param e an enum value
 *
 * @return the name if found, or Nothing
 */
template<typename IE>
static constexpr Option<std::string_view>
to_name(const IE& ie, typename IE::value_type::first_type e) {
	if (auto&& s{to_second(ie, e)}; s)
		return s->first;
	else
		return Nothing;
}

/**
 * Get the enum value for some InfoEnum, based on the name
 *
 * @param ie an InfoEnum
 * @param name some name (shortname)
 *
 * @return the name if found, or Nothing
 */
template<typename IE>
static constexpr Option<typename IE::value_type::first_type>
to_enum(const IE& ie, std::string_view name) {
	for(auto&& item: ie)
		if (item.second.first == name)
			return item.first;
		else
			return Nothing;
}

/**
 * List help options for as a string, with the default marked with '(*)'
 *
 * @param ie infoenum
 * @param default_opt default option
 *
 * @return a help string
 */
template<typename IE>
static std::string
options_help(const IE& ie, typename IE::value_type::first_type default_opt)
{
	std::string s;
	for(auto&& item: ie) {
		if (!s.empty())
			s += ", ";
		s += std::string{item.second.first};
		if (item.first == default_opt)
			s += "(*)"; /* default option */
	}
	return s;
}


/**
 * Get map from string->type
 */
template<typename IE>
static std::unordered_map<std::string, typename IE::value_type::first_type>
options_map(const IE& ie)
{
	std::unordered_map<std::string, typename IE::value_type::first_type> map;
	for (auto&& item : ie)
		map.emplace(std::string{item.second.first}, item.first);

	return map;
}

// transformers

// Expand the path using wordexp
static const std::function ExpandPath = [](std::string filepath)->std::string {
	if (auto&& res{expand_path(filepath)}; !res)
		throw CLI::ValidationError{res.error().what()};
	else
		return res.value();
};

// Canonicalize path
static const std::function CanonicalizePath = [](std::string filepath)->std::string {
	return filepath = canonicalize_filename(filepath);
};

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
	static constexpr InfoEnum<Format, 8> FormatInfos = {{
			{ Format::Plain,           {"plain", "Plain output"} },
			{ Format::MuttAlias,       {"mutt-alias", "Mutt alias"} },
			{ Format::MuttAddressBook, {"mutt-ab", "Mutt address book"}},
			{ Format::Wanderlust,      {"wl", "Wanderlust"}},
			{ Format::OrgContact,      {"org-contact", "org-contact"}},
			{ Format::Bbdb,            {"bbdb", "Emacs BBDB"}},
			{ Format::Csv,             {"csv", "comma-separated values"}},
			{ Format::Json,            {"json", "format as json array"}},
		}};

	const auto fhelp = options_help(FormatInfos, Format::Plain);
	const auto fmap = options_map(FormatInfos);

	sub.add_option("--format,-o", opts.cfind.format,
		       "Output format; one of " + fhelp)
		->type_name("<format>")
		->default_str("plain")
		->default_val(Format::Plain)
		->transform(CLI::CheckedTransformer(fmap));

	sub.add_option("pattern", opts.cfind.rx_pattern,
		       "Regular expression pattern to match");
	sub.add_flag("--personal,-p", opts.cfind.personal,
		       "Only show 'personal' contacts");
	sub.add_option("--after", opts.cfind.after,
		       "Only show results after some timestamps")
		->type_name("<time_t>")
		->check(CLI::PositiveNumber);
	sub.add_option("--maxnum,-n", opts.cfind.maxnum,
		       "Maximum number of results")
		->type_name("<number>")
		->check(CLI::PositiveNumber);
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
	static constexpr InfoEnum<Format, 7> FormatInfos = {{
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
		}};

	sub.add_flag("--threads,-t", opts.find.threads,
		     "Show message threads");
	sub.add_flag("--skip-dups,-u", opts.find.skip_dups,
		     "Show only one of messages with same message-id");
	sub.add_flag("--include-related,-r", opts.find.include_related,
		     "Include related messages in results");
	sub.add_flag("--analyze,-a", opts.find.analyze,
		     "Analyze the query");

	const auto fhelp = options_help(FormatInfos, Format::Plain);
	const auto fmap = options_map(FormatInfos);

	sub.add_option("--format,-o", opts.find.format,
		       "Output format; one of " + fhelp)
		->type_name("<format>")
		->default_str("plain")
		->default_val(Format::Plain)
		->transform(CLI::CheckedTransformer(fmap));

	sub.add_option("--maxnum,-n", opts.find.maxnum,
		       "Maximum number of results")
		->type_name("<number>")
		->check(CLI::PositiveNumber);

	sub.add_option("--fields,-f", opts.find.fields,
		       "Fields to display")
		->default_val("d f s");

	std::unordered_map<std::string, Field::Id> smap;
	std::string sopts;
	field_for_each([&](auto&& field){
		if (field.is_sortable()) {
			smap.emplace(std::string(field.name), field.id);
			smap.emplace(std::string(1, field.shortcut), field.id);
			if (!sopts.empty())
				sopts += ", ";
			sopts += mu_format("{}|{}", field.name, field.shortcut);
		}
	});
	sub.add_option("--sortfield,-s", opts.find.sortfield,
		       "Field to sort the results by; one of " + sopts)
		->type_name("<field>")
		->default_str("date")
		->default_val(Field::Id::Date)
		->transform(CLI::CheckedTransformer(smap));

	sub.add_flag("--reverse,-z", opts.find.reverse,
		     "Sort in descending order");

	sub.add_option("--bookmark,-b", opts.find.bookmark,
		       "Use bookmarked query")
		->type_name("<bookmark>");

	sub.add_flag("--clearlinks", opts.find.clearlinks,
		     "Clear old links first");
	sub.add_option("--linksdir", opts.find.linksdir,
		       "Use bookmarked query")
		->type_name("<dir>")
		->transform(ExpandPath, "expand linksdir path");

	sub.add_option("--summary-len", opts.find.summary_len,
		       "Use up to so many lines for the summary")
		->type_name("<lines>")
		->check(CLI::PositiveNumber);

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
	sub.add_option("--my-address", opts.init.my_addresses,
		       "Personal e-mail address or regexp")
		->type_name("<address>");
	sub.add_option("--ignored-address", opts.init.ignored_addresses,
		       "Ignored e-mail address or regexp")
		->type_name("<address>");

	sub.add_option("--max-message-size", opts.init.max_msg_size,
		       "Maximum allowed message size in bytes");
	sub.add_option("--batch-size", opts.init.batch_size,
		       "Maximum size of database transaction");
	sub.add_option("--support-ngrams", opts.init.support_ngrams,
		       "Support CJK n-grams if for querying/indexing");
	sub.add_flag("--reinit", opts.init.reinit,
		       "Re-initialize database with current settings")
		->excludes("--maildir")
		->excludes("--my-address")
		->excludes("--ignored-address")
		->excludes("--max-message-size")
		->excludes("--batch-size")
		->excludes("--support-ngrams");
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
	static constexpr InfoEnum<Format, 3> FormatInfos = {{
			{ Format::Plain,
			  {"plain", "Plain output"}
			},
			{ Format::Html,
			  {"html", "Plain output with HTML body"}
			},
			{ Format::Sexp,
			  {"sexp", "S-expressions"}
			},
		}};

	const auto fhelp = options_help(FormatInfos, Format::Plain);
	const auto fmap = options_map(FormatInfos);

	sub.add_option("--format,-o", opts.view.format,
		       "Output format; one of " + fhelp)
		->type_name("<format>")
		->default_str("plain")
		->default_val(Format::Plain)
		->transform(CLI::CheckedTransformer(fmap));

	sub_crypto(sub, opts.view);

	sub.add_option("--summary-len", opts.view.summary_len,
		       "Use up to so many lines for the summary")
		->type_name("<lines>")
		->check(CLI::PositiveNumber);

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
	typedef void(*setup_func_t)(CLI::App&, Options&);
	setup_func_t setup_func{};
};

static constexpr
AssocPairs<SubCommand, CommandInfo, Options::SubCommandNum> SubCommandInfos= {{
		{ SubCommand::Add,
		  { Category::NeedsWritableStore,
		    "add", "Add message(s) to the database", sub_add}
		},
		{ SubCommand::Cfind,
		  { Category::NeedsReadOnlyStore,
		    "cfind", "Find contacts matching pattern", sub_cfind}
		},
		{ SubCommand::Extract,
		  {Category::None,
		  "extract", "Extract MIME-parts from messages", sub_extract}
		},
		{ SubCommand::Fields,
		  {Category::None,
		  "fields", "Superseded by 'mu info'", sub_fields}
		},
		{ SubCommand::Find,
		  {Category::NeedsReadOnlyStore,
		  "find", "Find messages matching query", sub_find }
		},
		{ SubCommand::Help,
		  {Category::None,
		  "help", "Show help information", sub_help }
		},
		{ SubCommand::Index,
		  {Category::NeedsWritableStore,
		  "index", "Store message information in the database", sub_index }
		},
		{ SubCommand::Info,
		  {Category::NeedsReadOnlyStore,
		  "info", "Show information", sub_info }
		},
		{ SubCommand::Init,
		  {Category::NeedsWritableStore,
		  "init", "Initialize the database", sub_init }
		},
		{ SubCommand::Mkdir,
		  {Category::None,
		  "mkdir", "Create a new Maildir", sub_mkdir }
		},
		{ SubCommand::Move,
		  {Category::NeedsWritableStore,
		   "move", "Move a message or change flags", sub_move }
		},
		{ SubCommand::Remove,
		  {Category::NeedsWritableStore,
		  "remove", "Remove message from file-system and database", sub_remove }
		},
		{ SubCommand::Script,
		  // Note: SubCommand::Script is special; there's no literal
		// "script" subcommand, there subcommands for all the scripts.
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
	}};



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
	char *path = g_find_program_in_path("man");
	if (!path)
		return Err(Error::Code::Command,
			   "cannot find 'man' program");

	GError* err{};
	auto cmd{to_string_gchar(std::move(path)) + " " + name};
	auto res = g_spawn_command_line_sync(cmd.c_str(), {}, {}, {}, &err);
	if (!res)
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
	errno = 0;

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
	CLI::App app{"mu mail indexer/searcher", "mu"};

	app.description(R"(mu mail indexer/searcher
Copyright (C) 2008-2023 Dirk-Jan C. Binnema

License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
)");
	app.set_version_flag("-V,--version", PACKAGE_VERSION);
	app.set_help_flag("-h,--help", "Show help informmation");
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
			sub->add_option("--muhome",
					opts.muhome, "Specify alternative mu directory")
				->envname("MUHOME")
				->type_name("<dir>")
				->transform(ExpandPath, "expand muhome path");
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
	} catch (const CLI::ParseError& pe) {
		return Err(Error::Code::InvalidArgument, "{}", pe.what());
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
constexpr AssocPairs<TestEnum, std::string_view, 3>
test_epairs = {{
	{TestEnum::A, "a"},
	{TestEnum::B, "b"},
	{TestEnum::C, "c"},
}};

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

int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	g_test_add_func("/options/ids", test_ids);
	g_test_add_func("/option/enum-pairs", test_enum_pairs);

	return g_test_run();
}
#endif /*BUILD_TESTS*/
