/*
** Copyright (C) 2022-2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
**
** This program is free software; you can redistribute it and/or modify it
** under the terms of the GNU General Public License as published by the
** Free Software Foundation; either version 3, or (at your option) any
** later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software Foundation,
** Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
**
*/

#ifndef MU_OPTIONS_HH__
#define MU_OPTIONS_HH__

#include <sstream>
#include <string>
#include <vector>
#include <utils/mu-option.hh>
#include <utils/mu-result.hh>
#include <utils/mu-utils.hh>
#include <utils/mu-utils-file.hh>

#include <message/mu-fields.hh>
#include <mu-script.hh>
#include <ctime>
#include <sys/stat.h>

/* command-line options for Mu */
namespace Mu {
struct Options {
	using	OptSize	     = Option<std::size_t>;
	using	SizeVec	     = std::vector<std::size_t>;
	using	OptTStamp    = Option<std::time_t>;
	using	OptFieldId   = Option<Field::Id>;
	using	StringVec    = std::vector<std::string>;

	/*
	 * general options
	 */
	bool		quiet;				/**<  don't give any output */
	bool		debug;				/**<  log debug-level info */
	bool		version;			/**<  request mu version */
	bool		log_stderr;			/**<  log to stderr */
	bool		nocolor;			/**<  don't use use ansi-colors */
	bool		verbose;			/**<  verbose output */
	std::string	muhome;				/**<  alternative mu dir */

	/**
	 * Whether by default, we should show color
	 *
	 * @return true or false
	 */
	static bool default_no_color();

	enum struct SubCommand {
		Add, Cfind, Extract, Fields, Find, Help, Index,Info, Init, Mkdir,
		Move, Remove, Script, Server, Verify, View,
		// <private>
		__count__
	};
	static constexpr auto SubCommandNum = static_cast<size_t>(SubCommand::__count__);
	static constexpr std::array<SubCommand, SubCommandNum> SubCommands = {{
			SubCommand::Add,
			SubCommand::Cfind,
			SubCommand::Extract,
			SubCommand::Fields,
			SubCommand::Find,
			SubCommand::Help,
			SubCommand::Index,
			SubCommand::Info,
			SubCommand::Init,
			SubCommand::Mkdir,
			SubCommand::Move,
			SubCommand::Remove,
			SubCommand::Script,
			SubCommand::Server,
			SubCommand::Verify,
			SubCommand::View
		}};

	Option<SubCommand> sub_command;			/**< The chosen sub-command, if any. */

	/*
	 * Add
	 */
	struct Add {
		StringVec files;			/**< field to add */
	} add;

	/*
	 * Cfind
	 */
	struct Cfind {
		enum struct Format { Plain, MuttAlias, MuttAddressBook,
			Wanderlust, OrgContact, Bbdb, Csv, Json };
		Format		format;			/**< Output format */
		bool		personal;		/**< only show personal contacts */
		OptTStamp	after;			/**< only last seen after tstamp */
		OptSize		maxnum;			/**< maximum number of results */
		std::string	rx_pattern;		/**< contact regexp to match */
	} cfind;


	struct Crypto {
		bool	auto_retrieve;			/**< auto-retrieve keys */
		bool	decrypt;			/**< decrypt */
	};

	/*
	 * Extract
	 */
	struct Extract: public Crypto {
		std::string	message;		/**<  path to message file */
		bool		save_all;		/**<  extract all parts */
		bool		save_attachments;	/**<  extract all attachment parts */
		SizeVec		parts;			/**<  parts to save /  open */
		std::string	targetdir{};		/**<  where to save attachments */
		bool		overwrite;		/**<  overwrite same-named files */
		bool		play;			/**<  try to 'play' attachment */
		std::string	filename_rx;		/**<  Filename rx to save */
		bool		uncooked{};		/**<  Whether to avoid massaging
							    * the output filename */
	} extract;

	/*
	 * Fields
	 */

	/*
	 * Find
	 */
	struct Find {
		std::string	fields;			/**<  fields to show in output */
		Field::Id	sortfield;		/**<  field to sort by	*/
		OptSize		maxnum;			/**<  max # of entries to print */
		bool		reverse;		/**<  sort in revers order (z->a) */
		bool		threads;		/**<  show message threads */
		bool		clearlinks;		/**<  clear linksdir first */
		std::string	linksdir;		/**<  directory for links */
		OptSize		summary_len;		/**<  max # of lines for summary */
		std::string	bookmark;		/**<  use bookmark */
		bool		analyze;		/**<  analyze query */

		enum struct Format { Plain, Links, Xml, Json, Sexp, Exec };
		Format		format;			/**< Output format */
		std::string	exec;			/**<  cmd to execute on matches */
		bool		skip_dups;		/**< show only first with msg id */
		bool		include_related;	/**<  included related messages */
							/**<  for find and cind */
		OptTStamp	after;			/**<  only last seen after T */
		bool		auto_retrieve;		/**<  assume we're online */
		bool		decrypt;		/**<  try to decrypt the body */

		StringVec	query;			/**< search query */
	} find;

	struct Help {
		std::string command;			/**< Help parameter */
	} help;

	/*
	 * Index
	 */
	struct Index {
		bool	nocleanup;			/**< don't cleanup del'd mails */
		bool	lazycheck;			/**< don't check uptodate dirs */
		bool	reindex;			/**< do a full re-index */
	} index;


	/*
	 * Info
	 */
	struct Info {
		std::string topic;			/**< what to get info about? */
	} info;

	/*
	 * Init
	 */
	struct Init {
		std::string	maildir;		/**< where the mails are */
		StringVec	my_addresses;		/**< personal e-mail addresses */
		StringVec	ignored_addresses;	/**< addresses to be ignored for
							 * the contacts-cache */
		OptSize		max_msg_size;		/**< max size for message files */
		OptSize		batch_size;		/**< db transaction batch size */
		bool		reinit;			/**< re-initialize  */
		bool		support_ngrams;		/**< support CJK etc. ngrams */

	} init;

	/*
	 * Mkdir
	 */
	struct Mkdir {
		StringVec	dirs;			/**< Dir(s) to create */
		mode_t		mode;			/**< Mode for the maildir */
	} mkdir;

	/*
	 * Move
	 */
	struct Move {
		std::string	src;		       /**< Source file */
		std::string	dest;		       /**< Destination dir */
		std::string	flags;		       /**< Flags for destination */
		bool            change_name;           /**< Change basename for destination */
		bool            update_dups;           /**< Update duplicate messages too */
		bool            dry_run;               /**< Just print the result path,
							but do not change anything */
	} move;

	/*
	 * Remove
	 */
	struct Remove {
		StringVec	files;			/**< Files to remove */
	} remove;

	/*
	 * Scripts (i.e., finding scriot)
	 */
	struct Script {
		std::string	name;			/**< name of script */
		StringVec	params;			/**< script params */
	} script;

	/*
	 * Server
	 */
	struct Server {
		bool		commands;		/**< dump docs for commands */
		std::string	eval;			/**< command to evaluate */
		bool		allow_temp_file;	/**< temp-file optimization allowed? */
	} server;

	/*
	 * Verify
	 */
	struct Verify: public Crypto {
		StringVec	files;			/**< message files to verify */
	} verify;
	/*
	 * View
	 */
	struct View: public Crypto {
		bool	terminate;			/**< add \f between msgs in view */
		OptSize summary_len;			/**< max # of lines for summary */

		enum struct Format { Plain,  Sexp, Html };
		Format format;				/**< output format*/

		StringVec files;			/**< Message file(s) */
	} view;


	/**
	 * Create an Options structure fo the given command-line arguments.
	 *
	 * @param argc argc
	 * @param argv argc
	 *
	 * @return Options, or an Error
	 */
	static Result<Options> make(int argc, char *argv[]);


	/**
	 * Different commands need different things
	 *
	 */
	enum struct Category {
		None,
		NeedsReadOnlyStore,
		NeedsWritableStore,
	};

	/**
	 * Get the category for some subcommand
	 *
	 * @param sub subcommand
	 *
	 * @return the category
	 */
	static Category category(SubCommand sub);

	/**
	 * Get some well-known Path
	 *
	 * @param path the Path to find
	 *
	 * @return the path name
	 */
	std::string runtime_path(RuntimePath path) const {
		return Mu::runtime_path(path, muhome);
	}
};

} // namepace Mu

#endif /* MU_OPTIONS_HH__ */
