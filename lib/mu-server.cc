/*
** Copyright (C) 2020-2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "message/mu-message.hh"
#include "mu-server.hh"

#include <fstream>
#include <string>
#include <algorithm>
#include <atomic>
#include <thread>
#include <mutex>
#include <functional>

#include <cstring>
#include <glib.h>
#include <glib/gprintf.h>
#include <unistd.h>

#include "mu-maildir.hh"
#include "mu-query.hh"
#include "index/mu-indexer.hh"
#include "mu-store.hh"

#include "utils/mu-utils.hh"
#include "utils/mu-utils-file.hh"

#include "utils/mu-option.hh"
#include "utils/mu-command-handler.hh"
#include "utils/mu-readline.hh"

using namespace Mu;

/// @brief object to manage the server-context for all commands.
struct Server::Private {
	Private(Store& store, const Server::Options& opts, Output output)
	    : store_{store}, options_{opts}, output_{output},
	      command_handler_{make_command_map()},
	      keep_going_{true},
	      tmp_dir_{unwrap(make_temp_dir())}
	{}

	~Private() {
		indexer().stop();
		if (index_thread_.joinable())
			index_thread_.join();
		if (!tmp_dir_.empty())
			remove_directory(tmp_dir_);
	}
	//
	// construction helpers
	//
	CommandHandler::CommandInfoMap make_command_map();

	//
	// acccessors
	Store&            store() { return store_; }
	const Store&      store() const { return store_; }
	Indexer&          indexer() { return store().indexer(); }
	//CommandMap&       command_map() const { return command_map_; }

	//
	// invoke
	//
	bool invoke(const std::string& expr) noexcept;

	//
	// output
	void output(const std::string& str, Server::OutputFlags flags = {}) const {
		if (output_)
			output_(str, flags);
	}
	void output_sexp(const Sexp& sexp, Server::OutputFlags flags = {}) const {
		output(sexp.to_string(), flags);
	}

	size_t output_results(const QueryResults& qres, size_t batch_size) const;
	size_t output_results_temp_file(const QueryResults& qres, size_t batch_size) const;

	//
	// handlers for various commands.
	//
	void add_handler(const Command& cmd);
	void compose_handler(const Command& cmd);
	void contacts_handler(const Command& cmd);
	void find_handler(const Command& cmd);
	void help_handler(const Command& cmd);
	void index_handler(const Command& cmd);
	void move_handler(const Command& cmd);
	void mkdir_handler(const Command& cmd);
	void ping_handler(const Command& cmd);
	void queries_handler(const Command& cmd);
	void quit_handler(const Command& cmd);
	void remove_handler(const Command& cmd);
	void sent_handler(const Command& cmd);
	void view_handler(const Command& cmd);

private:
	void move_docid(Store::Id docid, Option<std::string> flagstr,
			bool new_name, bool no_view);

	void perform_move(Store::Id		docid,
			  const Message&	msg,
			  const std::string&	maildirarg,
			  Flags			flags,
			  bool			new_name,
			  bool			no_view);

	void view_mark_as_read(Store::Id docid, Message&& msg, bool rename);

	std::pair<std::ofstream, std::string> make_temp_file_stream() const;

	Store&			store_;
	Server::Options         options_;
	Server::Output		output_;
	const CommandHandler	command_handler_;
	std::atomic<bool>       keep_going_{};
	std::thread		index_thread_;
	std::string		tmp_dir_;
};

static void
append_metadata(std::string& str, const QueryMatch& qmatch)
{
	const auto td{::atoi(qmatch.thread_date.c_str())};

	str += mu_format(" :meta (:path \"{}\" :level {} :date \"{}\" "
			 ":data-tstamp ({} {} 0)",
			 qmatch.thread_path,
			 qmatch.thread_level,
			 qmatch.thread_date,
			 static_cast<unsigned>(td >> 16),
			 static_cast<unsigned>(td & 0xffff));

	if (qmatch.has_flag(QueryMatch::Flags::Root))
		str += " :root t";
	if (qmatch.has_flag(QueryMatch::Flags::Related))
		str += " :related t";
	if (qmatch.has_flag(QueryMatch::Flags::First))
		str += " :first-child t";
	if (qmatch.has_flag(QueryMatch::Flags::Last))
		str += " :last-child t";
	if (qmatch.has_flag(QueryMatch::Flags::Orphan))
		str += " :orphan t";
	if (qmatch.has_flag(QueryMatch::Flags::Duplicate))
		str += " :duplicate t";
	if (qmatch.has_flag(QueryMatch::Flags::HasChild))
		str += " :has-child t";
	if (qmatch.has_flag(QueryMatch::Flags::ThreadSubject))
		str += " :thread-subject t";

	str += ')';
}

/*
 * A message here consists of a message s-expression with optionally a :docid
 * and/or :meta expression added.
 *
 * Note, for speed reasons, we build is as a _string_, without any further
 * parsing.
 */
static std::string
msg_sexp_str(const Message& msg, Store::Id docid, const Option<QueryMatch&> qm)
{
	auto sexpstr{msg.document().sexp_str()};
	sexpstr.reserve(sexpstr.size () + (docid == 0 ? 0 : 16) + (qm ? 64 : 0));

	// remove the closing ( ... )
	sexpstr.erase(sexpstr.end() - 1);

	if (docid != 0)
		sexpstr += " :docid " +  to_string(docid);
	if (qm)
		append_metadata(sexpstr, *qm);

	sexpstr += ')'; // ... end close it again.

	return sexpstr;
}


CommandHandler::CommandInfoMap
Server::Private::make_command_map()
{
	CommandHandler::CommandInfoMap cmap;

	using	CommandInfo = CommandHandler::CommandInfo;
	using	ArgMap	    = CommandHandler::ArgMap;
	using	ArgInfo	    = CommandHandler::ArgInfo;
	using	Type	    = Sexp::Type;
	using	Type	    = Sexp::Type;

	cmap.emplace(
	    "add",
	    CommandInfo{
		ArgMap{{":path", ArgInfo{Type::String, true, "file system path to the message"}}},
		"add a message to the store",
		[&](const auto& params) { add_handler(params); }});

	cmap.emplace(
	    "compose",
	    CommandInfo{
		ArgMap{
		    {":type",
		     ArgInfo{Type::Symbol,
			     true,
			     "type of composition: reply/forward/edit/resend/new"}},
		    {":docid",
		     ArgInfo{Type::Number, false, "document id of parent-message, if any"}},
		    {":decrypt",
		     ArgInfo{Type::Symbol, false, "whether to decrypt encrypted parts (if any)"}}},
		"compose a new message",
		[&](const auto& params) { compose_handler(params); }});

	cmap.emplace(
	    "contacts",
	    CommandInfo{
		ArgMap{{":personal", ArgInfo{Type::Symbol, false, "only personal contacts"}},
		       {":after",
			ArgInfo{Type::String, false, "only contacts seen after time_t string"}},
		       {":tstamp", ArgInfo{Type::String, false, "return changes since tstamp"}},
		       {":maxnum", ArgInfo{Type::Number, false, "max number of contacts to return"}}
		},
		"get contact information",
		[&](const auto& params) { contacts_handler(params); }});
	cmap.emplace(
	    "find",
	    CommandInfo{
		ArgMap{{":query", ArgInfo{Type::String, true, "search expression"}},
		       {":threads",
			ArgInfo{Type::Symbol, false, "whether to include threading information"}},
		       {":sortfield", ArgInfo{Type::Symbol, false, "the field to sort results by"}},
		       {":descending",
			ArgInfo{Type::Symbol, false, "whether to sort in descending order"}},
		       {":batch-size", ArgInfo{Type::Number, false, "batch size for result"}},
		       {":maxnum", ArgInfo{Type::Number, false, "maximum number of result (hint)"}},
		       {":skip-dups",
			ArgInfo{Type::Symbol,
				false,
				"whether to skip messages with duplicate message-ids"}},
		       {":include-related",
			ArgInfo{Type::Symbol,
				false,
				"whether to include other message related to matching ones"}}},
		"query the database for messages",
		[&](const auto& params) { find_handler(params); }});

	cmap.emplace(
	    "help",
	    CommandInfo{
		ArgMap{{":command", ArgInfo{Type::Symbol, false, "command to get information for"}},
		       {":full", ArgInfo{Type::Symbol, false, "show full descriptions"}}},
		"get information about one or all commands",
		[&](const auto& params) { help_handler(params); }});
	cmap.emplace(
	    "index",
	    CommandInfo{
		ArgMap{{":my-addresses", ArgInfo{Type::List, false, "list of 'my' addresses"}},
		       {":cleanup",
			ArgInfo{Type::Symbol,
				false,
				"whether to remove stale messages from the store"}},
		       {":lazy-check",
			ArgInfo{Type::Symbol,
				false,
				"whether to avoid indexing up-to-date directories"}}},
		"scan maildir for new/updated/removed messages",
		[&](const auto& params) { index_handler(params); }});
	cmap.emplace(
	    "mkdir",
	    CommandInfo{
		ArgMap{
		    {":path", ArgInfo{Type::String, true, "location for the new maildir"}},
		    {":update", ArgInfo{Type::Symbol, false,
			     "whether to send an update after creating"}}
		}, "create a new maildir",
		[&](const auto& params) { mkdir_handler(params); }});
	cmap.emplace(
	    "move",
	    CommandInfo{
		ArgMap{
		    {":docid", ArgInfo{Type::Number, false, "document-id"}},
		    {":msgid", ArgInfo{Type::String, false, "message-id"}},
		    {":flags", ArgInfo{Type::String, false, "new flags for the message"}},
		    {":maildir", ArgInfo{Type::String, false, "the target maildir"}},
		    {":rename", ArgInfo{Type::Symbol, false, "change filename when moving"}},
		    {":no-view",
		     ArgInfo{Type::Symbol, false, "if set, do not hint at updating the view"}},
		},
		"move messages and/or change their flags",
		[&](const auto& params) { move_handler(params); }});
	cmap.emplace(
	    "ping",
	    CommandInfo{
		ArgMap{},
		"ping the mu-server and get server information in the response",
		[&](const auto& params) { ping_handler(params); }});

	cmap.emplace(
	    "queries",
	    CommandInfo{
		ArgMap{
		    {":queries",
		     ArgInfo{Type::List, false, "queries for which to get read/unread numbers"}},
		},
		"get unread/totals information for a list of queries",
		[&](const auto& params) { queries_handler(params); }});

	cmap.emplace("quit", CommandInfo{{}, "quit the mu server", [&](const auto& params) {
						 quit_handler(params);
					 }});

	cmap.emplace(
	    "remove",
	    CommandInfo{
		ArgMap{{":docid",
			ArgInfo{Type::Number, true, "document-id for the message to remove"}}},
		"remove a message from filesystem and database",
		[&](const auto& params) { remove_handler(params); }});

	cmap.emplace(
	    "sent",
	    CommandInfo{ArgMap{{":path", ArgInfo{Type::String, true, "path to the message file"}}},
			"tell mu about a message that was sent",
			[&](const auto& params) { sent_handler(params); }});

	cmap.emplace(
	    "view",
	    CommandInfo{ArgMap{
			    {":docid", ArgInfo{Type::Number, false, "document-id"}},
			    {":msgid", ArgInfo{Type::String, false, "message-id"}},
			    {":path", ArgInfo{Type::String, false, "message filesystem path"}},
			    {":mark-as-read",
			     ArgInfo{Type::Symbol, false, "mark message as read (if not already)"}},
			    {":rename", ArgInfo{Type::Symbol, false, "change filename when moving"}},
			},
			"view a message. exactly one of docid/msgid/path must be specified",
			[&](const auto& params) { view_handler(params); }});
	return cmap;
}

bool
Server::Private::invoke(const std::string& expr) noexcept
{
	auto make_error=[](auto&& code, auto&& msg) {
		return Sexp().put_props(
			":error", Error::error_number(code),
			":message", msg);
	};

	if (!keep_going_)
		return false;
	try {
		auto cmd{Command::make_parse(std::string{expr})};
		if (!cmd)
			throw cmd.error();

		auto res = command_handler_.invoke(*cmd);
		if (!res)
			throw res.error();

	} catch (const Mu::Error& me) {
		output_sexp(make_error(me.code(), mu_format("{}", me.what())));
		keep_going_ = true;
	} catch (const Xapian::Error& xerr) {
		output_sexp(make_error(Error::Code::Internal,
				       mu_format("xapian error: {}: {}",
						 xerr.get_type(), xerr.get_description())));
		keep_going_ = false;
	} catch (const std::runtime_error& re) {
		output_sexp(make_error(Error::Code::Internal,
				       mu_format("caught exception: {}", re.what())));
		keep_going_ = false;
	} catch (...) {
		output_sexp(make_error(Error::Code::Internal,
				       mu_format("something went wrong: quitting")));
		keep_going_ = false;
	}

	return keep_going_;
}

/* 'add' adds a message to the database, and takes two parameters: 'path', which
 * is the full path to the message, and 'maildir', which is the maildir this
 * message lives in (e.g. "/inbox"). response with an (:info ...) message with
 * information about the newly added message (details: see code below)
 */
void
Server::Private::add_handler(const Command& cmd)
{
	auto       path{cmd.string_arg(":path")};
	const auto docid_res{store().add_message(*path)};

	if (!docid_res)
		throw docid_res.error();

	const auto docid{docid_res.value()};
	output_sexp(Sexp().put_props(":info", "add"_sym,
				     ":path", *path,
				     ":docid", docid));

	auto msg_res{store().find_message(docid)};
	if (!msg_res)
		throw Error(Error::Code::Store,
			    "failed to get message at {} (docid={})", *path, docid);

	output(mu_format("(:update {})",
			 msg_sexp_str(msg_res.value(), docid, {})));
}

/* 'compose' produces the un-changed *original* message sexp (ie., the message
 * to reply to, forward or edit) for a new message to compose). It takes two
 * parameters: 'type' with the compose type (either reply, forward or
 * edit/resend), and 'docid' for the message to reply to. Note, type:new does
 * not have an original message, and therefore does not need a docid
 *
 * In returns a (:compose <type> [:original <original-msg>] [:include] ) message
 * (details: see code below)
 *
 * Note ':include' t or nil determines whether to include attachments
 */

static Option<Sexp>
maybe_add_attachment(Message& message, const MessagePart& part, size_t index)
{
	if (!part.is_attachment())
		return Nothing;

	const auto cache_path{message.cache_path(index)};
	if (!cache_path)
		throw cache_path.error();

	const auto cooked_name{part.cooked_filename()};
	const auto fname{join_paths(*cache_path, cooked_name.value_or("part"))};

	const auto res = part.to_file(fname, true);
	if (!res)
		throw res.error();

	Sexp pi;
	if (auto cdescr = part.content_description(); cdescr)
		pi.put_props(":description", *cdescr);
	else if (cooked_name)
		pi.put_props(":description", cooked_name.value());

	pi.put_props(":file-name", fname,
		     ":mime-type",
		     part.mime_type().value_or("application/octet-stream"));

	return Some(std::move(pi));
}


void
Server::Private::compose_handler(const Command& cmd)
{
	const auto ctype = cmd.symbol_arg(":type").value_or("<error>");

	auto comp_lst = Sexp().put_props(":compose", Sexp::Symbol(ctype));


	if (ctype == "reply" || ctype == "forward" ||
	    ctype == "edit" || ctype == "resend") {

		const unsigned docid{static_cast<unsigned>(cmd.number_arg(":docid").value_or(0))};
		auto  msg{store().find_message(docid)};
		if (!msg)
			throw Error{Error::Code::Store, "failed to get message {}", docid};

		auto msg_sexp = unwrap(Sexp::parse(msg_sexp_str(msg.value(), docid, {})));
		comp_lst.put_props(":original", msg_sexp);
		if (ctype == "forward") {
			// when forwarding, attach any attachment in the orig
			size_t index{};
			Sexp attseq;
			for (auto&& part: msg->parts()) {
				if (auto attsexp = maybe_add_attachment(
					    *msg, part, index); attsexp) {
					attseq.add(std::move(*attsexp));
					++index;
				}
			}
			if (!attseq.empty()) {
				comp_lst.put_props(":include", std::move(attseq),
						  ":cache-path", *msg->cache_path());
			}
		}

	} else if (ctype != "new")
		throw Error(Error::Code::InvalidArgument, "invalid compose type '{}'", ctype);

	output_sexp(comp_lst);
}

// create pair of ostream / name
std::pair<std::ofstream, std::string>
Server::Private::make_temp_file_stream() const
{
	auto tmp_eld{join_paths(tmp_dir_, mu_format("mu-{}.eld",  g_get_monotonic_time()))};
	std::ofstream output{tmp_eld, std::ios::out};
	if (!output.good())
		throw Mu::Error{Error::Code::File, "failed to create temp-file"};

	return std::make_pair<std::ofstream, std::string>(std::move(output),
							  std::move(tmp_eld));
}



void
Server::Private::contacts_handler(const Command& cmd)
{
	const auto personal  = cmd.boolean_arg(":personal");
	const auto afterstr  = cmd.string_arg(":after").value_or("");
	const auto tstampstr = cmd.string_arg(":tstamp").value_or("");
	const auto maxnum    = cmd.number_arg(":maxnum").value_or(0 /*unlimited*/);

	const auto after{afterstr.empty() ? 0 :
		parse_date_time(afterstr, true).value_or(0)};
	const auto tstamp = g_ascii_strtoll(tstampstr.c_str(), NULL, 10);

	mu_debug("find {} contacts last seen >= {} (tstamp: {})",
		 personal ? "personal" : "any", time_to_string("%c", after), tstamp);

	auto match_contact = [&](const Contact& ci)->bool {
		if (tstamp > ci.tstamp)
			return false; /* already seen? */
		else if (personal && !ci.personal)
			return false; /* not personal? */
		else if (after > ci.message_date)
			return true; /* too old? */
		else
			return false;
	};

	auto       n{0};
	if (options_.allow_temp_file) {
		// fast way: put all the contacts in a temp-file
		auto&& [tmp_file, tmp_file_name] = make_temp_file_stream();
		tmp_file << '(';
		store().contacts_cache().for_each([&](const Contact& ci) {
			if (!match_contact(ci))
				return true; // continue
			tmp_file << quote(ci.display_name()) << "\n";
			++n;
			return maxnum == 0 || n <  maxnum;
		});
		tmp_file << ')';
		output_sexp(Sexp{":tstamp"_sym, mu_format("\"{}\"", g_get_monotonic_time()),
				":contacts-temp-file"_sym, tmp_file_name});
	} else {
		Sexp contacts;
		store().contacts_cache().for_each([&](const Contact& ci) {
			if (!match_contact(ci))
				return true; // continue;
			contacts.add(ci.display_name());
			++n;
			return maxnum == 0 || n <  maxnum;
		});
		Sexp seq;
		seq.put_props(":contacts", std::move(contacts),
			      ":tstamp", mu_format("\"{}\"", g_get_monotonic_time()));
		output_sexp(seq);
	}

	mu_debug("sent {} of {} contact(s)", n, store().contacts_cache().size());
}

/*
 * creating a message object just to get a path seems a bit excessive maybe
 * mu_store_get_path could be added if this turns out to be a problem
 */
static std::string
path_from_docid(const Store& store, Store::Id docid)
{
	auto msg{store.find_message(docid)};
	if (!msg)
		throw Error(Error::Code::Store, "could not get message from store");

	if (auto path{msg->path()}; path.empty())
		throw Error(Error::Code::Store, "could not get path for message {}",
			    docid);
	else
		return path;
}

static std::vector<Store::Id>
determine_docids(const Store& store, const Command& cmd)
{
	auto       docid{cmd.number_arg(":docid").value_or(0)};
	const auto msgid{cmd.string_arg(":msgid").value_or("")};

	if ((docid == 0) == msgid.empty())
		throw Error(Error::Code::InvalidArgument,
			    "precisely one of docid and msgid must be specified");

	if (docid != 0)
		return {static_cast<Store::Id>(docid)};
	else
		return unwrap(store.find_docids_with_message_id(msgid));
}

size_t
Server::Private::output_results(const QueryResults& qres, size_t batch_size) const
{
	size_t  n{};
	Sexp	headers;

	const auto output_batch = [&](Sexp&& hdrs) {
		Sexp batch;
		batch.put_props(":headers", std::move(hdrs));
		output_sexp(batch);
	};

	for (auto&& mi : qres) {
		auto msg{mi.message()};
		if (!msg)
			continue;
		++n;
		// construct sexp for a single header.
		auto qm{mi.query_match()};
		auto msgsexp{unwrap(Sexp::parse(msg_sexp_str(*msg, mi.doc_id(), qm)))};
		headers.add(std::move(msgsexp));
		// we output up-to-batch-size lists of messages. It's much
		// faster (on the emacs side) to handle such batches than single
		// headers.
		if (headers.size() % batch_size == 0) {
			output_batch(std::move(headers));
			headers.clear();
		};
	}

	// remaining.
	if (!headers.empty())
		output_batch(std::move(headers));

	return n;
}

size_t
Server::Private::output_results_temp_file(const QueryResults& qres, size_t batch_size) const
{
	// create an output stream with a file name
	size_t n{};

	auto&& [tmp_file, tmp_file_name] = make_temp_file_stream();
	tmp_file << '(';
	for(auto&& mi: qres) {

		auto msg{mi.message()};
		if (!msg)
			continue;

		auto qm{mi.query_match()}; // construct sexp for a single header.
		tmp_file << msg_sexp_str(*msg, mi.doc_id(), qm) << '\n';
		++n;

		if (n % batch_size == 0) {
			tmp_file << ')';
			batch_size = 1000;
			output_sexp(Sexp{":headers-temp-file"_sym, tmp_file_name});
			auto new_stream{make_temp_file_stream()};
			tmp_file	= std::move(new_stream.first);
			tmp_file_name = std::move(new_stream.second);
			tmp_file << '(';
		}
	}
	tmp_file << ')';
	output_sexp(Sexp{":headers-temp-file"_sym, tmp_file_name});

	return n;
}


void
Server::Private::find_handler(const Command& cmd)
{
	const auto q{cmd.string_arg(":query").value_or("")};
	const auto threads{cmd.boolean_arg(":threads")};
	// perhaps let mu4e set this as frame-lines of the appropriate frame.
	const auto batch_size{cmd.number_arg(":batch-size").value_or(200)};
	const auto descending{cmd.boolean_arg(":descending")};
	const auto maxnum{cmd.number_arg(":maxnum").value_or(-1) /*unlimited*/};
	const auto skip_dups{cmd.boolean_arg(":skip-dups")};
	const auto include_related{cmd.boolean_arg(":include-related")};

	// complicated!
	auto sort_field_id = std::invoke([&]()->Field::Id {
		if (const auto arg = cmd.symbol_arg(":sortfield"); !arg)
			return Field::Id::Date;
		else if (arg->length() < 2)
			throw Error{Error::Code::InvalidArgument, "invalid sort field '{}'",
				*arg};
		else if (const auto field{field_from_name(arg->substr(1))}; !field)
			throw Error{Error::Code::InvalidArgument, "invalid sort field '{}'",
				*arg};
		else
			return field->id;
	});

	if (batch_size < 1)
		throw Error{Error::Code::InvalidArgument, "invalid batch-size {}", batch_size};

	auto qflags{QueryFlags::SkipUnreadable}; // don't show unreadables.
	if (descending)
		qflags |= QueryFlags::Descending;
	if (skip_dups)
		qflags |= QueryFlags::SkipDuplicates;
	if (include_related)
		qflags |= QueryFlags::IncludeRelated;
	if (threads)
		qflags |= QueryFlags::Threading;

	StopWatch sw{mu_format("{} (indexing: {})", __func__,
			       indexer().is_running() ? "yes" :  "no")};

	std::lock_guard l{store_.lock()};
	auto qres{store_.run_query(q, sort_field_id, qflags, maxnum)};
	if (!qres)
		throw Error(Error::Code::Query, "failed to run query: {}", qres.error().what());

	/* before sending new results, send an 'erase' message, so the frontend
	 * knows it should erase the headers buffer. this will ensure that the
	 * output of two finds will not be mixed. */
	output_sexp(Sexp().put_props(":erase", Sexp::t_sym));
	const auto bsize{static_cast<size_t>(batch_size)};
	const auto foundnum =options_.allow_temp_file ?
		output_results_temp_file(*qres, bsize) : output_results(*qres, bsize);
	output_sexp(Sexp().put_props(":found", foundnum));
}

void
Server::Private::help_handler(const Command& cmd)
{
	const auto command{cmd.symbol_arg(":command").value_or("")};
	const auto full{cmd.bool_arg(":full").value_or(!command.empty())};
	auto&& info_map{command_handler_.info_map()};

	if (command.empty()) {
		mu_println(";; Commands are single-line s-expressions of the form\n"
			   ";;   (<command-name> :param1 val1 :param2 val2 ...)\n"
			   ";; For instance:\n;;  (help :command mkdir)\n"
			   ";; to get more information about the 'mkdir' command\n;;\n"
			   ";; The following commands are available:");
	}

	std::vector<std::string> names;
	for (auto&& name_cmd: info_map)
		names.emplace_back(name_cmd.first);

	std::sort(names.begin(), names.end());

	for (auto&& name : names) {
		const auto& info{info_map.find(name)->second};

		if (!command.empty() && name != command)
			continue;

		mu_println(";;  {:<12} -- {}", name, info.docstring);

		if (!full)
			continue;

		for (auto&& argname : info.sorted_argnames()) {
			const auto& arg{info.args.find(argname)};
			mu_println(";;     {:<17} :: {:<24} -- {}",
				   arg->first, to_string(arg->second),
				   arg->second.docstring);
		}
		mu_println(";;");
	}
}

static Sexp
get_stats(const Indexer::Progress& stats, const std::string& state)
{
	Sexp sexp;

	sexp.put_props(
		":info", "index"_sym,
		":status", Sexp::Symbol(state),
		":checked", static_cast<int>(stats.checked),
		":updated", static_cast<int>(stats.updated),
		":cleaned-up", static_cast<int>(stats.removed));

	return sexp;
}

void
Server::Private::index_handler(const Command& cmd)
{
	Mu::Indexer::Config conf{};
	conf.cleanup    = cmd.boolean_arg(":cleanup");
	conf.lazy_check = cmd.boolean_arg(":lazy-check");
	// ignore .noupdate with an empty store.
	conf.ignore_noupdate = store().empty();

	indexer().stop();
	if (index_thread_.joinable())
		index_thread_.join();

	// start a background track.
	index_thread_ = std::thread([this, conf = std::move(conf)] {
		StopWatch sw{"indexing"};
		indexer().start(conf);
		while (indexer().is_running()) {
			std::this_thread::sleep_for(std::chrono::milliseconds(2000));
			output_sexp(get_stats(indexer().progress(), "running"),
				    Server::OutputFlags::Flush);
		}
		output_sexp(get_stats(indexer().progress(), "complete"),
			    Server::OutputFlags::Flush);
		store().commit(); /* ensure on-disk database is updated, too */
	});
}

void
Server::Private::mkdir_handler(const Command& cmd)
{
	const auto path{cmd.string_arg(":path").value_or("<error>")};
	const auto update{cmd.boolean_arg(":update")};

	if (auto&& res = maildir_mkdir(path, 0755, false); !res)
		throw res.error();

	/* mu4e does a lot of opportunistic 'mkdir', only send it updates when
	 * requested */
	if (update)
		output_sexp(Sexp().put_props(":info", "mkdir",
					     ":message",
					     mu_format("{} has been created", path)));
}

void
Server::Private::perform_move(Store::Id                 docid,
			      const Message&            msg,
			      const std::string&	maildirarg,
			      Flags			flags,
			      bool                      new_name,
			      bool                      no_view)
{
	bool different_mdir{};
	auto maildir{maildirarg};
	if (maildir.empty()) {
		maildir        = msg.maildir();
		different_mdir = false;
	} else /* are we moving to a different mdir, or is it just flags? */
		different_mdir = maildir != msg.maildir();

	Store::MoveOptions move_opts{Store::MoveOptions::DupFlags};
	if (new_name)
		move_opts |= Store::MoveOptions::ChangeName;

	/* note: we get back _all_ the messages that changed; the first is the
	 * primary mover; the rest (if present) are any dups affected */
	const auto idmsgvec{store().move_message(docid, maildir, flags, move_opts)};
	if (!idmsgvec)
		throw idmsgvec.error();

	for (auto&&[id, msg]: *idmsgvec) {
		auto sexpstr = "(:update " + msg_sexp_str(idmsgvec->at(0).second, id, {});
		/* note, the :move t thing is a hint to the frontend that it
		 * could remove the particular header */
		if (different_mdir)
			sexpstr += " :move t";
		if (!no_view && id == docid)
			sexpstr += " :maybe-view t";
		sexpstr += ')';
		output(std::move(sexpstr));
	}
}

static Flags
calculate_message_flags(const Message& msg, Option<std::string> flagopt)
{
	const auto flags = std::invoke([&]()->Option<Flags>{
			if (!flagopt)
				return msg.flags();
			else
				return flags_from_expr(*flagopt, msg.flags());
		});

	if (!flags)
		throw Error{Error::Code::InvalidArgument,
			"invalid flags '{}'", flagopt.value_or("")};
	else
		return flags.value();
}

void
Server::Private::move_docid(Store::Id		docid,
			    Option<std::string>	flagopt,
			    bool		new_name,
			    bool		no_view)
{
	if (docid == Store::InvalidId)
		throw Error{Error::Code::InvalidArgument, "invalid docid"};

	auto msg{store_.find_message(docid)};
	if (!msg)
		throw Error{Error::Code::Store, "failed to get message from store"};

	const auto flags = calculate_message_flags(msg.value(), flagopt);
	perform_move(docid, *msg, "", flags, new_name, no_view);
}

/*
 * 'move' moves a message to a different maildir and/or changes its
 * flags. parameters are *either* a 'docid:' or 'msgid:' pointing to
 * the message, a 'maildir:' for the target maildir, and a 'flags:'
 * parameter for the new flags.
 */
void
Server::Private::move_handler(const Command& cmd)
{
	auto       maildir{cmd.string_arg(":maildir").value_or("")};
	const auto flagopt{cmd.string_arg(":flags")};
	const auto rename{cmd.boolean_arg(":rename")};
	const auto no_view{cmd.boolean_arg(":noupdate")};
	const auto docids{determine_docids(store_, cmd)};

	if (docids.size() > 1) {
		if (!maildir.empty()) // ie. duplicate message-ids.
			throw Mu::Error{Error::Code::Store,
				"cannot move multiple messages at the same time"};
		// multi.
		for (auto&& docid : docids)
			move_docid(docid, flagopt, rename, no_view);
		return;
	}
	const auto docid{docids.at(0)};
	auto    msg = store().find_message(docid)
		.or_else([&]{throw Error{Error::Code::InvalidArgument,
					"cannot find message {}", docid};}).value();

	/* if maildir was not specified, take the current one */
	if (maildir.empty())
		maildir = msg.maildir();

	/* determine the real target flags, which come from the flags-parameter
	 * we received (ie., flagstr), if any, plus the existing message
	 * flags. */
	const auto flags = calculate_message_flags(msg, flagopt);
	perform_move(docid, msg, maildir, flags, rename, no_view);
}

void
Server::Private::ping_handler(const Command& cmd)
{
	const auto storecount{store().size()};
	if (storecount == (unsigned)-1)
		throw Error{Error::Code::Store, "failed to read store"};
	Sexp addrs;
	for (auto&& addr : store().config().get<Config::Id::PersonalAddresses>())
		addrs.add(addr);

	output_sexp(Sexp()
		    .put_props(":pong", "mu")
		    .put_props(":props",
			       Sexp().put_props(
				       ":version", VERSION,
				       ":personal-addresses", std::move(addrs),
				       ":database-path", store().path(),
				       ":root-maildir", store().root_maildir(),
				       ":doccount", storecount)));
}

void
Server::Private::queries_handler(const Command& cmd)
{
	const auto queries{cmd.string_vec_arg(":queries")
		.value_or(std::vector<std::string>{})};

	Sexp qresults;
	for (auto&& q : queries) {
		const auto count{store_.count_query(q)};
		const auto unreadq{mu_format("flag:unread AND ({})", q)};
		const auto unread{store_.count_query(unreadq)};
		qresults.add(Sexp().put_props(":query", q,
					      ":count", count,
					      ":unread", unread));
	}

	output_sexp(Sexp(":queries"_sym, std::move(qresults)));
}


void
Server::Private::quit_handler(const Command& cmd)
{
	keep_going_ = false;
}

void
Server::Private::remove_handler(const Command& cmd)
{
	const auto docid{cmd.number_arg(":docid").value_or(0)};
	const auto path{path_from_docid(store(), docid)};

	if (::unlink(path.c_str()) != 0 && errno != ENOENT)
		throw Error(Error::Code::File,
			    "could not delete {}: {}", path, g_strerror(errno));

	if (!store().remove_message(path))
		mu_warning("failed to remove message @ {} ({}) from store", path, docid);
	output_sexp(Sexp().put_props(":remove", docid));	// act as if it worked.
}

void
Server::Private::sent_handler(const Command& cmd)
{
	const auto path{cmd.string_arg(":path").value_or("")};
	const auto docid = store().add_message(path);
	if (!docid)
		throw Error{Error::Code::Store, "failed to add path: {}: {}",
			path, docid.error().what()};

	output_sexp(Sexp().put_props(
			    ":sent", Sexp::t_sym,
			    ":path", path,
			    ":docid", docid.value()));
}

void
Server::Private::view_mark_as_read(Store::Id docid, Message&& msg, bool rename)
{

	auto move_res = std::invoke([&]()->Result<Store::IdMessageVec> {
			const auto newflags{flags_from_delta_expr("+S-u-N", msg.flags())};
			if (!newflags || msg.flags() == *newflags) {
				/* case 1: message was already read; do nothing */
				Store::IdMessageVec idmvec;
				idmvec.emplace_back(docid, std::move(msg));
				return idmvec;
			} else {
				/* case 2: move message (and possibly dups) */
				Store::MoveOptions move_opts{Store::MoveOptions::DupFlags};
				if (rename)
					move_opts |= Store::MoveOptions::ChangeName;
				return store().move_message(docid, {}, newflags, move_opts);
			}
		});

	if (!move_res)
		throw move_res.error();

	for (auto&& [id, msg]: move_res.value())
		output(mu_format("({} {})", id == docid ? ":view" : ":update",
				 msg_sexp_str(msg, id, {})));
}

void
Server::Private::view_handler(const Command& cmd)
{
	StopWatch sw{mu_format("{} (indexing: {})", __func__, indexer().is_running())};

	const auto mark_as_read{cmd.boolean_arg(":mark-as-read")};
	/* for now, do _not_ rename, as it seems to confuse mbsync */
	const auto rename{false};
	//const auto rename{get_bool_or(params, ":rename")};

	const auto docids{determine_docids(store(), cmd)};

	if (docids.empty())
		throw Error{Error::Code::Store, "failed to find message for view"};
	const auto docid{docids.at(0)};
	auto msg = store().find_message(docid)
		.or_else([]{throw Error{Error::Code::Store,
					"failed to find message for view"};}).value();

	/* if the message should not be marked-as-read, we're done. */
	if (!mark_as_read)
		output(mu_format("(:view {})", msg_sexp_str(msg, docid, {})));
	else
		view_mark_as_read(docid, std::move(msg), rename);
	/* otherwise, mark message and and possible dups as read */
}

Server::Server(Store& store, const Server::Options& opts, Server::Output output)
    : priv_{std::make_unique<Private>(store, opts, output)}
{}

Server::~Server() = default;

bool
Server::invoke(const std::string& expr) noexcept
{
	return priv_->invoke(expr);
}
