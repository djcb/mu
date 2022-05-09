/*
** Copyright (C) 2020-2021 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <iostream>
#include <string>
#include <algorithm>
#include <atomic>
#include <thread>
#include <mutex>
#include <functional>

#include <cstring>
#include <glib.h>
#include <glib/gprintf.h>

#include "mu-runtime.hh"
#include "mu-maildir.hh"
#include "mu-query.hh"
#include "index/mu-indexer.hh"
#include "mu-store.hh"

#include "utils/mu-str.h"
#include "utils/mu-utils.hh"
#include "utils/mu-option.hh"
#include "utils/mu-command-parser.hh"
#include "utils/mu-readline.hh"

using namespace Mu;
using namespace Command;

/// @brief object to manage the server-context for all commands.
struct Server::Private {
	Private(Store& store, Output output)
	    : store_{store}, output_{output}, command_map_{make_command_map()},
	      keep_going_{true}
	{
	}

	~Private()
	{
		indexer().stop();
		if (index_thread_.joinable())
			index_thread_.join();
	}
	//
	// construction helpers
	//
	CommandMap make_command_map();

	//
	// acccessors
	Store&            store() { return store_; }
	const Store&      store() const { return store_; }
	Indexer&          indexer() { return store().indexer(); }
	const CommandMap& command_map() const { return command_map_; }

	//
	// invoke
	//
	bool invoke(const std::string& expr) noexcept;

	//
	// output
	//
	void output_sexp(Sexp&& sexp,Server::OutputFlags flags = {}) const
	{
		if (output_)
			output_(std::move(sexp), flags);
	}

	void output_sexp(Sexp::List&& lst, Server::OutputFlags flags = {}) const
	{
		output_sexp(Sexp::make_list(std::move(lst)), flags);
	}
	size_t output_results(const QueryResults& qres, size_t batch_size) const;

	//
	// handlers for various commands.
	//
	void add_handler(const Parameters& params);
	void compose_handler(const Parameters& params);
	void contacts_handler(const Parameters& params);
	void find_handler(const Parameters& params);
	void help_handler(const Parameters& params);
	void index_handler(const Parameters& params);
	void move_handler(const Parameters& params);
	void mkdir_handler(const Parameters& params);
	void ping_handler(const Parameters& params);
	void quit_handler(const Parameters& params);
	void remove_handler(const Parameters& params);
	void sent_handler(const Parameters& params);
	void view_handler(const Parameters& params);

private:
	// helpers
	Sexp build_message_sexp(const Message&            msg,
				Store::Id                 docid,
				const Option<QueryMatch&> qm) const;

	Sexp::List move_docid(Store::Id docid, Option<std::string> flagstr,
			      bool new_name, bool no_view);

	Sexp::List perform_move(Store::Id		docid,
				const Message&		msg,
				const std::string&	maildirarg,
				Flags			flags,
				bool			new_name,
				bool			no_view);

	bool maybe_mark_as_read(Store::Id docid, Flags old_flags, bool rename);
	bool maybe_mark_msgid_as_read(const std::string& msgid, bool rename);

	Store&            store_;
	Server::Output    output_;
	const CommandMap  command_map_;
	std::atomic<bool> keep_going_{};
	std::thread       index_thread_;
};

static Sexp
build_metadata(const QueryMatch& qmatch)
{
	Sexp::List mdata;

	auto symbol_t = [] { return Sexp::make_symbol("t"); };

	mdata.add_prop(":path", Sexp::make_string(qmatch.thread_path));
	mdata.add_prop(":level", Sexp::make_number(qmatch.thread_level));
	mdata.add_prop(":date", Sexp::make_string(qmatch.thread_date));

	Sexp::List dlist;
	const auto td{::atoi(qmatch.thread_date.c_str())};
	dlist.add(Sexp::make_number((unsigned)(td >> 16)));
	dlist.add(Sexp::make_number((unsigned)(td & 0xffff)));
	dlist.add(Sexp::make_number(0));
	mdata.add_prop(":date-tstamp", Sexp::make_list(std::move(dlist)));

	if (qmatch.has_flag(QueryMatch::Flags::Root))
		mdata.add_prop(":root", symbol_t());
	if (qmatch.has_flag(QueryMatch::Flags::Related))
		mdata.add_prop(":related", symbol_t());
	if (qmatch.has_flag(QueryMatch::Flags::First))
		mdata.add_prop(":first-child", symbol_t());
	if (qmatch.has_flag(QueryMatch::Flags::Last))
		mdata.add_prop(":last-child", symbol_t());
	if (qmatch.has_flag(QueryMatch::Flags::Orphan))
		mdata.add_prop(":orphan", symbol_t());
	if (qmatch.has_flag(QueryMatch::Flags::Duplicate))
		mdata.add_prop(":duplicate", symbol_t());
	if (qmatch.has_flag(QueryMatch::Flags::HasChild))
		mdata.add_prop(":has-child", symbol_t());
	if (qmatch.has_flag(QueryMatch::Flags::ThreadSubject))
		mdata.add_prop(":thread-subject", symbol_t());

	return Sexp::make_list(std::move(mdata));
}

/*
 * A message here is a Sexp::List consists of a message s-expression with
 * optionally a :meta expression added.
 */
Sexp
Server::Private::build_message_sexp(const Message&            msg,
				    Store::Id                 docid,
				    const Option<QueryMatch&> qm) const
{
	auto sexp_list = msg.to_sexp_list();
	if (docid != 0)
		sexp_list.add_prop(":docid", Sexp::make_number(docid));
	if (qm)
		sexp_list.add_prop(":meta", build_metadata(*qm));

	return Sexp::make_list(std::move(sexp_list));
}

CommandMap
Server::Private::make_command_map()
{
	CommandMap cmap;

	using Type = Sexp::Type;

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
		       {":maxnum", ArgInfo{Type::Number, false, "max number of contacts to return"}}},
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
	    "mkdir",
	    CommandInfo{
		ArgMap{{":path", ArgInfo{Type::String, true, "location for the new maildir"}}},
		"create a new maildir",
		[&](const auto& params) { mkdir_handler(params); }});
	cmap.emplace(
	    "ping",
	    CommandInfo{
		ArgMap{
		    {":queries",
		     ArgInfo{Type::List, false, "queries for which to get read/unread numbers"}},
		    {":skip-dups",
		     ArgInfo{Type::Symbol,
			     false,
			     "whether to exclude messages with duplicate message-ids"}},
		},
		"ping the mu-server and get information in response",
		[&](const auto& params) { ping_handler(params); }});

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

G_GNUC_PRINTF(2, 3)
static Sexp
make_error(Error::Code errcode, const char* frm, ...)
{
	char*   msg{};
	va_list ap;

	va_start(ap, frm);
	g_vasprintf(&msg, frm, ap);
	va_end(ap);

	Sexp::List err;
	err.add_prop(":error", Sexp::make_number(static_cast<int>(errcode)));
	err.add_prop(":message", Sexp::make_string(msg));
	g_free(msg);

	return Sexp::make_list(std::move(err));
}

bool
Server::Private::invoke(const std::string& expr) noexcept
{
	if (!keep_going_)
		return false;

	try {
		auto call{Sexp::Sexp::make_parse(expr)};
		Command::invoke(command_map(), call);

	} catch (const Mu::Error& me) {
		output_sexp(make_error(me.code(), "%s", me.what()));
		keep_going_ = true;
	} catch (const Xapian::Error& xerr) {
		output_sexp(make_error(Error::Code::Internal, "xapian error: %s: %s",
				       xerr.get_type(), xerr.get_description().c_str()));
		keep_going_ = false;
	} catch (const std::runtime_error& re) {
		output_sexp(make_error(Error::Code::Internal, "caught exception: %s", re.what()));
		keep_going_ = false;
	} catch (...) {
		output_sexp(make_error(Error::Code::Internal, "something went wrong: quiting"));
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
Server::Private::add_handler(const Parameters& params)
{
	auto       path{get_string_or(params, ":path")};
	const auto docid_res{store().add_message(path)};

	if (!docid_res)
		throw Error(Error::Code::Store, "failed to add message to store");

	const auto docid{docid_res.value()};

	Sexp::List expr;
	expr.add_prop(":info", Sexp::make_symbol("add"));
	expr.add_prop(":path", Sexp::make_string(path));
	expr.add_prop(":docid", Sexp::make_number(docid));

	output_sexp(Sexp::make_list(std::move(expr)));

	auto msg_res{store().find_message(docid)};
	if (!msg_res)
		throw Error(Error::Code::Store,
			    "failed to get message at %s (docid=%u)",
			    path.c_str(), docid);

	Sexp::List update;
	update.add_prop(":update", build_message_sexp(msg_res.value(), docid, {}));
	output_sexp(Sexp::make_list(std::move(update)));
}

/* 'compose' produces the un-changed *original* message sexp (ie., the message
 * to reply to, forward or edit) for a new message to compose). It takes two
 * parameters: 'type' with the compose type (either reply, forward or
 * edit/resend), and 'docid' for the message to reply to. Note, type:new does
 * not have an original message, and therefore does not need a docid
 *
 * In returns a (:compose <type> [:original <original-msg>] [:include] )
 * message (detals: see code below)
 *
 * Note ':include' t or nil determines whether to include attachments
 */

static Option<Sexp>
maybe_add_attachment(Message& message, const MessagePart& part, size_t index)
{
	if (!part.is_attachment())
		return Nothing;

	const auto cache_path{message.cache_path()};
	if (!cache_path)
		throw cache_path.error();

	const auto fname{format("%s/%zu-%s", cache_path->c_str(),
				index, part.cooked_filename()
				.value_or("part").c_str())};

	const auto res = part.to_file(fname, true);
	if (!res)
		throw res.error();

	Sexp::List pi;

	if (auto cdescr = part.content_description(); cdescr)
		pi.add_prop(":description", Sexp::make_string(*cdescr));

	pi.add_prop(":file-name", Sexp::make_string(fname));
	pi.add_prop(":mime-type", Sexp::make_string(
			    part.mime_type().value_or("application/octet-stream")));

	return Some(Sexp::make_list(std::move(pi)));
}


void
Server::Private::compose_handler(const Parameters& params)
{
	const auto ctype{get_symbol_or(params, ":type")};

	Sexp::List comp_lst;
	comp_lst.add_prop(":compose", Sexp::make_symbol(std::string(ctype)));


	if (ctype == "reply" || ctype == "forward" ||
	    ctype == "edit" || ctype == "resend") {

		const unsigned docid{(unsigned)get_int_or(params, ":docid")};
		auto  msg{store().find_message(docid)};
		if (!msg)
			throw Error{Error::Code::Store, "failed to get message %u", docid};

		comp_lst.add_prop(":original", build_message_sexp(msg.value(), docid, {}));

		if (ctype == "forward") {
			// when forwarding, attach any attachment in the orig
			size_t index{};
			Sexp::List attseq;
			for (auto&& part: msg->parts()) {
				if (auto attsexp = maybe_add_attachment(
					    *msg, part, index); attsexp) {
					attseq.add(std::move(*attsexp));
					++index;
				}
			}
			if (!attseq.empty()) {
				comp_lst.add_prop(":include",
						  Sexp::make_list(std::move(attseq)));
				comp_lst.add_prop(":cache-path",
						  Sexp::make_string(*msg->cache_path()));
			}
		}

	} else if (ctype != "new")
		throw Error(Error::Code::InvalidArgument, "invalid compose type '%s'",
			    ctype.c_str());

	output_sexp(std::move(comp_lst));
}

void
Server::Private::contacts_handler(const Parameters& params)
{
	const auto personal  = get_bool_or(params, ":personal");
	const auto afterstr  = get_string_or(params, ":after");
	const auto tstampstr = get_string_or(params, ":tstamp");
	const auto maxnum    = get_int_or(params, ":maxnum", 0 /*unlimited*/);

	const auto after{afterstr.empty() ? 0 :
		parse_date_time(afterstr, true).value_or(0)};
	const auto tstamp = g_ascii_strtoll(tstampstr.c_str(), NULL, 10);

	g_debug("find %s contacts last seen >= %s (tstamp: %zu)",
		personal ? "personal" : "any",
		time_to_string("%c", after).c_str(),
		static_cast<size_t>(tstamp));

	auto       rank{0};
	Sexp::List contacts;
	store().contacts_cache().for_each([&](const Contact& ci) {

		/* since the last time we got some contacts */
		if (tstamp > ci.tstamp)
			return;
		/* (maybe) only include 'personal' contacts */
		if (personal && !ci.personal)
			return;
		/* only include newer-than-x contacts */
		if (after > ci.message_date)
			return;

		rank++;

		Sexp::List contact;
		contact.add_prop(":address",
				 Sexp::make_string(ci.display_name(true/*encode-if-needed*/)));
		contact.add_prop(":rank", Sexp::make_number(rank));
		auto contacts_sexp{Sexp::make_list(std::move(contact))};
		contacts_sexp.formatting_opts |= Sexp::FormattingOptions::SplitList;
		contacts.add(std::move(contacts_sexp));
	}, static_cast<size_t>(maxnum));

	Sexp::List seq;
	seq.add_prop(":contacts", Sexp::make_list(std::move(contacts)));
	seq.add_prop(":tstamp",
		     Sexp::make_string(format("%" G_GINT64_FORMAT,
					      g_get_monotonic_time())));

	/* dump the contacts cache as a giant sexp */
	g_debug("sending %d of %zu contact(s)", rank, store().contacts_cache().size());
	output_sexp(std::move(seq), Server::OutputFlags::SplitList);
}

/* get a *list* of all messages with the given message id */
static std::vector<Store::Id>
docids_for_msgid(const Store& store, const std::string& msgid, size_t max = 100)
{
	if (msgid.size() > Store::MaxTermLength) {
		throw Error(Error::Code::InvalidArgument,
			    "invalid message-id '%s'", msgid.c_str());
	}

	const auto xprefix{field_from_id(Field::Id::MessageId).shortcut};
	/*XXX this is a bit dodgy */
	auto tmp{g_ascii_strdown(msgid.c_str(), -1)};
	auto expr{g_strdup_printf("%c:%s", xprefix, tmp)};
	g_free(tmp);

	GError*    gerr{};
	std::lock_guard l{store.lock()};
	const auto res{store.run_query(expr, {}, QueryFlags::None, max)};
	g_free(expr);
	if (!res)
		throw Error(Error::Code::Store, &gerr, "failed to run msgid-query");
	else if (res->empty())
		throw Error(Error::Code::NotFound,
			    "could not find message(s) for msgid %s",
			    msgid.c_str());

	std::vector<Store::Id> docids{};
	for (auto&& mi : *res)
		docids.emplace_back(mi.doc_id());

	return docids;
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
		throw Error(Error::Code::Store, "could not get path for message %u",
			    docid);
	else
		return path;
}

static std::vector<Store::Id>
determine_docids(const Store& store, const Parameters& params)
{
	auto       docid{get_int_or(params, ":docid", 0)};
	const auto msgid{get_string_or(params, ":msgid")};

	if ((docid == 0) == msgid.empty())
		throw Error(Error::Code::InvalidArgument,
			    "precisely one of docid and msgid must be specified");

	if (docid != 0)
		return {static_cast<Store::Id>(docid)};
	else
		return docids_for_msgid(store, msgid.c_str());
}

size_t
Server::Private::output_results(const QueryResults& qres, size_t batch_size) const
{
	size_t     n{};
	Sexp::List headers;

	const auto output_batch = [&](Sexp::List&& hdrs) {
		Sexp::List batch;
		batch.add_prop(":headers", Sexp::make_list(std::move(hdrs)));
		output_sexp(std::move(batch));
	};

	for (auto&& mi : qres) {
		auto msg{mi.message()};
		if (!msg)
			continue;
		++n;

		// construct sexp for a single header.
		auto qm{mi.query_match()};
		auto msgsexp{build_message_sexp(*msg, mi.doc_id(), qm)};
		msgsexp.formatting_opts |= Sexp::FormattingOptions::SplitList;
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

void
Server::Private::find_handler(const Parameters& params)
{
	const auto q{get_string_or(params, ":query")};
	const auto threads{get_bool_or(params, ":threads", false)};
	// perhaps let mu4e set this as frame-lines of the appropriate frame.
	const auto batch_size{get_int_or(params, ":batch-size", 110)};
	const auto sortfieldstr{get_symbol_or(params, ":sortfield", "")};
	const auto descending{get_bool_or(params, ":descending", false)};
	const auto maxnum{get_int_or(params, ":maxnum", -1 /*unlimited*/)};
	const auto skip_dups{get_bool_or(params, ":skip-dups", false)};
	const auto include_related{get_bool_or(params, ":include-related", false)};

	auto sort_field = std::invoke([&]()->Option<Field>{
		if (sortfieldstr.size() < 2)
			return Nothing;
		else
			return field_from_name(sortfieldstr.substr(1));
	});
	if (!sort_field && !sortfieldstr.empty())
		throw Error{Error::Code::InvalidArgument, "invalid sort field '%s'",
			sortfieldstr.c_str()};
	if (batch_size < 1)
		throw Error{Error::Code::InvalidArgument, "invalid batch-size %d", batch_size};

	auto qflags{QueryFlags::SkipUnreadable}; // don't show unreadables.
	if (descending)
		qflags |= QueryFlags::Descending;
	if (skip_dups)
		qflags |= QueryFlags::SkipDuplicates;
	if (include_related)
		qflags |= QueryFlags::IncludeRelated;
	if (threads)
		qflags |= QueryFlags::Threading;

	std::lock_guard l{store_.lock()};
	auto qres{store_.run_query(q, sort_field->id, qflags, maxnum)};
	if (!qres)
		throw Error(Error::Code::Query, "failed to run query");

	/* before sending new results, send an 'erase' message, so the frontend
	 * knows it should erase the headers buffer. this will ensure that the
	 * output of two finds will not be mixed. */
	{
		Sexp::List lst;
		lst.add_prop(":erase", Sexp::make_symbol("t"));
		output_sexp(std::move(lst));
	}

	const auto foundnum{output_results(*qres, static_cast<size_t>(batch_size))};

	{
		Sexp::List lst;
		lst.add_prop(":found", Sexp::make_number(foundnum));
		output_sexp(std::move(lst));
	}
}

void
Server::Private::help_handler(const Parameters& params)
{
	const auto command{get_symbol_or(params, ":command", "")};
	const auto full{get_bool_or(params, ":full", !command.empty())};

	if (command.empty()) {
		std::cout << ";; Commands are s-expressions of the form\n"
			  << ";;   (<command-name> :param1 val1 :param2 val2 ...)\n"
			  << ";; For instance:\n;;  (help :command quit)\n"
			  << ";; to get detailed information about the 'quit'\n;;\n";
		std::cout << ";; The following commands are available:\n\n";
	}

	std::vector<std::string> names;
	for (auto&& name_cmd : command_map())
		names.emplace_back(name_cmd.first);
	std::sort(names.begin(), names.end());

	for (auto&& name : names) {
		const auto& info{command_map().find(name)->second};

		if (!command.empty() && name != command)
			continue;

		if (!command.empty())
			std::cout << ";;   "
				  << format("%-10s -- %s\n", name.c_str(), info.docstring.c_str());
		else
			std::cout << ";;  " << name.c_str() << " -- " << info.docstring.c_str()
				  << '\n';
		if (!full)
			continue;

		for (auto&& argname : info.sorted_argnames()) {
			const auto& arg{info.args.find(argname)};
			std::cout << ";;        "
				  << format("%-17s  : %-24s ",
					    arg->first.c_str(),
					    to_string(arg->second).c_str());
			std::cout << "  " << arg->second.docstring << "\n";
		}
		std::cout << ";;\n";
	}
}

static Sexp::List
get_stats(const Indexer::Progress& stats, const std::string& state)
{
	Sexp::List lst;

	lst.add_prop(":info", Sexp::make_symbol("index"));
	lst.add_prop(":status", Sexp::make_symbol(std::string{state}));
	lst.add_prop(":checked", Sexp::make_number(stats.checked));
	lst.add_prop(":updated", Sexp::make_number(stats.updated));
	lst.add_prop(":cleaned-up", Sexp::make_number(stats.removed));

	return lst;
}

void
Server::Private::index_handler(const Parameters& params)
{
	Mu::Indexer::Config conf{};
	conf.cleanup    = get_bool_or(params, ":cleanup");
	conf.lazy_check = get_bool_or(params, ":lazy-check");
	// ignore .noupdate with an empty store.
	conf.ignore_noupdate = store().empty();

	indexer().stop();
	if (index_thread_.joinable())
		index_thread_.join();

	// start a background track.
	index_thread_ = std::thread([this, conf = std::move(conf)] {
		indexer().start(conf);
		while (indexer().is_running()) {
			std::this_thread::sleep_for(std::chrono::milliseconds(2000));
			output_sexp(get_stats(indexer().progress(), "running"),
				    Server::OutputFlags::Flush);
		}
		output_sexp(get_stats(indexer().progress(), "complete"),
			    Server::OutputFlags::Flush);
	});
}

void
Server::Private::mkdir_handler(const Parameters& params)
{
	const auto path{get_string_or(params, ":path")};
	if (auto&& res = maildir_mkdir(path, 0755, FALSE); !res)
		throw res.error();

	Sexp::List lst;
	lst.add_prop(":info", Sexp::make_string("mkdir"));
	lst.add_prop(":message", Sexp::make_string(format("%s has been created", path.c_str())));

	output_sexp(std::move(lst));
}

Sexp::List
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

	const auto new_msg = store().move_message(docid, maildir, flags, new_name);
	if (!new_msg)
		throw new_msg.error();

	Sexp::List seq;
	seq.add_prop(":update", build_message_sexp(new_msg.value(), docid, {}));
	/* note, the :move t thing is a hint to the frontend that it
	 * could remove the particular header */
	if (different_mdir)
		seq.add_prop(":move", Sexp::make_symbol("t"));
	if (!no_view)
		seq.add_prop(":maybe-view", Sexp::make_symbol("t"));

	return seq;
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
			"invalid flags '%s'", flagopt.value_or("").c_str()};
	else
		return flags.value();
}

Sexp::List
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
	auto lst = perform_move(docid, *msg, "", flags, new_name, no_view);
	return lst;
}

/*
 * 'move' moves a message to a different maildir and/or changes its
 * flags. parameters are *either* a 'docid:' or 'msgid:' pointing to
 * the message, a 'maildir:' for the target maildir, and a 'flags:'
 * parameter for the new flags.
 *
 * returns an (:update <new-msg-sexp>)
 *
 */
void
Server::Private::move_handler(const Parameters& params)
{
	auto       maildir{get_string_or(params, ":maildir")};
	const auto flagopt{get_string(params, ":flags")};
	const auto rename{get_bool_or(params, ":rename")};
	const auto no_view{get_bool_or(params, ":noupdate")};
	const auto docids{determine_docids(store_, params)};

	if (docids.size() > 1) {
		if (!maildir.empty()) // ie. duplicate message-ids.
			throw Mu::Error{Error::Code::Store,
					"can't move multiple messages at the same time"};
		// multi.
		for (auto&& docid : docids)
			output_sexp(move_docid(docid, flagopt,
					       rename, no_view));
		return;
	}
	auto docid{docids.at(0)};
	auto    msg = store().find_message(docid)
		.or_else([]{throw Error{Error::Code::InvalidArgument,
					"could not create message"};}).value();

	/* if maildir was not specified, take the current one */
	if (maildir.empty())
		maildir = msg.maildir();

	/* determine the real target flags, which come from the flags-parameter
	 * we received (ie., flagstr), if any, plus the existing message
	 * flags. */
	const auto flags = calculate_message_flags(msg, flagopt);
	output_sexp(perform_move(docid, msg, maildir, flags, rename, no_view));
}

void
Server::Private::ping_handler(const Parameters& params)
{
	const auto storecount{store().size()};
	if (storecount == (unsigned)-1)
		throw Error{Error::Code::Store, "failed to read store"};

	const auto queries{get_string_vec(params, ":queries")};
	Sexp::List qresults;
	for (auto&& q : queries) {
		const auto count{store_.count_query(q)};
		const auto unreadq{format("flag:unread AND (%s)", q.c_str())};
		const auto unread{store_.count_query(unreadq)};

		Sexp::List lst;
		lst.add_prop(":query", Sexp::make_string(q));
		lst.add_prop(":count", Sexp::make_number(count));
		lst.add_prop(":unread", Sexp::make_number(unread));

		qresults.add(Sexp::make_list(std::move(lst)));
	}

	Sexp::List addrs;
	for (auto&& addr : store().properties().personal_addresses)
		addrs.add(Sexp::make_string(addr));

	Sexp::List lst;
	lst.add_prop(":pong", Sexp::make_string("mu"));

	Sexp::List proplst;
	proplst.add_prop(":version", Sexp::make_string(VERSION));
	proplst.add_prop(":personal-addresses", Sexp::make_list(std::move(addrs)));
	proplst.add_prop(":database-path", Sexp::make_string(store().properties().database_path));
	proplst.add_prop(":root-maildir", Sexp::make_string(store().properties().root_maildir));
	proplst.add_prop(":doccount", Sexp::make_number(storecount));
	proplst.add_prop(":queries", Sexp::make_list(std::move(qresults)));

	lst.add_prop(":props", Sexp::make_list(std::move(proplst)));

	output_sexp(std::move(lst));
}

void
Server::Private::quit_handler(const Parameters& params)
{
	keep_going_ = false;
}

void
Server::Private::remove_handler(const Parameters& params)
{
	const auto docid{get_int_or(params, ":docid")};
	const auto path{path_from_docid(store(), docid)};

	if (::unlink(path.c_str()) != 0 && errno != ENOENT)
		throw Error(Error::Code::File,
			    "could not delete %s: %s",
			    path.c_str(),
			    g_strerror(errno));

	if (!store().remove_message(path))
		g_warning("failed to remove message @ %s (%d) from store", path.c_str(), docid);
	// act as if it worked.

	Sexp::List lst;
	lst.add_prop(":remove", Sexp::make_number(docid));

	output_sexp(std::move(lst));
}

void
Server::Private::sent_handler(const Parameters& params)
{
	const auto path{get_string_or(params, ":path")};
	const auto docid = store().add_message(path);
	if (!docid)
		throw Error{Error::Code::Store, "failed to add path"};

	Sexp::List lst;
	lst.add_prop(":sent", Sexp::make_symbol("t"));
	lst.add_prop(":path", Sexp::make_string(path));
	lst.add_prop(":docid", Sexp::make_number(docid.value()));

	output_sexp(std::move(lst));
}

bool
Server::Private::maybe_mark_as_read(Store::Id docid, Flags oldflags, bool rename)
{
	const auto newflags{flags_from_delta_expr("+S-u-N", oldflags)};
	if (!newflags || oldflags == *newflags)
		return false; // nothing to do.

	const auto msg = store().move_message(docid, {}, newflags, rename);
	if (!msg)
		throw msg.error();

	/* send an update */
	Sexp::List update;
	update.add_prop(":update", build_message_sexp(*msg, docid, {}));
	output_sexp(Sexp::make_list(std::move(update)));

	g_debug("marked message %d as read => %s", docid, msg->path().c_str());

	return true;
}

bool
Server::Private::maybe_mark_msgid_as_read(const std::string& msgid,
					  bool rename) try {
	if (!msgid.empty())
		return false; // nothing to do.

	for (auto&& docid: docids_for_msgid(store_, msgid))
		if (auto msg{store().find_message(docid)}; msg)
			maybe_mark_as_read(docid, msg->flags(), rename);

	return true;
} catch (...) { /* not fatal */
	g_warning("failed to mark <%s> as read", msgid.c_str());
	return false;
}

void
Server::Private::view_handler(const Parameters& params)
{
	const auto mark_as_read{get_bool_or(params, ":mark-as-read")};
	/* for now, do _not_ rename, as it seems to confuse mbsync */
	const auto rename{false};
	//const auto rename{get_bool_or(params, ":rename")};

	const auto docids{determine_docids(store(), params)};

	if (docids.empty())
		throw Error{Error::Code::Store, "failed to find message for view"};

	const auto docid{docids.at(0)};
	auto msg = store().find_message(docid)
		.or_else([]{throw Error{Error::Code::Store,
					"failed to find message for view"};}).value();

	if (mark_as_read) {
		// maybe mark the main message as read.
		maybe_mark_as_read(docid, msg.flags(), rename);
		/* maybe mark _all_ messsage with same message-id as read */
		maybe_mark_msgid_as_read(msg.message_id(), rename);
	}

	Sexp::List seq;
	seq.add_prop(":view", build_message_sexp(msg, docid, {}));
	output_sexp(std::move(seq));
}

Server::Server(Store& store, Server::Output output)
    : priv_{std::make_unique<Private>(store, output)}
{
}

Server::~Server() = default;

bool
Server::invoke(const std::string& expr) noexcept
{
	return priv_->invoke(expr);
}
