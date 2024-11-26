/*
** Copyright (C) 2021-2024 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include "mu-store.hh"

#include <chrono>
#include <mutex>
#include <array>
#include <cstdlib>
#include <stdexcept>
#include <unordered_map>
#include <atomic>
#include <type_traits>
#include <iostream>
#include <cstring>

#include "mu-maildir.hh"
#include "mu-query.hh"
#include "mu-xapian-db.hh"
#include "mu-scanner.hh"

#include "utils/mu-error.hh"

#include "utils/mu-utils.hh"
#include <utils/mu-utils-file.hh>

using namespace Mu;

static_assert(std::is_same<Store::Id, Xapian::docid>::value, "wrong type for Store::Id");

// Properties
constexpr auto ExpectedSchemaVersion = MU_STORE_SCHEMA_VERSION;

static std::string
remove_slash(const std::string& str)
{
	auto clean{str};
	while (!clean.empty() && clean[clean.length() - 1] == '/')
		clean.pop_back();

	return clean;
}

struct Store::Private {

	Private(const std::string& path, bool readonly):
		xapian_db_{XapianDb(path, readonly ? XapianDb::Flavor::ReadOnly
				    : XapianDb::Flavor::Open)},
		config_{xapian_db_},
		contacts_cache_{config_},
		root_maildir_{remove_slash(config_.get<Config::Id::RootMaildir>())},
		message_opts_{make_message_options(config_)}
		{}

	Private(const std::string& path, const std::string& root_maildir,
		Option<const Config&> conf):
		xapian_db_{XapianDb(path, XapianDb::Flavor::CreateOverwrite)},
		config_{make_config(xapian_db_, root_maildir, conf)},
		contacts_cache_{config_},
		root_maildir_{remove_slash(config_.get<Config::Id::RootMaildir>())},
		message_opts_{make_message_options(config_)} {
		// so tell xapian-db to update its internal cacheed values from
		// config. In practice: batch-size.
		xapian_db_.reinit();
	}

	~Private() try {
		mu_debug("closing store @ {}", xapian_db_.path());
		if (!xapian_db_.read_only())
			contacts_cache_.serialize();
	} catch (...) {
		mu_critical("caught exception in store dtor");
	}

	Config make_config(XapianDb& xapian_db, const std::string& root_maildir,
			   Option<const Config&> conf) {

		if (!g_path_is_absolute(root_maildir.c_str()))
			throw Error{Error::Code::File,
					"root maildir path is not absolute ({})",
					root_maildir};

		Config config{xapian_db};
		if (conf)
			config.import_configurable(*conf);

		config.set<Config::Id::RootMaildir>(remove_slash(root_maildir));
		config.set<Config::Id::SchemaVersion>(ExpectedSchemaVersion);

		return config;
	}

	Message::Options make_message_options(const Config& conf) {
		if (conf.get<Config::Id::SupportNgrams>())
			return Message::Options::SupportNgrams;
		else
			return Message::Options::None;
	}

	Option<Message> find_message_unlocked(Store::Id docid) const;
	Store::IdVec find_duplicates_unlocked(const Store& store,
					      const std::string& message_id) const;

	Result<Store::Id> add_message_unlocked(Message& msg);
	Result<Store::Id> update_message_unlocked(Message& msg, Store::Id docid);
	Result<Store::Id> update_message_unlocked(Message& msg, const std::string& old_path);


	using PathMessage = std::pair<std::string, Message>;
	Result<PathMessage> move_message_unlocked(Message&& msg,
						  Option<const std::string&> target_mdir,
						  Option<Flags> new_flags,
						  MoveOptions opts);
	XapianDb xapian_db_;
	Config config_;
	ContactsCache            contacts_cache_;
	std::unique_ptr<Indexer> indexer_;

	const std::string	root_maildir_;
	const Message::Options	message_opts_;

	std::mutex lock_;
};


Result<Store::Id>
Store::Private::add_message_unlocked(Message& msg)
{
	auto&& docid{xapian_db_.add_document(msg.document().xapian_document())};
	if (docid)
		mu_debug("added message @ {}; docid = {}", msg.path(), *docid);

	return docid;
}


Result<Store::Id>
Store::Private::update_message_unlocked(Message& msg, Store::Id docid)
{
	auto&& res{xapian_db_.replace_document(docid, msg.document().xapian_document())};
	if (res)
		mu_debug("updated message @ {}; docid = {}", msg.path(), *res);

	return res;
}

Result<Store::Id>
Store::Private::update_message_unlocked(Message& msg, const std::string& path_to_replace)
{
	return xapian_db_.replace_document(
		field_from_id(Field::Id::Path).xapian_term(path_to_replace),
		msg.document().xapian_document());
}

Option<Message>
Store::Private::find_message_unlocked(Store::Id docid) const
{
	if (auto&& doc{xapian_db_.document(docid)}; !doc)
		return Nothing;
	else if (auto&& msg{Message::make_from_document(std::move(*doc))}; !msg)
		return Nothing;
	else
		return Some(std::move(*msg));
}

Store::IdVec
Store::Private::find_duplicates_unlocked(const Store& store,
					 const std::string& message_id) const
{
	if (message_id.empty() || message_id.size() > MaxTermLength) {
		mu_warning("invalid message-id '{}'", message_id);
		return {};
	}

	auto expr{mu_format("{}:{}",
			    field_from_id(Field::Id::MessageId).shortcut,
			    message_id)};
	if (auto&& res{store.run_query(expr)}; !res) {
		mu_warning("error finding message-ids: {}", res.error().what());
		return {};

	} else {
		Store::IdVec ids;
		ids.reserve(res->size());
		for (auto&& mi: *res)
			ids.emplace_back(mi.doc_id());
		return ids;
	}
}


Store::Store(const std::string& path, Store::Options opts)
    : priv_{std::make_unique<Private>(path, none_of(opts & Store::Options::Writable))}
{
	if (none_of(opts & Store::Options::Writable) &&
	    any_of(opts & Store::Options::ReInit))
		throw Mu::Error(Error::Code::InvalidArgument,
				"Options::ReInit requires Options::Writable");

	const auto s_version{config().get<Config::Id::SchemaVersion>()};
	if (any_of(opts & Store::Options::ReInit)) {
		/* don't try to recover from version with an incompatible scheme */
		if (s_version < 500)
			throw Mu::Error(Error::Code::CannotReinit,
					"old schema ({}) is too old to re-initialize from",
					s_version).add_hint("Invoke 'mu init' without '--reinit'; "
							    "see mu-init(1) for details");
		const auto old_root_maildir{root_maildir()};

		MemDb mem_db;
		Config old_config(mem_db);
		old_config.import_configurable(config());

		this->priv_.reset();
		/* and create a new one "in place" */
		Store new_store(path, old_root_maildir, old_config);
		this->priv_ = std::move(new_store.priv_);
	}

	/* otherwise, the schema version should match. */
	if (s_version != ExpectedSchemaVersion)
		throw Mu::Error(Error::Code::SchemaMismatch,
				"expected schema-version {}, but got {}",
				ExpectedSchemaVersion, s_version).
			add_hint("Please (re)initialize with 'mu init'; see mu-init(1) for details");
}

Store::Store(const std::string& path,
	     const std::string& root_maildir,
	     Option<const Config&> conf):
	priv_{std::make_unique<Private>(path, root_maildir, conf)}
{}

Store::Store(Store&& other)
{
	priv_ = std::move(other.priv_);
	priv_->indexer_.reset();
}

Store::~Store() = default;

Store::Statistics
Store::statistics() const
{
	Statistics stats{};

	stats.size = size();
	stats.last_change = config().get<Config::Id::LastChange>();
	stats.last_index = config().get<Config::Id::LastIndex>();

	return stats;
}

const XapianDb&
Store::xapian_db() const
{
	return priv_->xapian_db_;
}

XapianDb&
Store::xapian_db()
{
	return priv_->xapian_db_;
}

const Config&
Store::config() const
{
	return priv_->config_;
}

Config&
Store::config()
{
	return priv_->config_;
}

const std::string&
Store::root_maildir() const {
	return priv_->root_maildir_;
}

const ContactsCache&
Store::contacts_cache() const
{
	return priv_->contacts_cache_;
}

Indexer&
Store::indexer()
{
	std::lock_guard guard{priv_->lock_};

	if (xapian_db().read_only())
		throw Error{Error::Code::Store, "no indexer for read-only store"};
	else if (!priv_->indexer_)
		priv_->indexer_ = std::make_unique<Indexer>(*this);

	return *priv_->indexer_.get();
}

Result<Store::Id>
Store::add_message(Message& msg, bool is_new)
{
	const auto mdir{maildir_from_path(msg.path(), root_maildir())};
	if (!mdir)
		return Err(mdir.error());
	if (auto&& res = msg.set_maildir(mdir.value()); !res)
		return Err(res.error());

	// we shouldn't mix ngrams/non-ngrams messages.
	if (any_of(msg.options() & Message::Options::SupportNgrams) !=
	    any_of(message_options() & Message::Options::SupportNgrams))
		return Err(Error::Code::InvalidArgument, "incompatible message options");

	/* add contacts from this message to cache; this cache
	 * also determines whether those contacts are _personal_, i.e. match
	 * our personal addresses.
	 *
	 * if a message has any personal contacts, mark it as personal; do
	 * this by updating the message flags.
	 */
	bool is_personal{};
	priv_->contacts_cache_.add(msg.all_contacts(), is_personal);
	if (is_personal)
		msg.set_flags(msg.flags() | Flags::Personal);

	std::lock_guard guard{priv_->lock_};
	auto&& res = is_new ?
		priv_->add_message_unlocked(msg) :
		priv_->update_message_unlocked(msg, msg.path());
	if (!res)
		return Err(res.error());

	mu_debug("added {}{}message @ {}; docid = {}",
		 is_new ? "new " : "", is_personal ? "personal " : "", msg.path(), *res);

	return res;
}

Result<Store::Id>
Store::add_message(const std::string& path, bool is_new)
{
	if (auto msg{Message::make_from_path(path, priv_->message_opts_)}; !msg)
		return Err(msg.error());
	else
		return add_message(msg.value(), is_new);
}


bool
Store::remove_message(const std::string& path)
{
	const auto term{field_from_id(Field::Id::Path).xapian_term(path)};

	std::lock_guard guard{priv_->lock_};

	xapian_db().delete_document(term);
	mu_debug("deleted message @ {} from store", path);
	return true;
}

void
Store::remove_messages(const std::vector<Store::Id>& ids)
{
	std::lock_guard guard{priv_->lock_};

	xapian_db().request_transaction();

	for (auto&& id : ids)
		xapian_db().delete_document(id);

	xapian_db().request_commit(true/*force*/);
}


Option<Message>
Store::find_message(Store::Id docid) const
{
	std::lock_guard guard{priv_->lock_};

	return priv_->find_message_unlocked(docid);
}

Option<Store::Id>
Store::find_message_id(const std::string& path) const
{
	constexpr auto path_field{field_from_id(Field::Id::Path)};

	std::lock_guard guard{priv_->lock_};

	auto enq{xapian_db().enquire()};
	enq.set_query(Xapian::Query{path_field.xapian_term(path)});

	if (auto mset{enq.get_mset(0, 1)}; mset.empty())
		return Nothing; // message not found
	else
		return Some(*mset.begin());
}


Store::IdMessageVec
Store::find_messages(IdVec ids) const
{
	std::lock_guard guard{priv_->lock_};

	IdMessageVec id_msgs;
	for (auto&& id: ids) {
		if (auto&& msg{priv_->find_message_unlocked(id)}; msg)
			id_msgs.emplace_back(std::make_pair(id, std::move(*msg)));
	}

	return id_msgs;
}

/**
 * Move a message in store and filesystem; with DryRun, only calculate the target name.
 *
 * Lock is assumed taken already
 *
 * @param id message id
 * @param target_mdir target_mdir (or Nothing for current)
 * @param new_flags new flags (or Nothing)
 * @param opts move_options
 *
 * @return the Message after the moving, or an Error
 */
Result<Store::Private::PathMessage>
Store::Private::move_message_unlocked(Message&& msg,
				      Option<const std::string&> target_mdir,
				      Option<Flags> new_flags,
				      MoveOptions opts)
{
	const auto old_path       = msg.path();
	const auto target_flags   = new_flags.value_or(msg.flags());
	const auto target_maildir = target_mdir.value_or(msg.maildir());

	/* 1. first determine the file system path of the target */
	const auto target_path =
		maildir_determine_target(msg.path(), root_maildir_,
					 target_maildir, target_flags,
					 any_of(opts & MoveOptions::ChangeName));
	if (!target_path)
		return Err(target_path.error());

	// in dry-run mode, we only determine the target-path
	if (none_of(opts & MoveOptions::DryRun)) {

		/* 2. let's move it */
		if (const auto res = maildir_move_message(msg.path(), target_path.value()); !res)
			return Err(res.error());

		/* 3. file move worked, now update the message with the new info.*/
		if (auto&& res = msg.update_after_move(
			    target_path.value(), target_maildir, target_flags); !res)
			return Err(res.error());

		/* 4. update message worked; re-store it */
		if (auto&& res = update_message_unlocked(msg, old_path); !res)
			return Err(res.error());
	}

	/* 6. Profit! */
	return Ok(PathMessage{std::move(*target_path), std::move(msg)});
}

Store::IdVec
Store::find_duplicates(const std::string& message_id) const
{
	std::lock_guard guard{priv_->lock_};

	return priv_->find_duplicates_unlocked(*this, message_id);
}


Result<Store::IdPathVec>
Store::move_message(Store::Id id,
		    Option<const std::string&> target_mdir,
		    Option<Flags> new_flags,
		    MoveOptions opts)
{
	auto filter_dup_flags=[](Flags old_flags, Flags new_flags) -> Flags {
		new_flags = flags_keep_unmutable(old_flags, new_flags, Flags::Draft);
		new_flags = flags_keep_unmutable(old_flags, new_flags, Flags::Flagged);
		new_flags = flags_keep_unmutable(old_flags, new_flags, Flags::Trashed);
		return new_flags;
	};

	std::lock_guard guard{priv_->lock_};

	auto msg{priv_->find_message_unlocked(id)};
	if (!msg)
		return Err(Error::Code::Store, "cannot find message <{}>", id);

	const auto message_id{msg->message_id()};
	auto res{priv_->move_message_unlocked(std::move(*msg), target_mdir, new_flags, opts)};
	if (!res)
		return Err(res.error());

	IdPathVec id_paths{{id, res->first}};
	if (none_of(opts & Store::MoveOptions::DupFlags) || message_id.empty() || !new_flags)
		return Ok(std::move(id_paths));

	/* handle the dup-flags case; i.e. apply (a subset of) the flags to
	 * all messages with the same message-id as well */
	auto dups{priv_->find_duplicates_unlocked(*this, message_id)};
	for (auto&& dupid: dups) {

		if (dupid == id)
			continue; // already

		auto dup_msg{priv_->find_message_unlocked(dupid)};
		if (!dup_msg)
			continue; // no such message

		/* For now, don't change Draft/Flagged/Trashed */
		const auto dup_flags{filter_dup_flags(dup_msg->flags(), *new_flags)};
		/* use the updated new_flags and MoveOptions without DupFlags (so we don't
		 * recurse) */
		opts = opts & ~MoveOptions::DupFlags;
		if (auto dup_res = priv_->move_message_unlocked(
			    std::move(*dup_msg), Nothing, dup_flags, opts); !dup_res)
			mu_warning("failed to move dup: {}", dup_res.error().what());
		else
			id_paths.emplace_back(dupid, dup_res->first);
	}

	// sort the dup paths by name;
	std::sort(id_paths.begin() + 1, id_paths.end(),
		  [](const auto& idp1, const auto& idp2) { return idp1.second < idp2.second; });

	return Ok(std::move(id_paths));
}

Store::IdVec
Store::id_vec(const IdPathVec& ips)
{
	IdVec idv;
	for (auto&& ip: ips)
		idv.emplace_back(ip.first);

	return idv;
}


time_t
Store::dirstamp(const std::string& path) const
{
	std::string ts;

	{
		std::unique_lock lock{priv_->lock_};
		ts = xapian_db().metadata(path);
	}

	return ts.empty() ? 0 /*epoch*/ : ::strtoll(ts.c_str(), {}, 16);
}

void
Store::set_dirstamp(const std::string& path, time_t tstamp)
{
	std::unique_lock lock{priv_->lock_};

	xapian_db().set_metadata(path, mu_format("{:x}", tstamp));
}

bool
Store::contains_message(const std::string& path) const
{
	std::unique_lock lock{priv_->lock_};

	return xapian_db().term_exists(field_from_id(Field::Id::Path).xapian_term(path));
}

std::size_t
Store::for_each_message_path(Store::ForEachMessageFunc msg_func) const
{
	size_t n{};

	xapian_try([&] {
		std::lock_guard guard{priv_->lock_};
		auto enq{xapian_db().enquire()};

		enq.set_query(Xapian::Query::MatchAll);
		enq.set_cutoff(0, 0);

		Xapian::MSet matches(enq.get_mset(0, xapian_db().size()));
		constexpr auto path_no{field_from_id(Field::Id::Path).value_no()};
		for (auto&& it = matches.begin(); it != matches.end(); ++it, ++n)
			if (!msg_func(*it, it.get_document().get_value(path_no)))
				break;
	});

	return n;
}

std::size_t
Store::for_each_term(Field::Id field_id, Store::ForEachTermFunc func) const
{
	return xapian_db().all_terms(field_from_id(field_id).xapian_term(), func);
}

std::mutex&
Store::lock() const
{
	return priv_->lock_;
}

Result<QueryResults>
Store::run_query(const std::string& expr,
		 Field::Id sortfield_id,
		 QueryFlags flags, size_t maxnum) const
{
	return Query{*this}.run(expr, sortfield_id, flags, maxnum);
}

size_t
Store::count_query(const std::string& expr) const
{
	return xapian_try([&] {
		std::lock_guard guard{priv_->lock_};
		Query           q{*this};
		return q.count(expr); }, 0);
}

std::string
Store::parse_query(const std::string& expr, bool xapian) const
{
	return xapian_try([&] {
		std::lock_guard guard{priv_->lock_};
		Query           q{*this};

		return q.parse(expr, xapian);
	}, std::string{});
}


std::vector<std::string>
Store::maildirs() const
{
	std::vector<std::string> mdirs;
	const auto prefix_size{root_maildir().size()};

	Scanner::Handler handler = [&](const std::string& path, auto&& _1, auto&& _2) {
		auto md{path.substr(prefix_size)};
		mdirs.emplace_back(md.empty() ? "/" : std::move(md));
		return true;
	};

	Scanner scanner{root_maildir(), handler, Scanner::Mode::MaildirsOnly};
	scanner.start();
	std::sort(mdirs.begin(), mdirs.end());

	return mdirs;
}

Message::Options
Store::message_options() const
{
	return priv_->message_opts_;
}
