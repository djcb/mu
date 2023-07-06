/*
** Copyright (C) 2021-2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <chrono>
#include <memory>
#include <mutex>
#include <array>
#include <cstdlib>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <atomic>
#include <type_traits>
#include <iostream>
#include <cstring>

#include <vector>
#include <xapian.h>

#include "mu-maildir.hh"
#include "mu-store.hh"
#include "mu-query.hh"
#include "mu-xapian-db.hh"

#include "utils/mu-error.hh"

#include "utils/mu-utils.hh"
#include <utils/mu-utils-file.hh>

using namespace Mu;

static_assert(std::is_same<Store::Id, Xapian::docid>::value, "wrong type for Store::Id");

// Properties
constexpr auto ExpectedSchemaVersion = MU_STORE_SCHEMA_VERSION;

struct Store::Private {

	Private(const std::string& path, bool readonly):
		xapian_db_{make_db(path, readonly ? XapianDb::Flavor::ReadOnly
				   : XapianDb::Flavor::Open)},
		config_{xapian_db_},
		contacts_cache_{config_},
		root_maildir_{config_.get<Config::Id::RootMaildir>()}
		{}

	Private(const std::string& path, const std::string& root_maildir,
		Option<const Config&> conf):
		xapian_db_{make_db(path, XapianDb::Flavor::CreateOverwrite)},
		config_{make_config(xapian_db_, root_maildir, conf)},
		contacts_cache_{config_},
		root_maildir_{config_.get<Config::Id::RootMaildir>()}
		{}

	~Private() try {
		mu_debug("closing store @ {}", xapian_db_.path());
		if (!xapian_db_.read_only()) {
			transaction_maybe_commit(true /*force*/);
		}
	} catch (...) {
		mu_critical("caught exception in store dtor");
	}

	// If not started yet, start a transaction. Otherwise, just update the transaction size.
	void transaction_inc() noexcept {
		if (transaction_size_ == 0) {
			mu_debug("starting transaction");
			xapian_db_.begin_transaction();
		}
		++transaction_size_;
	}

	// Opportunistically commit a transaction if the transaction size
	// filled up a batch, or with force.
	void transaction_maybe_commit(bool force = false) noexcept {
		static auto batch_size = config_.get<Config::Id::BatchSize>();
		if (force || transaction_size_ >= batch_size) {
			contacts_cache_.serialize();

			if (indexer_) // save last index time.
				if (auto&& t{indexer_->completed()}; t != 0)
					config_.set<Config::Id::LastIndex>(::time({}));

			if (transaction_size_ == 0)
				return; // nothing more to do here.

			mu_debug("committing transaction (n={})", transaction_size_);
			xapian_db_.commit_transaction();
			transaction_size_ = 0;
		}
	}

	XapianDb make_db(const std::string& path, XapianDb::Flavor flavor) {
		if (auto&& res{XapianDb::make(path, flavor)}; res)
			return std::move(res.value());
		else
			throw res.error();
	}

	Config make_config(XapianDb& xapian_db, const std::string& root_maildir,
			   Option<const Config&> conf) {

		Config config{xapian_db};

		if (conf)
			config.import_configurable(*conf);

		config.set<Config::Id::RootMaildir>(root_maildir);
		config.set<Config::Id::SchemaVersion>(ExpectedSchemaVersion);

		return config;
	}

	Option<Message> find_message_unlocked(Store::Id docid) const;
	Result<Store::Id> update_message_unlocked(Message& msg, Store::Id docid);
	Result<Store::Id> update_message_unlocked(Message& msg, const std::string& old_path);
	Result<Message> move_message_unlocked(Message&& msg,
					      Option<const std::string&> target_mdir,
					      Option<Flags> new_flags,
					      MoveOptions opts);
	XapianDb xapian_db_;
	Config config_;
	ContactsCache            contacts_cache_;
	std::unique_ptr<Indexer> indexer_;

	const std::string root_maildir_;

	size_t     transaction_size_{};
	std::mutex lock_;
};

Result<Store::Id>Store::Private::update_message_unlocked(Message& msg, Store::Id docid)
{
	xapian_db_.replace_document(docid, msg.document().xapian_document());
	g_debug("updated message @ %s; docid = %u", msg.path().c_str(), docid);

	return Ok(std::move(docid));
}

Result<Store::Id>
Store::Private::update_message_unlocked(Message& msg, const std::string& path_to_replace)
{
	auto id = xapian_db_.replace_document(
		field_from_id(Field::Id::Path).xapian_term(path_to_replace),
		msg.document().xapian_document());
	return Ok(std::move(id));
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
					s_version);
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
				ExpectedSchemaVersion, s_version);
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
Store::add_message(const std::string& path, bool use_transaction)
{
	if (auto msg{Message::make_from_path(path)}; !msg)
		return Err(msg.error());
	else
		return add_message(msg.value(), use_transaction);
}

Result<Store::Id>
Store::add_message(Message& msg, bool use_transaction)
{
	std::lock_guard guard{priv_->lock_};

	const auto mdir{maildir_from_path(msg.path(),
					  root_maildir())};
	if (!mdir)
		return Err(mdir.error());

	if (auto&& res = msg.set_maildir(mdir.value()); !res)
		return Err(res.error());
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

	if (use_transaction)
		priv_->transaction_inc();

	auto res = priv_->update_message_unlocked(msg, msg.path());
	if (!res)
		return Err(res.error());

	if (use_transaction) /* commit if batch is full */
		priv_->transaction_maybe_commit();

	g_debug("added %smessage @ %s; docid = %u",
		is_personal ? "personal " : "", msg.path().c_str(), *res);

	return res;
}

Result<Store::Id>
Store::update_message(Message& msg, Store::Id docid)
{
	std::lock_guard guard{priv_->lock_};

	return priv_->update_message_unlocked(msg, docid);
}

bool
Store::remove_message(const std::string& path)
{
	std::lock_guard guard{priv_->lock_};
	const auto term{field_from_id(Field::Id::Path).xapian_term(path)};
	xapian_db().delete_document(term);
	g_debug("deleted message @ %s from store", path.c_str());
	return true;
}

void
Store::remove_messages(const std::vector<Store::Id>& ids)
{
	std::lock_guard guard{priv_->lock_};

	priv_->transaction_inc();

	for (auto&& id : ids)
		xapian_db().delete_document(id);

	priv_->transaction_maybe_commit(true /*force*/);
}


Option<Message>
Store::find_message(Store::Id docid) const
{
	std::lock_guard guard{priv_->lock_};

	return priv_->find_message_unlocked(docid);
}

/**
 * Move a message in store and filesystem.
 *
 * Lock is assumed taken already
 *
 * @param id message id
 * @param target_mdir target_midr (or Nothing for current)
 * @param new_flags new flags (or Notthing)
 * @param opts move_optionss
 *
 * @return the Message after the moving, or an Error
 */
Result<Message>
Store::Private::move_message_unlocked(Message&& msg,
				      Option<const std::string&> target_mdir,
				      Option<Flags> new_flags,
				      MoveOptions opts)
{
	const auto	old_path       = msg.path();
	const auto	target_flags   = new_flags.value_or(msg.flags());
	const auto	target_maildir = target_mdir.value_or(msg.maildir());

	/* 1. first determine the file system path of the target */
	const auto target_path =
		maildir_determine_target(msg.path(), root_maildir_,
					 target_maildir, target_flags,
					 any_of(opts & MoveOptions::ChangeName));
	if (!target_path)
		return Err(target_path.error());

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

	/* 6. Profit! */
	return Ok(std::move(msg));
}



/* get a vec of all messages with the given message id */
static Store::IdMessageVec
messages_with_msgid(const Store& store, const std::string& msgid, size_t max=100)
{
	if (msgid.size() > MaxTermLength) {
		mu_warning("invalid message-id '{}'", msgid.c_str());
		return {};
	} else if (msgid.empty())
		return {};

	const auto xprefix{field_from_id(Field::Id::MessageId).shortcut};
	/*XXX this is a bit dodgy */
	auto tmp{g_ascii_strdown(msgid.c_str(), -1)};
	auto expr{g_strdup_printf("%c:%s", xprefix, tmp)};
	g_free(tmp);

	const auto res{store.run_query(expr, {}, QueryFlags::None, max)};
	g_free(expr);
	if (!res) {
		mu_warning("failed to run message-id-query: {}", res.error().what());
		return {};
	}
	if (res->empty()) {
		mu_warning("could not find message(s) for msgid {}", msgid);
		return {};
	}

	Store::IdMessageVec imvec;
	for (auto&& mi : *res)
		imvec.emplace_back(std::make_pair(mi.doc_id(), mi.message().value()));

	return imvec;
}


static Flags
filter_dup_flags(Flags old_flags, Flags new_flags)
{
	new_flags = flags_keep_unmutable(old_flags, new_flags, Flags::Draft);
	new_flags = flags_keep_unmutable(old_flags, new_flags, Flags::Flagged);
	new_flags = flags_keep_unmutable(old_flags, new_flags, Flags::Trashed);

	return new_flags;
}

Result<Store::IdMessageVec>
Store::move_message(Store::Id id,
		    Option<const std::string&> target_mdir,
		    Option<Flags> new_flags,
		    MoveOptions opts)
{
	std::lock_guard guard{priv_->lock_};

	auto msg{priv_->find_message_unlocked(id)};
	if (!msg)
		return Err(Error::Code::Store, "cannot find message <{}>", id);

	auto res{priv_->move_message_unlocked(std::move(*msg), target_mdir, new_flags, opts)};
	if (!res)
		return Err(res.error());

	IdMessageVec imvec;
	imvec.emplace_back(std::make_pair(id, std::move(*res)));
	if (none_of(opts & Store::MoveOptions::DupFlags) || !new_flags)
		return Ok(std::move(imvec));

	/* handle the dupflags case; i.e. apply (a subset of) the flags to
	 * all messages with the same message-id as well */
	for (auto&& [docid, msg]: messages_with_msgid(*this, imvec.at(0).second.message_id())) {

		if (docid == id)
			continue; // already

		/* For now, don't change Draft/Flagged/Trashed */
		Flags dup_flags = filter_dup_flags(msg.flags(), *new_flags);

		/* use the updated new_flags and default MoveOptions (so we don't recurse, nor do we
		 * change the base-name of moved messages) */
		auto dup_res = priv_->move_message_unlocked(std::move(msg), Nothing,
							    dup_flags,
							    Store::MoveOptions::None);
		// just log a warning if it fails, but continue.
		if (dup_res)
			imvec.emplace_back(docid, std::move(*dup_res));
		else
			mu_warning("failed to move dup: {}", dup_res.error().what());
	}

	return Ok(std::move(imvec));
}

time_t
Store::dirstamp(const std::string& path) const
{
	constexpr auto epoch = static_cast<time_t>(0);
	const auto ts{xapian_db().metadata(path)};
	if (ts.empty())
		return epoch;
	else
		return static_cast<time_t>(strtoll(ts.c_str(), NULL, 16));
}

void
Store::set_dirstamp(const std::string& path, time_t tstamp)
{
	std::array<char, 2 * sizeof(tstamp) + 1> data{};
	const auto len = static_cast<size_t>(
	    g_snprintf(data.data(), data.size(), "%zx", tstamp));

	xapian_db().set_metadata(path, std::string{data.data(), len});
}

bool
Store::contains_message(const std::string& path) const
{
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

void
Store::commit()
{
	std::lock_guard guard{priv_->lock_};
	priv_->transaction_maybe_commit(true /*force*/);
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
