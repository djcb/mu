/*
** Copyright (C) 2021-2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include "utils/mu-str.h"
#include "utils/mu-error.hh"

#include "utils/mu-utils.hh"
#include "utils/mu-xapian-utils.hh"

using namespace Mu;

static_assert(std::is_same<Store::Id, Xapian::docid>::value, "wrong type for Store::Id");

constexpr auto SchemaVersionKey     = "schema-version";
constexpr auto RootMaildirKey       = "maildir"; // XXX: make this 'root-maildir'
constexpr auto ContactsKey          = "contacts";
constexpr auto PersonalAddressesKey = "personal-addresses";
constexpr auto CreatedKey           = "created";
constexpr auto BatchSizeKey         = "batch-size";
constexpr auto DefaultBatchSize     = 250'000U;

constexpr auto MaxMessageSizeKey     = "max-message-size";
constexpr auto DefaultMaxMessageSize = 100'000'000U;

constexpr auto ExpectedSchemaVersion = MU_STORE_SCHEMA_VERSION;


struct Store::Private {
	enum struct XapianOpts { ReadOnly, Open, CreateOverwrite };

	Private(const std::string& path, bool readonly)
	    : read_only_{readonly}, db_{make_xapian_db(path,
						       read_only_ ? XapianOpts::ReadOnly
								  : XapianOpts::Open)},
	      properties_{make_properties(path)}, contacts_cache_{db().get_metadata(ContactsKey),
						     properties_.personal_addresses}
	{}

	Private(const std::string& path,
		const std::string& root_maildir,
		const StringVec& personal_addresses,
		const Store::Config& conf)
	    : read_only_{false}, db_{make_xapian_db(path, XapianOpts::CreateOverwrite)},
	      properties_{init_metadata(conf, path, root_maildir, personal_addresses)},
	      contacts_cache_{"", properties_.personal_addresses}
	{}

	~Private()
	try {
		g_debug("closing store @ %s", properties_.database_path.c_str());
		if (!read_only_) {
			transaction_maybe_commit(true /*force*/);
		}
	} catch (...) {
		g_critical("caught exception in store dtor");
	}

	std::unique_ptr<Xapian::Database> make_xapian_db(const std::string db_path, XapianOpts opts)
	try {
		switch (opts) {
		case XapianOpts::ReadOnly: return std::make_unique<Xapian::Database>(db_path);
		case XapianOpts::Open:
			return std::make_unique<Xapian::WritableDatabase>(db_path, Xapian::DB_OPEN);
		case XapianOpts::CreateOverwrite:
			return std::make_unique<Xapian::WritableDatabase>(
			    db_path,
			    Xapian::DB_CREATE_OR_OVERWRITE);
		default: throw std::logic_error("invalid xapian options");
		}

	} catch (const Xapian::DatabaseError& xde) {
		throw Mu::Error(Error::Code::Store,
				"failed to open store @ %s: %s",
				db_path.c_str(),
				xde.get_msg().c_str());
	} catch (...) {
		throw Mu::Error(Error::Code::Internal,
				"something went wrong when opening store @ %s",
				db_path.c_str());
	}

	const Xapian::Database& db() const { return *db_.get(); }

	Xapian::WritableDatabase& writable_db()
	{
		if (read_only_)
			throw Mu::Error(Error::Code::AccessDenied, "database is read-only");
		return dynamic_cast<Xapian::WritableDatabase&>(*db_.get());
	}

	// If not started yet, start a transaction. Otherwise, just update the transaction size.
	void transaction_inc() noexcept
	{
		if (transaction_size_ == 0) {
			g_debug("starting transaction");
			xapian_try([this] { writable_db().begin_transaction(); });
		}
		++transaction_size_;
	}

	// Opportunistically commit a transaction if the transaction size
	// filled up a batch, or with force.
	void transaction_maybe_commit(bool force = false) noexcept
	{
		if (force || transaction_size_ >= properties_.batch_size) {
			if (contacts_cache_.dirty()) {
				xapian_try([&] {
					writable_db().set_metadata(ContactsKey,
								   contacts_cache_.serialize());
				});
			}
			if (transaction_size_ == 0)
				return; // nothing more to do here.

			g_debug("committing transaction (n=%zu,%zu)",
				transaction_size_, metadata_cache_.size());
			xapian_try([this] {
				writable_db().commit_transaction();
				for (auto&& mdata : metadata_cache_)
					writable_db().set_metadata(mdata.first, mdata.second);
				transaction_size_ = 0;
			});
		}
	}

	void add_synonyms()
	{
		for (auto&& info: AllMessageFlagInfos) {
			constexpr auto field{field_from_id(Field::Id::Flags)};
			const auto s1{field.xapian_term(info.name)};
			const auto s2{field.xapian_term(info.shortcut)};
			writable_db().clear_synonyms(s1);
			writable_db().clear_synonyms(s2);
			writable_db().add_synonym(s1, s2);
		}

		for (auto&& prio : AllMessagePriorities) {
			constexpr auto field{field_from_id(Field::Id::Priority)};
			const auto s1{field.xapian_term(to_string(prio))};
			const auto s2{field.xapian_term(to_char(prio))};
			writable_db().clear_synonyms(s1);
			writable_db().clear_synonyms(s2);
			writable_db().add_synonym(s1, s2);
		}
	}

	time_t metadata_time_t(const std::string& key) const
	{
		const auto ts = db().get_metadata(key);
		return (time_t)atoll(db().get_metadata(key).c_str());
	}

	Store::Properties make_properties(const std::string& db_path)
	{
		Store::Properties props;

		props.database_path	 = db_path;
		props.schema_version	 = db().get_metadata(SchemaVersionKey);
		props.created		 = ::atoll(db().get_metadata(CreatedKey).c_str());
		props.read_only		 = read_only_;
		props.batch_size	 = ::atoll(db().get_metadata(BatchSizeKey).c_str());
		props.max_message_size	 = ::atoll(db().get_metadata(MaxMessageSizeKey).c_str());
		props.root_maildir       = db().get_metadata(RootMaildirKey);
		props.personal_addresses = Mu::split(db().get_metadata(PersonalAddressesKey), ",");

		return props;
	}

	Store::Properties init_metadata(const Store::Config& conf,
				      const std::string&   path,
				      const std::string&   root_maildir,
				      const StringVec&     personal_addresses)
	{
		writable_db().set_metadata(SchemaVersionKey, ExpectedSchemaVersion);
		writable_db().set_metadata(CreatedKey, Mu::format("%" PRId64, (int64_t)::time({})));

		const size_t batch_size = conf.batch_size ? conf.batch_size : DefaultBatchSize;
		writable_db().set_metadata(BatchSizeKey, Mu::format("%zu", batch_size));

		const size_t max_msg_size = conf.max_message_size ? conf.max_message_size
								  : DefaultMaxMessageSize;
		writable_db().set_metadata(MaxMessageSizeKey, Mu::format("%zu", max_msg_size));

		writable_db().set_metadata(RootMaildirKey, root_maildir);

		std::string addrs;
		for (const auto& addr : personal_addresses) { // _very_ minimal check.
			if (addr.find(",") != std::string::npos)
				throw Mu::Error(Error::Code::InvalidArgument,
						"e-mail address '%s' contains comma",
						addr.c_str());
			addrs += (addrs.empty() ? "" : ",") + addr;
		}
		writable_db().set_metadata(PersonalAddressesKey, addrs);

		return make_properties(path);
	}

	/* metadata to write as part of a transaction commit */
	std::unordered_map<std::string, std::string> metadata_cache_;

	const bool                        read_only_{};
	std::unique_ptr<Xapian::Database> db_;

	const Store::Properties properties_;
	ContactsCache            contacts_cache_;
	std::unique_ptr<Indexer> indexer_;

	size_t     transaction_size_{};
	std::mutex lock_;
};

Store::Store(const std::string& path, bool readonly)
    : priv_{std::make_unique<Private>(path, readonly)}
{
	if (properties().schema_version != ExpectedSchemaVersion)
		throw Mu::Error(Error::Code::SchemaMismatch,
				"expected schema-version %s, but got %s; "
				"please use 'mu init'",
				ExpectedSchemaVersion,
				properties().schema_version.c_str());
}

Store::Store(const std::string&   path,
	     const std::string&   maildir,
	     const StringVec&     personal_addresses,
	     const Store::Config& conf)
    : priv_{std::make_unique<Private>(path, maildir, personal_addresses, conf)}
{
}

Store::~Store() = default;

const Store::Properties&
Store::properties() const
{
	return priv_->properties_;
}

const ContactsCache&
Store::contacts_cache() const
{
	return priv_->contacts_cache_;
}

const Xapian::Database&
Store::database() const
{
	return priv_->db();
}

Xapian::WritableDatabase&
Store::writable_database()
{
	return priv_->writable_db();
}

Indexer&
Store::indexer()
{
	std::lock_guard guard{priv_->lock_};

	if (properties().read_only)
		throw Error{Error::Code::Store, "no indexer for read-only store"};
	else if (!priv_->indexer_)
		priv_->indexer_ = std::make_unique<Indexer>(*this);

	return *priv_->indexer_.get();
}

std::size_t
Store::size() const
{
	std::lock_guard guard{priv_->lock_};
	return priv_->db().get_doccount();
}

bool
Store::empty() const
{
	return size() == 0;
}

static std::string
maildir_from_path(const std::string& root, const std::string& path)
{
	if (G_UNLIKELY(root.empty()) || root.length() >= path.length() || path.find(root) != 0)
		throw Mu::Error{Error::Code::InvalidArgument,
				"root '%s' is not a proper suffix of path '%s'",
				root.c_str(),
				path.c_str()};

	auto mdir{path.substr(root.length())};
	auto slash{mdir.rfind('/')};

	if (G_UNLIKELY(slash == std::string::npos) || slash < 4)
		throw Mu::Error{Error::Code::InvalidArgument, "invalid path: %s", path.c_str()};
	mdir.erase(slash);
	auto subdir = mdir.data() + slash - 4;
	if (G_UNLIKELY(strncmp(subdir, "/cur", 4) != 0 && strncmp(subdir, "/new", 4)))
		throw Mu::Error{Error::Code::InvalidArgument,
				"cannot find '/new' or '/cur' - invalid path: %s",
				path.c_str()};
	if (mdir.length() == 4)
		return "/";

	mdir.erase(mdir.length() - 4);
	return mdir;
}

Result<Store::Id>
Store::add_message(const std::string& path, bool use_transaction)
{
	const auto maildir{maildir_from_path(properties().root_maildir, path)};
	auto msg{Message::make_from_path(Message::Options::None, path, maildir)};
	if (G_UNLIKELY(!msg))
		return Err(msg.error());

	std::lock_guard guard{priv_->lock_};

	if (use_transaction)
		priv_->transaction_inc();

	const auto docid = priv_->writable_db().add_document(
		msg->document().xapian_document());

	if (use_transaction) /* commit if batch is full */
		priv_->transaction_maybe_commit();

	if (G_UNLIKELY(docid == InvalidId))
		return Err(Error::Code::Message, "failed to add message");

	g_debug("added message @ %s; docid = %u", path.c_str(), docid);

	return Ok(static_cast<Store::Id>(docid));
}

bool
Store::update_message(const Message& msg, unsigned docid)
{
	return xapian_try(
		[&]{
			priv_->writable_db().replace_document(
				docid, msg.document().xapian_document());

			g_debug("updated message %u @ %s", docid, msg.path().c_str());
			return true;
	}, false);
}

bool
Store::remove_message(const std::string& path)
{
	return xapian_try(
	    [&] {
		    std::lock_guard   guard{priv_->lock_};
		    const auto term{field_from_id(Field::Id::Path).xapian_term(path)};
		    priv_->writable_db().delete_document(term);
		    g_debug("deleted message @ %s from store", path.c_str());

		    return true;
	    },
	    false);
}

void
Store::remove_messages(const std::vector<Store::Id>& ids)
{
	std::lock_guard guard{priv_->lock_};

	priv_->transaction_inc();

	xapian_try([&] {
		for (auto&& id : ids) {
			priv_->writable_db().delete_document(id);
		}
	});

	priv_->transaction_maybe_commit(true /*force*/);
}


Result<Message>
Store::move_message(Store::Id id,
		    Option<const std::string&> target_mdir,
		    Option<Flags> new_flags, bool change_name)
{
	auto msg = find_message(id);
	if (!msg)
		return Err(Error::Code::Store, "cannot find message <%u>", id);

	const auto	target_flags   = new_flags.value_or(msg->flags());
	const auto	target_maildir = target_mdir.value_or(msg->maildir());

	/* 1. first determine the file system path of the target */
	const auto target_path =
		mu_maildir_determine_target(msg->path(),
					    properties().root_maildir,
					    target_maildir,
					    target_flags,
					    change_name);
	if (!target_path)
		return Err(target_path.error());

	/* 2. let's move it */
	const auto move_res =
		mu_maildir_move_message(msg->path(),
					target_path.value(),
					true/*ignore dups*/);
	if (!move_res)
		return Err(move_res.error());

	/* 3. file move worked, now update the message with the new info.*/
	const auto update_res = msg->update_after_move(target_path.value(),
						       target_maildir,
						       target_flags);
	if (!update_res)
		return Err(update_res.error());

	/* 4. update message worked; re-store it */
	if (!update_message(*msg, id))
		return Err(Error::Code::Store, "failed to update message <%u>", id);

	/* 5. Profit! */
	return Ok(std::move(msg.value()));
}




std::string
Store::metadata(const std::string& key) const
{
	// get metadata either from the (uncommitted) cache or from the store.

	std::lock_guard guard{priv_->lock_};

	const auto it = priv_->metadata_cache_.find(key);
	if (it != priv_->metadata_cache_.end())
		return it->second;
	else
		return xapian_try([&] {
			return priv_->db().get_metadata(key);
		}, "");
}

void
Store::set_metadata(const std::string& key, const std::string& val)
{
	// get metadata either from the (uncommitted) cache or from the store.

	std::lock_guard guard{priv_->lock_};

	priv_->metadata_cache_.erase(key);
	priv_->metadata_cache_.emplace(key, val);
}


time_t
Store::dirstamp(const std::string& path) const
{
	constexpr auto epoch = static_cast<time_t>(0);
	const auto ts{metadata(path)};
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

	set_metadata(path, std::string{data.data(), len});
}

Option<Message>
Store::find_message(Store::Id docid) const
{
	return xapian_try(
	    [&]()->Option<Message> {
		    std::lock_guard guard{priv_->lock_};
		    auto res = Message::make_from_document(priv_->db().get_document(docid));
		    if (res)
			    return Some(std::move(res.value()));
		    else
			    return Nothing;
	    },
	    Nothing);
}

bool
Store::contains_message(const std::string& path) const
{
	return xapian_try(
	    [&] {
		    std::lock_guard   guard{priv_->lock_};
		    const auto term{field_from_id(Field::Id::Path).xapian_term(path)};
		    return priv_->db().term_exists(term);
	    },
	    false);
}

std::size_t
Store::for_each_message_path(Store::ForEachMessageFunc msg_func) const
{
	size_t n{};

	xapian_try([&] {
		std::lock_guard guard{priv_->lock_};
		Xapian::Enquire enq{priv_->db()};

		enq.set_query(Xapian::Query::MatchAll);
		enq.set_cutoff(0, 0);

		Xapian::MSet matches(enq.get_mset(0, priv_->db().get_doccount()));
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
	size_t n{};

	xapian_try([&] {
		/*
		 * Do _not_ take a lock; this is only called from
		 * the message parser which already has the lock
		 */
		std::vector<std::string> terms;
		const auto prefix{field_from_id(field_id).xapian_term()};
		for (auto it = priv_->db().allterms_begin(prefix);
		     it != priv_->db().allterms_end(prefix); ++it) {
			if (!func(*it))
				break;
		}
	});

	return n;
}

std::mutex&
Store::lock() const
{
	return priv_->lock_;
}

Option<QueryResults>
Store::run_query(const std::string& expr,
		 Option<Field::Id> sortfield_id,
		 QueryFlags flags, size_t maxnum) const
{
	return xapian_try([&] {
		Query q{*this};
		return q.run(expr, sortfield_id, flags, maxnum);}, Nothing);
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
	},
			  std::string{});
}
