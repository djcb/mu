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
#include "utils/mu-error.hh"

#include "utils/mu-utils.hh"
#include <utils/mu-utils-file.hh>
#include "utils/mu-xapian-utils.hh"

using namespace Mu;

static_assert(std::is_same<Store::Id, Xapian::docid>::value, "wrong type for Store::Id");

// Properties
constexpr auto SchemaVersionKey     = "schema-version";
constexpr auto RootMaildirKey       = "maildir"; // XXX: make this 'root-maildir'
constexpr auto ContactsKey          = "contacts";
constexpr auto PersonalAddressesKey = "personal-addresses";
constexpr auto CreatedKey           = "created";
constexpr auto BatchSizeKey         = "batch-size";
constexpr auto DefaultBatchSize     = 50'000U;

constexpr auto MaxMessageSizeKey     = "max-message-size";
constexpr auto DefaultMaxMessageSize = 100'000'000U;
constexpr auto ExpectedSchemaVersion = MU_STORE_SCHEMA_VERSION;

// Stats.
constexpr auto ChangedKey           = "changed";
constexpr auto IndexedKey           = "indexed";


static std::string
tstamp_to_string(::time_t t)
{
	char buf[17];
	::snprintf(buf, sizeof(buf), "%" PRIx64, static_cast<int64_t>(t));
	return std::string(buf);
}

static ::time_t
string_to_tstamp(const std::string& str)
{
	return static_cast<::time_t>(::strtoll(str.c_str(), {}, 16));
}

struct Store::Private {
	enum struct XapianOpts { ReadOnly, Open, CreateOverwrite };

	Private(const std::string& path, bool readonly)
		: read_only_{readonly}, db_{make_xapian_db(path,
							   read_only_ ? XapianOpts::ReadOnly
							   : XapianOpts::Open)},
		  properties_{make_properties(path)},
		  contacts_cache_{db().get_metadata(ContactsKey),
		properties_.personal_addresses} {
	}

	Private(const std::string& path,
		const std::string& root_maildir,
		const StringVec& personal_addresses,
		const Store::Config& conf)
	    : read_only_{false}, db_{make_xapian_db(path, XapianOpts::CreateOverwrite)},
	      properties_{init_metadata(conf, path, root_maildir, personal_addresses)},
	      contacts_cache_{"", properties_.personal_addresses} {
	}

	~Private() try {

		g_debug("closing store @ %s", properties_.database_path.c_str());
		if (!read_only_) {
			transaction_maybe_commit(true /*force*/);
		}
	} catch (...) {
		g_critical("caught exception in store dtor");
	}

	std::unique_ptr<Xapian::Database> make_xapian_db(const std::string db_path, XapianOpts opts)
	try {
		/* we do our own flushing, set Xapian's internal one as the
		 * backstop*/
		g_setenv("XAPIAN_FLUSH_THRESHOLD", "500000", 1);

		if (g_mkdir_with_parents(db_path.c_str(), 0700) != 0)
			throw Mu::Error(Error::Code::Internal,
				"failed to create database dir %s: %s",
					db_path.c_str(), ::strerror(errno));

		switch (opts) {
		case XapianOpts::ReadOnly:
			return std::make_unique<Xapian::Database>(db_path);
		case XapianOpts::Open:
			return std::make_unique<Xapian::WritableDatabase>(db_path, Xapian::DB_OPEN);
		case XapianOpts::CreateOverwrite:
			return std::make_unique<Xapian::WritableDatabase>(
				db_path,
			    Xapian::DB_CREATE_OR_OVERWRITE);
		default:
			throw std::logic_error("invalid xapian options");
		}

	} catch (const Xapian::DatabaseLockError& xde) {
		throw Mu::Error(Error::Code::StoreLock,
				"%s", xde.get_msg().c_str());
	} catch (const Xapian::DatabaseError& xde) {
		throw Mu::Error(Error::Code::Store,
				"%s", xde.get_msg().c_str());
	} catch (const Mu::Error& me) {
		throw;
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
	void transaction_maybe_commit(bool force = false) noexcept {
		if (force || transaction_size_ >= properties_.batch_size) {
			if (contacts_cache_.dirty()) {
				xapian_try([&] {
					writable_db().set_metadata(ContactsKey,
								   contacts_cache_.serialize());
				});
			}

			if (indexer_) { // save last index time.
				if (auto&& t{indexer_->completed()}; t != 0)
					writable_db().set_metadata(
						IndexedKey, tstamp_to_string(t));
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

	time_t metadata_time_t(const std::string& key) const {
		const auto ts = db().get_metadata(key);
		return (time_t)atoll(db().get_metadata(key).c_str());
	}

	Store::Properties make_properties(const std::string& db_path)
	{
		Store::Properties props;

		props.database_path	 = db_path;
		props.schema_version	 = db().get_metadata(SchemaVersionKey);
		props.created		 = string_to_tstamp(db().get_metadata(CreatedKey));
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
					const StringVec&     personal_addresses) {

		writable_db().set_metadata(SchemaVersionKey, ExpectedSchemaVersion);
		writable_db().set_metadata(CreatedKey, tstamp_to_string(::time({})));

		const size_t batch_size = conf.batch_size ? conf.batch_size : DefaultBatchSize;
		writable_db().set_metadata(BatchSizeKey, Mu::format("%zu", batch_size));
		const size_t max_msg_size = conf.max_message_size ? conf.max_message_size
								  : DefaultMaxMessageSize;
		writable_db().set_metadata(MaxMessageSizeKey, Mu::format("%zu", max_msg_size));

		writable_db().set_metadata(RootMaildirKey, canonicalize_filename(root_maildir, {}));

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

	Option<Message> find_message_unlocked(Store::Id docid) const;
	Result<Store::Id> update_message_unlocked(Message& msg, Store::Id docid);
	Result<Store::Id> update_message_unlocked(Message& msg, const std::string& old_path);
	Result<Message> move_message_unlocked(Message&& msg,
					      Option<const std::string&> target_mdir,
					      Option<Flags> new_flags,
					      MoveOptions opts);

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

Result<Store::Id>
Store::Private::update_message_unlocked(Message& msg, Store::Id docid)
{
	return xapian_try_result([&]{
		writable_db().replace_document(docid, msg.document().xapian_document());
		g_debug("updated message @ %s; docid = %u", msg.path().c_str(), docid);
		//g_info("%s", msg.sexp().to_string().c_str());
		writable_db().set_metadata(ChangedKey, tstamp_to_string(::time({})));
		return Ok(std::move(docid));
	});
}

Result<Store::Id>
Store::Private::update_message_unlocked(Message& msg, const std::string& path_to_replace)
{
	return xapian_try_result([&]{
		auto id = writable_db().replace_document(
			field_from_id(Field::Id::Path).xapian_term(path_to_replace),
			msg.document().xapian_document());

		writable_db().set_metadata(ChangedKey, tstamp_to_string(::time({})));
		return Ok(std::move(id));
	});
}

Option<Message>
Store::Private::find_message_unlocked(Store::Id docid) const
{
	return xapian_try([&]()->Option<Message> {
		auto res = Message::make_from_document(db().get_document(docid));
		if (res)
			return Some(std::move(res.value()));
		else
			return Nothing;
		}, Nothing);
}


Store::Store(const std::string& path, Store::Options opts)
    : priv_{std::make_unique<Private>(path, none_of(opts & Store::Options::Writable))}
{
	if (none_of(opts & Store::Options::Writable) &&
	    any_of(opts & Store::Options::ReInit))
		throw Mu::Error(Error::Code::InvalidArgument,
				"Options::ReInit requires Options::Writable");

	if (any_of(opts & Store::Options::ReInit)) {
		/* user wants to re-initialize an existing store */
		Config conf{};
		conf.batch_size = properties().batch_size;
		conf.max_message_size = properties().max_message_size;
		const auto root_maildir{properties().root_maildir};
		const auto addrs{properties().personal_addresses};
		/* close the old one */
		this->priv_.reset();
		/* and create a new one. */
		Store new_store(path, root_maildir, addrs, conf);
		this->priv_ = std::move(new_store.priv_);
	}

	/* otherwise, the schema version should match. */
	if (properties().schema_version != ExpectedSchemaVersion)
		throw Mu::Error(Error::Code::SchemaMismatch,
				"expected schema-version %s, but got %s",
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

Store::Store(Store&& other)
{
	priv_ = std::move(other.priv_);
	priv_->indexer_.reset();
}

Store::~Store() = default;

const Store::Properties&
Store::properties() const
{
	return priv_->properties_;
}

Store::Statistics
Store::statistics() const
{
	Statistics stats{};

	stats.size = size();
	stats.last_change = string_to_tstamp(priv_->db().get_metadata(ChangedKey));
	stats.last_index = string_to_tstamp(priv_->db().get_metadata(IndexedKey));

	return stats;
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
					     properties().root_maildir)};
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
	return xapian_try(
	    [&] {
		    std::lock_guard   guard{priv_->lock_};
		    const auto term{field_from_id(Field::Id::Path).xapian_term(path)};
		    priv_->writable_db().delete_document(term);
		    priv_->writable_db().set_metadata(
			    ChangedKey, tstamp_to_string(::time({})));
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
		priv_->writable_db().set_metadata(
			ChangedKey, tstamp_to_string(::time({})));
	});

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
		maildir_determine_target(msg.path(), properties_.root_maildir,
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
		g_warning("invalid message-id '%s'", msgid.c_str());
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
		g_warning("failed to run message-id-query: %s", res.error().what());
		return {};
	}
	if (res->empty()) {
		g_warning("could not find message(s) for msgid %s", msgid.c_str());
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
		return Err(Error::Code::Store, "cannot find message <%u>", id);

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
			g_warning("failed to move dup: %s", dup_res.error().what());
	}

	return Ok(std::move(imvec));
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
			++n;
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
	},
			  std::string{});
}
