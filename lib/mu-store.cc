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

#include "mu-message-flags.hh"
#include "mu-msg-fields.h"
#include "mu-msg.hh"
#include "mu-store.hh"
#include "mu-query.hh"
#include "utils/mu-str.h"
#include "utils/mu-error.hh"

#include "mu-msg-part.hh"
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


/**
 * calculate a 64-bit hash for the given string, based on a combination of the
 * DJB and BKDR hash functions.
 *
 * @param a string
 *
 * @return the hash
 */
static uint64_t get_hash64 (const char* str)
{
	guint32 djbhash;
	guint32 bkdrhash;
	guint32 bkdrseed;
	guint64 hash;

	djbhash  = 5381;
	bkdrhash = 0;
	bkdrseed = 1313;

	for(unsigned u = 0U; str[u]; ++u) {
		djbhash  = ((djbhash << 5) + djbhash) + str[u];
		bkdrhash = bkdrhash * bkdrseed + str[u];
	}

	hash = djbhash;
	return (hash<<32) | bkdrhash;
}


/* we cache these prefix strings, so we don't have to allocate them all
 * the time; this should save 10-20 string allocs per message */
G_GNUC_CONST static const std::string&
prefix(MuMsgFieldId mfid)
{
	static std::string fields[MU_MSG_FIELD_ID_NUM];
	static bool        initialized = false;

	if (G_UNLIKELY(!initialized)) {
		for (int i = 0; i != MU_MSG_FIELD_ID_NUM; ++i)
			fields[i] = std::string(1, mu_msg_field_xapian_prefix((MuMsgFieldId)i));
		initialized = true;
	}

	return fields[mfid];
}

struct Store::Private {
	enum struct XapianOpts { ReadOnly, Open, CreateOverwrite, InMemory };

	Private(const std::string& path, bool readonly)
	    : read_only_{readonly}, db_{make_xapian_db(path,
						       read_only_ ? XapianOpts::ReadOnly
								  : XapianOpts::Open)},
	      properties_{make_properties(path)}, contacts_{db().get_metadata(ContactsKey),
						     properties_.personal_addresses}
	{
	}

	Private(const std::string& path,
		const std::string& root_maildir,
		const StringVec& personal_addresses,
		const Store::Config& conf)
	    : read_only_{false}, db_{make_xapian_db(path, XapianOpts::CreateOverwrite)},
	      properties_{init_metadata(conf, path, root_maildir, personal_addresses)},
	      contacts_{"", properties_.personal_addresses}
	{
	}

	Private(const std::string&   root_maildir,
		const StringVec&     personal_addresses,
		const Store::Config& conf)
	    : read_only_{false}, db_{make_xapian_db("", XapianOpts::InMemory)},
	      properties_{init_metadata(conf, "", root_maildir, personal_addresses)},
	      contacts_{"", properties_.personal_addresses}
	{
	}

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
		case XapianOpts::InMemory:
			return std::make_unique<Xapian::WritableDatabase>(
			    std::string{},
			    Xapian::DB_BACKEND_INMEMORY);
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
		if (properties_.in_memory)
			return; // not supported

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
		if (properties_.in_memory || transaction_size_ == 0)
			return; // not supported or not in transaction

		if (force || transaction_size_ >= properties_.batch_size) {
			if (contacts_.dirty()) {
				xapian_try([&] {
					writable_db().set_metadata(ContactsKey,
								   contacts_.serialize());
				});
			}
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
			const auto s1{prefix(MU_MSG_FIELD_ID_FLAGS) + std::string{info.name}};
			const auto s2{prefix(MU_MSG_FIELD_ID_FLAGS) + std::string{1,info.shortcut}};
			writable_db().clear_synonyms(s1);
			writable_db().clear_synonyms(s2);
			writable_db().add_synonym(s1, s2);
		}

		for (auto&& prio : AllMessagePriorities) {
			const auto s1{prefix(MU_MSG_FIELD_ID_PRIO) + to_string(prio)};
			const auto s2{prefix(MU_MSG_FIELD_ID_PRIO) +
				      std::string{1, to_char(prio)}};
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
		props.in_memory		 = db_path.empty();
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

	Xapian::docid    add_or_update_msg(Xapian::docid docid, MuMsg* msg);
	Xapian::Document new_doc_from_message(MuMsg* msg);

	/* metadata to write as part of a transaction commit */
	std::unordered_map<std::string, std::string> metadata_cache_;

	const bool                        read_only_{};
	std::unique_ptr<Xapian::Database> db_;

	const Store::Properties properties_;
	Contacts                 contacts_;
	std::unique_ptr<Indexer> indexer_;

	size_t     transaction_size_{};
	std::mutex lock_;
};

static void
hash_str(char* buf, size_t buf_size, const char* data)
{
	g_snprintf(buf, buf_size, "016%" PRIx64, get_hash64(data));
}

static std::string
get_uid_term(const char* path)
{
	char uid_term[1 + 16 + 1] = {'\0'};
	uid_term[0]               = mu_msg_field_xapian_prefix(MU_MSG_FIELD_ID_UID);
	hash_str(uid_term + 1, sizeof(uid_term) - 1, path);

	return std::string{uid_term, sizeof(uid_term)};
}

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

Store::Store(const std::string& maildir, const StringVec& personal_addresses, const Config& conf)
    : priv_{std::make_unique<Private>(maildir, personal_addresses, conf)}
{
}

Store::~Store() = default;

const Store::Properties&
Store::properties() const
{
	return priv_->properties_;
}

const Contacts&
Store::contacts() const
{
	return priv_->contacts_;
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

unsigned
Store::add_message(const std::string& path, bool use_transaction)
{
	std::lock_guard guard{priv_->lock_};

	GError*    gerr{};
	const auto maildir{maildir_from_path(properties().root_maildir, path)};
	auto       msg{mu_msg_new_from_file(path.c_str(), maildir.c_str(), &gerr)};
	if (G_UNLIKELY(!msg))
		throw Error{Error::Code::Message,
			    "failed to create message: %s",
			    gerr ? gerr->message : "something went wrong"};

	if (use_transaction)
		priv_->transaction_inc();

	const auto docid{priv_->add_or_update_msg(0, msg)};

	if (use_transaction) /* commit if batch is full */
		priv_->transaction_maybe_commit();

	mu_msg_unref(msg);

	if (G_UNLIKELY(docid == InvalidId))
		throw Error{Error::Code::Message, "failed to add message"};

	g_debug("added message @ %s; docid = %u", path.c_str(), docid);

	return docid;
}

bool
Store::update_message(MuMsg* msg, unsigned docid)
{
	const auto docid2{priv_->add_or_update_msg(docid, msg)};

	if (G_UNLIKELY(docid != docid2))
		throw Error{Error::Code::Internal, "failed to update message"};

	g_debug("updated message @ %s; docid = %u", mu_msg_get_path(msg), docid);

	return true;
}

bool
Store::remove_message(const std::string& path)
{
	return xapian_try(
	    [&] {
		    std::lock_guard   guard{priv_->lock_};
		    const std::string term{(get_uid_term(path.c_str()))};
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

MuMsg*
Store::find_message(unsigned docid) const
{
	return xapian_try(
	    [&] {
		    std::lock_guard   guard{priv_->lock_};
		    Xapian::Document* doc{new Xapian::Document{priv_->db().get_document(docid)}};
		    GError*           gerr{};
		    auto              msg{mu_msg_new_from_doc(
				     reinterpret_cast<XapianDocument*>(doc), &gerr)};
		    if (!msg) {
			    g_warning("could not create message: %s",
				      gerr ? gerr->message : "something went wrong");
			    g_clear_error(&gerr);
		    }
		    return msg;
	    },
	    (MuMsg*)nullptr);
}

bool
Store::contains_message(const std::string& path) const
{
	return xapian_try(
	    [&] {
		    std::lock_guard   guard{priv_->lock_};
		    const std::string term(get_uid_term(path.c_str()));
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
		for (auto&& it = matches.begin(); it != matches.end(); ++it, ++n)
			if (!msg_func(*it, it.get_document().get_value(MU_MSG_FIELD_ID_PATH)))
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

static MuMsgFieldId
field_id(const std::string& field)
{
	if (field.empty())
		return MU_MSG_FIELD_ID_NONE;

	MuMsgFieldId id = mu_msg_field_id_from_name(field.c_str(), FALSE);
	if (id != MU_MSG_FIELD_ID_NONE)
		return id;
	else if (field.length() == 1)
		return mu_msg_field_id_from_shortcut(field[0], FALSE);
	else
		return MU_MSG_FIELD_ID_NONE;
}

std::size_t
Store::for_each_term(const std::string& field, Store::ForEachTermFunc func) const
{
	size_t n{};

	xapian_try([&] {
		/*
		 * Do _not_ take a lock; this is only called from
		 * the message parser which already has the lock */
		const auto id = field_id(field.c_str());
		if (id == MU_MSG_FIELD_ID_NONE)
			return;

		char                     pfx[] = {mu_msg_field_xapian_prefix(id), '\0'};
		std::vector<std::string> terms;
		for (auto it = priv_->db().allterms_begin(pfx); it != priv_->db().allterms_end(pfx);
		     ++it) {
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
Store::run_query(const std::string& expr, MuMsgFieldId sortfieldid,
		 QueryFlags flags, size_t maxnum) const
{
	return xapian_try([&] {
		Query           q{*this};
		return q.run(expr, sortfieldid, flags, maxnum);
	},
			  Nothing);
}

size_t
Store::count_query(const std::string& expr) const
{
	return xapian_try([&] {
		std::lock_guard guard{priv_->lock_};
		Query           q{*this};

		return q.count(expr);
	},
			  0);
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

static void
add_terms_values_date(Xapian::Document& doc, MuMsg* msg, MuMsgFieldId mfid)
{
	const auto dstr = Mu::date_to_time_t_string((time_t)mu_msg_get_field_numeric(msg, mfid));

	doc.add_value((Xapian::valueno)mfid, dstr);
}

static void
add_terms_values_size(Xapian::Document& doc, MuMsg* msg, MuMsgFieldId mfid)
{
	const auto szstr = Mu::size_to_string(mu_msg_get_field_numeric(msg, mfid));
	doc.add_value((Xapian::valueno)mfid, szstr);
}

static const std::string&
prio_val(MessagePriority prio)
{
	static const std::string pfx(prefix(MU_MSG_FIELD_ID_PRIO));
	static const std::string low(pfx + std::string(1, to_char(MessagePriority::Low))),
	    norm(pfx + std::string(1, to_char(MessagePriority::Normal))),
	    high(pfx + std::string(1, to_char(MessagePriority::High)));

	switch (prio) {
	case MessagePriority::Low: return low;
	case MessagePriority::Normal: return norm;
	case MessagePriority::High: return high;
	default: g_return_val_if_reached(norm); return norm;
	}
}

static void // add term, truncate if needed.
add_term(Xapian::Document& doc, const std::string& term)
{
	if (term.length() < Store::MaxTermLength)
		doc.add_term(term);
	else
		doc.add_term(term.substr(0, Store::MaxTermLength));
}

static void
add_terms_values_number(Xapian::Document& doc, MuMsg* msg, MuMsgFieldId mfid)
{
	gint64 num = mu_msg_get_field_numeric(msg, mfid);

	const std::string numstr(Xapian::sortable_serialise((double)num));
	doc.add_value((Xapian::valueno)mfid, numstr);

	if (mfid == MU_MSG_FIELD_ID_FLAGS) {
		static const std::string pfx(prefix(MU_MSG_FIELD_ID_FLAGS));
		const auto flags{static_cast<MessageFlags>(num)};
		for (auto&& info: AllMessageFlagInfos) {
			if (any_of(info.flag & flags))
				add_term(doc, pfx + static_cast<char>(::tolower(info.shortcut)));
		}
	} else if (mfid == MU_MSG_FIELD_ID_PRIO)
		add_term(doc, prio_val(static_cast<MessagePriority>(num)));
}

/* for string and string-list */
static void
add_terms_values_str(Xapian::Document& doc, const char* val, MuMsgFieldId mfid)
{
	const auto flat = Mu::utf8_flatten(val);

	if (mu_msg_field_xapian_index(mfid)) {
		Xapian::TermGenerator termgen;
		termgen.set_document(doc);
		termgen.index_text(flat, 1, prefix(mfid));
	}

	if (mu_msg_field_xapian_term(mfid))
		add_term(doc, prefix(mfid) + flat);
}

static void
add_terms_values_string(Xapian::Document& doc, MuMsg* msg, MuMsgFieldId mfid)
{
	const char* orig;

	if (!(orig = mu_msg_get_field_string(msg, mfid)))
		return; /* nothing to do */

	/* the value is what we display in search results; the
	 * unchanged original */
	if (mu_msg_field_xapian_value(mfid))
		doc.add_value((Xapian::valueno)mfid, orig);

	add_terms_values_str(doc, orig, mfid);
}

static void
add_terms_values_string_list(Xapian::Document& doc, MuMsg* msg, MuMsgFieldId mfid)
{
	const GSList* lst;

	lst = mu_msg_get_field_string_list(msg, mfid);
	if (!lst)
		return;

	if (mu_msg_field_xapian_value(mfid)) {
		gchar* str;
		str = mu_str_from_list(lst, ',');
		if (str)
			doc.add_value((Xapian::valueno)mfid, str);
		g_free(str);
	}

	if (mu_msg_field_xapian_term(mfid)) {
		for (; lst; lst = g_slist_next((GSList*)lst))
			add_terms_values_str(doc, (const gchar*)lst->data, mfid);
	}
}

struct PartData {
	PartData(Xapian::Document& doc, MuMsgFieldId mfid)
	    : _doc(doc), _mfid(mfid) {}
	Xapian::Document _doc;
	MuMsgFieldId     _mfid;
};

/* index non-body text parts */
static void
maybe_index_text_part(MuMsg* msg, MuMsgPart* part, PartData* pdata)
{
	char*                 txt;
	Xapian::TermGenerator termgen;

	/* only deal with attachments/messages; inlines are indexed as
	 * body parts */
	if (!(part->part_type & MU_MSG_PART_TYPE_ATTACHMENT) &&
	    !(part->part_type & MU_MSG_PART_TYPE_MESSAGE))
		return;

	txt = mu_msg_part_get_text(msg, part, MU_MSG_OPTION_NONE);
	if (!txt)
		return;

	termgen.set_document(pdata->_doc);
	const auto str = Mu::utf8_flatten(txt);
	g_free(txt);

	termgen.index_text(str, 1, prefix(MU_MSG_FIELD_ID_EMBEDDED_TEXT));
}

static void
each_part(MuMsg* msg, MuMsgPart* part, PartData* pdata)
{
	char*                    fname;
	static const std::string file(prefix(MU_MSG_FIELD_ID_FILE)),
	    mime(prefix(MU_MSG_FIELD_ID_MIME));

	/* save the mime type of any part */
	if (part->type) {
		char ctype[Store::MaxTermLength + 1];
		g_snprintf(ctype, sizeof(ctype), "%s/%s", part->type, part->subtype);
		add_term(pdata->_doc, mime + ctype);
	}

	if ((fname = mu_msg_part_get_filename(part, FALSE))) {
		const auto flat = Mu::utf8_flatten(fname);
		g_free(fname);
		add_term(pdata->_doc, file + flat);
	}

	maybe_index_text_part(msg, part, pdata);
}

static void
add_terms_values_attach(Xapian::Document& doc, MuMsg* msg, MuMsgFieldId mfid)
{
	PartData pdata(doc, mfid);
	mu_msg_part_foreach(msg,
			    MU_MSG_OPTION_RECURSE_RFC822,
			    (MuMsgPartForeachFunc)each_part,
			    &pdata);
}

static void
add_terms_values_body(Xapian::Document& doc, MuMsg* msg, MuMsgFieldId mfid)
{
	if (any_of(mu_msg_get_flags(msg) & MessageFlags::Encrypted))
		return; /* ignore encrypted bodies */

	auto str = mu_msg_get_body_text(msg, MU_MSG_OPTION_NONE);
	if (!str) /* FIXME: html->txt fallback needed */
		str = mu_msg_get_body_html(msg, MU_MSG_OPTION_NONE);
	if (!str)
		return; /* no body... */

	Xapian::TermGenerator termgen;
	termgen.set_document(doc);

	const auto flat = Mu::utf8_flatten(str);
	termgen.index_text(flat, 1, prefix(mfid));
}

struct MsgDoc {
	Xapian::Document* _doc;
	MuMsg*            _msg;
	Store::Private*   _priv;
	/* callback data, to determine whether this message is 'personal' */
	gboolean         _personal;
	const StringVec* _my_addresses;
};

static void
add_terms_values_default(MuMsgFieldId mfid, MsgDoc* msgdoc)
{
	if (mu_msg_field_is_numeric(mfid))
		add_terms_values_number(*msgdoc->_doc, msgdoc->_msg, mfid);
	else if (mu_msg_field_is_string(mfid))
		add_terms_values_string(*msgdoc->_doc, msgdoc->_msg, mfid);
	else if (mu_msg_field_is_string_list(mfid))
		add_terms_values_string_list(*msgdoc->_doc, msgdoc->_msg, mfid);
	else
		g_return_if_reached();
}

static void
add_terms_values(MuMsgFieldId mfid, MsgDoc* msgdoc)
{
	/* note: contact-stuff (To/Cc/From) will handled in
	 * each_contact_info, not here */
	if (!mu_msg_field_xapian_index(mfid) && !mu_msg_field_xapian_term(mfid) &&
	    !mu_msg_field_xapian_value(mfid))
		return;

	switch (mfid) {
	case MU_MSG_FIELD_ID_DATE:
		add_terms_values_date(*msgdoc->_doc, msgdoc->_msg, mfid);
		break;
	case MU_MSG_FIELD_ID_SIZE:
		add_terms_values_size(*msgdoc->_doc, msgdoc->_msg, mfid);
		break;
	case MU_MSG_FIELD_ID_BODY_TEXT:
		add_terms_values_body(*msgdoc->_doc, msgdoc->_msg, mfid);
		break;
	/* note: add_terms_values_attach handles _FILE, _MIME and
	 * _ATTACH_TEXT msgfields */
	case MU_MSG_FIELD_ID_FILE:
		add_terms_values_attach(*msgdoc->_doc, msgdoc->_msg, mfid);
		break;
	case MU_MSG_FIELD_ID_MIME:
	case MU_MSG_FIELD_ID_EMBEDDED_TEXT:
		break;
	case MU_MSG_FIELD_ID_THREAD_ID:
	case MU_MSG_FIELD_ID_UID:
		break; /* already taken care of elsewhere */
	default:
		return add_terms_values_default(mfid, msgdoc);
	}
}


static const std::string&
xapian_pfx(const MessageContact& contact)
{
	static const std::string empty;

	/* use ptr to string to prevent copy... */
	switch (contact.type) {
	case MessageContact::Type::To:
		return prefix(MU_MSG_FIELD_ID_TO);
	case MessageContact::Type::From:
		return prefix(MU_MSG_FIELD_ID_FROM);
	case MessageContact::Type::Cc:
		return prefix(MU_MSG_FIELD_ID_CC);
	case MessageContact::Type::Bcc:
		return prefix(MU_MSG_FIELD_ID_BCC);
	default: /* REPLY_TO  not supported */
		return empty;
	}
}

static void
add_contacts_terms_values(Xapian::Document& doc, MuMsg *msg,
			  Contacts& contacts_store)
{
	Xapian::TermGenerator termgen;
	termgen.set_document(doc);

	for (auto&& contact: mu_msg_get_contacts(msg)) {

		const std::string pfx{xapian_pfx(contact)};
		if (pfx.empty())
			continue; // not supported

		if (!contact.name.empty()) {
			const auto flat = Mu::utf8_flatten(contact.name.c_str());
			termgen.index_text(flat, 1, pfx);
		}

		add_term(doc, pfx + contact.email);

		// index name / domain separately, too.
		if (auto at = contact.email.find('@'); at != std::string::npos) {
			add_term(doc, pfx + contact.email.substr(0, at));
			add_term(doc, pfx + contact.email.substr(at));
		}

		termgen.index_text_without_positions(contact.email, 1, pfx);

		/* and add to the contact store.*/
		contacts_store.add(ContactInfo{
				contact.display_name(),
				contact.email,
				contact.name,
				contacts_store.is_personal(contact.email),
				contact.message_date});
	}
}

Xapian::Document
Store::Private::new_doc_from_message(MuMsg* msg)
{
	Xapian::Document doc;
	MsgDoc           docinfo = {&doc, msg, this, 0, NULL};

	add_contacts_terms_values(doc, msg, contacts_);
	mu_msg_field_foreach((MuMsgFieldForeachFunc)add_terms_values, &docinfo);
	// g_printerr ("\n--%s\n--\n", doc.serialise().c_str());

	return doc;
}

static void
update_threading_info(MuMsg* msg, Xapian::Document& doc)
{
	const GSList* refs;

	// refs contains a list of parent messages, with the oldest
	// one first until the last one, which is the direct parent of
	// the current message. of course, it may be empty.
	//
	// NOTE: there may be cases where the list is truncated; we happily
	// ignore that case.
	refs = mu_msg_get_references(msg);

	char thread_id[16 + 1];
	hash_str(thread_id,
		 sizeof(thread_id),
		 refs ? (const char*)refs->data : mu_msg_get_msgid(msg));

	add_term(doc, prefix(MU_MSG_FIELD_ID_THREAD_ID) + thread_id);
	doc.add_value((Xapian::valueno)MU_MSG_FIELD_ID_THREAD_ID, thread_id);
}

Xapian::docid
Store::Private::add_or_update_msg(unsigned docid, MuMsg* msg)
{
	g_return_val_if_fail(msg, InvalidId);

	return xapian_try(
	    [&] {
		    Xapian::Document  doc(new_doc_from_message(msg));
		    const std::string term(get_uid_term(mu_msg_get_path(msg)));

		    add_term(doc, term);

		    // update the threading info if this message has a message id
		    if (mu_msg_get_msgid(msg))
			    update_threading_info(msg, doc);

		    if (docid == 0)
			    return writable_db().replace_document(term, doc);

		    writable_db().replace_document(docid, doc);
		    return docid;
	    },
	    InvalidId);
}
