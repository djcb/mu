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

struct Store::Private {
	enum struct XapianOpts { ReadOnly, Open, CreateOverwrite, InMemory };

	Private(const std::string& path, bool readonly)
	    : read_only_{readonly}, db_{make_xapian_db(path,
						       read_only_ ? XapianOpts::ReadOnly
								  : XapianOpts::Open)},
	      properties_{make_properties(path)}, contacts_cache_{db().get_metadata(ContactsKey),
						     properties_.personal_addresses}
	{
	}

	Private(const std::string& path,
		const std::string& root_maildir,
		const StringVec& personal_addresses,
		const Store::Config& conf)
	    : read_only_{false}, db_{make_xapian_db(path, XapianOpts::CreateOverwrite)},
	      properties_{init_metadata(conf, path, root_maildir, personal_addresses)},
	      contacts_cache_{"", properties_.personal_addresses}
	{
	}

	Private(const std::string&   root_maildir,
		const StringVec&     personal_addresses,
		const Store::Config& conf)
	    : read_only_{false}, db_{make_xapian_db("", XapianOpts::InMemory)},
	      properties_{init_metadata(conf, "", root_maildir, personal_addresses)},
	      contacts_cache_{"", properties_.personal_addresses}
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
			if (contacts_cache_.dirty()) {
				xapian_try([&] {
					writable_db().set_metadata(ContactsKey,
								   contacts_cache_.serialize());
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
	ContactsCache            contacts_cache_;
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
	return field_from_id(Field::Id::Uid).xapian_term(
		format("016%" PRIx64, get_hash64(path)));
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

static void
add_terms_values_date(Xapian::Document& doc, MuMsg* msg)
{
	constexpr auto value_no{field_from_id(Field::Id::Date).value_no()};
	const auto dstr = Mu::date_to_time_t_string(
		static_cast<time_t>(mu_msg_get_field_numeric(msg, Field::Id::Date)));

	doc.add_value(value_no, dstr);
}

static void
add_terms_values_size(Xapian::Document& doc, MuMsg* msg)
{
	constexpr auto value_no{field_from_id(Field::Id::Size).value_no()};
	const auto szstr = Mu::size_to_string(mu_msg_get_field_numeric(
						      msg, Field::Id::Size));
	doc.add_value(value_no, szstr);
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
add_terms_values_number(Xapian::Document& doc, MuMsg* msg, const Field& field)
{
	const auto num{mu_msg_get_field_numeric(msg, field.id)};
	if (field.is_value()) {
		const std::string numstr(Xapian::sortable_serialise((double)num));
		doc.add_value(field.value_no(), numstr);
	}

	if (field.id == Field::Id::Flags) {
		g_return_if_fail(num < static_cast<int64_t>(Flags::_final_));
		const auto msgflag{static_cast<Flags>(num)};
		flag_infos_for_each([&](auto&& info) {
			if (any_of(info.flag & msgflag))
				add_term(doc, field.xapian_term(info.shortcut_lower()));
		});
	} else if (field.id == Field::Id::Priority)
		add_term(doc,
			 field.xapian_term(to_char(static_cast<Priority>(num))));
}

/* for string and string-list */
static void
add_terms_values_str(Xapian::Document& doc, const char* val, const Field& field)
{
	const auto flat = Mu::utf8_flatten(val);
	if (field.is_indexable_term()) {
		Xapian::TermGenerator termgen;
		termgen.set_document(doc);
		termgen.index_text(flat, 1, field.xapian_term());
	}

	if (field.is_normal_term())
		add_term(doc, field.xapian_term(flat));
}

static void
add_terms_values_string(Xapian::Document& doc, MuMsg* msg, const Field& field)
{
	const char* orig{mu_msg_get_field_string(msg, field.id)};
	if (!orig)
		return; /* nothing to do */

	/* the value is what we display in search results; the
	 * unchanged original */
	if (field.is_value())
		doc.add_value(field.value_no(), orig);

	add_terms_values_str(doc, orig, field);
}

static void
add_terms_values_string_list(Xapian::Document& doc, MuMsg* msg, const Field& field)
{
	const GSList* lst;

	lst = mu_msg_get_field_string_list(msg, field.id);
	if (!lst)
		return;

	if (field.is_value()) {
		gchar* str;
		str = mu_str_from_list(lst, ',');
		if (str)
			doc.add_value(field.value_no(), str);
		g_free(str);
	}

	if (field.is_normal_term()) {
		for (; lst; lst = g_slist_next((GSList*)lst))
			add_terms_values_str(doc, (const gchar*)lst->data, field);
	}
}

/* index non-body text parts */
static void
maybe_index_text_part(Xapian::Document& doc, MuMsg* msg, MuMsgPart* part)
{
	char*                 txt;

	/* only deal with attachments/messages; inlines are indexed as
	 * body parts */
	if (!(part->part_type & MU_MSG_PART_TYPE_ATTACHMENT) &&
	    !(part->part_type & MU_MSG_PART_TYPE_MESSAGE))
		return;

	txt = mu_msg_part_get_text(msg, part, MU_MSG_OPTION_NONE);
	if (!txt)
		return;

	Xapian::TermGenerator termgen;
	termgen.set_document(doc);

	const auto str = Mu::utf8_flatten(txt);
	g_free(txt);

	static const auto pfx{field_from_id(Field::Id::EmbeddedText).xapian_term()};
	termgen.index_text(str, 1, pfx);
}

struct DocHolder {
	Xapian::Document& doc;
};

static void
each_part(MuMsg* msg, MuMsgPart* part, DocHolder* doc_holder)
{
	Xapian::Document& doc{doc_holder->doc};

	/* save the mime type of any part */
	if (part->type)
		add_term(doc, field_from_id(Field::Id::Mime)
			 .xapian_term(format("%s/%s", part->type, part->subtype)));

	if (char *fname = mu_msg_part_get_filename(part, FALSE); fname) {
		const auto flat{Mu::utf8_flatten(fname)};
		g_free(fname);
		add_term(doc, field_from_id(Field::Id::File).xapian_term(flat));
	}

	maybe_index_text_part(doc, msg, part);
}

static void
add_terms_values_attach(Xapian::Document& doc, MuMsg* msg)
{
	DocHolder holder{doc};

	mu_msg_part_foreach(msg,
			    MU_MSG_OPTION_RECURSE_RFC822,
			    (MuMsgPartForeachFunc)each_part,
			    &holder);
}

static void
add_terms_values_body(Xapian::Document& doc, MuMsg* msg, const Field& field)
{
	if (any_of(mu_msg_get_flags(msg) & Flags::Encrypted))
		return; /* ignore encrypted bodies */

	Xapian::TermGenerator termgen;
	termgen.set_document(doc);

	if (field.id == Field::Id::BodyText){
		if (auto str{mu_msg_get_body_text(msg, MU_MSG_OPTION_NONE)}; str)
			termgen.index_text(Mu::utf8_flatten(str), 1,
					   field.xapian_term());
	} else if (field.id == Field::Id::BodyText) {
		if (auto str{mu_msg_get_body_html(msg, MU_MSG_OPTION_NONE)}; str)
			termgen.index_text(Mu::utf8_flatten(str), 1,
					   field.xapian_term());
	}
}

static void
add_terms_values_default(Xapian::Document& doc, MuMsg *msg, const Field& field)
{
	if (field.is_numerical())
		add_terms_values_number(doc, msg, field);
	else if (field.is_string())
		add_terms_values_string(doc, msg, field);
	else if (field.is_string_list())
		add_terms_values_string_list(doc, msg, field);
	else
		g_return_if_reached();
}


static void
add_contacts_terms_values(Xapian::Document& doc, MuMsg *msg,
			  ContactsCache& contacts_cache)
{
	Xapian::TermGenerator termgen;
	termgen.set_document(doc);

	auto contacts{mu_msg_get_contacts(msg)};

	for (auto&& contact: contacts) {

		// e.g. Reply-To doesn't have a field connected.
		const auto field_opt{contact.field_id};
		if (!field_opt)
			continue;

		const auto field{field_from_id(*field_opt)};
		doc.add_value(field.value_no(), contact.display_name());
		add_term(doc, field.xapian_term(contact.email));

		if (!contact.name.empty())
			termgen.index_text(utf8_flatten(contact.name),
					   1, field.xapian_term());


		// index name / domain separately, too.
		if (const auto at = contact.email.find('@'); at != std::string::npos) {
			add_term(doc, field.xapian_term(contact.email.substr(0, at)));
			add_term(doc, field.xapian_term(contact.email.substr(at)));
		}
	}

	contacts_cache.add(std::move(contacts));
}

Xapian::Document
Store::Private::new_doc_from_message(MuMsg* msg)
{
	Xapian::Document doc;

	add_contacts_terms_values(doc, msg, this->contacts_cache_);
	field_for_each([&](auto&& field) {

		if (!field.is_searchable() && !field.is_value())
			return;

		if (field.is_contact())
			return;

		switch (field.id) {
		case Field::Id::Date:
			add_terms_values_date(doc, msg);
			break;
		case Field::Id::Size:
			add_terms_values_size(doc, msg);
			break;
		case Field::Id::BodyHtml:
		case Field::Id::BodyText:
			add_terms_values_body(doc, msg, field);
			break;
		case Field::Id::File:
			/* note: add_terms_values_attach handles _FILE, _MIME and
			 * _ATTACH_TEXT msgfields */
			add_terms_values_attach(doc, msg);
			break;
		case Field::Id::Mime:
		case Field::Id::EmbeddedText:
			break;
		case Field::Id::ThreadId:
		case Field::Id::Uid:
			break; /* already taken care of elsewhere */
		default:
			return add_terms_values_default(doc, msg, field);
		}
	});

	return doc;
}

static void
update_threading_info(MuMsg* msg, Xapian::Document& doc)
{
	const GSList* refs;
	const auto field{field_from_id(Field::Id::ThreadId)};

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
	add_term(doc, field.xapian_term(std::string{thread_id}));
	doc.add_value(field.value_no(), thread_id);
}

Xapian::docid
Store::Private::add_or_update_msg(Xapian::docid docid, MuMsg* msg)
{
	g_return_val_if_fail(msg, InvalidId);

	return xapian_try(
	    [&] {
		    Xapian::Document doc{new_doc_from_message(msg)};
		    const std::string term{get_uid_term(mu_msg_get_path(msg))};
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
