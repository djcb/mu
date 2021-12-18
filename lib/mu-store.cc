/*
** Copyright (C) 2021 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <unordered_map>
#include <atomic>
#include <type_traits>
#include <iostream>
#include <cstring>

#include <xapian.h>

#include "mu-store.hh"
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

static void
add_synonym_for_flag(MuFlags flag, Xapian::WritableDatabase* db)
{
	static const std::string pfx(prefix(MU_MSG_FIELD_ID_FLAGS));

	db->clear_synonyms(pfx + mu_flag_name(flag));
	db->add_synonym(pfx + mu_flag_name(flag),
	                pfx + (std::string(1, (char)(tolower(mu_flag_char(flag))))));
}

static void
add_synonym_for_prio(MuMsgPrio prio, Xapian::WritableDatabase* db)
{
	static const std::string pfx(prefix(MU_MSG_FIELD_ID_PRIO));

	std::string s1(pfx + mu_msg_prio_name(prio));
	std::string s2(pfx + (std::string(1, mu_msg_prio_char(prio))));

	db->clear_synonyms(s1);
	db->clear_synonyms(s2);

	db->add_synonym(s1, s2);
}

struct Store::Private {
#define LOCKED std::lock_guard<std::mutex> l(lock_);

	enum struct XapianOpts { ReadOnly, Open, CreateOverwrite, InMemory };

	Private(const std::string& path, bool readonly)
	    : read_only_{readonly}, db_{make_xapian_db(path,
	                                               read_only_ ? XapianOpts::ReadOnly
	                                                          : XapianOpts::Open)},
	      mdata_{make_metadata(path)}, contacts_{db().get_metadata(ContactsKey),
	                                             mdata_.personal_addresses}
	{
	}

	Private(const std::string&   path,
	        const std::string&   root_maildir,
	        const StringVec&     personal_addresses,
	        const Store::Config& conf)
	    : read_only_{false}, db_{make_xapian_db(path, XapianOpts::CreateOverwrite)},
	      mdata_{init_metadata(conf, path, root_maildir, personal_addresses)},
	      contacts_{"", mdata_.personal_addresses}
	{
	}

	Private(const std::string&   root_maildir,
	        const StringVec&     personal_addresses,
	        const Store::Config& conf)
	    : read_only_{false}, db_{make_xapian_db("", XapianOpts::InMemory)},
	      mdata_{init_metadata(conf, "", root_maildir, personal_addresses)},
	      contacts_{"", mdata_.personal_addresses}
	{
	}

	~Private()
	{
		g_debug("closing store @ %s", mdata_.database_path.c_str());
		if (!read_only_) {
			xapian_try([&] {
				writable_db().set_metadata(ContactsKey, contacts_.serialize());
			});
			transaction_maybe_commit(true /*force*/);
		}
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
		if (mdata_.in_memory)
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
		if (mdata_.in_memory || transaction_size_ == 0)
			return; // not supported or not in transaction

		if (force || transaction_size_ >= mdata_.batch_size) {
			g_debug("committing transaction (n=%zu)", transaction_size_);
			xapian_try([this] {
				writable_db().commit_transaction();
				transaction_size_ = 0;
			});
		}
	}

	void add_synonyms()
	{
		mu_flags_foreach((MuFlagsForeachFunc)add_synonym_for_flag, &writable_db());
		mu_msg_prio_foreach((MuMsgPrioForeachFunc)add_synonym_for_prio, &writable_db());
	}

	time_t metadata_time_t(const std::string& key) const
	{
		const auto ts = db().get_metadata(key);
		return (time_t)atoll(db().get_metadata(key).c_str());
	}

	Store::Metadata make_metadata(const std::string& db_path)
	{
		Store::Metadata mdata;

		mdata.database_path  = db_path;
		mdata.schema_version = db().get_metadata(SchemaVersionKey);
		mdata.created        = ::atoll(db().get_metadata(CreatedKey).c_str());
		mdata.read_only      = read_only_;

		mdata.batch_size       = ::atoll(db().get_metadata(BatchSizeKey).c_str());
		mdata.max_message_size = ::atoll(db().get_metadata(MaxMessageSizeKey).c_str());
		mdata.in_memory        = db_path.empty();

		mdata.root_maildir       = db().get_metadata(RootMaildirKey);
		mdata.personal_addresses = Mu::split(db().get_metadata(PersonalAddressesKey), ",");

		return mdata;
	}

	Store::Metadata init_metadata(const Store::Config& conf,
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

		return make_metadata(path);
	}

	Xapian::docid    add_or_update_msg(Xapian::docid docid, MuMsg* msg);
	Xapian::Document new_doc_from_message(MuMsg* msg);

	const bool                        read_only_{};
	std::unique_ptr<Xapian::Database> db_;

	const Store::Metadata    mdata_;
	Contacts                 contacts_;
	std::unique_ptr<Indexer> indexer_;

	size_t     transaction_size_{};
	std::mutex lock_;
};

static void
hash_str(char* buf, size_t buf_size, const char* data)
{
	g_snprintf(buf, buf_size, "016%" PRIx64, mu_util_get_hash(data));
}

static std::string
get_uid_term(const char* path)
{
	char uid_term[1 + 16 + 1] = {'\0'};
	uid_term[0]               = mu_msg_field_xapian_prefix(MU_MSG_FIELD_ID_UID);
	hash_str(uid_term + 1, sizeof(uid_term) - 1, path);

	return std::string{uid_term, sizeof(uid_term)};
}

#undef LOCKED
#define LOCKED std::lock_guard<std::mutex> l__(priv_->lock_)

Store::Store(const std::string& path, bool readonly)
    : priv_{std::make_unique<Private>(path, readonly)}
{
	if (metadata().schema_version != ExpectedSchemaVersion)
		throw Mu::Error(Error::Code::SchemaMismatch,
		                "expected schema-version %s, but got %s; "
		                "please use 'mu init'",
		                ExpectedSchemaVersion,
		                metadata().schema_version.c_str());
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

const Store::Metadata&
Store::metadata() const
{
	return priv_->mdata_;
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
	LOCKED;

	if (metadata().read_only)
		throw Error{Error::Code::Store, "no indexer for read-only store"};
	else if (!priv_->indexer_)
		priv_->indexer_ = std::make_unique<Indexer>(*this);

	return *priv_->indexer_.get();
}

std::size_t
Store::size() const
{
	LOCKED;
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
	LOCKED;

	GError*    gerr{};
	const auto maildir{maildir_from_path(metadata().root_maildir, path)};
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
		    LOCKED;
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
	LOCKED;

	priv_->transaction_inc();

	xapian_try([&] {
		for (auto&& id : ids) {
			priv_->writable_db().delete_document(id);
		}
	});

	priv_->transaction_maybe_commit(true /*force*/);
}

time_t
Store::dirstamp(const std::string& path) const
{
	LOCKED;

	constexpr auto epoch = static_cast<time_t>(0);
	return xapian_try([&] {
		const auto ts = priv_->db().get_metadata(path);
		if (ts.empty())
			return epoch;
		else
			return static_cast<time_t>(strtoll(ts.c_str(), NULL, 16));
	}, epoch);
}

void
Store::set_dirstamp(const std::string& path, time_t tstamp)
{
	LOCKED;

	std::array<char, 2 * sizeof(tstamp) + 1> data{};
	const auto len = static_cast<size_t>(g_snprintf(data.data(), data.size(), "%zx", tstamp));

	xapian_try([&] {
		priv_->writable_db().set_metadata(path, std::string{data.data(), len});
	});
}

MuMsg*
Store::find_message(unsigned docid) const
{
	return xapian_try(
	    [&] {
		    LOCKED;
		    Xapian::Document* doc{new Xapian::Document{priv_->db().get_document(docid)}};
		    GError*           gerr{};
		    auto msg{mu_msg_new_from_doc(reinterpret_cast<XapianDocument*>(doc), &gerr)};
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
		    LOCKED;
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
		LOCKED;
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
	LOCKED;
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
		LOCKED;
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

G_GNUC_CONST
static const std::string&
flag_val(char flagchar)
{
	static const std::string pfx(prefix(MU_MSG_FIELD_ID_FLAGS)),
	    draftstr(pfx + (char)tolower(mu_flag_char(MU_FLAG_DRAFT))),
	    flaggedstr(pfx + (char)tolower(mu_flag_char(MU_FLAG_FLAGGED))),
	    passedstr(pfx + (char)tolower(mu_flag_char(MU_FLAG_PASSED))),
	    repliedstr(pfx + (char)tolower(mu_flag_char(MU_FLAG_REPLIED))),
	    seenstr(pfx + (char)tolower(mu_flag_char(MU_FLAG_SEEN))),
	    trashedstr(pfx + (char)tolower(mu_flag_char(MU_FLAG_TRASHED))),
	    newstr(pfx + (char)tolower(mu_flag_char(MU_FLAG_NEW))),
	    signedstr(pfx + (char)tolower(mu_flag_char(MU_FLAG_SIGNED))),
	    cryptstr(pfx + (char)tolower(mu_flag_char(MU_FLAG_ENCRYPTED))),
	    attachstr(pfx + (char)tolower(mu_flag_char(MU_FLAG_HAS_ATTACH))),
	    unreadstr(pfx + (char)tolower(mu_flag_char(MU_FLAG_UNREAD))),
	    liststr(pfx + (char)tolower(mu_flag_char(MU_FLAG_LIST)));

	switch (flagchar) {
	case 'D': return draftstr;
	case 'F': return flaggedstr;
	case 'P': return passedstr;
	case 'R': return repliedstr;
	case 'S': return seenstr;
	case 'T': return trashedstr;

	case 'N': return newstr;

	case 'z': return signedstr;
	case 'x': return cryptstr;
	case 'a': return attachstr;
	case 'l': return liststr;

	case 'u': return unreadstr;

	default: g_return_val_if_reached(flaggedstr); return flaggedstr;
	}
}

/* pre-calculate; optimization */
G_GNUC_CONST static const std::string&
prio_val(MuMsgPrio prio)
{
	static const std::string pfx(prefix(MU_MSG_FIELD_ID_PRIO));

	static const std::string low(pfx + std::string(1, mu_msg_prio_char(MU_MSG_PRIO_LOW))),
	    norm(pfx + std::string(1, mu_msg_prio_char(MU_MSG_PRIO_NORMAL))),
	    high(pfx + std::string(1, mu_msg_prio_char(MU_MSG_PRIO_HIGH)));

	switch (prio) {
	case MU_MSG_PRIO_LOW: return low;
	case MU_MSG_PRIO_NORMAL: return norm;
	case MU_MSG_PRIO_HIGH: return high;
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
		const char* cur = mu_flags_to_str_s((MuFlags)num, (MuFlagType)MU_FLAG_TYPE_ANY);
		g_return_if_fail(cur);
		while (*cur) {
			add_term(doc, flag_val(*cur));
			++cur;
		}

	} else if (mfid == MU_MSG_FIELD_ID_PRIO)
		add_term(doc, prio_val((MuMsgPrio)num));
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
	PartData(Xapian::Document& doc, MuMsgFieldId mfid) : _doc(doc), _mfid(mfid) {}
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
	if (mu_msg_get_flags(msg) & MU_FLAG_ENCRYPTED)
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
	case MU_MSG_FIELD_ID_DATE: add_terms_values_date(*msgdoc->_doc, msgdoc->_msg, mfid); break;
	case MU_MSG_FIELD_ID_SIZE: add_terms_values_size(*msgdoc->_doc, msgdoc->_msg, mfid); break;
	case MU_MSG_FIELD_ID_BODY_TEXT:
		add_terms_values_body(*msgdoc->_doc, msgdoc->_msg, mfid);
		break;
	/* note: add_terms_values_attach handles _FILE, _MIME and
	 * _ATTACH_TEXT msgfields */
	case MU_MSG_FIELD_ID_FILE:
		add_terms_values_attach(*msgdoc->_doc, msgdoc->_msg, mfid);
		break;
	case MU_MSG_FIELD_ID_MIME:
	case MU_MSG_FIELD_ID_EMBEDDED_TEXT: break;
	case MU_MSG_FIELD_ID_THREAD_ID:
	case MU_MSG_FIELD_ID_UID: break; /* already taken care of elsewhere */
	default: return add_terms_values_default(mfid, msgdoc);
	}
}

static const std::string&
xapian_pfx(MuMsgContact* contact)
{
	static const std::string empty;

	/* use ptr to string to prevent copy... */
	switch (contact->type) {
	case MU_MSG_CONTACT_TYPE_TO: return prefix(MU_MSG_FIELD_ID_TO);
	case MU_MSG_CONTACT_TYPE_FROM: return prefix(MU_MSG_FIELD_ID_FROM);
	case MU_MSG_CONTACT_TYPE_CC: return prefix(MU_MSG_FIELD_ID_CC);
	case MU_MSG_CONTACT_TYPE_BCC: return prefix(MU_MSG_FIELD_ID_BCC);
	default: g_warning("unsupported contact type %u", (unsigned)contact->type); return empty;
	}
}

static void
add_address_subfields(Xapian::Document& doc, const char* addr, const std::string& pfx)
{
	const char *at, *domain_part;
	char*       name_part;

	/* add "foo" and "bar.com" as terms as well for
	 * "foo@bar.com" */
	if (G_UNLIKELY(!(at = (g_strstr_len(addr, -1, "@")))))
		return;

	name_part   = g_strndup(addr, at - addr); // foo
	domain_part = at + 1;

	add_term(doc, pfx + name_part);
	add_term(doc, pfx + domain_part);

	g_free(name_part);
}

static gboolean
each_contact_info(MuMsgContact* contact, MsgDoc* msgdoc)
{
	/* for now, don't store reply-to addresses */
	if (mu_msg_contact_type(contact) == MU_MSG_CONTACT_TYPE_REPLY_TO)
		return TRUE;

	const std::string pfx(xapian_pfx(contact));
	if (pfx.empty())
		return TRUE; /* unsupported contact type */

	if (!mu_str_is_empty(contact->name)) {
		Xapian::TermGenerator termgen;
		termgen.set_document(*msgdoc->_doc);
		const auto flat = Mu::utf8_flatten(contact->name);
		termgen.index_text(flat, 1, pfx);
	}

	if (!mu_str_is_empty(contact->email)) {
		const auto flat = Mu::utf8_flatten(contact->email);
		add_term(*msgdoc->_doc, pfx + flat);
		add_address_subfields(*msgdoc->_doc, contact->email, pfx);
		/* store it also in our contacts cache */
		auto& contacts{msgdoc->_priv->contacts_};
		contacts.add(Mu::ContactInfo(contact->full_address,
		                             contact->email,
		                             contact->name ? contact->name : "",
		                             msgdoc->_personal,
		                             mu_msg_get_date(msgdoc->_msg)));
	}

	return TRUE;
}

Xapian::Document
Store::Private::new_doc_from_message(MuMsg* msg)
{
	Xapian::Document doc;
	MsgDoc           docinfo = {&doc, msg, this, 0, NULL};

	mu_msg_field_foreach((MuMsgFieldForeachFunc)add_terms_values, &docinfo);

	mu_msg_contact_foreach(
	    msg,
	    [](auto contact, gpointer msgdocptr) -> gboolean {
		    auto msgdoc{reinterpret_cast<MsgDoc*>(msgdocptr)};

		    if (!contact->email)
			    return FALSE; // invalid contact
		    else if (msgdoc->_personal)
			    return TRUE; // already deemed personal

		    if (msgdoc->_priv->contacts_.is_personal(contact->email))
			    msgdoc->_personal = true; // this one's personal.

		    return TRUE;
	    },
	    &docinfo);

	/* also store the contact-info as separate terms, and add it
	 * to the cache */
	mu_msg_contact_foreach(msg, (MuMsgContactForeachFunc)each_contact_info, &docinfo);

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
