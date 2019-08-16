/*
** Copyright (C) 2019 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <mutex>
#include <array>
#include <cstdlib>
#include <xapian.h>
#include <unordered_map>
#include <atomic>

#include "mu-store.hh"
#include "mu-str.h"
#include "mu-msg-part.h"
#include "parser/utils.hh"

using namespace Mu;

constexpr auto SchemaVersionKey     = "schema-version";
constexpr auto MaildirKey           = "maildir";
constexpr auto ContactsKey          = "contacts";
constexpr auto PersonalAddressesKey = "personal-addresses";
constexpr auto CreatedKey           = "created";

constexpr auto ExpectedSchemaVersion = MU_STORE_SCHEMA_VERSION;

using Addresses = Store::Addresses;

/* we cache these prefix strings, so we don't have to allocate them all
 * the time; this should save 10-20 string allocs per message */
G_GNUC_CONST static const std::string&
prefix (MuMsgFieldId mfid)
{
	static std::string fields[MU_MSG_FIELD_ID_NUM];
	static bool initialized = false;

	if (G_UNLIKELY(!initialized)) {
		for (int i = 0; i != MU_MSG_FIELD_ID_NUM; ++i)
			fields[i] = std::string (1, mu_msg_field_xapian_prefix
						 ((MuMsgFieldId)i));
		initialized = true;
	}

	return fields[mfid];
}

static void
add_synonym_for_flag (MuFlags flag, Xapian::WritableDatabase *db)
{
	static const std::string pfx(prefix(MU_MSG_FIELD_ID_FLAGS));

	db->clear_synonyms (pfx + mu_flag_name (flag));
	db->add_synonym (pfx + mu_flag_name (flag), pfx +
			 (std::string(1, (char)(tolower(mu_flag_char(flag))))));
}


static void
add_synonym_for_prio (MuMsgPrio prio, Xapian::WritableDatabase *db)
{
	static const std::string pfx (prefix(MU_MSG_FIELD_ID_PRIO));

	std::string s1 (pfx + mu_msg_prio_name (prio));
	std::string s2 (pfx + (std::string(1, mu_msg_prio_char (prio))));

	db->clear_synonyms (s1);
	db->clear_synonyms (s2);

	db->add_synonym (s1, s2);
}

struct Store::Private {

#define LOCKED std::lock_guard<std::mutex> l(lock_);

        Private (const std::string& path, bool readonly):
                db_path_{path},
                db_{readonly?
                std::make_shared<Xapian::Database>(db_path_) :
                std::make_shared<Xapian::WritableDatabase>(db_path_, Xapian::DB_OPEN)},
                maildir_{db()->get_metadata(MaildirKey)},
                created_{atoll(db()->get_metadata(CreatedKey).c_str())},
                schema_version_{db()->get_metadata(SchemaVersionKey)},
                personal_addresses_{Mux::split(db()->get_metadata(PersonalAddressesKey),",")},
                contacts_{db()->get_metadata(ContactsKey)} {
        }

        Private (const std::string& path, const std::string& maildir):
                db_path_{path},
                db_{std::make_shared<Xapian::WritableDatabase>(
                                db_path_, Xapian::DB_CREATE_OR_OVERWRITE)},
                maildir_{maildir},
                created_{time({})},
                schema_version_{MU_STORE_SCHEMA_VERSION} {

                writable_db()->set_metadata(SchemaVersionKey, schema_version_);
                writable_db()->set_metadata(MaildirKey, maildir_);
                writable_db()->set_metadata(CreatedKey,
                                            Mux::format("%" PRId64, (int64_t)created_));
        }

        ~Private() {
                if (wdb())
                        wdb()->set_metadata (ContactsKey, contacts_.serialize());
        }

        std::shared_ptr<Xapian::Database> db() const {
                if (!db_)
                        throw std::runtime_error ("no db");
                return db_;
        }

        std::shared_ptr<Xapian::WritableDatabase> wdb() const {
                return std::dynamic_pointer_cast<Xapian::WritableDatabase>(db_);
        }

        std::shared_ptr<Xapian::WritableDatabase> writable_db() const {
                auto w_db{wdb()};
                if (!w_db)
                        throw std::runtime_error ("database is read-only");
                else
                        return w_db;
        }

        void set_personal_addresses (const Addresses& addresses) {

                std::string all_addresses;
                personal_addresses_.clear();

                for (const auto& addr : addresses) {
                        // very basic check; just ensure there's no ',' in the address.
                        // we don't insist on full RFC5322
                        if (addr.find(",") != std::string::npos)
                                throw std::runtime_error ("e-mail address with ',': " + addr);
                        if (!all_addresses.empty())
                                all_addresses += ',';
                        all_addresses += addr;
                        personal_addresses_.emplace_back(addr);
                }
                writable_db()->set_metadata (PersonalAddressesKey, all_addresses);
        }

        void add_synonyms () {
                mu_flags_foreach ((MuFlagsForeachFunc)add_synonym_for_flag,
                                  writable_db().get());
                mu_msg_prio_foreach ((MuMsgPrioForeachFunc)add_synonym_for_prio,
                                     writable_db().get());
        }


        time_t metadata_time_t (const std::string& key) const {
                const auto ts = db()->get_metadata(key);
                return (time_t)atoll(db()->get_metadata(key).c_str());
        }

        const std::string                 db_path_;
        std::shared_ptr<Xapian::Database> db_;
        const std::string                 maildir_;
        const time_t                      created_{};
        const std::string                 schema_version_;
        Addresses                         personal_addresses_;
        Contacts                          contacts_;

        bool                              in_transaction_{};
        std::mutex                        lock_;

        mutable std::atomic<std::size_t> ref_count_{1};
};


#undef  LOCKED
#define LOCKED std::lock_guard<std::mutex> l(priv_->lock_);

struct NeedsReIndex: public std::runtime_error {
        using std::runtime_error::runtime_error;
};

Store::Store (const std::string& path, bool readonly):
        priv_{std::make_unique<Private>(path, readonly)}
{
        if (ExpectedSchemaVersion == schema_version())
                return; // All is good; nothing further to do

        if (readonly || maildir().empty())
                throw NeedsReIndex("database needs reindexing");

        g_debug ("upgrading database");
        const auto addresses{personal_addresses()};
        const auto mdir{maildir()};

        priv_.reset();
        priv_ = std::make_unique<Private> (path, mdir);
        set_personal_addresses (addresses);
}

Store::Store (const std::string& path, const std::string& maildir):
        priv_{std::make_unique<Private>(path, maildir)}
{}

Store::~Store() = default;

bool
Store::read_only() const
{
        LOCKED;
        return !priv_->wdb();
}

const std::string&
Store::maildir () const
{
	LOCKED;
        return priv_->maildir_;
}

void
Store::set_personal_addresses(const Store::Addresses& addresses)
{
	LOCKED;
	priv_->set_personal_addresses (addresses);
}

const Store::Addresses&
Store::personal_addresses(void) const
{
	LOCKED;
	return priv_->personal_addresses_;
}

const std::string&
Store::database_path() const
{
        LOCKED;
        return priv_->db_path_;
}

const Contacts&
Store::contacts() const
{
        LOCKED;
        return priv_->contacts_;
}

std::size_t
Store::size() const
{
        LOCKED;

        return priv_->db()->get_doccount();
}

bool
Store::empty() const
{
        return size() == 0;
}


const std::string&
Store::schema_version() const
{
	LOCKED;

        return priv_->schema_version_;
}

time_t
Store::created() const
{
	LOCKED;

        return priv_->created_;
}

time_t
Store::path_tstamp (const std::string& path) const
{
        LOCKED;

        const auto ts = priv_->db()->get_metadata(path);
        if (ts.empty())
                return 0;
        else
                return (time_t)strtoll(ts.c_str(), NULL, 16);
}

void
Store::set_path_tstamp (const std::string& path, time_t tstamp)
{
        LOCKED;

        std::array<char, 2*sizeof(tstamp)+1> data{};
        const std::size_t len = snprintf (data.data(), data.size(), "%x", tstamp);

        priv_->writable_db()->set_metadata(path, std::string{data.data(), len});
}

void
Store::begin_transaction () try
{
        LOCKED;

        priv_->wdb()->begin_transaction();
        priv_->in_transaction_ = true;

} MU_XAPIAN_CATCH_BLOCK;

void
Store::commit_transaction () try
{
        LOCKED;

        priv_->in_transaction_ = false;
        priv_->wdb()->commit_transaction();

} MU_XAPIAN_CATCH_BLOCK;

void
Store::cancel_transaction () try
{
        LOCKED;

        priv_->in_transaction_ = false;
        priv_->wdb()->cancel_transaction();

} MU_XAPIAN_CATCH_BLOCK;

bool
Store::in_transaction () const
{
        LOCKED;

        return priv_->in_transaction_;
}


////////////////////////////////////////////////////////////////////////////////
// C compat
extern "C" {


struct MuStore_ { Mu::Store* self; };


static const Mu::Store*
self (const MuStore *store)
{
        if (!store) {
                g_error ("invalid store"); // terminates
                return {};
        }

        return reinterpret_cast<const Mu::Store*>(store);
}

static Mu::Store*
mutable_self (MuStore *store)
{
        if (!store) {
                g_error ("invalid store"); // terminates
                return {};
        }

        auto s = reinterpret_cast<Mu::Store*>(store);
        if (s->read_only()) {
                g_error ("store is read-only"); // terminates
                return {};
        }

        return s;
}


static void
hash_str (char *buf, size_t buf_size, const char *data)
{
        snprintf(buf, buf_size, "016%" PRIx64, mu_util_get_hash(data));
}


static std::string
get_uid_term (const char* path)
{
        char uid_term[1 + 16 + 1] = {'\0'};
        uid_term[0] = mu_msg_field_xapian_prefix(MU_MSG_FIELD_ID_UID);
        hash_str(uid_term + 1, sizeof(uid_term)-1, path);

        return std::string{uid_term, sizeof(uid_term)};
}


MuStore*
mu_store_new_readable (const char* xpath, GError **err)
{
	g_return_val_if_fail (xpath, NULL);

	g_debug ("opening database at %s (read-only)", xpath);

	try {
		return reinterpret_cast<MuStore*>(new Store (xpath));

        } catch (const NeedsReIndex& nri) {
                g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_XAPIAN_NEEDS_REINDEX,
                             "database @ %s needs (re)indexing", xpath);
        // } catch (const Xapian::DatabaseNotFoundError& dbe) { // Xapian 1.4.10
        //         g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_XAPIAN_NEEDS_REINDEX,
        //                      "database @ %s not found", xpath);
        } catch (const Xapian::DatabaseOpeningError& dbe) {
                g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_XAPIAN_NEEDS_REINDEX,
                             "failed to open database @ %s", xpath);
        } catch (...) {
                g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_XAPIAN,
                             "error opening database @ %s", xpath);
        }

        return NULL;
}

MuStore*
mu_store_new_writable (const char* xpath, GError **err)
{
	g_return_val_if_fail (xpath, NULL);

	g_debug ("opening database at %s (writable)", xpath);

	try {
                return reinterpret_cast<MuStore*>(new Store (xpath, false/*!readonly*/));
        } catch (const NeedsReIndex& nri) {
                g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_XAPIAN_NEEDS_REINDEX,
                             "database @ %s needs (re)indexing", xpath);
        // } catch (const Xapian::DatabaseNotFoundError& dbe) { // Xapian 1.4.10
        //         g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_XAPIAN_NEEDS_REINDEX,
        //                      "database @ %s not found", xpath);
        } catch (const Xapian::DatabaseOpeningError& dbe) {
                g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_XAPIAN_NEEDS_REINDEX,
                             "failed to open database @ %s", xpath);
        } catch (const Xapian::DatabaseLockError& dle) {
                g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_XAPIAN_CANNOT_GET_WRITELOCK,
                             "database @ %s is write-locked already", xpath);
        } catch (...) {
                g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_XAPIAN,
                             "error opening database @ %s", xpath);
        }

        return NULL;
}

MuStore*
mu_store_new_create (const char* xpath, const char *maildir, GError **err)
{
	g_return_val_if_fail (xpath, NULL);
        g_return_val_if_fail (maildir, NULL);

	g_debug ("create database at %s (maildir=%s)", xpath, maildir);

	try {
                return reinterpret_cast<MuStore*>(
			new Store (xpath, std::string{maildir}));

        } catch (const Xapian::DatabaseLockError& dle) {
                g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_XAPIAN_CANNOT_GET_WRITELOCK,
                             "database @ %s is write-locked already", xpath);
        } catch (...) {
                g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_XAPIAN,
                             "error opening database @ %s", xpath);
        }

        return NULL;
}


MuStore*
mu_store_ref (MuStore* store)
{
        g_return_val_if_fail (store, NULL);
        g_return_val_if_fail (self(store)->priv()->ref_count_ > 0, NULL);

        ++self(store)->priv()->ref_count_;
        return store;
}


MuStore*
mu_store_unref (MuStore* store)
{
        g_return_val_if_fail (store, NULL);
        g_return_val_if_fail (self(store)->priv()->ref_count_ > 0, NULL);

	auto me = reinterpret_cast<Mu::Store*>(store);

        if (--me->priv()->ref_count_ == 0)
                delete me;

        return NULL;
}

gboolean
mu_store_is_read_only (const MuStore *store)
{
	g_return_val_if_fail (store, FALSE);

	try {
		return self(store)->read_only() ? TRUE : FALSE;

	} MU_XAPIAN_CATCH_BLOCK_RETURN(FALSE);

}

gboolean
mu_store_clear (MuStore *store, GError **err)
{
	g_return_val_if_fail (store, FALSE);

	// FIXME: implement
	return TRUE;
}


const MuContacts*
mu_store_contacts (MuStore *store)
{
	g_return_val_if_fail (store, FALSE);

	try {
		return self(store)->contacts().mu_contacts();

	} MU_XAPIAN_CATCH_BLOCK_RETURN(FALSE);
}

unsigned
mu_store_count (const MuStore *store, GError **err)
{
	g_return_val_if_fail (store, (unsigned)-1);

	try {
		return self(store)->size();

 	} MU_XAPIAN_CATCH_BLOCK_G_ERROR_RETURN(err, MU_ERROR_XAPIAN,
					       (unsigned)-1);
}


const char*
mu_store_schema_version (const MuStore *store)
{
	g_return_val_if_fail (store, NULL);

	return self(store)->schema_version().c_str();
}

XapianDatabase*
mu_store_get_read_only_database (MuStore *store)
{
	g_return_val_if_fail (store, NULL);
	return (XapianWritableDatabase*)self(store)->priv()->db().get();
}

gboolean
mu_store_contains_message (const MuStore *store, const char* path, GError **err)
{
	g_return_val_if_fail (store, FALSE);
	g_return_val_if_fail (path, FALSE);

	try {
		const std::string term (get_uid_term(path));
 		return self(store)->priv()->db()->term_exists (term) ? TRUE: FALSE;

	} MU_XAPIAN_CATCH_BLOCK_G_ERROR_RETURN(err, MU_ERROR_XAPIAN, FALSE);
}

unsigned
mu_store_get_docid_for_path (const MuStore *store, const char* path, GError **err)
{
	g_return_val_if_fail (store, FALSE);
	g_return_val_if_fail (path, FALSE);

	try {
		const std::string term (get_uid_term(path));
		Xapian::Query query (term);
		Xapian::Enquire enq (*self(store)->priv()->db().get());

		enq.set_query (query);

		Xapian::MSet mset (enq.get_mset (0,1));
		if (mset.empty())
			throw std::runtime_error ("message not found");

		return *mset.begin();

	} MU_XAPIAN_CATCH_BLOCK_G_ERROR_RETURN(err, MU_ERROR_XAPIAN,
					       MU_STORE_INVALID_DOCID);
}


MuError
mu_store_foreach (MuStore *store,
		  MuStoreForeachFunc func, void *user_data, GError **err)
{
	g_return_val_if_fail (self, MU_ERROR);
	g_return_val_if_fail (func, MU_ERROR);

	try {
		Xapian::Enquire enq (*self(store)->priv()->db().get());

		enq.set_query  (Xapian::Query::MatchAll);
		enq.set_cutoff (0,0);

		Xapian::MSet matches(enq.get_mset (0, self(store)->size()));
		if (matches.empty())
			return MU_OK; /* database is empty */

		for (Xapian::MSet::iterator iter = matches.begin();
		     iter != matches.end(); ++iter) {
			Xapian::Document doc (iter.get_document());
			const std::string path(doc.get_value(MU_MSG_FIELD_ID_PATH));
			MuError res = func (path.c_str(), user_data);
			if (res != MU_OK)
				return res;
		}

	} MU_XAPIAN_CATCH_BLOCK_G_ERROR_RETURN(err, MU_ERROR_XAPIAN,
					       MU_ERROR_XAPIAN);

	return MU_OK;
}


MuMsg*
mu_store_get_msg (const MuStore *store, unsigned docid, GError **err)
{
	g_return_val_if_fail (store, NULL);
	g_return_val_if_fail (docid != 0, NULL);

	try {
		Xapian::Document *doc =
			new Xapian::Document
			(self(store)->priv()->db()->get_document (docid));
		return mu_msg_new_from_doc ((XapianDocument*)doc, err);

	} MU_XAPIAN_CATCH_BLOCK_G_ERROR_RETURN (err, MU_ERROR_XAPIAN, 0);
}


const char*
mu_store_database_path (const MuStore *store)
{
        g_return_val_if_fail (store, NULL);

        return self(store)->database_path().c_str();
}


const char*
mu_store_maildir (const MuStore *store)
{
        g_return_val_if_fail (store, NULL);

        return self(store)->maildir().c_str();
}


time_t
mu_store_created (const MuStore *store)
{
        g_return_val_if_fail (store, (time_t)0);

        return self(store)->created();
}



void
mu_store_set_personal_addresses (MuStore *store, const char **my_addresses)
{
	g_return_if_fail (store);

	if (!my_addresses)
		return;

	Store::Addresses addrs;
	for (auto i = 0; my_addresses[i]; ++i)
		addrs.emplace_back(my_addresses[i]);

	mutable_self(store)->set_personal_addresses (addrs);
}


char**
mu_store_personal_addresses (const MuStore *store)
{
	g_return_val_if_fail (store, NULL);

        const auto size = self(store)->personal_addresses().size();
        auto addrs = g_new0 (char*, 1 + size);
        for (size_t i = 0; i != size; ++i)
                addrs[i] = g_strdup(self(store)->personal_addresses()[i].c_str());

        return addrs;
}


void
mu_store_flush (MuStore *store) try {

        g_return_if_fail (store);

        if (self(store)->priv()->in_transaction_)
                mutable_self(store)->commit_transaction ();

        mutable_self(store)->priv()->wdb()->set_metadata(
                ContactsKey, self(store)->priv()->contacts_.serialize());

} MU_XAPIAN_CATCH_BLOCK;

static void
add_terms_values_date (Xapian::Document& doc, MuMsg *msg, MuMsgFieldId mfid)
{
	const auto dstr = Mux::date_to_time_t_string (
		(time_t)mu_msg_get_field_numeric (msg, mfid));

	doc.add_value ((Xapian::valueno)mfid, dstr);
}

static void
add_terms_values_size (Xapian::Document& doc, MuMsg *msg, MuMsgFieldId mfid)
{
	const auto szstr =
		Mux::size_to_string (mu_msg_get_field_numeric (msg, mfid));
	doc.add_value ((Xapian::valueno)mfid, szstr);
}

G_GNUC_CONST
static const std::string&
flag_val (char flagchar)
{
	static const std::string
		pfx (prefix(MU_MSG_FIELD_ID_FLAGS)),
		draftstr   (pfx + (char)tolower(mu_flag_char(MU_FLAG_DRAFT))),
		flaggedstr (pfx + (char)tolower(mu_flag_char(MU_FLAG_FLAGGED))),
		passedstr  (pfx + (char)tolower(mu_flag_char(MU_FLAG_PASSED))),
		repliedstr (pfx + (char)tolower(mu_flag_char(MU_FLAG_REPLIED))),
		seenstr	   (pfx + (char)tolower(mu_flag_char(MU_FLAG_SEEN))),
		trashedstr (pfx + (char)tolower(mu_flag_char(MU_FLAG_TRASHED))),
		newstr	   (pfx + (char)tolower(mu_flag_char(MU_FLAG_NEW))),
		signedstr  (pfx + (char)tolower(mu_flag_char(MU_FLAG_SIGNED))),
		cryptstr   (pfx + (char)tolower(mu_flag_char(MU_FLAG_ENCRYPTED))),
		attachstr  (pfx + (char)tolower(mu_flag_char(MU_FLAG_HAS_ATTACH))),
		unreadstr  (pfx + (char)tolower(mu_flag_char(MU_FLAG_UNREAD))),
		liststr    (pfx + (char)tolower(mu_flag_char(MU_FLAG_LIST)));

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

	default:
		g_return_val_if_reached (flaggedstr);
		return flaggedstr;
	}
}

/* pre-calculate; optimization */
G_GNUC_CONST static const std::string&
prio_val (MuMsgPrio prio)
{
	static const std::string pfx (prefix(MU_MSG_FIELD_ID_PRIO));

	static const std::string
		low (pfx + std::string(1, mu_msg_prio_char(MU_MSG_PRIO_LOW))),
		norm (pfx + std::string(1, mu_msg_prio_char(MU_MSG_PRIO_NORMAL))),
		high (pfx + std::string(1, mu_msg_prio_char(MU_MSG_PRIO_HIGH)));

	switch (prio) {
	case MU_MSG_PRIO_LOW:    return low;
	case MU_MSG_PRIO_NORMAL: return norm;
	case MU_MSG_PRIO_HIGH:   return high;
	default:
		g_return_val_if_reached (norm);
		return norm;
	}
}


static void // add term, truncate if needed.
add_term (Xapian::Document& doc, const std::string& term)
{
	if (term.length() < MU_STORE_MAX_TERM_LENGTH)
		doc.add_term(term);
	else
		doc.add_term(term.substr(0, MU_STORE_MAX_TERM_LENGTH));
}



static void
add_terms_values_number (Xapian::Document& doc, MuMsg *msg, MuMsgFieldId mfid)
{
	gint64 num = mu_msg_get_field_numeric (msg, mfid);

	const std::string numstr (Xapian::sortable_serialise((double)num));
	doc.add_value ((Xapian::valueno)mfid, numstr);

	if (mfid == MU_MSG_FIELD_ID_FLAGS) {
		const char *cur = mu_flags_to_str_s
			((MuFlags)num,(MuFlagType)MU_FLAG_TYPE_ANY);
		g_return_if_fail (cur);
		while (*cur) {
			add_term (doc, flag_val(*cur));
			++cur;
		}

	} else if (mfid == MU_MSG_FIELD_ID_PRIO)
		add_term (doc, prio_val((MuMsgPrio)num));
}


/* for string and string-list */
static void
add_terms_values_str (Xapian::Document& doc, const char *val, MuMsgFieldId mfid)
{
	const auto flat = Mux::utf8_flatten (val);

	if (mu_msg_field_xapian_index (mfid)) {
		Xapian::TermGenerator termgen;
		termgen.set_document (doc);
		termgen.index_text (flat, 1, prefix(mfid));
	}

	if (mu_msg_field_xapian_term(mfid))
		add_term(doc, prefix(mfid) + flat);

}

static void
add_terms_values_string (Xapian::Document& doc, MuMsg *msg, MuMsgFieldId mfid)
{
	const char *orig;

	if (!(orig = mu_msg_get_field_string (msg, mfid)))
		return; /* nothing to do */

	/* the value is what we display in search results; the
	 * unchanged original */
	if (mu_msg_field_xapian_value(mfid))
		doc.add_value ((Xapian::valueno)mfid, orig);

	add_terms_values_str (doc, orig, mfid);
}

static void
add_terms_values_string_list (Xapian::Document& doc, MuMsg *msg,
			      MuMsgFieldId mfid)
{
	const GSList *lst;

	lst = mu_msg_get_field_string_list (msg, mfid);
	if (!lst)
		return;

	if (mu_msg_field_xapian_value (mfid)) {
		gchar *str;
		str = mu_str_from_list (lst, ',');
		if (str)
			doc.add_value ((Xapian::valueno)mfid, str);
		g_free (str);
	}

	if (mu_msg_field_xapian_term (mfid)) {
		for (; lst; lst = g_slist_next ((GSList*)lst))
			add_terms_values_str (doc, (const gchar*)lst->data,
					      mfid);
	}
}


struct PartData {
	PartData (Xapian::Document& doc, MuMsgFieldId mfid):
		_doc (doc), _mfid(mfid) {}
	Xapian::Document _doc;
	MuMsgFieldId _mfid;
};

/* index non-body text parts */
static void
maybe_index_text_part (MuMsg *msg, MuMsgPart *part, PartData *pdata)
{
	char *txt;
	Xapian::TermGenerator termgen;

	/* only deal with attachments/messages; inlines are indexed as
	 * body parts */
	if (!(part->part_type & MU_MSG_PART_TYPE_ATTACHMENT) &&
	    !(part->part_type & MU_MSG_PART_TYPE_MESSAGE))
		return;

	txt = mu_msg_part_get_text (msg, part, MU_MSG_OPTION_NONE);
	if (!txt)
		return;

	termgen.set_document(pdata->_doc);
	const auto str = Mux::utf8_flatten (txt);
	g_free (txt);

	termgen.index_text (str, 1, prefix(MU_MSG_FIELD_ID_EMBEDDED_TEXT));
}


static void
each_part (MuMsg *msg, MuMsgPart *part, PartData *pdata)
{
	char *fname;
	static const std::string
		file (prefix(MU_MSG_FIELD_ID_FILE)),
		mime (prefix(MU_MSG_FIELD_ID_MIME));

	/* save the mime type of any part */
	if (part->type) {
		char ctype[MU_STORE_MAX_TERM_LENGTH + 1];
		snprintf(ctype, sizeof(ctype), "%s/%s", part->type, part->subtype);
		add_term(pdata->_doc, mime + ctype);
	}

	if ((fname = mu_msg_part_get_filename (part, FALSE))) {
		const auto flat = Mux::utf8_flatten (fname);
		g_free (fname);
		add_term(pdata->_doc, file + flat);
	}

	maybe_index_text_part (msg, part, pdata);
}


static void
add_terms_values_attach (Xapian::Document& doc, MuMsg *msg,
			 MuMsgFieldId mfid)
{
	PartData pdata (doc, mfid);
	mu_msg_part_foreach (msg, MU_MSG_OPTION_RECURSE_RFC822,
			     (MuMsgPartForeachFunc)each_part, &pdata);
}


static void
add_terms_values_body (Xapian::Document& doc, MuMsg *msg,
		       MuMsgFieldId mfid)
{
	if (mu_msg_get_flags(msg) & MU_FLAG_ENCRYPTED)
		return; /* ignore encrypted bodies */

	auto str = mu_msg_get_body_text (msg, MU_MSG_OPTION_NONE);
	if (!str) /* FIXME: html->txt fallback needed */
		str = mu_msg_get_body_html (msg, MU_MSG_OPTION_NONE);
	if (!str)
		return; /* no body... */

	Xapian::TermGenerator termgen;
	termgen.set_document(doc);

	const auto flat = Mux::utf8_flatten(str);
	termgen.index_text (flat, 1, prefix(mfid));
}

struct MsgDoc {
	Xapian::Document *_doc;
	MuMsg		 *_msg;
	Store            *_store;
	/* callback data, to determine whether this message is 'personal' */
	gboolean          _personal;
	const Addresses  *_my_addresses;
};


static void
add_terms_values_default (MuMsgFieldId mfid, MsgDoc *msgdoc)
{
	if (mu_msg_field_is_numeric (mfid))
		add_terms_values_number
			(*msgdoc->_doc, msgdoc->_msg, mfid);
	else if (mu_msg_field_is_string (mfid))
		add_terms_values_string
			(*msgdoc->_doc, msgdoc->_msg, mfid);
	else if (mu_msg_field_is_string_list(mfid))
		add_terms_values_string_list
			(*msgdoc->_doc, msgdoc->_msg, mfid);
	else
		g_return_if_reached ();
}

static void
add_terms_values (MuMsgFieldId mfid, MsgDoc* msgdoc)
{
	/* note: contact-stuff (To/Cc/From) will handled in
	 * each_contact_info, not here */
	if (!mu_msg_field_xapian_index(mfid) &&
	    !mu_msg_field_xapian_term(mfid) &&
	    !mu_msg_field_xapian_value(mfid))
		return;

	switch (mfid) {
	case MU_MSG_FIELD_ID_DATE:
		add_terms_values_date (*msgdoc->_doc, msgdoc->_msg, mfid);
		break;
	case MU_MSG_FIELD_ID_SIZE:
		add_terms_values_size (*msgdoc->_doc, msgdoc->_msg, mfid);
		break;
	case MU_MSG_FIELD_ID_BODY_TEXT:
		add_terms_values_body (*msgdoc->_doc, msgdoc->_msg, mfid);
		break;
	/* note: add_terms_values_attach handles _FILE, _MIME and
	 * _ATTACH_TEXT msgfields */
	case MU_MSG_FIELD_ID_FILE:
		add_terms_values_attach (*msgdoc->_doc, msgdoc->_msg, mfid);
		break;
	case MU_MSG_FIELD_ID_MIME:
	case MU_MSG_FIELD_ID_EMBEDDED_TEXT:
		break;
	case MU_MSG_FIELD_ID_THREAD_ID:
	case MU_MSG_FIELD_ID_UID:
		break; /* already taken care of elsewhere */
	default:
		return add_terms_values_default (mfid, msgdoc);
	}
}


static const std::string&
xapian_pfx (MuMsgContact *contact)
{
	static const std::string empty;

	/* use ptr to string to prevent copy... */
	switch (contact->type) {
	case MU_MSG_CONTACT_TYPE_TO:
		return prefix(MU_MSG_FIELD_ID_TO);
	case MU_MSG_CONTACT_TYPE_FROM:
		return prefix(MU_MSG_FIELD_ID_FROM);
	case MU_MSG_CONTACT_TYPE_CC:
		return prefix(MU_MSG_FIELD_ID_CC);
	case MU_MSG_CONTACT_TYPE_BCC:
		return prefix(MU_MSG_FIELD_ID_BCC);
	default:
		g_warning ("unsupported contact type %u",
			   (unsigned)contact->type);
		return empty;
	}
}


static void
add_address_subfields (Xapian::Document& doc, const char *addr,
		       const std::string& pfx)
{
	const char *at, *domain_part;
	char *name_part;

	/* add "foo" and "bar.com" as terms as well for
	 * "foo@bar.com" */
	if (G_UNLIKELY(!(at = (g_strstr_len (addr, -1, "@")))))
		return;

	name_part   = g_strndup(addr, at - addr); // foo
	domain_part = at + 1;

	add_term(doc, pfx + name_part);
	add_term(doc, pfx + domain_part);

 	g_free (name_part);
}

static gboolean
each_contact_info (MuMsgContact *contact, MsgDoc *msgdoc)
{
	/* for now, don't store reply-to addresses */
	if (mu_msg_contact_type (contact) == MU_MSG_CONTACT_TYPE_REPLY_TO)
		return TRUE;

	const std::string pfx (xapian_pfx(contact));
	if (pfx.empty())
		return TRUE; /* unsupported contact type */

	if (!mu_str_is_empty(contact->name)) {
		Xapian::TermGenerator termgen;
		termgen.set_document (*msgdoc->_doc);
		const auto flat = Mux::utf8_flatten(contact->name);
		termgen.index_text (flat, 1, pfx);
	}

	if (!mu_str_is_empty(contact->email)) {
		const auto flat = Mux::utf8_flatten(contact->email);
		add_term(*msgdoc->_doc, pfx + flat);
		add_address_subfields (*msgdoc->_doc, contact->email, pfx);
		/* store it also in our contacts cache */
		auto& contacts = msgdoc->_store->priv()->contacts_;
                contacts.add(Mu::ContactInfo(contact->full_address,
                                             contact->email,
                                             contact->name ? contact->name : "",
                                             msgdoc->_personal,
                                             mu_msg_get_date(msgdoc->_msg)));
	}

	return TRUE;
}


static gboolean
each_contact_check_if_personal (MuMsgContact *contact, MsgDoc *msgdoc)
{
	if (msgdoc->_personal || !contact->email)
		return TRUE;

	for (const auto& cur : *msgdoc->_my_addresses) {
		if (g_ascii_strcasecmp
                    (contact->email,
                     (const char*)cur.c_str()) == 0) {
			msgdoc->_personal = TRUE;
			break;
		}
	}

	return TRUE;
}

static Xapian::Document
new_doc_from_message (MuStore *store, MuMsg *msg)
{
	Xapian::Document doc;
	MsgDoc docinfo = {&doc, msg, mutable_self(store), 0, NULL};

	mu_msg_field_foreach ((MuMsgFieldForeachFunc)add_terms_values, &docinfo);

	/* determine whether this is 'personal' email, ie. one of my
	 * e-mail addresses is explicitly mentioned -- it's not a
	 * mailing list message. Callback will update docinfo->_personal */
        const auto& personal_addresses = self(store)->personal_addresses();
        if (personal_addresses.size()) {
		docinfo._my_addresses = &personal_addresses;
		mu_msg_contact_foreach
			(msg,
			 (MuMsgContactForeachFunc)each_contact_check_if_personal,
			 &docinfo);
	}

	/* also store the contact-info as separate terms, and add it
	 * to the cache */
	mu_msg_contact_foreach (msg, (MuMsgContactForeachFunc)each_contact_info,
				&docinfo);

	// g_printerr ("\n--%s\n--\n", doc.serialise().c_str());

	return doc;
}

static void
update_threading_info (Xapian::WritableDatabase* db,
		       MuMsg *msg, Xapian::Document& doc)
{
	const GSList *refs;

	// refs contains a list of parent messages, with the oldest
	// one first until the last one, which is the direct parent of
	// the current message. of course, it may be empty.
	//
	// NOTE: there may be cases where the list is truncated; we happily
	// ignore that case.
	refs  = mu_msg_get_references (msg);

        char thread_id[16+1];
        hash_str(thread_id, sizeof(thread_id),
                 refs ? (const char*)refs->data : mu_msg_get_msgid (msg));

	add_term (doc, prefix(MU_MSG_FIELD_ID_THREAD_ID) + thread_id);
	doc.add_value((Xapian::valueno)MU_MSG_FIELD_ID_THREAD_ID, thread_id);
}


static unsigned
add_or_update_msg (MuStore *store, unsigned docid, MuMsg *msg, GError **err)
{
	g_return_val_if_fail (store, MU_STORE_INVALID_DOCID);
	g_return_val_if_fail (msg, MU_STORE_INVALID_DOCID);

	try {
		Xapian::docid id;
		Xapian::Document doc (new_doc_from_message(store, msg));
		const std::string term (get_uid_term (mu_msg_get_path(msg)));

                auto self = mutable_self(store);
                auto wdb   = self->priv()->wdb();

		if (!self->in_transaction())
			self->begin_transaction();

		add_term (doc, term);

		// update the threading info if this message has a message id
		if (mu_msg_get_msgid (msg))
			update_threading_info (wdb.get(), msg, doc);

		if (docid == 0)
			id = wdb->replace_document (term, doc);
		else {
			wdb->replace_document (docid, doc);
			id = docid;
		}

                // FIXME
		// if (self->inc_processed() % store->batch_size() == 0)
		// 	self->commit_transaction();

		return id;

	} MU_XAPIAN_CATCH_BLOCK_G_ERROR (err, MU_ERROR_XAPIAN_STORE_FAILED);

        // FIXME
        // if (store->in_transaction())
	// 	store->rollback_transaction();

	return MU_STORE_INVALID_DOCID;
}

unsigned
mu_store_add_msg (MuStore *store, MuMsg *msg, GError **err)
{
	g_return_val_if_fail (store, MU_STORE_INVALID_DOCID);
	g_return_val_if_fail (msg, MU_STORE_INVALID_DOCID);

	return add_or_update_msg (store, 0, msg, err);
}

unsigned
mu_store_update_msg (MuStore *store, unsigned docid, MuMsg *msg, GError **err)
{
	g_return_val_if_fail (store, MU_STORE_INVALID_DOCID);
	g_return_val_if_fail (msg, MU_STORE_INVALID_DOCID);
	g_return_val_if_fail (docid != 0, MU_STORE_INVALID_DOCID);

	return add_or_update_msg (store, docid, msg, err);
}

unsigned
mu_store_add_path (MuStore *store, const char *path, const char *maildir,
		   GError **err)
{
	MuMsg *msg;
	unsigned docid;

	g_return_val_if_fail (store, FALSE);
	g_return_val_if_fail (path, FALSE);

	msg = mu_msg_new_from_file (path, maildir, err);
	if (!msg)
		return MU_STORE_INVALID_DOCID;

	docid = add_or_update_msg (store, 0, msg, err);
	mu_msg_unref (msg);

	return docid;
}


XapianWritableDatabase*
mu_store_get_writable_database (MuStore *store)
{
	g_return_val_if_fail (store, NULL);

	return (XapianWritableDatabase*)mutable_self(store)->priv()->wdb().get();
}


gboolean
mu_store_remove_path (MuStore *store, const char *msgpath)
{
	g_return_val_if_fail (store, FALSE);
	g_return_val_if_fail (msgpath, FALSE);

	try {
		const std::string term{(get_uid_term(msgpath))};
                auto wdb = mutable_self(store)->priv()->wdb();

		wdb->delete_document (term);
		//store->inc_processed();

		return TRUE;

	} MU_XAPIAN_CATCH_BLOCK_RETURN (FALSE);
}


gboolean
mu_store_set_timestamp (MuStore *store, const char* msgpath,
			time_t stamp, GError **err)
{
        g_return_val_if_fail (store, FALSE);
	g_return_val_if_fail (msgpath, FALSE);

	mutable_self(store)->set_path_tstamp(msgpath, stamp);

        return TRUE;
}

time_t
mu_store_get_timestamp (const MuStore *store, const char *msgpath, GError **err)
{
	g_return_val_if_fail (store, 0);
	g_return_val_if_fail (msgpath, 0);

        return self(store)->path_tstamp(msgpath);
}


}
