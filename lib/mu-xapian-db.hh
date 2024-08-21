/*
** Copyright (C) 2024 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef MU_XAPIAN_DB_HH__
#define MU_XAPIAN_DB_HH__

#include <variant>
#include <memory>
#include <string>
#include <mutex>
#include <thread>
#include <functional>
#include <unordered_map>

#include <glib.h>

#include <utils/mu-result.hh>
#include <utils/mu-utils.hh>

/* starting with 1.4.6, Xapian supports C++ move semantics,
 * but only with XAPIAN_MOVE_SEMANTICS defined
 */
#ifndef XAPIAN_MOVE_SEMANTICS
#define XAPIAN_MOVE_SEMANTICS
#endif /*XAPIAN_MOVE_SEMANTICS*/
#include <xapian.h>

namespace Mu {

// LCOV_EXCL_START

// avoid exception-handling boilerplate.
template <typename Func> void
xapian_try(Func&& func) noexcept
try {
	func();
} catch (const Mu::Error& me) {
	mu_critical("{}: mu error '{}'", __func__, me.what());
} catch (const Xapian::Error& xerr) {
	mu_critical("{}: xapian error '{}'", __func__, xerr.get_msg());
} catch (const std::runtime_error& re) {
	mu_critical("{}: runtime error: {}", __func__, re.what());
} catch (const std::exception& e) {
	mu_critical("{}: caught std::exception: {}", __func__, e.what());
} catch (...) {
	mu_critical("{}: caught exception", __func__);
}

template <typename Func, typename Default = std::invoke_result<Func>> auto
xapian_try(Func&& func, Default&& def) noexcept -> std::decay_t<decltype(func())>
try {
	return func();
} catch (const Mu::Error& me) {
	mu_critical("{}: mu error '{}'", __func__, me.what());
	return static_cast<Default>(def);
} catch (const Xapian::DocNotFoundError& xerr) {
	return static_cast<Default>(def);
} catch (const Xapian::Error& xerr) {
	mu_warning("{}: xapian error '{}'", __func__, xerr.get_msg());
	return static_cast<Default>(def);
} catch (const std::runtime_error& re) {
	mu_critical("{}: runtime error: {}", __func__, re.what());
	return static_cast<Default>(def);
} catch (const std::exception& e) {
	mu_critical("{}: caught std::exception: {}", __func__, e.what());
	return static_cast<Default>(def);
} catch (...) {
	mu_critical("{}: caught exception", __func__);
	return static_cast<Default>(def);
}

template <typename Func> auto
xapian_try_result(Func&& func) noexcept -> std::decay_t<decltype(func())>
try {
	return func();

} catch (const Mu::Error& me) {
	return Err(std::move(me));
} catch (const Xapian::DatabaseNotFoundError& nferr) {
	return Err(Error{Error::Code::Xapian, "failed to open database"}.
		   add_hint("Try (re)creating using `mu init'"));
} catch (const Xapian::DatabaseLockError& dlerr) {
	return Err(Error{Error::Code::StoreLock, "database locked"}.
		   add_hint("Perhaps mu is already running?"));
} catch (const Xapian::DatabaseCorruptError& dcerr) {
	return Err(Error{Error::Code::Xapian, "failed to read database"}.
		   add_hint("Try (re)creating using `mu init'"));
} catch (const Xapian::DocNotFoundError& dnferr) {
	return Err(Error{Error::Code::Xapian, "message not found in database"}.
		   add_hint("Try reopening the database"));
} catch (const Xapian::Error& xerr) {
	return Err(Error::Code::Xapian, "{}", xerr.get_msg());
} catch (const std::runtime_error& re) {
	return Err(Error::Code::Internal, "runtime error: {}", re.what());
} catch (const std::exception& e) {
	return Err(Error::Code::Internal, "caught std::exception: {}", e.what());
} catch (...) {
	return Err(Error::Code::Internal, "caught exception");
}

// LCOV_EXCL_STOP

/// abstract base
struct MetadataIface {
	virtual ~MetadataIface(){}
	virtual void set_metadata(const std::string& name, const std::string& val) = 0;
	virtual std::string metadata(const std::string& name) const		   = 0;
	virtual bool read_only() const						   = 0;

	using each_func = std::function<void(const std::string&, const std::string&)>;
	virtual void for_each(each_func&& func) const =0;

	/*
	 * These are special: handled on the Xapian db level
	 * rather than Config
	 */
	static inline constexpr std::string_view	created_key	= "created";
	static inline constexpr std::string_view	last_change_key = "last-change";
};


/// In-memory db
struct MemDb final: public MetadataIface {
	/**
	 * Create a new memdb
	 *
	 * @param readonly read-only? (for testing)
	 */
	explicit MemDb(bool readonly=false):read_only_{readonly} {}

	/**
	 * Set some metadata
	 *
	 * @param name key name
	 * @param val value
	 */
	void set_metadata(const std::string& name, const std::string& val) override {
		map_.erase(name);
		map_[name] = val;
	}

	/**
	 * Get metadata for given key, empty if not found
	 *
	 * @param name key name
	 *
	 * @return string
	 */
	std::string metadata(const std::string& name) const override {
		if (auto&& it = map_.find(name); it != map_.end())
			return it->second;
		else
			return {};
	}

	/**
	 * Is this db read-only?
	 *
	 * @return true or false
	 */
	bool read_only() const override { return read_only_; }


	/**
	 * Invoke function for each key/value pair. Do not call
	 * @this from each_func().
	 *
	 * @param func a function
	 */
	void for_each(MetadataIface::each_func&& func) const override {
		for (const auto& [key, value] : map_)
			func(key, value);
	}

private:
	std::unordered_map<std::string, std::string> map_;
	const bool read_only_;
};

/**
 * Fairly thin wrapper around Xapian::Database and Xapian::WritableDatabase
 */
class XapianDb final: public MetadataIface {
public:
	/**
	 * Type of database to create.
	 *
	 */
	enum struct Flavor {
		ReadOnly,	 /**< Read-only database */
		Open,		 /**< Open existing read-write */
		CreateOverwrite, /**< Create new or overwrite existing */
	};

	/**
	 * XapianDb CTOR. This may throw.
	 *
	 * @param db_path path to the database
	 * @param flavor kind of database
	 */
	XapianDb(const std::string& db_path, Flavor flavor);

	/**
	 * DTOR
	 */
	~XapianDb() override {
		// shouldn't use read_only() here, since that's virtual.
		if (std::holds_alternative<Xapian::WritableDatabase>(db_))
			request_commit(true/*force*/);
		mu_debug("closing db");
	}

	/**
	 * Reinitialize from inner-config. Needed after CreateOverwrite.
	 *
	 * This is bit of a hack, needed since we cannot setup the config
	 * before we have a database.
	 */
	void reinit();

	/**
	 * Is the database read-only?
	 *
	 * @return true or false
	 */
	bool read_only() const override;

	/**
	 * Path to the database; empty for in-memory databases
	 *
	 * @return path to database
	 */
	const std::string& path() const {
		return path_;
	}

	/**
	 * Get a description of the Xapian database
	 *
	 * @return description
	 */
	const std::string description() const {
		return db().get_description();
	}

	/**
	 * Get the number of documents (messages) in the database
	 *
	 * @return number
	 */
	size_t size() const noexcept {
		return xapian_try([this]{
			return db().get_doccount(); }, 0);
	}

	/**
	 * Is the the base empty?
	 *
	 * @return true or false
	 */
	size_t empty() const noexcept { return size() == 0; }

	/**
	 * Get a database enquire object for queries.
	 *
	 * @return an enquire object
	 */
	Xapian::Enquire enquire() const {
		return Xapian::Enquire(db());
	}

	/**
	 * Get a document from the database if there is one
	 *
	 * @param id id of the document
	 *
	 * @return the document or an error
	 */
	Result<Xapian::Document> document(Xapian::docid id) const {
		return xapian_try_result([&]{
			return Ok(db().get_document(id)); });
	}

	/**
	 * Get metadata for the given key
	 *
	 * @param key key (non-empty)
	 *
	 * @return the value or empty
	 */
	std::string metadata(const std::string& key) const override {
		return xapian_try([&]{
			return db().get_metadata(key);}, "");
	}

	/**
	 * Set metadata for the given key
	 *
	 * @param key key (non-empty)
	 * @param val new value for key
	 */
	void set_metadata(const std::string& key, const std::string& val) override {
		xapian_try([&] { wdb().set_metadata(key, val);
				maybe_commit();});
	}

	/**
	 * Invoke function for each key/value pair. This is called with the lock
	 * held, so do not call functions on @this is each_func().
	 *
	 * @param each_func a function
	 */
	//using each_func = MetadataIface::each_func;
	void for_each(MetadataIface::each_func&& func) const override {
		xapian_try([&]{
			for (auto&& it = db().metadata_keys_begin();
			     it != db().metadata_keys_end(); ++it)
				func(*it, db().get_metadata(*it));
		});
	}

	/**
	 * Does the given term exist in the database?
	 *
	 * @param term some term
	 *
	 * @return true or false
	 */
	bool term_exists(const std::string& term) const {
		return xapian_try([&]{
			return db().term_exists(term);}, false);
	}

	/**
	 * Add a new document to the database
	 *
	 * @param doc a document (message)
	 *
	 * @return new docid or 0
	 */
	Result<Xapian::docid> add_document(const Xapian::Document& doc) {
		return xapian_try_result([&]{
			auto&& id{wdb().add_document(doc)};
			set_timestamp(MetadataIface::last_change_key);
			maybe_commit();
			return Ok(std::move(id));
		});
	}

	/**
	 * Replace document in database
	 *
	 * @param term unique term
	 * @param id docid
	 * @param doc replacement document
	 *
	 * @return new docid or an error
	 */
	Result<Xapian::docid>
	replace_document(const std::string& term,
			 const Xapian::Document& doc) {
		return xapian_try_result([&]{
			auto&& id{wdb().replace_document(term, doc)};
			set_timestamp(MetadataIface::last_change_key);
			maybe_commit();
			return Ok(std::move(id));
		});
	}
	Result<Xapian::docid>
	replace_document(Xapian::docid id,
			 const Xapian::Document& doc) {
		return xapian_try_result([&]{
			wdb().replace_document(id, doc);
			set_timestamp(MetadataIface::last_change_key);
			maybe_commit();
			return Ok(std::move(id));
		});
	}

	/**
	 * Delete document(s) for the given term or id
	 *
	 * @param term a term
	 *
	 * @return Ok or Error
	 */
	Result<void> delete_document(const std::string& term) {
		return xapian_try_result([&]{
			wdb().delete_document(term);
			set_timestamp(MetadataIface::last_change_key);
			maybe_commit();
			return Ok();
		});
	}
	Result<void> delete_document(Xapian::docid id) {
		return xapian_try_result([&]{
			wdb().delete_document(id);
			set_timestamp(MetadataIface::last_change_key);
			maybe_commit();
			return Ok();
		});
	}

	template<typename Func>
	size_t all_terms(const std::string& prefix, Func&& func) const {
		size_t n{};
		for (auto it = db().allterms_begin(prefix); it != db().allterms_end(prefix); ++it) {
			if (!func(*it))
				break;
			++n;
		}
		return n;
	}

	/**
	 * Requests a transaction to be started; this is only
	 * a request, which may not be granted.
	 *
	 * If you're already in a transaction but that transaction
	 * was started in another thread, that transaction will be
	 * committed before starting a new one.
	 *
	 * Otherwise, start a transaction if you're not already in one.
	 *
	 * @return A result; either true if a transaction was started; false
	 * otherwise, or an error.
	 */
	Result<bool> request_transaction() {
		return xapian_try_result([this]() {
			auto& db = wdb();
			if (in_transaction())
				return Ok(false); // nothing to

			db.begin_transaction();
			mu_debug("begin transaction");
			in_transaction_ = true;
			return Ok(true);
		});
	}


	/**
	 * Explicitly request the Xapian DB to be committed to disk
	 *
	 * @param force whether to force-commit
	 */
	void request_commit(bool force = false) { request_commit(wdb(), force); }
	void maybe_commit() { request_commit(false); }

	/**
	 * Are we inside a transaction?
	 *
	 * @return true or false
	 */
	bool in_transaction() const { return in_transaction_; }

	using DbType = std::variant<Xapian::Database, Xapian::WritableDatabase>;

private:
	/**
	 * To be called with DB_LOCKED held.
	 */
	void request_commit(Xapian::WritableDatabase& db, bool force) {
		if ((++changes_ < batch_size_) && !force)
			return;
		xapian_try([&]{
			mu_debug("committing {} changes; transaction={}; "
				 "forced={}", changes_,
				 in_transaction() ? "yes" : "no",
				 force ? "yes" : "no");
			if (in_transaction()) {
				db.commit_transaction();
				in_transaction_ = {};
			}
			db.commit();
			changes_ = 0;

		});
	}

	void set_timestamp(const std::string_view key);

	/**
	 * Get a reference to the underlying database
	 *
	 * @return db database reference
	 */
	const Xapian::Database& db() const;
	/**
	 * Get a reference to the underlying writable database. It is
	 * an error to call this on a read-only database.
	 *
	 * @return db writable database reference
	 */
	Xapian::WritableDatabase& wdb();

	std::string path_;
	DbType db_;
	size_t changes_{};
	bool in_transaction_{};
	size_t batch_size_;
};

constexpr std::string_view
format_as(XapianDb::Flavor flavor)
{
	switch(flavor) {
	case XapianDb::Flavor::CreateOverwrite:
		return "create-overwrite";
	case XapianDb::Flavor::Open:
		return "open";
	case XapianDb::Flavor::ReadOnly:
		return "read-only";
	default:
		return "??";
	}
}

static inline std::string
format_as(const XapianDb& db)
{
	return mu_format("{} @ {}", db.description(), db.path());
}

} // namespace Mu

#endif /* MU_XAPIAN_DB_HH__ */
