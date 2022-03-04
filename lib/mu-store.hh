/*
** Copyright (C) 2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef __MU_STORE_HH__
#define __MU_STORE_HH__

#include <mu-msg.hh>

#include <string>
#include <vector>
#include <mutex>
#include <ctime>

#include "mu-contacts-cache.hh"
#include <xapian.h>

#include <utils/mu-utils.hh>
#include <index/mu-indexer.hh>
#include <mu-query-results.hh>
#include <utils/mu-utils.hh>
#include <mu-message.hh>

namespace Mu {

class Store {
public:
	using Id                      = Xapian::docid; /**< Id for a message in the store */
	static constexpr Id InvalidId = 0;             /**< Invalid  store id */

	static constexpr size_t MaxTermLength = 240; /**< Maximum length of a term,
	  http://article.gmane.org/gmane.comp.search.xapian.general/3656 */
	/**
	 * Construct a store for an existing document database
	 *
	 * @param path path to the database
	 * @param readonly whether to open the database in read-only mode
	 */
	Store(const std::string& path, bool readonly = true);

	struct Config {
		size_t max_message_size{};
		/**< maximum size (in bytes) for a message, or 0 for default */
		size_t batch_size{};
		/**< size of batches before committing, or 0 for default */
	};

	/**
	 * Construct a store for a not-yet-existing document database
	 *
	 * @param path path to the database
	 * @param maildir maildir to use for this store
	 * @param personal_addresses addresses that should be recognized as
	 * 'personal' for identifying personal messages.
	 */
	Store(const std::string& path,
	      const std::string& maildir,
	      const StringVec&   personal_addresses,
	      const Config&      conf);

	/**
	 * Construct an in-memory, writeable store for testing
	 *
	 * @param maildir maildir to use for this store
	 * @param personal_addresses addresses that should be recognized as
	 * 'personal' for identifying personal messages.
	 */
	Store(const std::string& maildir, const StringVec& personal_addresses, const Config& conf);

	/**
	 * DTOR
	 */
	~Store();

	/**
	 * Store properties
	 */
	struct Properties {
		std::string database_path;  /**< Full path to the Xapian database */
		std::string schema_version; /**< Database schema version */
		std::time_t created;        /**<  database creation time */

		bool   read_only;  /**< Is the database opened read-only? */
		size_t batch_size; /**< Maximum database transaction batch size */
		bool   in_memory;  /**< Is this an in-memory database (for testing)?*/

		std::string root_maildir; /**<  Absolute path to the top-level maildir */

		StringVec personal_addresses; /**< Personal e-mail addresses */
		size_t    max_message_size;   /**<  Maximus allowed message size */
	};

	/**
	 * Get properties about this store.
	 *
	 * @return the metadata
	 */
	const Properties& properties() const;
	/**
	 * Get the ContactsCache object for this store
	 *
	 * @return the Contacts object
	 */
	const ContactsCache& contacts_cache() const;

	/**
	 * Get the underlying Xapian database for this store.
	 *
	 * @return the database
	 */
	const Xapian::Database& database() const;

	/**
	 * Get the underlying writable Xapian database for this
	 * store. Throws is this store is not writable.
	 *
	 * @return the writable database
	 */
	Xapian::WritableDatabase& writable_database();

	/**
	 * Get the Indexer associated with this store. It is an error to call
	 * this on a read-only store.
	 *
	 * @return the indexer.
	 */
	Indexer& indexer();

	/**
	 * Run a						query; see the `mu-query` man page for the syntax.
	 *
	 * Multi-threaded callers must aquire the lock and keep it
	 * at least as long as the return value.
	 *
	 * @param expr the search expression
	 * @param sortfieldid the sortfield-id. If the field is NONE, sort by DATE
	 * @param flags query flags
	 * @param maxnum maximum number of results to return. 0 for 'no limit'
	 *
	 * @return the query-results, or Nothing in case of error.
	 */
	std::mutex& lock() const;
	Option<QueryResults> run_query(const std::string&	expr        = "",
				       std::optional<Message::Field::Id>    sortfield_id = {},
				       QueryFlags		flags       = QueryFlags::None,
				       size_t			maxnum      = 0) const;

	/**
	 * run a Xapian query merely to count the number of matches; for the
	 * syntax, please refer to the mu-query manpage
	 *
	 * @param expr the search expression; use "" to match all messages
	 *
	 * @return the number of matches
	 */
	size_t count_query(const std::string& expr = "") const;

	/**
	 * For debugging, get the internal string representation of the parsed
	 * query
	 *
	 * @param expr a xapian search expression
	 * @param xapian if true, show Xapian's internal representation,
	 * otherwise, mu's.

	 * @return the string representation of the query
	 */
	std::string parse_query(const std::string& expr, bool xapian) const;

	/**
	 * Add a message to the store. When planning to write many messages,
	 * it's much faster to do so in a transaction. If so, set
	 * @in_transaction to true. When done with adding messages, call commit().
	 *
	 * @param path the message path.
	 * @param whether to bundle up to batch_size changes in a transaction
	 *
	 * @return the doc id of the added message
	 */
	Id add_message(const std::string& path, bool use_transaction = false);

	/**
	 * Update a message in the store.
	 *
	 * @param msg a message
	 * @param id the id for this message
	 *
	 * @return false in case of failure; true otherwise.
	 */
	bool update_message(MuMsg* msg, Id id);

	/**
	 * Remove a message from the store. It will _not_ remove the message
	 * fromt he file system.
	 *
	 * @param path the message path.
	 *
	 * @return true if removing happened; false otherwise.
	 */
	bool remove_message(const std::string& path);

	/**
	 * Remove a number if messages from the store. It will _not_ remove the
	 * message fromt he file system.
	 *
	 * @param ids vector with store ids for the message
	 */
	void remove_messages(const std::vector<Id>& ids);

	/**
	 * Remove a message from the store. It will _not_ remove the message
	 * fromt he file system.
	 *
	 * @param id the store id for the message
	 */
	void remove_message(Id id) { remove_messages({id}); }

	/**
	 * Find message in the store.
	 *
	 * @param id doc id for the message to find
	 *
	 * @return a message (owned by caller), or nullptr
	 */
	MuMsg* find_message(Id id) const;

	/**
	 * does a certain message exist in the store already?
	 *
	 * @param path the message path
	 *
	 * @return true if the message exists in the store, false otherwise
	 */
	bool contains_message(const std::string& path) const;

	/**
	 * Prototype for the ForEachMessageFunc
	 *
	 * @param id :t store Id for the message
	 * @param path: the absolute path to the message
	 *
	 * @return true if for_each should continue; false to quit
	 */
	using ForEachMessageFunc = std::function<bool(Id, const std::string&)>;

	/**
	 * Call @param func for each document in the store. This takes a lock on
	 * the store, so the func should _not_ call any other Store:: methods.
	 *
	 * @param func a Callable invoked for each message.
	 *
	 * @return the number of times func was invoked
	 */
	size_t for_each_message_path(ForEachMessageFunc func) const;

	/**
	 * Prototype for the ForEachTermFunc
	 *
	 * @param term:
	 *
	 * @return true if for_each should continue; false to quit
	 */
	using ForEachTermFunc = std::function<bool(const std::string&)>;

	/**
	 * Call @param func for each term for the given field in the store. This
	 * takes a lock on the store, so the func should _not_ call any other
	 * Store:: methods.
	 *
	 * @param a prefix
	 * @param func a Callable invoked for each message.
	 *
	 * @return the number of times func was invoked
	 */
	size_t for_each_term(const std::string& prefix, ForEachTermFunc func) const;


	/**
	 * Get the store metadata for @p key
	 *
	 * @param key the metadata key
	 *
	 * @return the metadata value or empty for none.
	 */
	std::string metadata(const std::string& key) const;


	/**
	 * Write metadata to the store.
	 *
	 * @param key key
	 * @param val value
	 */
	void set_metadata(const std::string& key, const std::string& val);

	/**
	 * Get the timestamp for some message, or 0 if not found
	 *
	 * @param path the path
	 *
	 * @return the timestamp, or 0 if not found
	 */
	time_t message_tstamp(const std::string& path) const;

	/**
	 * Get the timestamp for some directory
	 *
	 * @param path the path
	 *
	 * @return the timestamp, or 0 if not found
	 */
	time_t dirstamp(const std::string& path) const;

	/**
	 * Set the timestamp for some directory
	 *
	 * @param path a filesystem path
	 * @param tstamp the timestamp for that path
	 */
	void set_dirstamp(const std::string& path, time_t tstamp);

	/**
	 * Get the number of documents in the document database
	 *
	 * @return the number
	 */
	std::size_t size() const;

	/**
	 * Is the database empty?
	 *
	 * @return true or false
	 */
	bool empty() const;

	/**
	 * Commit the current batch of modifications to disk, opportunistically.
	 * If no transaction is underway, do nothing.
	 */
	void commit();

	/**
	 * Get a reference to the private data. For internal use.
	 *
	 * @return private reference.
	 */
	struct Private;
	std::unique_ptr<Private>&       priv() { return priv_; }
	const std::unique_ptr<Private>& priv() const { return priv_; }

private:
	std::unique_ptr<Private> priv_;
};

} // namespace Mu

#endif /* __MU_STORE_HH__ */
