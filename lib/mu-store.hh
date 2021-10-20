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

#ifndef __MU_STORE_HH__
#define __MU_STORE_HH__

#include <mu-msg.hh>

#include <string>
#include <vector>
#include <mutex>
#include <ctime>

#include "mu-contacts.hh"
#include <xapian.h>

#include <utils/mu-utils.hh>
#include <index/mu-indexer.hh>

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

	struct Metadata {
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
	 * Get metadata about this store.
	 *
	 * @return the metadata
	 */
	const Metadata& metadata() const;
	/**
	 * Get the Contacts object for this store
	 *
	 * @return the Contacts object
	 */
	const Contacts& contacts() const;

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
	 * Add a message to the store.
	 *
	 * @param path the message path.
	 *
	 * @return the doc id of the added message
	 */
	Id add_message(const std::string& path);

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
	 * @param func a Callable invoked for each message.
	 *
	 * @return the number of times func was invoked
	 */
	size_t for_each_term(const std::string& field, ForEachTermFunc func) const;

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
	 * Commit the current group of modifications (i.e., transaction) to disk;
	 * This rarely needs to be called explicitly, as Store will take care of
	 * it.
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
