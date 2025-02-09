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

#ifndef MU_STORE_HH__
#define MU_STORE_HH__

#include <string>
#include <vector>
#include <mutex>
#include <ctime>
#include <memory>

#include "mu-contacts-cache.hh"
#include "mu-xapian-db.hh"
#include "mu-config.hh"
#include "mu-indexer.hh"
#include "mu-query-results.hh"

#include <utils/mu-utils.hh>
#include <utils/mu-utils.hh>
#include <utils/mu-option.hh>

#include <message/mu-message.hh>

namespace Mu {

class Store {
public:
	using Id                      = Xapian::docid;   /**< Id for a message in the store */
	static constexpr Id InvalidId = 0;               /**< Invalid  store id */
	using IdVec                   = std::vector<Id>; /**< Vector of document ids */
	using IdPathVec               = std::vector<std::pair<Id, std::string>>;
	/**< vector of id, path pairs */

	/**
	 * Configuration options.
	 */
	enum struct Options {
		None	 = 0,	/**< No specific options */
		Writable = 1 << 0, /**< Open in writable mode */
		ReInit	 = 1 << 1, /**< Re-initialize based on existing */
	};

	/**
	 * Make a store for an existing document database
	 *
	 * @param path path to the database
	 * @param options startup options
	 *
	 * @return A store or an error.
	 */
	static Result<Store> make(const std::string& path,
				  Options opts=Options::None) noexcept {
		return xapian_try_result(
			[&]{return Ok(Store{path, opts});});
	}

	/**
	 * Construct a store for a not-yet-existing document database
	 *
	 * @param path path to the database
	 * @param root_maildir absolute path to maildir to use for this store
	 * @param conf a configuration object
	 *
	 * @return a store or an error
	 */
	static Result<Store> make_new(const std::string& path,
				      const std::string& root_maildir,
				      Option<const Config&> conf={}) noexcept {
		return xapian_try_result(
			[&]{return Ok(Store(path, root_maildir, conf));});
	}

	/**
	 * Move CTOR
	 *
	 */
	Store(Store&&);

	/**
	 * DTOR
	 */
	~Store();

	/**
	 * Store statistics. Unlike the properties, these can change
	 * during the lifetime of a store.
	 *
	 */
	struct Statistics {
		size_t   size;	/**< number of messages in store */
		::time_t last_change; /**< last time any update happened */
		::time_t last_index; /**< last time an indexing op was performed */
	};

	/**
	 * Get store statistics
	 *
	 * @return statistics
	 */
	Statistics statistics() const;

	/**
	 * Get the underlying xapian db object
	 *
	 * @return the XapianDb for this store
	 */
	const XapianDb& xapian_db() const;
	XapianDb& xapian_db();

	/**
	 * Get the Config for this store
	 *
	 * @return the Config
	 */
	const Config& config() const;
	Config& config();

	/**
	 * Get the ContactsCache object for this store
	 *
	 * @return the Contacts object
	 */
	const ContactsCache& contacts_cache() const;

	/**
	 * Get the Indexer associated with this store. It is an error to call
	 * this on a read-only store.
	 *
	 * @return the indexer.
	 */
	Indexer& indexer();

	/**
	 * Run a query; see the `mu-query` man page for the syntax.
	 *
	 * Multi-threaded callers must acquire the lock and keep it
	 * at least as long as the return value.
	 *
	 * @param expr the search expression
	 * @param sortfieldid the sortfield-id. If the field is NONE, sort by DATE
	 * @param flags query flags
	 * @param maxnum maximum number of results to return. 0 for 'no limit'
	 *
	 * @return the query-results or an error.
	 */
	std::mutex& lock() const;
	Result<QueryResults> run_query(const std::string&	expr,
				       Field::Id		sortfield_id = Field::Id::Date,
				       QueryFlags		flags	     = QueryFlags::None,
				       size_t			maxnum	     = 0) const;

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
	 *
	 * @return the string representation of the query
	 */
	std::string parse_query(const std::string& expr, bool xapian) const;

	/**
	 * Add or update a message to the store. When planning to write many
	 * messages, it's much faster to do so in a transaction. If so, set
	 * @param in_transaction to true. When done with adding messages, call
	 * commit().
	 *
	 * Optimization: If you are sure the message (i.e., a message with the
	 * given file-system path) does not yet exist in the database, ie., when
	 * doing the initial indexing, set @p is_new to true since we then don't
	 * have to check for the existing message.
	 *
	 * @param msg a message
	 * @param is_new whether this is a completely new message
	 *
	 * @return the doc id of the added message or an error.
	 */
	Result<Id> add_message(Message &msg, bool is_new = false);
	Result<Id> add_message(const std::string &path, bool is_new = false);

	/**
	 * Like add_message(), however, this consumes the message and disposes
	 * of it when the function ends. This can be useful when injecting
	 * messages from a worker thread, to ensure no Xapian::Documents
	 * live in different threads.
	 *
	 * @param msg a message
	 * @param is_new whether this is a completely new message
	 */
	Result<Id> consume_message(Message&& msg, bool is_new = false) {
		Message consumed{std::move(msg)};
		return add_message(consumed, is_new);
	}

	/**
	 * Remove a message from the store. It will _not_ remove the message
	 * from the file system.
	 *
	 * @param path the message path.
	 *
	 * @return true if removing happened; false otherwise.
	 */
	bool remove_message(const std::string& path);

	/**
	 * Remove a number if messages from the store. It will _not_ remove the
	 * message from the file system.
	 *
	 * @param ids vector with store ids for the message
	 */
	void remove_messages(const std::vector<Id>& ids);

	/**
	 * Remove a message from the store. It will _not_ remove the message
	 * from the file system.
	 *
	 * @param id the store id for the message
	 */
	void remove_message(Id id) { remove_messages({id}); }

	/**
	 * Find message in the store.
	 *
	 * @param id doc id for the message to find
	 *
	 * @return a message (if found) or Nothing
	 */
	Option<Message> find_message(Id id) const;

	/**
	 * Find a message's docid based on its path
	 *
	 * @param path path to the message
	 *
	 * @return the docid or Nothing if not found
	 */
	Option<Id> find_message_id(const std::string& path) const;

	/**
	 * Find the messages for the given ids
	 *
	 * @param ids document ids for the message
	 *
	 * @return id, message pairs for the messages found
	 * (which not necessarily _all_ of the ids)
	 */
	using IdMessageVec = std::vector<std::pair<Id, Message>>;
	IdMessageVec find_messages(IdVec ids) const;

	/**
	 * Find the ids for all messages with a give message-id
	 *
	 * @param message_id a message id
	 *
	 * @return the ids of all messages with the given message-id
	 */
	IdVec find_duplicates(const std::string& message_id) const;

	/**
	 * does a certain message exist in the store already?
	 *
	 * @param path the message path
	 *
	 * @return true if the message exists in the store, false otherwise
	 */
	bool contains_message(const std::string& path) const;

	/**
	 * Options for moving
	 *
	 */
	enum struct MoveOptions {
		None	     = 0,	/**< Defaults */
		ChangeName   = 1 << 0,	/**< Change the name when moving */
		DupFlags     = 1 << 1,  /**< Update flags for duplicate messages too */
		DryRun       = 1 << 2,  /**< Don't really move, just determine target paths */
	};

	/**
	 * Move a message both in the filesystem and in the store. After a successful move, the
	 * message is updated.
	 *
	 * @param id the id for some message
	 * @param target_mdir the target maildir (if any)
	 * @param new_flags new flags (if any)
	 * @param opts move options
	 *
	 * @return Result, either an IdPathVec with ids and paths for the moved message(s) or some
	 * error. Note that in case of success at least one message is returned, and only with
	 * MoveOptions::DupFlags can it be more than one.
	 *
	 * The first element of the IdPathVec, is the main message that got move; any subsequent
	 * (if any) are the duplicate paths, sorted by path-name.
	 */
	Result<IdPathVec> move_message(Store::Id id,
				       Option<const std::string&> target_mdir = Nothing,
				       Option<Flags> new_flags = Nothing,
				       MoveOptions opts = MoveOptions::None);
	/**
	 * Convert IdPathVec -> IdVec
	 *
	 * @param ips idpath vector
	 *
	 * @return vector of ids
	 */
	static IdVec id_vec(const IdPathVec& ips);

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
	 * @param id the field id
	 * @param func a Callable invoked for each message.
	 *
	 * @return the number of times func was invoked
	 */
	size_t for_each_term(Field::Id id, ForEachTermFunc func) const;

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

	/*
	 *
	 * Some convenience
	 *
	 */

	/**
	 * Get the Xapian database-path for this store
	 *
	 * @return the path
	 */
	const std::string& path() const { return xapian_db().path(); }

	/**
	 * Get the root-maildir for this store
	 *
	 * @return the root-maildir
	 */
	const std::string& root_maildir() const;

	/**
	 * Get the number of messages in the store
	 *
	 * @return the number
	 */
	size_t size() const { return xapian_db().size(); }

	/**
	 * Is the store empty?
	 *
	 * @return true or false
	 */
	bool empty() const { return xapian_db().empty(); }


	/**
	 * Get the list of maildirs, that is, the list of maildirs
	 * under root_maildir, without file-system prefix.
	 *
	 * This does a file-system scan.
	 *
	 * @return list of maildirs
	 */
	std::vector<std::string> maildirs() const;


	/**
	 * Compatible message-options for this store
	 *
	 * @return message-options.
	 */
	Message::Options message_options() const;


	/*
	 * _almost_ private
	 */

	/**
	 * Get a reference to the private data. For internal use.
	 *
	 * @return private reference.
	 */
	struct Private;
	std::unique_ptr<Private>&       priv() { return priv_; }
	const std::unique_ptr<Private>& priv() const { return priv_; }

private:
	/**
	 * Construct a store for an existing document database
	 *
	 * @param path path to the database
	 * @param options startup options
	 */
	Store(const std::string& path, Options opts=Options::None);

	/**
	 * Construct a store for a not-yet-existing document database
	 *
	 * @param path path to the database
	 * @param config a configuration object
	 */
	Store(const std::string& path, const std::string& root_maildir,
	      Option<const Config&> conf);

	std::unique_ptr<Private> priv_;
};

MU_ENABLE_BITOPS(Store::Options);
MU_ENABLE_BITOPS(Store::MoveOptions);

static inline std::string
format_as(const Store& store)
{
	return mu_format("store ({}/{})", format_as(store.xapian_db()),
			 store.root_maildir());
}

} // namespace Mu

#endif /* MU_STORE_HH__ */
