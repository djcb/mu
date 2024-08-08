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


/**
 * The store worker maintains a worker thread and an async queue to which
 * commands can be added from any thread; the worker thread that is the sole
 * thread to talk to the store / Xapian (at least for writing).
 */

#ifndef MU_STORE_WORKER_HH
#define MU_STORE_WORKER_HH

#include <variant>
#include <string>
#include <thread>
#include <atomic>
#include <vector>
#include <functional>

#include <message/mu-message.hh>
#include <utils/mu-async-queue.hh>

namespace Mu {

/**< Sum type for all commands  */

class Store; /// fwd declaration

/**
 * Worker for sending requests to the Store
 *
 * I.e. to execute database commands in a single thread.
 */
class StoreWorker {
public:
	/**
	 * CTOR. This will create the store worker and start the worker thread.
	 *
	 * @param store a store
	 */
	StoreWorker(Store& store):
		store_{store},
		runner_ {std::thread([this]{run();})}
		{}

	/**
	 * DTOR. Destroy the store worker after joining the worker thread
	 */
	~StoreWorker() {
		running_ = false;
		if (runner_.joinable())
			runner_.join();
	}

	/*
	 * The following types of work-item can be added to the queue:
	 */
	struct SetDirStamp {
		std::string path; /**< full path to directory */
		::time_t tstamp; /**< Timestamp for directory */
	}; /**<  Write a directory timestamp to the store */

	struct SetLastIndex {
		::time_t tstamp; /**< Timestamp */
	}; /**<  Write last indexing timestamp to the store */

	struct StartTransaction{}; /**< Request transaction start
				    * (opportunistically) */
	struct EndTransaction{}; /**< Request transaction end/commit
				  * (opportunistically) */
	struct AddMessage  {
		Message msg; /**< Add a new message */
	}; /**< Add a new message; this is faster version of UpdateMessage
	    * if we know the message does not exist yet. */
	struct UpdateMessage  {
		Message msg; /**< Add or update a message */
	}; /**< Add message or update if it already exists */

	using RemoveMessages = std::vector<unsigned>;
	/**< Remove all message with the given ids */
	using SexpCommand = std::string; /**< A sexp-command (i.e., from mu4e);
					  * requires install_sexp_handler() */

	using WorkItem = std::variant<SetDirStamp, SetLastIndex,
				      AddMessage, UpdateMessage,
				      StartTransaction, EndTransaction,
				      RemoveMessages, SexpCommand>;
	/// Sumtype with all types of work-item

	using QueueType = AsyncQueue<WorkItem>;
	const QueueType& queue() const { return q_; }
	QueueType& queue() { return q_; }

	/**
	 * Push a work item to the que
	 *
	 * @param item
	 */
	void push(WorkItem&& item) {
		q_.push(std::move(item));
	}

	/**
	 * Get the current size of the work item queue
	 *
	 * @return the size
	 */
	size_t size() const {
		return q_.size();
	}

	/**
	 * Is the work item queue empty?
	 *
	 * @return true or false
	 */
	bool empty() const {
		return q_.empty();
	}

	/**
	 * Clear the queue of any items
	 */
	void clear() {
		q_.clear();
	}

	using SexpCommandHandler = std::function<void(const std::string& sexp)>;
	/**< Prototype for a SexpCommand handler function */

	/**
	 * Install a handler for Sexp commands
	 *
	 * @param handler
	 */
	void install_sexp_handler(SexpCommandHandler&& handler) {
		sexp_handler_ = handler;
	}

private:
	void run();
	size_t cleanup_orphans();

	QueueType q_;
	Store& store_;
	std::thread runner_;
	std::atomic<bool> running_{};
	SexpCommandHandler sexp_handler_{};
};

} // namespace Mu
#endif /*MU_STORE_WORKER_HH*/
