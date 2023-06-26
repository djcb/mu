/*
** Copyright (C) 2020-2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef __MU_ASYNC_QUEUE_HH__
#define __MU_ASYNC_QUEUE_HH__

#include <deque>
#include <mutex>
#include <chrono>
#include <condition_variable>

namespace Mu {

constexpr std::size_t UnlimitedAsyncQueueSize{0};

template <typename ItemType,                              /**< the type of Item to queue */
	  std::size_t MaxSize = UnlimitedAsyncQueueSize,  /**< maximum size for the queue */
	  typename Allocator  = std::allocator<ItemType>> /**< allocator for the items */

class AsyncQueue {
      public:
	using value_type      = ItemType;
	using allocator_type  = Allocator;
	using size_type       = std::size_t;
	using reference       = value_type&;
	using const_reference = const value_type&;
	using pointer         = typename std::allocator_traits<allocator_type>::pointer;
	using const_pointer   = typename std::allocator_traits<allocator_type>::const_pointer;

	using Timeout = std::chrono::steady_clock::duration;

	/**
	 * Push an item to the end of the queue by moving it
	 *
	 * @param item the item to move to the end of the queue
	 * @param timeout and optional timeout
	 *
	 * @return true if the item was pushed; false otherwise.
	 */
	bool push(const value_type& item, Timeout timeout = {}) {
		return push(std::move(value_type(item)), timeout);
	}

	/**
	 * Push an item to the end of the queue by moving it
	 *
	 * @param item the item to move to the end of the queue
	 * @param timeout and optional timeout
	 *
	 * @return true if the item was pushed; false otherwise.
	 */
	bool push(value_type&& item, Timeout timeout = {}) {
		std::unique_lock lock{m_};

		if (!unlimited()) {
			const auto rv = cv_full_.wait_for(lock, timeout, [&]() {
				return !full_unlocked();
			}) && !full_unlocked();
			if (!rv)
				return false;
		}

		q_.emplace_back(std::move(item));
		cv_empty_.notify_one();

		return true;
	}

	/**
	 * Pop an item from the queue
	 *
	 * @param receives the value if the function returns true
	 * @param timeout optional time to wait for an item to become available
	 *
	 * @return true if an item was popped (into val), false otherwise.
	 */
	bool pop(value_type& val, Timeout timeout = {}) {
		std::unique_lock lock{m_};

		if (timeout != Timeout{}) {
			const auto rv = cv_empty_.wait_for(lock, timeout, [&]() {
				return !q_.empty();
			}) && !q_.empty();
			if (!rv)
				return false;

		} else if (q_.empty())
			return false;

		val = std::move(q_.front());
		q_.pop_front();
		cv_full_.notify_one();

		return true;
	}

	/**
	 * Clear the queue
	 *
	 */
	void clear() {
		std::unique_lock lock{m_};
		q_.clear();
		cv_full_.notify_one();
	}

	/**
	 * Size of the queue
	 *
	 *
	 * @return the size
	 */
	size_type size() const {
		std::unique_lock lock{m_};
		return q_.size();
	}

	/**
	 * Maximum size of the queue if specified through the template
	 * parameter; otherwise the (theoretical) max_size of the inner
	 * container.
	 *
	 * @return the maximum size
	 */
	size_type max_size() const { return unlimited() ? q_.max_size() : MaxSize; }

	/**
	 * Is the queue empty?
	 *
	 * @return true or false
	 */
	bool empty() const {
		std::unique_lock lock{m_};
		return q_.empty();
	}

	/**
	 * Is the queue full? Returns false unless a maximum size was specified
	 * (as a template argument)
	 *
	 * @return true or false.
	 */
	bool full() const {
		if (unlimited())
			return false;

		std::unique_lock lock{m_};
		return full_unlocked();
	}

	/**
	 * Is this queue (theoretically) unlimited in size?
	 *
	 * @return true or false
	 */
	constexpr static bool unlimited() { return MaxSize == UnlimitedAsyncQueueSize; }

private:
	bool full_unlocked() const { return q_.size() >= max_size(); }

	std::deque<ItemType, Allocator> q_;
	mutable std::mutex              m_;
	std::condition_variable         cv_full_, cv_empty_;
};

} // namespace Mu

#endif /* __MU_ASYNC_QUEUE_HH__ */
