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

#include "mu-store-worker.hh"
#include "mu-store.hh"
#include "utils/mu-utils.hh"

#include <type_traits>

using namespace Mu;

// helper constant for the visitor
template<class> inline constexpr bool always_false_v = false;

void
StoreWorker::run() {

	running_ = true;

	while (running_) {
		WorkItem workitem;

		if (!q_.pop(workitem))
			continue;

		std::visit([&](auto&& item) {
			using T = std::decay_t<decltype(item)>;

			if constexpr (std::is_same_v<T, SexpCommand>) {
				if (!sexp_handler_)
					mu_critical("no handler for sexp '{}'", item);
				else
					sexp_handler_(item);
			} else if constexpr (std::is_same_v<T, SetDirStamp>) {
				store_.set_dirstamp(item.path, item.tstamp);
			} else if constexpr (std::is_same_v<T, SetLastIndex>) {
				store_.config().set<Mu::Config::Id::LastIndex>(item.tstamp);
			} else if constexpr (std::is_same_v<T, StartTransaction>) {
				store_.xapian_db().request_transaction();
			} else if constexpr (std::is_same_v<T, EndTransaction>) {
				store_.xapian_db().request_commit(true);
			} else if constexpr (std::is_same_v<T, RemoveMessages>) {
				store_.remove_messages(item);
			} else if constexpr (std::is_same_v<T, AddMessage>) {
				store_.consume_message(std::move(item.msg), true/*new*/);
			} else if constexpr (std::is_same_v<T, UpdateMessage>) {
				store_.consume_message(std::move(item.msg), false/*maybe not new*/);
			} else
				static_assert(always_false_v<T>, "non-exhaustive visitor");
		}, workitem);
	}
}
