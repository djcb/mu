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

#ifndef MU_QUERY_THREADS__
#define MU_QUERY_THREADS__

#include "mu-query-results.hh"

namespace Mu {
/**
 * Calculate the threads for these query results; that is, determine the
 * thread-paths for each message, so we can let Xapian order them in the correct
 * order.
 *
 * Note - threads are sorted chronologically, and the messages below the top
 * level are always sorted in ascending orde
 *
 * @param qres query results
 * @param descending whether to sort the top-level in descending order
 */
void calculate_threads(QueryResults& qres, bool descending);

} // namespace Mu

#endif /*MU_QUERY_THREADS__*/
