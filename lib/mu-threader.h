/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

/*
** Copyright (C) 2012-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef __MU_THREADER_H__
#define __MU_THREADER_H__

#include <glib.h>
#include <mu-msg-iter.h>

G_BEGIN_DECLS

/**
 * takes an iter and the total number of matches, and from this
 * generates a hash-table with information about the thread structure
 * of these matches.
 *
 * the algorithm to find this structure is based on JWZ's
 * message-threading algorithm, as descrbed in:
 *     http://www.jwz.org/doc/threading.html
 *
 * the returned hashtable maps the Xapian docid of iter (msg) to a ptr
 * to a MuMsgIterThreadInfo structure (see mu-msg-iter.h)
 *
 * @param iter an iter; note this function will mu_msgi_iter_reset this iterator
 * @param matches the number of matches in the set *
 * @param sortfield the field to sort results by, or
 * MU_MSG_FIELD_ID_NONE if no sorting should be performed
 * @param revert if TRUE, if revert the sorting order
 *
 * @return a hashtable; free with g_hash_table_destroy when done with it
 */
GHashTable *mu_threader_calculate (MuMsgIter *iter, size_t matches,
				   MuMsgFieldId sortfield, gboolean revert);


G_END_DECLS

#endif /*__MU_THREADER_H__*/
