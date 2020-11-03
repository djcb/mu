/*
** Copyright (C) 2008-2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 3 of the License, or
** (at your option) any later version.
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

#ifndef __MU_QUERY_HH__
#define __MU_QUERY_HH__

#include <memory>

#include <glib.h>
#include <mu-store.hh>
#include <mu-msg-iter.h>
#include <utils/mu-utils.hh>

namespace Mu {

class Query {
public:
        /**
         * Construct a new Query instance.
         *
         * @param store a MuStore object
         */
        Query (const Store& store);
        /**
         * DTOR
         *
         */
        ~Query ();


        /**
         * Move CTOR
         *
         * @param other
         */
        Query(Query&& other);


       enum struct Flags {
                None           = 0, /**< no flags */
                Descending     = 1 << 0, /**< sort z->a */
                SkipUnreadable = 1 << 1, /**< skip unreadable msgs */
                SkipDups       = 1 << 2, /**< skip duplicate msgs */
                IncludeRelated = 1 << 3, /**< include related msgs */
                Threading      = 1 << 4, /**< calculate threading info */
        };


        /**
         * run a query; for the syntax, please refer to the mu-query manpage
         *
         * @param expr the search expression; use "" to match all messages
         * @param sortfield the field id to sort by or MU_MSG_FIELD_ID_NONE if
         * sorting is not desired
         * @param flags bitwise OR'd flags to influence the query (see MuQueryFlags)
         * @param maxnum maximum number of search results to return, or 0 for
         * unlimited
         * @param err receives error information (if there is any); if
         * function returns non-NULL, err will _not_be set. err can be NULL
         * possible error (err->code) is MU_ERROR_QUERY,
         *
         * @return a MuMsgIter instance you can iterate over, or NULL in
         * case of error
         */
        MuMsgIter* run (const std::string& expr="",
                        MuMsgFieldId sortfieldid=MU_MSG_FIELD_ID_NONE,
                        Flags flags=Flags::None,
                        size_t maxnum=0,
                        GError **err=nullptr) const
                G_GNUC_MALLOC G_GNUC_WARN_UNUSED_RESULT;


        /**
         * run a Xapian query to count the number of matches; for the syntax, please
         * refer to the mu-query manpage
         *
         * @param expr the search expression; use "" to match all messages
         *
         * @return the number of matches
         */
        size_t count (const std::string& expr="") const;

        /**
         * For debugging, get the internal string representation of the parsed
         * query
         *
         * @param expr a xapian search expression
         * @param xapian if true, show Xapian's internal representation,
         * otherwise, mu's.

         * @return the string representation of the query
         */
        std::string parse (const std::string& expr, bool xapian) const;

private:
        struct Private;
        std::unique_ptr<Private> priv_;

};
MU_ENABLE_BITOPS(Query::Flags);

}

#endif /*__MU_QUERY_HH__*/
