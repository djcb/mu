/*
** Copyright (C) 2011 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef __MU_MSG_DB_H__
#define __MU_MSG_DB_H__

#include <glib.h>

G_BEGIN_DECLS

struct _MuMsgDb;
typedef struct _MuMsgDb MuMsgDb;

typedef gpointer XapianDocument;

/**
 * create a new MuMsgDb instance 
 * 
 * @param doc a Xapian::Document* (you'll need to cast the
 * Xapian::Document* to XapianDocument*, because only C (not C++) is
 * allowed in this header file.
 * @param err receives error info, or NULL
 * 
 * @return a new MuMsgDb instance (free with mu_msg_db_destroy), or
 * NULL in case of error.
 */
MuMsgDb* mu_msg_db_new (XapianDocument *doc, GError **err);


/**
 * destroy a MuMsgDb instance -- free all the resources. Note, after
 * destroying, any strings returned from mu_msg_db_get_str_field with
 * do_free==FALSE are no longer valid
 * 
 * @param self a MuMsgDb instance
 */
void mu_msg_db_destroy (MuMsgDb *self);


/**
 * get a string parameter from the msgdb
 * 
 * @param self a MuMsgDb instance
 * @param mfid a MuMsgFieldId for a string field
 * @param do_free receives either TRUE or FALSE, where TRUE means that
 * the caller owns the string, and has to free it (g_free) when done
 * with it; FALSE means that the MuMsgDb owns the string, and it is
 * only valid as long as the MuMsgDb is valid (ie., before
 * mu_msg_db_destroy).
 * 
 * @return a string for the given field (see do_free), or NULL in case of error
 */
gchar* mu_msg_db_get_str_field (MuMsgDb *self, MuMsgFieldId mfid, gboolean *do_free);

/**
 * 
 * get a numeric parameter from the msgdb
 * 
 * @param self a MuMsgDb instance
 * @param mfid a MuMsgFieldId for a numeric field
 * 
 * @return the numerical value, or -1 in case of error. You'll need to
 * cast this value to the actual type (e.g. time_t for MU_MSG_FIELD_ID_DATE)
 */
gint64 mu_msg_db_get_num_field (MuMsgDb *self, MuMsgFieldId mfid);



G_END_DECLS

#endif /*__MU_MSG_DB_H__*/
