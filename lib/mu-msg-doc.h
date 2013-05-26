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

#ifndef __MU_MSG_DOC_H__
#define __MU_MSG_DOC_H__

#include <glib.h>
#include <mu-util.h> /* for XapianDocument */

G_BEGIN_DECLS

struct _MuMsgDoc;
typedef struct _MuMsgDoc MuMsgDoc;

/**
 * create a new MuMsgDoc instance
 *
 * @param doc a Xapian::Document* (you'll need to cast the
 * Xapian::Document* to XapianDocument*, because only C (not C++) is
 * allowed in this header file. MuMsgDoc takes _ownership_ of this pointer;
 * don't touch it afterwards
 * @param err receives error info, or NULL
 *
 * @return a new MuMsgDoc instance (free with mu_msg_doc_destroy), or
 * NULL in case of error.
 */
MuMsgDoc* mu_msg_doc_new (XapianDocument *doc, GError **err)
	G_GNUC_MALLOC G_GNUC_WARN_UNUSED_RESULT;

/**
 * destroy a MuMsgDoc instance -- free all the resources. Note, after
 * destroying, any strings returned from mu_msg_doc_get_str_field with
 * do_free==FALSE are no longer valid
 *
 * @param self a MuMsgDoc instance
 */
void mu_msg_doc_destroy (MuMsgDoc *self);


/**
 * get a string parameter from the msgdoc
 *
 * @param self a MuMsgDoc instance
 * @param mfid a MuMsgFieldId for a string field
 *
 * @return a string for the given field (see do_free), or NULL in case of error.
 * free with g_free
 */
gchar* mu_msg_doc_get_str_field (MuMsgDoc *self, MuMsgFieldId mfid)
          G_GNUC_WARN_UNUSED_RESULT;

/**
 * get a string-list parameter from the msgdoc
 *
 * @param self a MuMsgDoc instance
 * @param mfid a MuMsgFieldId for a string-list field
 *
 * @return a list for the given field (see do_free), or NULL in case
 * of error. free with mu_str_free_list
 */
GSList* mu_msg_doc_get_str_list_field (MuMsgDoc *self, MuMsgFieldId mfid)
    G_GNUC_WARN_UNUSED_RESULT;


/**
 *
 * get a numeric parameter from the msgdoc
 *
 * @param self a MuMsgDoc instance
 * @param mfid a MuMsgFieldId for a numeric field
 *
 * @return the numerical value, or -1 in case of error. You'll need to
 * cast this value to the actual type (e.g. time_t for MU_MSG_FIELD_ID_DATE)
 */
gint64 mu_msg_doc_get_num_field (MuMsgDoc *self, MuMsgFieldId mfid);


G_END_DECLS

#endif /*__MU_MSG_DOC_H__*/
