/*
** Copyright (C) 2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef __MU_MSG_FIELDS_H__
#define __MU_MSG_FIELDS_H__

#include <glib.h>

G_BEGIN_DECLS

/* don't change the order, add new types at the end */
enum _MuMsgFieldId {	
	MU_MSG_FIELD_ID_BODY_TEXT,
	MU_MSG_FIELD_ID_BODY_HTML,
	MU_MSG_FIELD_ID_CC,
	MU_MSG_FIELD_ID_DATE,
	MU_MSG_FIELD_ID_FLAGS,
	MU_MSG_FIELD_ID_FROM,
	MU_MSG_FIELD_ID_PATH,
	MU_MSG_FIELD_ID_PRIORITY,
	MU_MSG_FIELD_ID_SIZE,
	MU_MSG_FIELD_ID_SUBJECT,
	MU_MSG_FIELD_ID_TO,
	MU_MSG_FIELD_ID_MSGID,
	MU_MSG_FIELD_ID_TIMESTAMP,

	MU_MSG_FIELD_ID_NUM,
};
typedef enum _MuMsgFieldId MuMsgFieldId;
static const guint MU_MSG_FIELD_ID_NONE = MU_MSG_FIELD_ID_NUM + 1;

struct _MuMsgField;
typedef struct _MuMsgField MuMsgField;

/* don't change the order, add new types at the end (before _NUM)*/
enum _MuMsgFieldType {
	MU_MSG_FIELD_TYPE_STRING,

	MU_MSG_FIELD_TYPE_BYTESIZE, 
	MU_MSG_FIELD_TYPE_TIME_T,
	MU_MSG_FIELD_TYPE_INT,

	MU_MSG_FIELD_TYPE_NUM
};
typedef enum _MuMsgFieldType MuMsgFieldType;
static const guint MU_MSG_FIELD_TYPE_NONE = MU_MSG_FIELD_TYPE_NUM + 1;

typedef void (*MuMsgFieldForEachFunc) (const MuMsgField *field, 
				       gconstpointer data);

/** 
 * iterator over all possible message fields
 * 
 * @param func a function called for each field
 * @param data a user data pointer passed the callback function
 */
void mu_msg_field_foreach (MuMsgFieldForEachFunc func, gconstpointer data);


/** 
 * get the name of the field -- this a name that can be use in queries,
 * ie. 'subject:foo', with 'subject' being the name
 * 
 * @param field a MuMsgField
 * 
 * @return the name of the field as a constant string, or
 * NULL if the field is unknown
 */
const char*  mu_msg_field_name (const MuMsgField *field)         G_GNUC_CONST;

/** 
 * get the shortcut of the field -- this a shortcut that can be use in
 * queries, ie. 's:foo', with 's' meaning 'subject' being the name
 * 
 * @param field a MuMsgField
 * 
 * @return the shortcut of the field as a constant string, or
 * NULL if the field is unknown
 */
const char*  mu_msg_field_shortcut (const MuMsgField *field)     G_GNUC_CONST;

/** 
 * get the xapian prefix of the field -- that is, the prefix used in
 * the Xapian database to identify the field
 * 
 * @param field a MuMsgField
 * 
 * @return the xapian prefix of the field as a constant string, or
 * NULL if the field is unknown
 */
const char*  mu_msg_field_xapian_prefix (const MuMsgField *field) G_GNUC_PURE;

/** 
 * get the numerical ID of the field
 * 
 * @param field a MuMsgField
 * 
 * @return an id, or MU_MSG_FIELD_TYPE_NONE if the field is not known
 */
MuMsgFieldId mu_msg_field_id (const MuMsgField *field)           G_GNUC_CONST;


/** 
 * get the type of the field (string, size, time etc.)
 * 
 * @param field a MuMsgField
 * 
 * @return the type of the field (a #MuMsgFieldType), or
 * MU_MSG_FIELD_TYPE_NONE if it is not found
 */
MuMsgFieldType mu_msg_field_type (const MuMsgField *field)         G_GNUC_CONST;


/** 
 * is the field numeric (has type MU_MSG_FIELD_TYPE_(BYTESIZE|TIME_T|INT))?
 * 
 * @param field a MuMsgField
 * 
 * @return TRUE if the field is numeric, FALSE otherwise
 */
gboolean mu_msg_field_is_numeric          (const MuMsgField *field) G_GNUC_CONST;

/** 
 * is the field Xapian-enabled? That is, should this field be stored
 * (as a string) in the Xapian database?
 * 
 * @param field a MuMsgField
 * 
 * @return TRUE if the field is Xapian-enabled, FALSE otherwise
 */
gboolean mu_msg_field_is_xapian_enabled   (const MuMsgField *field) G_GNUC_PURE;


/** 
 * is the field Xapian-indexable? That is, should this field be
 * indexed in the in the Xapian database, so we can use the all the
 * phrasing, stemming etc. magic
 * 
 * @param field a MuMsgField
 * 
 * @return TRUE if the field is Xapian-enabled, FALSE otherwise
 */
gboolean mu_msg_field_is_xapian_indexable (const MuMsgField *field) G_GNUC_PURE;


/** 
 * is the field gmime-enabled? That is, can be field be retrieved
 * using GMime?
 * 
 * @param field a MuMsgField
 * 
 * @return TRUE if the field is Gmime-enabled, FALSE otherwise
 */
gboolean mu_msg_field_is_gmime_enabled    (const MuMsgField *field) G_GNUC_PURE;


/** 
 * get the corresponding MuMsgField for a name (as in mu_msg_field_name)
 * 
 * @param str a name
 * 
 * @return a MuMsgField, or NULL if it could not be found
 */
const MuMsgField*  mu_msg_field_from_name     (const char* str)  G_GNUC_PURE;


/** 
 * get the corresponding MuMsgField for a shortcut (as in mu_msg_field_shortcut)
 * 
 * @param kar a shortcut character
 * 
 * @return a MuMsgField, or NULL if it could not be found
 */
const MuMsgField*  mu_msg_field_from_shortcut (char kar)         G_GNUC_CONST;

/** 
 * get the corresponding MuMsgField for an id (as in mu_msg_field_id)
 * 
 * @param id an id
 * 
 * @return a MuMsgField, or NULL if it could not be found
 */
const MuMsgField*  mu_msg_field_from_id       (MuMsgFieldId id)  G_GNUC_CONST;

G_END_DECLS

#endif /*__MU_MSG_FIELDS_H__*/
