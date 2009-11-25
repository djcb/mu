/*
** Copyright (C) 2008 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <string.h>
#include "mu-msg-fields.h"

enum _FieldFlags { 
	FLAG_XAPIAN   = 1 << 1, /* field available in xapian */
	FLAG_GMIME    = 1 << 2, /* field available in gmime */
	FLAG_INDEX    = 1 << 3  /* field is indexable */
};
typedef enum _FieldFlags FieldFlags;

struct _MuMsgField {
	const char     *_name;
	const char     *_shortcut;
	const char     *_prefix;
	MuMsgFieldId    _id;
	MuMsgFieldType  _type;
	
	FieldFlags      _flags;
};

static const MuMsgField FIELD_DATA[] = {
	
	{ "body", "b", "B", 
	  MU_MSG_FIELD_ID_BODY_TEXT,
	  MU_MSG_FIELD_TYPE_STRING, 
	  FLAG_XAPIAN | FLAG_GMIME | FLAG_INDEX
	},
	
	{ "bodyhtml", "h", NULL,
	  MU_MSG_FIELD_ID_BODY_HTML,
	  MU_MSG_FIELD_TYPE_STRING,
	  FLAG_GMIME
	},
	
	{ "cc", "c", "C",
	  MU_MSG_FIELD_ID_CC,
	  MU_MSG_FIELD_TYPE_STRING, 
	  FLAG_XAPIAN | FLAG_GMIME | FLAG_INDEX
	},
	
	{ "date", "d", "D",
	  MU_MSG_FIELD_ID_DATE, 
	  MU_MSG_FIELD_TYPE_TIME_T,
	  FLAG_XAPIAN | FLAG_GMIME  
	},
	
	{ "flags", "F", "G",
	  MU_MSG_FIELD_ID_FLAGS, 
	  MU_MSG_FIELD_TYPE_INT,
	  FLAG_XAPIAN | FLAG_GMIME
	},

	{ "from", "f", "F",
	  MU_MSG_FIELD_ID_FROM,
	  MU_MSG_FIELD_TYPE_STRING,
	  FLAG_XAPIAN | FLAG_GMIME | FLAG_INDEX
	},

	{ "path", "p", "P",  
	  MU_MSG_FIELD_ID_PATH, 
	  MU_MSG_FIELD_TYPE_STRING,
	  FLAG_XAPIAN | FLAG_GMIME 
	},
	
	{ "prio", "P", "I",  
	  MU_MSG_FIELD_ID_PRIORITY,
	  MU_MSG_FIELD_TYPE_INT,
	  FLAG_XAPIAN | FLAG_GMIME
	}, 

	{ "size", "z", "Z",  
	  MU_MSG_FIELD_ID_SIZE, 
	  MU_MSG_FIELD_TYPE_BYTESIZE,
	  FLAG_XAPIAN | FLAG_GMIME
	},
	
	 { "subject", "s", "S",
	   MU_MSG_FIELD_ID_SUBJECT,
	   MU_MSG_FIELD_TYPE_STRING,
	   FLAG_XAPIAN | FLAG_GMIME | FLAG_INDEX
	 },
	
	{ "to", "t", "T",
	  MU_MSG_FIELD_ID_TO,
	  MU_MSG_FIELD_TYPE_STRING,
	  FLAG_XAPIAN | FLAG_GMIME | FLAG_INDEX
	},
	
	{ "msgid", "m", NULL,
	  MU_MSG_FIELD_ID_MSGID,
	  MU_MSG_FIELD_TYPE_STRING,
	  FLAG_GMIME 
	},
	
	{ "timestamp", "i", NULL,
	  MU_MSG_FIELD_ID_TIMESTAMP,
	  MU_MSG_FIELD_TYPE_TIME_T,
	  FLAG_GMIME 
	},
	
	{ NULL, NULL, NULL, 0, 0, 0 }
};

void
mu_msg_field_foreach (MuMsgFieldForEachFunc func, gconstpointer data)
{
	const MuMsgField* cursor = &FIELD_DATA[0];
	while (cursor->_name) {
		func (cursor, data);
		++cursor;
	}
}

typedef gboolean (*FieldMatchFunc) (const MuMsgField *field, 
				    gconstpointer data);

static const MuMsgField*
find_field (FieldMatchFunc matcher, gconstpointer data)
{
	const MuMsgField* cursor = &FIELD_DATA[0];
	while (cursor->_name) {
		if (matcher (cursor, data))
			return cursor;
		++cursor;
	}
	return NULL;
}

static gboolean
match_name (const MuMsgField *field, const gchar* name)
{
	return strcmp (field->_name, name) == 0;
}

const MuMsgField*
mu_msg_field_from_name (const char* str)
{
	g_return_val_if_fail (str, NULL);
	return find_field ((FieldMatchFunc)match_name, str);
}

static gboolean
match_shortcut (const MuMsgField *field, char kar)
{
	return field->_shortcut[0] == kar;
}

const MuMsgField*
mu_msg_field_from_shortcut (char kar)
{
	return find_field ((FieldMatchFunc)match_shortcut,
			   GUINT_TO_POINTER((guint)kar));
}

static gboolean
match_id (const MuMsgField *field, MuMsgFieldId id)
{
	return field->_id == id;
}

const MuMsgField*  
mu_msg_field_from_id (MuMsgFieldId id)    
{
	return find_field ((FieldMatchFunc)match_id,
			   GUINT_TO_POINTER(id));
}


gboolean
mu_msg_field_is_xapian_enabled    (const MuMsgField *field)
{
	g_return_val_if_fail (field, FALSE);
	return field->_flags & FLAG_XAPIAN;
}


gboolean
mu_msg_field_is_gmime_enabled     (const MuMsgField *field)
{
	g_return_val_if_fail (field, FALSE);
	return field->_flags & FLAG_GMIME;
}


gboolean
mu_msg_field_is_xapian_indexable  (const MuMsgField *field)
{
	g_return_val_if_fail (field, FALSE);
	return field->_flags & FLAG_INDEX;
}


gboolean
mu_msg_field_is_numeric (const MuMsgField *field)
{
	MuMsgFieldType type;
	
	g_return_val_if_fail (field, FALSE);
	
	type = mu_msg_field_type (field);
	
	return type == MU_MSG_FIELD_TYPE_BYTESIZE ||
		type == MU_MSG_FIELD_TYPE_TIME_T ||
		type == MU_MSG_FIELD_TYPE_INT;
}

const char*    
mu_msg_field_name (const MuMsgField *field)
{
	g_return_val_if_fail (field, NULL);
	return field->_name;
}

const char*
mu_msg_field_shortcut (const MuMsgField *field)
{
	g_return_val_if_fail (field, NULL);
	return field->_shortcut;
}

MuMsgFieldId
mu_msg_field_id (const MuMsgField *field)
{
	g_return_val_if_fail (field, MU_MSG_FIELD_ID_NONE);
	return field->_id;
}

const char*
mu_msg_field_xapian_prefix (const MuMsgField *field)
{
	g_return_val_if_fail (field, NULL);
	return field->_prefix;
}


MuMsgFieldType 
mu_msg_field_type (const MuMsgField *field)
{
	g_return_val_if_fail (field, MU_MSG_FIELD_TYPE_NONE);
	return field->_type;
}
