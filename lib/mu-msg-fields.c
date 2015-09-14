/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

/*
** Copyright (C) 2008-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

/*
 * note: the differences for our purposes between a xapian field and a
 * term: - there is only a single value for some item in per document
 * (msg), ie.  one value containing the list of To: addresses - there
 * can be multiple terms, each containing e.g. one of the To:
 * addresses - searching uses terms, but to display some field, it
 * must be in the value (at least when using MuMsgIter)
 */
enum _FieldFlags {
	FLAG_GMIME	         = 1 << 0, /* field retrieved through
					    * gmime */
	FLAG_XAPIAN_INDEX        = 1 << 1, /* field is indexed in
			          	    * xapian (i.e., the text
			          	    * is processed */
	FLAG_XAPIAN_TERM         = 1 << 2, /* field stored as term in
			          	    * xapian (so it can be searched) */
	FLAG_XAPIAN_VALUE        = 1 << 3, /* field stored as value in
			          	    * xapian (so the literal
			          	    * value can be
			          	    * retrieved) */
	FLAG_XAPIAN_CONTACT      = 1 << 4, /* field contains one or more
					    * e-mail-addresses */
	FLAG_XAPIAN_BOOLEAN      = 1 << 5, /* use 'add_boolean_prefix'
					    * for Xapian queries;
					    * wildcards do NOT WORK
					    * for such fields */
	FLAG_PREPROCESS	         = 1 << 6, /* field needs flattening for
					    * case/accents */
	FLAG_DONT_CACHE          = 1 << 7,  /* don't cache this field in
					    * the MuMsg cache */
	FLAG_RANGE_FIELD         = 1 << 8  /* whether this is a range field */

};
typedef enum _FieldFlags	FieldFlags;

/*
 * this struct describes the fields of an e-mail
 /*/
struct _MuMsgField {
	MuMsgFieldId      _id;	     /* the id of the field */
	MuMsgFieldType    _type;     /* the type of the field */
	const char       *_name;     /* the name of the field */
	const char        _shortcut; /* the shortcut for use in
				      * --fields and sorting */
	const char        _xprefix;  /* the Xapian-prefix  */
	FieldFlags        _flags;    /* the flags that tells us
				      * what to do */


};
typedef struct _MuMsgField MuMsgField;

/* the name and shortcut fields must be lower case, or they might be
 * misinterpreted by the query-preprocesser which turns queries into
 * lowercase */
static const MuMsgField FIELD_DATA[] = {

	{
		MU_MSG_FIELD_ID_BCC,
		MU_MSG_FIELD_TYPE_STRING,
		"bcc" , 'h', 'H',  /* 'hidden */
		FLAG_GMIME | FLAG_XAPIAN_CONTACT |
		FLAG_XAPIAN_VALUE | FLAG_PREPROCESS
	},

	{
		MU_MSG_FIELD_ID_BODY_TEXT,
		MU_MSG_FIELD_TYPE_STRING,
		"body", 'b', 'B',
		FLAG_GMIME | FLAG_XAPIAN_INDEX | FLAG_PREPROCESS |
		FLAG_DONT_CACHE
	},

	{
		MU_MSG_FIELD_ID_BODY_HTML,
		MU_MSG_FIELD_TYPE_STRING,
		"bodyhtml", 0, 0,
		FLAG_GMIME | FLAG_DONT_CACHE
	},

	{
		MU_MSG_FIELD_ID_CC,
		MU_MSG_FIELD_TYPE_STRING,
		"cc", 'c', 'C',
		FLAG_GMIME | FLAG_XAPIAN_CONTACT | FLAG_XAPIAN_VALUE |
		FLAG_PREPROCESS
	},

	{
		MU_MSG_FIELD_ID_DATE,
		MU_MSG_FIELD_TYPE_TIME_T,
		"date", 'd', 'D',
		FLAG_GMIME | FLAG_XAPIAN_TERM | FLAG_XAPIAN_VALUE |
		FLAG_XAPIAN_BOOLEAN | FLAG_RANGE_FIELD
	},

	{
		MU_MSG_FIELD_ID_EMBEDDED_TEXT,
		MU_MSG_FIELD_TYPE_STRING,
		"embed", 'e', 'E',
		FLAG_GMIME | FLAG_XAPIAN_INDEX | FLAG_PREPROCESS |
		FLAG_DONT_CACHE
	},

	{
		MU_MSG_FIELD_ID_FILE,
		MU_MSG_FIELD_TYPE_STRING,
		"file" , 'j', 'J',
		FLAG_GMIME | FLAG_XAPIAN_TERM | FLAG_PREPROCESS |
		FLAG_DONT_CACHE
	},


	{
		MU_MSG_FIELD_ID_FLAGS,
		MU_MSG_FIELD_TYPE_INT,
		"flag", 'g', 'G',  /* flaGs */
		FLAG_GMIME | FLAG_XAPIAN_TERM | FLAG_XAPIAN_VALUE
	},

	{
		MU_MSG_FIELD_ID_FROM,
		MU_MSG_FIELD_TYPE_STRING,
		"from", 'f', 'F',
		FLAG_GMIME | FLAG_XAPIAN_CONTACT | FLAG_XAPIAN_VALUE |
		FLAG_PREPROCESS
	},


	{
		MU_MSG_FIELD_ID_MAILDIR,
		MU_MSG_FIELD_TYPE_STRING,
		"maildir", 'm', 'M',
		FLAG_GMIME | FLAG_XAPIAN_TERM | FLAG_XAPIAN_VALUE |
		FLAG_PREPROCESS
	},


	{
		MU_MSG_FIELD_ID_MAILING_LIST,
		MU_MSG_FIELD_TYPE_STRING,
		"list", 'v', 'V',
		FLAG_GMIME | FLAG_XAPIAN_TERM | FLAG_XAPIAN_VALUE |
		FLAG_PREPROCESS
	},


	{
		MU_MSG_FIELD_ID_MIME,
		MU_MSG_FIELD_TYPE_STRING,
		"mime" , 'y', 'Y',
		FLAG_XAPIAN_TERM | FLAG_PREPROCESS
	},


	{
		MU_MSG_FIELD_ID_MSGID,
		MU_MSG_FIELD_TYPE_STRING,
		"msgid", 'i', 'I',  /* 'i' for Id */
		FLAG_GMIME | FLAG_XAPIAN_TERM | FLAG_XAPIAN_VALUE |
		FLAG_PREPROCESS
	},


	{
		MU_MSG_FIELD_ID_PATH,
		MU_MSG_FIELD_TYPE_STRING,
		"path", 'l', 'L',   /* 'l' for location */
		FLAG_GMIME | FLAG_XAPIAN_VALUE |
		FLAG_XAPIAN_BOOLEAN | FLAG_PREPROCESS
	},


	{
		MU_MSG_FIELD_ID_PRIO,
		MU_MSG_FIELD_TYPE_INT,
		"prio", 'p', 'P',
		FLAG_GMIME | FLAG_XAPIAN_TERM | FLAG_XAPIAN_VALUE
	},


	{
		MU_MSG_FIELD_ID_REFS,
		MU_MSG_FIELD_TYPE_STRING_LIST,
		"refs", 'r', 'R',
		FLAG_GMIME | FLAG_XAPIAN_VALUE
	},


	{
		MU_MSG_FIELD_ID_SIZE,
		MU_MSG_FIELD_TYPE_BYTESIZE,
		"size", 'z', 'Z', /* siZe */
		FLAG_GMIME | FLAG_XAPIAN_TERM | FLAG_XAPIAN_VALUE |
		FLAG_RANGE_FIELD
	},

	{
		MU_MSG_FIELD_ID_SUBJECT,
		MU_MSG_FIELD_TYPE_STRING,
		"subject", 's', 'S',
		FLAG_GMIME | FLAG_XAPIAN_INDEX | FLAG_XAPIAN_VALUE |
		FLAG_XAPIAN_TERM | FLAG_PREPROCESS
	},

	{
		MU_MSG_FIELD_ID_TAGS,
		MU_MSG_FIELD_TYPE_STRING_LIST,
		"tag", 'x', 'X',
		FLAG_GMIME | FLAG_XAPIAN_TERM |	FLAG_PREPROCESS |
		FLAG_XAPIAN_VALUE
	},


	{	/* remember which thread this message is in */
		MU_MSG_FIELD_ID_THREAD_ID,
		MU_MSG_FIELD_TYPE_STRING,
		"thread", 0, 'W',
		FLAG_XAPIAN_TERM
	},

	{
		MU_MSG_FIELD_ID_TO,
		MU_MSG_FIELD_TYPE_STRING,
		"to", 't', 'T',
		FLAG_GMIME | FLAG_XAPIAN_CONTACT | FLAG_XAPIAN_VALUE |
		FLAG_PREPROCESS
	},

	{	/* special, internal field, to get a unique key */
		MU_MSG_FIELD_ID_UID,
		MU_MSG_FIELD_TYPE_STRING,
		"uid", 0, 'U',
		FLAG_XAPIAN_TERM
	}

	/* note, mu-store also use the 'Q' internal prefix for its uids */
};

/* the MsgField data in an array, indexed by the MsgFieldId;
 * this allows for O(1) access
 */
static MuMsgField* _msg_field_data[MU_MSG_FIELD_ID_NUM];
static const MuMsgField* mu_msg_field (MuMsgFieldId id)
{
	static gboolean _initialized;

	_initialized = FALSE;

	/* initialize the array, but only once... */
	if (G_UNLIKELY(!_initialized)) {
		int i;
		for (i = 0; i != G_N_ELEMENTS(FIELD_DATA); ++i)
			_msg_field_data[FIELD_DATA[i]._id] =
				(MuMsgField*)&FIELD_DATA[i];
		_initialized = TRUE;
	}

	return _msg_field_data[id];
}


void
mu_msg_field_foreach (MuMsgFieldForeachFunc func, gconstpointer data)
{
	int i;
	for (i = 0; i != MU_MSG_FIELD_ID_NUM; ++i)
		func (i, data);
}


MuMsgFieldId
mu_msg_field_id_from_name (const char* str, gboolean err)
{
	int i;

	g_return_val_if_fail (str, MU_MSG_FIELD_ID_NONE);

	for (i = 0; i != G_N_ELEMENTS(FIELD_DATA); ++i)
		if (g_strcmp0(str, FIELD_DATA[i]._name) == 0)
			return FIELD_DATA[i]._id;
	if (err)
		g_return_val_if_reached (MU_MSG_FIELD_ID_NONE);

	return MU_MSG_FIELD_ID_NONE;
}


MuMsgFieldId
mu_msg_field_id_from_shortcut (char kar, gboolean err)
{
	int i;
	for (i = 0; i != G_N_ELEMENTS(FIELD_DATA); ++i)
		if (kar == FIELD_DATA[i]._shortcut)
			return FIELD_DATA[i]._id;

	if (err)
		g_return_val_if_reached (MU_MSG_FIELD_ID_NONE);

	return MU_MSG_FIELD_ID_NONE;
}


gboolean
mu_msg_field_gmime (MuMsgFieldId id)
{
	g_return_val_if_fail (mu_msg_field_id_is_valid(id),FALSE);
	return mu_msg_field(id)->_flags & FLAG_GMIME ? TRUE: FALSE;
}


gboolean
mu_msg_field_xapian_index  (MuMsgFieldId id)
{
	g_return_val_if_fail (mu_msg_field_id_is_valid(id),FALSE);
	return mu_msg_field(id)->_flags & FLAG_XAPIAN_INDEX ? TRUE: FALSE;
}

gboolean
mu_msg_field_xapian_value (MuMsgFieldId id)
{
	g_return_val_if_fail (mu_msg_field_id_is_valid(id),FALSE);
	return mu_msg_field(id)->_flags & FLAG_XAPIAN_VALUE ? TRUE: FALSE;
}

gboolean
mu_msg_field_xapian_term (MuMsgFieldId id)
{
	g_return_val_if_fail (mu_msg_field_id_is_valid(id),FALSE);
	return mu_msg_field(id)->_flags & FLAG_XAPIAN_TERM ? TRUE: FALSE;
}


gboolean
mu_msg_field_is_range_field (MuMsgFieldId id)
{
	g_return_val_if_fail (mu_msg_field_id_is_valid(id),FALSE);
	return mu_msg_field(id)->_flags & FLAG_RANGE_FIELD ? TRUE: FALSE;
}



gboolean
mu_msg_field_uses_boolean_prefix (MuMsgFieldId id)
{
	g_return_val_if_fail (mu_msg_field_id_is_valid(id),FALSE);
	return mu_msg_field(id)->_flags & FLAG_XAPIAN_BOOLEAN ? TRUE:FALSE;
}



gboolean
mu_msg_field_is_cacheable (MuMsgFieldId id)
{
	g_return_val_if_fail (mu_msg_field_id_is_valid(id),FALSE);
	/* note the FALSE: TRUE */
	return mu_msg_field(id)->_flags & FLAG_DONT_CACHE ? FALSE : TRUE;
}

gboolean
mu_msg_field_xapian_contact (MuMsgFieldId id)
{
	g_return_val_if_fail (mu_msg_field_id_is_valid(id),FALSE);
	return mu_msg_field(id)->_flags & FLAG_XAPIAN_CONTACT ? TRUE: FALSE;
}


gboolean
mu_msg_field_preprocess (MuMsgFieldId id)
{
	g_return_val_if_fail (mu_msg_field_id_is_valid(id),FALSE);
	return mu_msg_field(id)->_flags & FLAG_PREPROCESS ? TRUE: FALSE;
}


gboolean
mu_msg_field_is_numeric (MuMsgFieldId mfid)
{
	MuMsgFieldType type;

	g_return_val_if_fail (mu_msg_field_id_is_valid(mfid),FALSE);

	type = mu_msg_field_type (mfid);

	return  type == MU_MSG_FIELD_TYPE_BYTESIZE ||
		type == MU_MSG_FIELD_TYPE_TIME_T ||
		type == MU_MSG_FIELD_TYPE_INT;
}

const char*
mu_msg_field_name (MuMsgFieldId id)
{
	g_return_val_if_fail (mu_msg_field_id_is_valid(id),NULL);
	return mu_msg_field(id)->_name;
}


char
mu_msg_field_shortcut (MuMsgFieldId id)
{
	g_return_val_if_fail (mu_msg_field_id_is_valid(id),0);
	return mu_msg_field(id)->_shortcut;
}


char
mu_msg_field_xapian_prefix (MuMsgFieldId id)
{
	g_return_val_if_fail (mu_msg_field_id_is_valid(id),0);
	return mu_msg_field(id)->_xprefix;
}




MuMsgFieldType
mu_msg_field_type (MuMsgFieldId id)
{
	g_return_val_if_fail (mu_msg_field_id_is_valid(id),
			      MU_MSG_FIELD_TYPE_NONE);
	return mu_msg_field(id)->_type;
}
