/*
** Copyright (C) 2008-2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef __MU_MSG_PRIV_H__
#define __MU_MSG_PRIV_H__

#include <gmime/gmime.h>
#include <stdlib.h>

#include "mu-msg.h"

/* we put the the MuMsg definition in this separate -priv file, so we
 * can split the mu_msg implementations over separate files */

enum _StringFields {

	HTML_FIELD  = 0,   /* body as HTML */
	TEXT_FIELD,        /* body as plain text */
	SUMMARY_FIELD,     /* body summary */

	TO_FIELD,          /* To: */
	CC_FIELD,	   /* Cc: */
	
	PATH_FIELD,        /* full path */
	MDIR_FIELD,        /* the maildir */
	
	FLAGS_FIELD_STR,   /* message flags */
	
	FIELD_NUM
};
typedef enum _StringFields StringFields;

struct _MuMsg {
	GMimeMessage    *_mime_msg;
	MuMsgFlags	_flags;
	
	char*           _fields[FIELD_NUM];

	size_t		_size;
	time_t		_timestamp;	
	MuMsgPrio       _prio;
};

#endif /*__MU_MSG_PRIV_H__*/
