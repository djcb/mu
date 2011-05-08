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
#include "mu-msg-file.h"

G_BEGIN_DECLS

/* we put the the MuMsg definition in this separate -priv file, so we
 * can split the mu_msg implementations over separate files */

struct _MuMsgFile {
	GMimeMessage *_mime_msg;

	/* we waste a few bytes here for none-string fields... */
	gchar *_str_cache[MU_MSG_FIELD_ID_NUM];

	GSList *_refs;
	
	time_t	_timestamp;
	size_t	_size;

	MuMsgFlags	_flags;
	MuMsgPrio	_prio;
};


struct _MuMsg {
	guint		 _refcount;
	MuMsgFile	*_file;
};

G_END_DECLS

#endif /*__MU_MSG_PRIV_H__*/
