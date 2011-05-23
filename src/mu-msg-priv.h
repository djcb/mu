/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

/*
** Copyright (C) 2008-2011 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <mu-msg.h>
#include <mu-msg-file.h>
#include <mu-msg-doc.h>
#include <mu-msg-cache.h>

G_BEGIN_DECLS


struct _MuMsgFile {
	GMimeMessage	*_mime_msg;
	time_t		 _timestamp;
	size_t		 _size;
	char		 _path    [PATH_MAX + 1];
	char		 _maildir [PATH_MAX + 1];
};


/* we put the the MuMsg definition in this separate -priv file, so we
 * can split the mu_msg implementations over separate files */
struct _MuMsg {

	guint		 _refcount;

	/* our two backend */
	MuMsgFile	*_file; /* based on GMime, ie. a file on disc */
	MuMsgDoc        *_doc;  /* based on Xapian::Document */
	
	MuMsgCache      *_cache;
};

G_END_DECLS

#endif /*__MU_MSG_PRIV_H__*/
