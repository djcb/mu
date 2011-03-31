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

#ifndef __MU_MSG_DATA_CACHE_H__
#define __MU_MSG_DATA_CACHE_H__

#include <mu-msg-data.h>

G_BEGIN_DECLS

struct _MuMsgDataCache;
typedef struct _MuMsgDataCache MuMsgDataCache;

MuMsgDataCache *mu_msg_data_cache_new (guint32 max_size)
	G_GNUC_MALLOC G_GNUC_WARN_UNUSED_RESULT;
void mu_msg_data_cache_destroy		(MuMsgDataCache *self);
void mu_msg_data_cache_clear		(MuMsgDataCache *self);
void mu_msg_data_cache_append		(MuMsgDataCache *self, MuMsgData *data, gboolean own);
const MuMsgData *mu_msg_data_cache_get	(MuMsgDataCache *self, guint index);

G_END_DECLS

#endif /*__MU_MSG_DATA_CACHE_H__*/
