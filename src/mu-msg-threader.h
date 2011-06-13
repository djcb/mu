/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

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


#ifndef __MU_MSG_THREADER_H__
#define __MU_MSG_THREADER_H__

#include <glib.h>
#include <mu-msg-iter.h>

G_BEGIN_DECLS

struct _MuMsgThreader;
typedef struct _MuMsgThreader MuMsgThreader;

MuMsgThreader *mu_msg_threader_new (void);
void mu_msg_threader_destroy (MuMsgThreader *self);
gboolean mu_msg_threader_calculate (MuMsgThreader *self, MuMsgIter *iter);

G_END_DECLS

#endif /*__MU_MSG_THREADER_H__*/
