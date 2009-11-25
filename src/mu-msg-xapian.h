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

#ifndef __MU_MSG_XAPIAN_H__
#define __MU_MSG_XAPIAN_H__

#include "mu-msg.h"

G_BEGIN_DECLS

struct _MuMsgXapian;
typedef struct _MuMsgXapian MuMsgXapian;

gboolean         mu_msg_xapian_next              (MuMsgXapian *msg);
gboolean         mu_msg_xapian_is_done           (MuMsgXapian *msg);
void		 mu_msg_xapian_destroy           (MuMsgXapian *msg);

unsigned int     mu_msg_xapian_get_id            (MuMsgXapian *row);
const char*      mu_msg_xapian_get_path          (MuMsgXapian *row);
size_t           mu_msg_xapian_get_size          (MuMsgXapian *row);  
time_t           mu_msg_xapian_get_timestamp     (MuMsgXapian *row);  
time_t           mu_msg_xapian_get_date          (MuMsgXapian *row);  
const char*      mu_msg_xapian_get_from          (MuMsgXapian *row);
const char*      mu_msg_xapian_get_to            (MuMsgXapian *row);
const char*      mu_msg_xapian_get_cc            (MuMsgXapian *row);
const char*      mu_msg_xapian_get_subject       (MuMsgXapian *row);
MuMsgFlags       mu_msg_xapian_get_flags         (MuMsgXapian *row);
MuMsgPriority    mu_msg_xapian_get_priority      (MuMsgXapian *row);

const gchar*     mu_msg_xapian_get_field         (MuMsgXapian *row, 
						  const MuMsgField *field);
gint64           mu_msg_xapian_get_field_numeric     (MuMsgXapian *row, 
						      const MuMsgField *field);
G_END_DECLS

#endif /*__MU_MSG_XAPIAN_H__*/
