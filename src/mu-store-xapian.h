/* 
** Copyright (C) 2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef __MU_STORE_XAPIAN_H__
#define __MU_STORE_XAPIAN_H__

#include <glib.h>
#include <inttypes.h>

#include "mu-result.h"
#include "mu-msg-gmime.h"

G_BEGIN_DECLS

struct _MuStoreXapian;
typedef struct _MuStoreXapian MuStoreXapian;

MuStoreXapian*    mu_store_xapian_new     (const char* path);

void              mu_store_xapian_destroy (MuStoreXapian *store);
MuResult	  mu_store_xapian_store   (MuStoreXapian *store,
					   MuMsgGMime *msg);
MuResult          mu_store_xapian_cleanup (MuStoreXapian *store, 
					   const char* msgpath);

void              mu_store_xapian_set_timestamp (MuStoreXapian *store,
						   const char* msgpath, 
						   time_t stamp);
time_t            mu_store_xapian_get_timestamp (MuStoreXapian *store,
						   const char* msgpath);

G_END_DECLS

#endif /*__MU_STORE_XAPIAN_H__*/
