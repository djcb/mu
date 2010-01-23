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

/** 
 * create a new Xapian store, a place to store documents
 * 
 * @param path the path to the database
 * 
 * @return a new MuStoreXapian object, or NULL in case of error
 */
MuStoreXapian*    mu_store_xapian_new     (const char* path);

/** 
 * destroy the MuStoreXapian object and free resources
 * 
 * @param store a valid store, or NULL
 */
void              mu_store_xapian_destroy (MuStoreXapian *store);



/** 
 * get a version string for the database
 * 
 * @param store a valid MuStoreXapian
 * 
 * @return the version string (free with g_free), or NULL in case of error
 */
char* mu_store_xapian_version (MuStoreXapian *store);

/** 
 * set the version string for the database
 * 
 * @param store a valid MuStoreXapian
 * @param version the version string (non-NULL)
 *
 * @return TRUE if setting the version succeeded, FALSE otherwise  
 */
gboolean  mu_store_xapian_set_version (MuStoreXapian *store,
				       const char* version);


/** 
 * try to flush/commit all outstanding work
 * 
 * @param store a valid xapian store
 */
void mu_store_xapian_flush (MuStoreXapian *store);

/** 
 * store an email message in the XapianStore
 * 
 * @param store a valid store
 * @param msg a valid message
 * 
 * @return TRUE if it succeeded, FALSE otherwise
 */
MuResult	  mu_store_xapian_store   (MuStoreXapian *store,
					   MuMsgGMime *msg);


/** 
 * remove a message from the database
 * 
 * @param store a valid store
 * @param msgpath path of the message (note, this is only used to
 * *identify* the message; a common use of this function is to remove
 * a message from the database, for which there is no message anymore
 * in the filesystem.
 * 
 * @return TRUE if it succeeded, FALSE otherwise
 */
MuResult          mu_store_xapian_remove (MuStoreXapian *store,
					  const char* msgpath);


/** 
 * does a certain message exist in the database already?
 * 
 * @param store a store
 * @param path the message path
 * 
 * @return 
 */
gboolean          mu_store_contains_message (MuStoreXapian *store,
					     const char* path);

/** 
 * store a timestamp for a directory
 * 
 * @param store a valid store
 * @param msgpath path to a maildir
 * @param stamp a timestamp
 */
void              mu_store_xapian_set_timestamp (MuStoreXapian *store,
						 const char* msgpath, 
						 time_t stamp);

/** 
 * get the timestamp for a directory
 * 
 * @param store a valid store
 * @param msgpath path to a maildir
 * 
 * @return the timestamp, or 0 in case of error
 */
time_t            mu_store_xapian_get_timestamp (MuStoreXapian *store,
						 const char* msgpath);

/** 
 * call a function for each document in the database
 * 
 * @param self a valid store
 * @param func a callback function to to call for each document
 * @param user_data a user pointer passed to the callback function
 * 
 * @return MU_OK if all went well, MU_STOP if the foreach was interrupted,
 * MU_ERROR in case of error
 */
typedef MuResult (*MuStoreXapianForeachFunc) (const char* path,
					      void *user_data);
MuResult         mu_store_xapian_foreach (MuStoreXapian *self,
					  MuStoreXapianForeachFunc func,
					  void *user_data);

G_END_DECLS

#endif /*__MU_STORE_XAPIAN_H__*/
