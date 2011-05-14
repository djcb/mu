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

#ifndef __MU_MSG_CACHE_H__
#define __MU_MSG_CACHE_H__

#include <glib.h>
#include <mu-msg-fields.h>

/* MuMsgCache caches all values for one specific MuMsg, whether its
 * backend is a MuMsgFile or MuMsgDb. The idea is to take all the
 * values from the MuMsg so we can release the backend (file or db).
 *
 * It is specifically designed minimize memory allocations; you can
 * either store dynamically-allocated strings, of which the cache will
 * take ownership (and g_free in the end), *or* use strings, either
 * static or owned elsewhere. In the later case, no copy will be made
 * until mu_msg_cache_allocate_all is called
 *
 * Minimizing allocations in esp. necessary when storing
 * search-results as a list of MuMsg instances
 */

G_BEGIN_DECLS

struct _MuMsgCache;
typedef struct _MuMsgCache MuMsgCache;

/**
 * initialize the cache
 * 
 * @param self ptr to a cache struct
 */
MuMsgCache *mu_msg_cache_new (void);

/**
 * free the strings in the cache
 * 
 * @param self ptr to a cache struct
 */
void mu_msg_cache_destroy (MuMsgCache *self);



/**
 * add a string value to the cache; it will *not* be freed
 * 
 * @param self a cache struc 
 * @param mfid the MessageFieldId
 * @param str the string value to set
 */
void mu_msg_cache_set_str (MuMsgCache *self, MuMsgFieldId mfid,
			   const char *str);



/**
 * add a string value to the cache; the cache takes ownership, and
 * will free it when done with it.
 * 
 * @param self a cache struc 
 * @param mfid the MessageFieldId
 * @param str the string value to set
 */
void mu_msg_cache_set_str_alloc (MuMsgCache *self, MuMsgFieldId mfid,
				 char *str);

/**
 * get a string value from the cache
 * 
 * @param self ptr to a MuMsgCache
 * @param mfid the MessageFieldId for a string value
 * 
 * @return the string, or NULL. Don't modify.
 */
const char* mu_msg_cache_str (MuMsgCache *cache, MuMsgFieldId mfid);



/**
 * add a numeric value to the cache
 * 
 * @param self a MuMsgCache ptr
 * @param mfid the MessageFieldId for a numeric value
 * @param val the value
 */
void mu_msg_cache_set_num (MuMsgCache *self, MuMsgFieldId mfid, guint64 val);


/**
 * get a numeric value from the cache
 * 
 * @param self a MuMsgCache ptr
 * @param mfid the MessageFieldId for a numeric value
 * 
 * @return a numeric value
 */
gint64 mu_msg_cache_num (MuMsgCache *self, MuMsgFieldId mfid);



/**
 * is the value cached already?
 * 
 * @param self a MuMsgCache ptr
 * @param mfid the MessageFieldId for a numeric value
 *
 * @return TRUE if the value is cached, FALSE otherwise
 */
gboolean mu_msg_cache_cached (MuMsgCache *self, MuMsgFieldId mfid);



/**
 * copy all data that was not already owned by the cache
 * 
 * @param self a MuMsgCache ptr
 */
void mu_msg_cache_allocate_all (MuMsgCache *self);


G_END_DECLS

#endif /*__MU_MSG_CACHE_H__*/

