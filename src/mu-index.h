/* 
** Copyright (C) 2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef __MU_INDEX_H__
#define __MU_INDEX_H__

#include "mu-result.h" /* for MuResult */

/* opaque structure */
struct _MuIndex;
typedef struct _MuIndex MuIndex;

struct _MuIndexStats {
	int _processed;     /* number of msgs processed or counted */
	int _updated;       /* number of msgs new or updated */
	int _cleaned_up;    /* number of msgs cleaned up */
	int _uptodate;      /* number of msgs already uptodate */
};
typedef struct _MuIndexStats MuIndexStats;

/** 
 * create a new MuIndex instance. NOTE(1): the database does not
 * have to exist yet, but the directory already has to exist;
 * NOTE(2): before doing anything with the returned Index object,
 * make sure you haved called g_type_init, and mu_msg_init somewhere
 * in your code.
 *
 * @param xpath path to the 'homedir'; the xapian directory will be
 * this homedir/xapian
 * 
 * @return a new MuIndex instance, or NULL in case of error
 */
MuIndex* mu_index_new (const char* muhome);


/** 
 * destroy the index instance
 * 
 * @param index a MuIndex instance, or NULL
 */
void mu_index_destroy (MuIndex *index);


/** 
 * callback function for mu_index_(run|stats|cleanup), for each message
 * 
 * @param stats pointer to structure to receive statistics data 
 * @param user_data pointer to user data
 *
 * @return  MU_OK to continue, MU_STOP to stop, or MU_ERROR in
 * case of some error.
 */
typedef MuResult (*MuIndexMsgCallback) (MuIndexStats* stats, void *user_data); 


/** 
 * callback function for mu_index_(run|stats|cleanup), for each dir enter/leave
 * 
 * @param path dirpath we just entered / left
 * @param enter did we enter (TRUE) or leave(FALSE) the dir?
 * @param user_data pointer to user data
 *
 * @return  MU_OK to contiue, MU_STOP to stopd or MU_ERROR in
 * case of some error.
 */
typedef MuResult (*MuIndexDirCallback) (const char* path, gboolean enter, 
					void *user_data);

/** 
 * start the indexing process 
 * 
 * @param index a valid MuIndex instance
 * @param path the path to index
 * @param force if != 0, force re-indexing already index messages; this is
 *         obviously a lot slower than only indexing new/changed messages
 * @param result a structure with some statistics about the results
 * @param cb_msg a callback function called for every msg indexed;
 * @param cb_dir a callback function called for every dir entered/left; 
 * @param user_data a user pointer that will be passed to the callback function
 * 
 * @return MU_OK if the stats gathering was completed succesfully, 
 * MU_STOP if the user stopped or MU_ERROR in
 * case of some error.
 */
MuResult mu_index_run (MuIndex *index, const char* path, gboolean force, 
		       MuIndexStats *result, MuIndexMsgCallback msg_cb,
		       MuIndexDirCallback dir_cb, void *user_data);

/** 
 * gather some statistics about the Maildir; this is usually much faster
 * than mu_index_run, and can thus be used to provide some information to the user
 * note though that the statistics may be different from the reality that 
 * mu_index_run sees, when there are updates in the Maildir
 * 
 * @param index a valid MuIndex instance
 * @param path the path to get stats for
 * @param result a structure with some statistics about the results
 * @param cb a callback function which will be called for every msg; 
 * @param user_data a user pointer that will be passed to the callback function
 * xb
 * @return MU_OK if the stats gathering was completed succesfully, 
 * MU_STOP if the user stopped or MU_ERROR in
 * case of some error.
 */
MuResult mu_index_stats (MuIndex *index, const char* path, MuIndexStats *result,
			 MuIndexMsgCallback msg_cb, MuIndexDirCallback dir_cb,
			 void *user_data);



/** 
 * callback function to determine if a message should be delete from
 * the database; if it returs MU_OK it will be delete, if returns
 * MU_IGNORE, the message will be ignored. In other cases, stop the callback
 * 
 * @param MuIndexCleanupCallback 
 * 
 * @return 
 */
typedef MuResult (*MuIndexCleanupDeleteCallback) (MuIndexStats*, void *user_data); 

/** 
 * cleanup the database; ie. remove entries for which no longer a corresponding
 * file exists in the maildir
 * 
 * @param index a valid MuIndex instance
 * @param result a structure with some statistics about the results
 * @param cb a callback function which will be called for every msg; 
 * @param user_data a user pointer that will be passed to the callback function
 * 
 * @return MU_OK if the stats gathering was completed succesfully, 
 * MU_STOP if the user stopped or MU_ERROR in
 * case of some error.
 */
MuResult mu_index_cleanup (MuIndex *index, MuIndexStats *result,
			   MuIndexCleanupDeleteCallback cb,
			   void *user_data);

#endif /*__MU_INDEX_H__*/
