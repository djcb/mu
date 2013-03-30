/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

/*
** Copyright (C) 2008-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <stdlib.h>
#include <glib.h>
#include <mu-util.h> /* for MuResult */
#include <mu-store.h>

G_BEGIN_DECLS

/* opaque structure */
struct _MuIndex;
typedef struct _MuIndex MuIndex;

struct _MuIndexStats {
	unsigned _processed;     /* number of msgs processed or counted */
	unsigned _updated;       /* number of msgs new or updated */
	unsigned _cleaned_up;    /* number of msgs cleaned up */
	unsigned _uptodate;      /* number of msgs already uptodate */
};
typedef struct _MuIndexStats MuIndexStats;

/**
 * create a new MuIndex instance. NOTE(1): the database does not have
 * to exist yet, but the directory must already exist; NOTE(2): before
 * doing anything with the returned Index object, make sure you haved
 * called g_type_init, and mu_msg_init somewhere in your code.
 *
 * @param store a writable MuStore object
 * @param err to receive error or NULL; there are only errors when this
 * function returns NULL. Possible errors: see mu-error.h
 *
 * @return a new MuIndex instance, or NULL in case of error
 */
MuIndex* mu_index_new (MuStore *store, GError **err)
    G_GNUC_MALLOC G_GNUC_WARN_UNUSED_RESULT;


/**
 * destroy the index instance
 *
 * @param index a MuIndex instance, or NULL
 */
void mu_index_destroy (MuIndex *index);


/**
 * change the maximum file size that mu-index considers from its
 * default (MU_INDEX_MAX_FILE_SIZE). Note that the maximum size is a
 * protection against mu (or the libraries it uses) allocating too
 * much memory, which can lead to problems
 *
 * @param index a mu index object
 * @param max_size the maximum msg size, or 0 to reset to the default
 */
void mu_index_set_max_msg_size (MuIndex *index, guint max_size);


/**
 * change batch size for Xapian store transaction (see
 * 'mu_store_set_batch_size')
 *
 * @param index a mu index object
 * @param max_size the batch size, or 0 to reset to the default
 */
void mu_index_set_xbatch_size (MuIndex *index, guint xbatchsize);


/**
 * callback function for mu_index_(run|stats|cleanup), for each message
 *
 * @param stats pointer to structure to receive statistics data
 * @param user_data pointer to user data
 *
 * @return  MU_OK to continue, MU_STOP to stop, or MU_ERROR in
 * case of some error.
 */
typedef MuError (*MuIndexMsgCallback) (MuIndexStats* stats, void *user_data);


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
typedef MuError (*MuIndexDirCallback) (const char* path, gboolean enter,
				       void *user_data);

/**
 * start the indexing process
 *
 * @param index a valid MuIndex instance
 * @param path the path to index. This must be an absolute path
 * @param force if != 0, force re-indexing already index messages; this is
 *         obviously a lot slower than only indexing new/changed messages
 * @param stats a structure with some statistics about the results;
 * note that this function does *not* reset the struct values to allow
 * for cumulative stats from multiple calls. If needed, you can use
 * @mu_index_stats_clear before calling this function
 * @param cb_msg a callback function called for every msg indexed;
 * @param cb_dir a callback function called for every dir entered/left or NULL
 * @param user_data a user pointer that will be passed to the callback function
 *
 * @return MU_OK if the stats gathering was completed succesfully,
 * MU_STOP if the user stopped or MU_ERROR in
 * case of some error.
 */
MuError mu_index_run (MuIndex *index, const char *path, gboolean force,
		      MuIndexStats *stats, MuIndexMsgCallback msg_cb,
		      MuIndexDirCallback dir_cb, void *user_data);

/**
 * gather some statistics about the Maildir; this is usually much faster
 * than mu_index_run, and can thus be used to provide some information to the user
 * note though that the statistics may be different from the reality that
 * mu_index_run sees, when there are updates in the Maildir
 *
 * @param index a valid MuIndex instance
 * @param path the path to get stats for; this must be an absolute path
 * @param stats a structure with some statistics about the results;
 * note that this function does *not* reset the struct values to allow
 * for cumulative stats from multiple calls. If needed, you can use
 * @mu_index_stats_clear before calling this function
 * @param msg_cb a callback function which will be called for every msg;
 * @param dir_cb a callback function which will be called for every dir or NULL
 * @param user_data a user pointer that will be passed to the callback function
 * xb
 * @return MU_OK if the stats gathering was completed succesfully,
 * MU_STOP if the user stopped or MU_ERROR in
 * case of some error.
 */
MuError mu_index_stats (MuIndex *index, const char *path, MuIndexStats *stats,
			MuIndexMsgCallback msg_cb, MuIndexDirCallback dir_cb,
			void *user_data);



/**
 * callback function called for each message
 *
 * @param MuIndexCleanupCallback
 *
 * @return a MuResult
 */
typedef MuError (*MuIndexCleanupDeleteCallback) (MuIndexStats *stats,
						 void *user_data);

/**
 * cleanup the database; ie. remove entries for which no longer a corresponding
 * file exists in the maildir
 *
 * @param index a valid MuIndex instance
 * @param stats a structure with some statistics about the results;
 * note that this function does *not* reset the struct values to allow
 * for cumulative stats from multiple calls. If needed, you can use
 * @mu_index_stats_clear before calling this function
 * @param cb a callback function which will be called for every msg;
 * @param user_data a user pointer that will be passed to the callback function
 * @param err to receive error info or NULL. err->code is MuError value
 *
 * @return MU_OK if the stats gathering was completed succesfully,
 * MU_STOP if the user stopped or MU_ERROR in
 * case of some error.
 */
MuError mu_index_cleanup (MuIndex *index, MuIndexStats *stats,
			  MuIndexCleanupDeleteCallback cb,
			  void *user_data, GError **err);

/**
 * clear the stats structure
 *
 * @param stats a MuIndexStats object
 *
 * @return TRUE if stats != NULL, FALSE otherwise
 */
gboolean mu_index_stats_clear (MuIndexStats *stats);

G_END_DECLS

#endif /*__MU_INDEX_H__*/
