/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

/*
** Copyright (C) 2008-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
**
** This program is free software; you can redistribute it and/or modify
1** it under the terms of the GNU General Public License as published by
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include "mu-index.h"

#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <glib.h>
#include <glib/gstdio.h>
#include <errno.h>

#include "mu-maildir.h"
#include "mu-store.h"
#include "mu-util.h"

#define	MU_LAST_USED_MAILDIR_KEY "last_used_maildir"
#define MU_INDEX_MAX_FILE_SIZE (50*1000*1000) /* 50 Mb */

struct _MuIndex {
	MuStore		*_store;
	gboolean	 _needs_reindex;
	guint            _max_filesize;
};

MuIndex*
mu_index_new (MuStore *store, GError **err)
{
	MuIndex *index;
	unsigned count;

	g_return_val_if_fail (store, NULL);
	g_return_val_if_fail (!mu_store_is_read_only(store), NULL);

	index = g_new0 (MuIndex, 1);

	index->_store = mu_store_ref (store);

	/* set the default max file size */
	index->_max_filesize = MU_INDEX_MAX_FILE_SIZE;

	count = mu_store_count (store, err);
	if (count == (unsigned)-1)
		return NULL;
	else if (count  == 0)
		index->_needs_reindex = FALSE;

	/* FIXME */
	/* else */
	/* 	index->_needs_reindex = */
	/* 		mu_store_database_needs_upgrade (xpath); */

	return index;
}

void
mu_index_destroy (MuIndex *index)
{
	if (!index)
		return;

	mu_store_unref (index->_store);
	g_free (index);
}


struct _MuIndexCallbackData {
	MuIndexMsgCallback	_idx_msg_cb;
	MuIndexDirCallback	_idx_dir_cb;
	MuStore*		_store;
	void*			_user_data;
	MuIndexStats*		_stats;
	gboolean		_reindex;
	time_t			_dirstamp;
	guint			_max_filesize;
};
typedef struct _MuIndexCallbackData	MuIndexCallbackData;


/* checks to determine if we need to (re)index this message note:
 * simply checking timestamps is not good enough because message may
 * be moved from other dirs (e.g. from 'new' to 'cur') and the time
 * stamps won't change. */
static inline gboolean
needs_index (MuIndexCallbackData *data, const char *fullpath,
	     time_t filestamp)
{
	/* unconditionally reindex */
	if (data->_reindex)
		return TRUE;

	/* it's not in the database yet (FIXME: GError)*/
	if (!mu_store_contains_message (data->_store, fullpath, NULL))
		return TRUE;

	/* it's there, but it's not up to date */
	if ((unsigned)filestamp >= (unsigned)data->_dirstamp)
		return TRUE;

	return FALSE; /* index not needed */
}


static MuError
insert_or_update_maybe (const char *fullpath, const char *mdir,
			time_t filestamp, MuIndexCallbackData *data,
			gboolean *updated)
{
	MuMsg		*msg;
	GError		*err;
	gboolean	 rv;

	*updated = FALSE;
	if (!needs_index (data, fullpath, filestamp))
		return MU_OK; /* nothing to do for this one */

	err = NULL;
	msg = mu_msg_new_from_file (fullpath, mdir, &err);
	if (!msg) {
		if (!err)
			g_warning ("error creating message object: %s",
				   fullpath);
		else {
			g_warning ("%s", err->message);
			g_clear_error (&err);
		}
		/* warn, then simply continue */
		return MU_OK;
	}

	/* we got a valid id; scan the message contents as well */
	rv = mu_store_add_msg (data->_store, msg, &err);
	mu_msg_unref (msg);

	if (!rv) {
		g_warning ("error storing message object: %s",
			   err ? err->message : "cause unknown");
		g_clear_error (&err);
		return MU_ERROR;
	}

 	*updated = TRUE;
	return MU_OK;
}


static MuError
run_msg_callback_maybe (MuIndexCallbackData *data)
{
	MuError result;

	if (!data || !data->_idx_msg_cb)
		return MU_OK;

	result = data->_idx_msg_cb (data->_stats, data->_user_data);
	if (G_UNLIKELY(result != MU_OK && result != MU_STOP))
		g_warning ("error in callback");

	return result;
}


static MuError
on_run_maildir_msg (const char *fullpath, const char *mdir,
		    struct stat *statbuf, MuIndexCallbackData *data)
{
	MuError result;
	gboolean updated;

	/* protect against too big messages */
	if (G_UNLIKELY(statbuf->st_size > data->_max_filesize)) {
		g_warning ("ignoring because bigger than %u bytes: %s",
			   data->_max_filesize, fullpath);
		return MU_OK; /* not an error */
	}

	result = run_msg_callback_maybe (data);
	if (result != MU_OK)
		return result;

	/* see if we need to update/insert anything...
	 * use the ctime, so any status change will be visible (perms,
	 * filename etc.)*/
	result = insert_or_update_maybe (fullpath, mdir, statbuf->st_ctime,
					 data, &updated);

	if (result == MU_OK && data && data->_stats) { 	/* update statistics */
		++data->_stats->_processed;
		updated ? ++data->_stats->_updated : ++data->_stats->_uptodate;
	}

	return result;
}


static MuError
on_run_maildir_dir (const char* fullpath, gboolean enter,
		    MuIndexCallbackData *data)
{
	GError *err;
	err = NULL;

	/* xapian stores a per-dir timestamp; we use this timestamp
	 *  to determine whether a message is up-to-data
	 */
	if (enter) {
		data->_dirstamp =
			mu_store_get_timestamp (data->_store, fullpath, &err);
		g_debug ("entering %s (ts==%u)",
			 fullpath, (unsigned)data->_dirstamp);
	} else {
		time_t now;
		now = time (NULL);

		mu_store_set_timestamp (data->_store, fullpath,
					now, &err);
		g_debug ("leaving %s (ts=%u)",
			 fullpath, (unsigned)data->_dirstamp);
	}

	if (data->_idx_dir_cb)
		return data->_idx_dir_cb (fullpath, enter,
					  data->_user_data);

	if (err) {
		MU_WRITE_LOG ("%s: %s", __func__, err->message);
		g_clear_error(&err);
	}

	return MU_OK;
}

static gboolean
check_path (const char *path)
{
	g_return_val_if_fail (path, FALSE);

	if (!g_path_is_absolute (path)) {
		g_warning ("%s: not an absolute path: %s",
			   __func__, path);
		return FALSE;
	}

	if (access (path, R_OK) != 0) {
		g_warning ("%s: cannot open '%s': %s",
			   __func__, path, strerror (errno));
		return FALSE;
	}

	return TRUE;
}

static void
init_cb_data (MuIndexCallbackData *cb_data, MuStore  *xapian,
	      gboolean reindex, guint max_filesize, MuIndexStats *stats,
	      MuIndexMsgCallback msg_cb, MuIndexDirCallback dir_cb,
	      void *user_data)
{
	cb_data->_idx_msg_cb    = msg_cb;
	cb_data->_idx_dir_cb    = dir_cb;

	cb_data->_user_data     = user_data;
	cb_data->_store         = xapian;

	cb_data->_reindex       = reindex;
	cb_data->_dirstamp      = 0;
	cb_data->_max_filesize  = max_filesize;

	cb_data->_stats         = stats;
	if (cb_data->_stats)
		memset (cb_data->_stats, 0, sizeof(MuIndexStats));
}


void
mu_index_set_max_msg_size (MuIndex *index, guint max_size)
{
	g_return_if_fail (index);

	if (max_size == 0)
		index->_max_filesize = MU_INDEX_MAX_FILE_SIZE;
	else
		index->_max_filesize = max_size;
}

void
mu_index_set_xbatch_size (MuIndex *index, guint xbatchsize)
{
	g_return_if_fail (index);
	mu_store_set_batch_size (index->_store, xbatchsize);
}



MuError
mu_index_run (MuIndex *index, const char *path,
	      gboolean reindex, MuIndexStats *stats,
	      MuIndexMsgCallback msg_cb, MuIndexDirCallback dir_cb,
	      void *user_data)
{
	MuIndexCallbackData	cb_data;
	MuError			rv;

	g_return_val_if_fail (index && index->_store, MU_ERROR);
	g_return_val_if_fail (msg_cb, MU_ERROR);

	if (!check_path (path))
		return MU_ERROR;

	if (!reindex && index->_needs_reindex) {
		g_warning ("database not up-to-date; needs full reindex");
		return MU_ERROR;
	}

	init_cb_data (&cb_data, index->_store, reindex,
		      index->_max_filesize, stats,
		      msg_cb, dir_cb, user_data);

	rv = mu_maildir_walk (path,
			      (MuMaildirWalkMsgCallback)on_run_maildir_msg,
			      (MuMaildirWalkDirCallback)on_run_maildir_dir,
			      reindex, /* re-index, ie. do a full update */
			      &cb_data);

	mu_store_flush (index->_store);

	return rv;
}

static MuError
on_stats_maildir_file (const char *fullpath, const char *mdir,
		       struct stat *statbuf,
		       MuIndexCallbackData *cb_data)
{
	MuError result;

	if (cb_data && cb_data->_idx_msg_cb)
		result = cb_data->_idx_msg_cb (cb_data->_stats,
					       cb_data->_user_data);
	else
		result = MU_OK;

	if (result == MU_OK) {
		if (cb_data->_stats)
			++cb_data->_stats->_processed;
		return MU_OK;
	}

	return result; /* MU_STOP or MU_OK */
}


MuError
mu_index_stats (MuIndex *index, const char *path,
		MuIndexStats *stats, MuIndexMsgCallback cb_msg,
		MuIndexDirCallback cb_dir, void *user_data)
{
	MuIndexCallbackData cb_data;

	g_return_val_if_fail (index, MU_ERROR);
	g_return_val_if_fail (cb_msg, MU_ERROR);

	if (!check_path (path))
		return MU_ERROR;

	if (stats)
		memset (stats, 0, sizeof(MuIndexStats));

	cb_data._idx_msg_cb = cb_msg;
	cb_data._idx_dir_cb = cb_dir;

	cb_data._stats     = stats;
	cb_data._user_data = user_data;

	cb_data._dirstamp  = 0;

	return mu_maildir_walk (path,
				(MuMaildirWalkMsgCallback)on_stats_maildir_file,
				NULL, FALSE, &cb_data);
}

struct _CleanupData {
	MuStore *_store;
	MuIndexStats  *_stats;
	MuIndexCleanupDeleteCallback _cb;
	void *_user_data;

};
typedef struct _CleanupData CleanupData;


static MuError
foreach_doc_cb (const char* path, CleanupData *cudata)
{
	if (access (path, R_OK) != 0) {
		if (errno != EACCES)
			g_debug ("cannot access %s: %s", path, strerror(errno));
		if (!mu_store_remove_path (cudata->_store, path))
			return MU_ERROR; /* something went wrong... bail out */
		if (cudata->_stats)
			++cudata->_stats->_cleaned_up;
	}

	if (cudata->_stats)
		++cudata->_stats->_processed;

	if (!cudata->_cb)
		return MU_OK;

	return cudata->_cb (cudata->_stats, cudata->_user_data);
}


MuError
mu_index_cleanup (MuIndex *index, MuIndexStats *stats,
		  MuIndexCleanupDeleteCallback cb,
		  void *user_data, GError **err)
{
	MuError rv;
	CleanupData cudata;

	g_return_val_if_fail (index, MU_ERROR);

	cudata._store	  = index->_store;
	cudata._stats	  = stats;
	cudata._cb	  = cb;
	cudata._user_data = user_data;

	rv = mu_store_foreach (index->_store,
			       (MuStoreForeachFunc)foreach_doc_cb,
			       &cudata, err);

	mu_store_flush (index->_store);

	return rv;
}

gboolean
mu_index_stats_clear (MuIndexStats *stats)
{
	if (!stats)
		return FALSE;

	memset (stats, 0, sizeof(MuIndexStats));
	return TRUE;
}
