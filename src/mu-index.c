/* 
** Copyright (C) 2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "config.h"

#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <glib.h>
#include <glib/gstdio.h>
#include <errno.h>

#include "mu-maildir.h"
#include "mu-index.h"
#include "mu-store-xapian.h"
#include "mu-util.h"

struct _MuIndex {
	MuStoreXapian  *_xapian;
};

MuIndex* 
mu_index_new (const char *xpath)
{
	MuIndex *index;

	g_return_val_if_fail (xpath, NULL);
	
	index = g_new0 (MuIndex, 1);				
	index->_xapian = mu_store_xapian_new (xpath);
	
	if (!index->_xapian) {
		g_warning ("%s: failed to open xapian store (%s)",
			   __FUNCTION__, xpath); 
		g_free (index);
		return NULL;
	}
	
	return index;
}


void 
mu_index_destroy (MuIndex *index)
{
	if (!index)
		return;
	
	mu_store_xapian_destroy (index->_xapian);
	g_free (index);
}

struct _MuIndexCallbackData {
	MuIndexMsgCallback    _idx_msg_cb;
	MuIndexDirCallback    _idx_dir_cb;
	MuStoreXapian*        _xapian;
	void*                 _user_data;
	MuIndexStats*         _stats;
	gboolean	      _reindex;
	time_t		      _dirstamp;
};
typedef struct _MuIndexCallbackData MuIndexCallbackData;


static MuResult
_insert_or_update_maybe (const char* fullpath, time_t filestamp,
			 MuIndexCallbackData *data, gboolean *updated)
{ 
	MuMsgGMime *msg;
	
	*updated = FALSE;

	/* checks to determine if we need to (re)index this message */
	do {
		/* unconditionally reindex */
		if (data->_reindex)
			break;

		/* it's not in the database yet */
		if (!mu_store_contains_message (data->_xapian, fullpath)) {
			g_debug ("not yet in db: %s", fullpath);
			break;
		}

		/* it's there, but it's not up to date */
		if ((size_t)filestamp >= (size_t)data->_dirstamp)
			break;

		return MU_OK; /* nope: no need to insert/update! */

	} while (0);

		
	msg = mu_msg_gmime_new (fullpath);
	if (!msg) {
		g_warning ("%s: failed to create mu_msg for %s",
			   __FUNCTION__, fullpath);
		return MU_ERROR;
	}
	
	/* we got a valid id; scan the message contents as well */
	if (mu_store_xapian_store (data->_xapian, msg) != MU_OK) {
		g_warning ("%s: storing content %s failed", __FUNCTION__, 
			   fullpath);
		/* ignore...*/
	} 
	
	mu_msg_gmime_destroy (msg);
	*updated = TRUE;

	return MU_OK;	
}

static MuResult
_run_msg_callback_maybe (MuIndexCallbackData *data)
{
	if (data && data->_idx_msg_cb) {
		
		MuResult result;
		
		result = data->_idx_msg_cb (data->_stats, data->_user_data);
		if (result != MU_OK && result != MU_STOP)
 			g_warning ("%s: callback said %d", __FUNCTION__, result);
	}

	return MU_OK;
}


static MuResult
on_run_maildir_msg (const char* fullpath, time_t filestamp, 
		    MuIndexCallbackData *data)
{
	MuResult result;
	gboolean updated;
	
	result = _run_msg_callback_maybe (data);
	if (result != MU_OK)
		return result;
			
	/* see if we need to update/insert anything...*/
	result = _insert_or_update_maybe (fullpath, filestamp, data,
					  &updated);
	
	/* update statistics */
	if (data && data->_stats) {
		++data->_stats->_processed;
		if (data && data->_stats)  {
			if (updated) 
				++data->_stats->_updated;
			else
				++data->_stats->_uptodate;
		}
	}

	return result;
}


static MuResult
on_run_maildir_dir (const char* fullpath, gboolean enter, 
		    MuIndexCallbackData *data)
{
	/* xapian stores a per-dir timestamp; we use this timestamp
	 *  to determine whether a message is up-to-data
	 */
	if (enter) {
		data->_dirstamp =
			mu_store_xapian_get_timestamp (data->_xapian,
						       fullpath);
		g_debug ("entering %s (ts==%u)",
			 fullpath, (unsigned)data->_dirstamp);
	} else {
		time_t now = time (NULL);
		mu_store_xapian_set_timestamp (data->_xapian, fullpath,
					       now);
		g_debug ("leaving %s (ts=%u)",
			 fullpath, (unsigned)data->_dirstamp);
	}
	
	if (data->_idx_dir_cb)
		return data->_idx_dir_cb (fullpath, enter, 
					  data->_user_data);

	return MU_OK;
}

static gboolean
_check_path (const char* path)
{
	g_return_val_if_fail (path, FALSE);
	
	if (access (path, R_OK) != 0) {
		g_warning ("%s: cannot open '%s': %s", 
			   __FUNCTION__, path, strerror (errno));
		return FALSE;
	}
	
	return TRUE;
}


MuResult
mu_index_run (MuIndex *index, const char* path,
	      gboolean reindex, MuIndexStats *stats,
	      MuIndexMsgCallback msg_cb, MuIndexDirCallback dir_cb, 
	      void *user_data)
{
	MuIndexCallbackData cb_data;
	MuResult rv;
	
	g_return_val_if_fail (index && index->_xapian, MU_ERROR);
	
	if (!_check_path (path))
		return MU_ERROR;

	if (stats)
		memset (stats, 0, sizeof(MuIndexStats));
	
	cb_data._idx_msg_cb    = msg_cb;
	cb_data._idx_dir_cb    = dir_cb;
	
	cb_data._user_data = user_data;
	cb_data._xapian    = index->_xapian;
	cb_data._stats     = stats;
	cb_data._reindex   = reindex;

	cb_data._dirstamp  = 0;

	rv = mu_maildir_walk (path,
			      (MuMaildirWalkMsgCallback)on_run_maildir_msg,
			      (MuMaildirWalkDirCallback)on_run_maildir_dir,
			      &cb_data);
	mu_store_xapian_flush (index->_xapian);

	return rv;
}

static MuResult
on_stats_maildir_file (const char *fullpath, time_t timestamp, 
		       MuIndexCallbackData *cb_data)
{
	MuResult result;
	
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


MuResult
mu_index_stats (MuIndex *index, const char* path,
		MuIndexStats *stats, MuIndexMsgCallback cb_msg,
		MuIndexDirCallback cb_dir, void *user_data)
{
	MuIndexCallbackData cb_data;
	
	g_return_val_if_fail (index, MU_ERROR);

	if (!_check_path (path))
		return MU_ERROR;

	if (stats)
		memset (stats, 0, sizeof(MuIndexStats));
	
	cb_data._idx_msg_cb        = cb_msg;
	cb_data._idx_dir_cb        = cb_dir;

	cb_data._stats     = stats;
	cb_data._user_data = user_data;

	cb_data._dirstamp  = 0;
	
	return mu_maildir_walk (path,
				(MuMaildirWalkMsgCallback)on_stats_maildir_file,
				NULL,&cb_data);
}


struct _CleanupData {
	MuStoreXapian *_xapian;
	MuIndexStats  *_stats;
	MuIndexCleanupDeleteCallback _cb;
	void *_user_data;
	
};
typedef struct _CleanupData CleanupData;


static MuResult
_foreach_doc_cb (const char* path, CleanupData *cudata)
{
	MuResult rv;
	
	if (access (path, R_OK) != 0) {
		
		g_debug ("not readable: %s; removing", path);
		rv = mu_store_xapian_remove (cudata->_xapian, path);
		if (rv != MU_OK)
			return rv; /* something went wrong... bail out */	
		
		if (cudata->_stats)
			++cudata->_stats->_cleaned_up;
	}

	if (cudata->_stats)
		++cudata->_stats->_processed;
	
	if (!cudata->_cb)
		return MU_OK;

	return cudata->_cb (cudata->_stats, cudata->_user_data);
}


MuResult
mu_index_cleanup (MuIndex *index, MuIndexStats *stats,
		  MuIndexCleanupDeleteCallback cb,
		  void *user_data)
{
	MuResult rv;
	CleanupData cudata;
	
	g_return_val_if_fail (index, MU_ERROR);

	cudata._xapian	  = index->_xapian;
	cudata._stats	  = stats;
	cudata._cb	  = cb;
	cudata._user_data = user_data;
	
	rv = mu_store_xapian_foreach (index->_xapian,
				      (MuStoreXapianForeachFunc)_foreach_doc_cb,
				      &cudata);
	mu_store_xapian_flush (index->_xapian);

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
