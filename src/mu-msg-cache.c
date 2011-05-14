/*
** Copyright (C) 2011 Dirk-Jan C. Binnema  <djcb@djcbsoftware.nl>
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

#include <mu-msg-flags.h>
#include <mu-msg-prio.h>

#include "mu-msg-cache.h"


struct _MuMsgCache {

	/* all string properties */
	char *_str[MU_MSG_STRING_FIELD_ID_NUM];
	
	time_t		_timestamp, _date;
	size_t		_size;
	MuMsgFlags	_flags;
	MuMsgPrio	_prio;

	/* <private> */
	unsigned _cached, _allocated;
};

/* _cached and _allocated have a bit for each MuMsgFieldId to remember
 * which ones have been cached, and which ones have been allocated. */

#define is_allocated(C,MFID)    ((C)->_allocated & (1 << (MFID)))	
#define is_cached(C,MFID)       ((C)->_cached    & (1 << (MFID)))	

#define set_allocated(C,MFID)   ((C)->_allocated |= (1 << (MFID)))
#define set_cached(C,MFID)      ((C)->_cached    |= (1 << (MFID)))

#define reset_allocated(C,MFID) ((C)->_allocated &= ~(1 << (MFID)))
#define reset_cached(C,MFID)    ((C)->_cached    &= ~(1 << (MFID)))

MuMsgCache*
mu_msg_cache_new (void)
{
	int i;
	MuMsgCache *self;
	
	self = g_slice_new (MuMsgCache);
		
	for (i = 0; i != MU_MSG_STRING_FIELD_ID_NUM; ++i)
		self->_str[i] = NULL;

	self->_timestamp = (time_t)-1;
	self->_size      = (size_t)-1;
	self->_flags     = MU_MSG_FLAG_NONE;
	self->_prio	 = MU_MSG_PRIO_NONE;
	self->_date	 = (time_t)-1;
	
	self->_cached	 = 0;
	self->_allocated = 0;

	return self;
}


void
mu_msg_cache_destroy (MuMsgCache *self)
{
	int i;

	if (!self)
		return;
	
	g_return_if_fail (self);

	for (i = 0; i != MU_MSG_STRING_FIELD_ID_NUM; ++i)
		if (is_allocated(self, i))
			g_free (self->_str[i]);

	g_slice_free (MuMsgCache, self);
}
	


void
cache_set_str (MuMsgCache *self, MuMsgFieldId mfid, char *str, gboolean alloc)
{
	g_return_if_fail (self);
	g_return_if_fail (mu_msg_field_is_string(mfid));

	/* g_warning ("set_str: '%s'=>'%s'; alloc'd: %s; cached: %s", */
	/* 	   mu_msg_field_name(mfid), str ? str : "", */
	/* 	   is_allocated(mfid) ? "yes" : "no", */
	/* 	   is_cached(mfid) ? "yes" : "no"); */
	
	/* maybe there was an old, allocated value there already? */
	if (is_allocated(self, mfid)) 
		g_free (self->_str[mfid]);

	self->_str[mfid] = str;
	set_cached (self, mfid);

	if (alloc)
		set_allocated (self, mfid);
	else
		reset_allocated (self, mfid);
}

	

void
mu_msg_cache_set_str (MuMsgCache *self, MuMsgFieldId mfid,
		      const char *str)
{
	cache_set_str (self, mfid, (char*)str, FALSE);
}


void
mu_msg_cache_set_str_alloc (MuMsgCache *self, MuMsgFieldId mfid,
			    char *str)
{
	cache_set_str (self, mfid, str, TRUE);
}

const char*
mu_msg_cache_str (MuMsgCache *cache, MuMsgFieldId mfid)
{
	g_return_val_if_fail (mu_msg_field_is_string(mfid), NULL);
	return cache->_str[mfid];	
}


void
mu_msg_cache_set_num (MuMsgCache *self, MuMsgFieldId mfid, guint64 val)
{
	g_return_if_fail(mu_msg_field_is_numeric(mfid));

	switch (mfid) {
	case MU_MSG_FIELD_ID_DATE:
		self->_date = (time_t)val;
		break;
	case MU_MSG_FIELD_ID_TIMESTAMP:
		self->_timestamp = (time_t)val;
		break;
	case MU_MSG_FIELD_ID_PRIO:
		self->_prio = (MuMsgPrio)val;
		break;
	case MU_MSG_FIELD_ID_FLAGS:
		self->_flags = (MuMsgFlags)val;
		break;
	case MU_MSG_FIELD_ID_SIZE:
		self->_size = (size_t)val;
		break;
	default: g_return_if_reached();
	}

	set_cached (self, mfid);	
}


gint64
mu_msg_cache_num (MuMsgCache *self, MuMsgFieldId mfid)
{
	g_return_val_if_fail(mu_msg_field_is_numeric(mfid), -1);

	switch (mfid) {
	case MU_MSG_FIELD_ID_DATE:
		return (gint64)self->_date;
	case MU_MSG_FIELD_ID_TIMESTAMP:
		return (gint64)self->_timestamp;
	case MU_MSG_FIELD_ID_PRIO:
		return (gint64)self->_prio;
	case MU_MSG_FIELD_ID_FLAGS:
		return (gint64)self->_flags;
	case MU_MSG_FIELD_ID_SIZE:
		return (gint64)self->_size;
	default: g_return_val_if_reached(-1);
	}

	set_cached (self, mfid);	
}


gboolean
mu_msg_cache_cached (MuMsgCache *self, MuMsgFieldId mfid)
{
	g_return_val_if_fail (mfid < MU_MSG_FIELD_ID_NUM, FALSE);
	return is_cached(self, mfid) ? TRUE : FALSE;
}


void
mu_msg_cache_allocate_all (MuMsgCache *self)
{
	int mfid;

	g_return_if_fail (self);
	
	for (mfid = 0; mfid != MU_MSG_STRING_FIELD_ID_NUM; ++mfid) {
		if (self->_str[mfid] && !is_allocated(self, mfid)) {
			self->_str[mfid] = g_strdup (self->_str[mfid]);
			set_allocated(self, mfid);
		}
	}
}
