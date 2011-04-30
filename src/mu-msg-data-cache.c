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

#include "mu-msg-data-cache.h"

/* we cannot use GPtrArray, because we require the index to be stable... */
struct _MuMsgDataCache {
	MuMsgData	**_data;
	guint		  _index, _size;
};

MuMsgDataCache *
mu_msg_data_cache_new (guint32 max_size)
{
	MuMsgDataCache *cache;
	MuMsgData **data;

	data = g_try_new0 (MuMsgData*, max_size);
	if (!data) {
		g_warning ("cannot allocate %u bytes", (unsigned)max_size);
		return NULL;
	}

	cache	      = g_new (MuMsgDataCache, 1);
	cache->_data  = (MuMsgData**)data;
	cache->_index = 0;
	cache->_size  = max_size;
	
	return cache;
}


void
mu_msg_data_cache_destroy (MuMsgDataCache *self)
{
	if (!self)
		return;

	mu_msg_data_cache_clear (self);

	g_free (self);
}


void
mu_msg_data_cache_clear (MuMsgDataCache *self)
{
	unsigned i;
	
	g_return_if_fail (self);

	for (i = 0; i != self->_index; ++i) {
		mu_msg_data_destroy (self->_data[i]);
		self->_data[i] = NULL;
	}

	self->_index = 0;
}

void
mu_msg_data_cache_append (MuMsgDataCache *self, MuMsgData *data, gboolean copy)
{
	g_return_if_fail (self);
	g_return_if_fail (self->_index < G_MAXUINT32);
	g_return_if_fail (data);
	
	self->_data[self->_index++] = copy ? mu_msg_data_copy (data) : data;
}

const MuMsgData *
mu_msg_data_cache_get	(MuMsgDataCache *self, guint index)
{
	g_return_val_if_fail (self, NULL);
	g_return_val_if_fail (index < self->_size, NULL);

	return self->_data[index];
}
