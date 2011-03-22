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

#include "mu-msg-data.h"

MuMsgData*
mu_msg_data_new (void)
{
	/* TODO: check if this is much slower than g_slice_new */
	return g_slice_new0 (MuMsgData);
}

MuMsgData*
mu_msg_data_copy (MuMsgData *mdata)
{
	MuMsgData *copy;
	
	if (!mdata)
		return NULL;
	
	copy = mu_msg_data_new ();

	/* shallow copy */
	memcpy (copy, mdata, sizeof(MuMsgData));

	/* now, deep copy ptr data */
	copy->cc      = g_strdup (mdata->cc);
	copy->from    = g_strdup (mdata->from);
	copy->maildir = g_strdup (mdata->maildir);
	copy->msgid   = g_strdup (mdata->msgid);
	copy->path    = g_strdup (mdata->path);
	copy->subject = g_strdup (mdata->subject);
	copy->to      = g_strdup (mdata->to);

	return copy;
}

void
mu_msg_data_destroy (MuMsgData *mdata)
{
	if (!mdata)
		return;

	g_free (mdata->cc);
	g_free (mdata->from);
	g_free (mdata->maildir);
	g_free (mdata->msgid);
	g_free (mdata->path);
	g_free (mdata->subject);
	g_free (mdata->to);

	g_slice_free (MuMsgData, mdata);
}
