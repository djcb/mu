/*
** Copyright (C) 2008-2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include "mu-msg-part-info.h"

MuMsgPartInfo*
mu_msg_part_info_new (void)
{
	return g_slice_new0 (MuMsgPartInfo);
}


void
mu_msg_part_info_destroy (MuMsgPartInfo *pi)
{
	if (!pi)
		return;

	if (pi->own_members) {
		g_free (pi->content_id);
		g_free (pi->type);
		g_free (pi->subtype);
		/* g_free (pi->content_type); */
		g_free (pi->file_name);
		g_free (pi->disposition);
	}

	g_slice_free (MuMsgPartInfo, pi);
}

		

void
mu_msg_part_infos_foreach (GSList *lst,
			   MuMsgPartInfoForeachFunc func,
			   gpointer user_data)
{
	for (; lst ; lst = g_slist_next (lst))
		func((MuMsgPartInfo*)lst->data, user_data);
}


void
mu_msg_part_infos_free (GSList *lst)
{	
	g_slist_foreach (lst, (GFunc)mu_msg_part_info_destroy, NULL);
	g_slist_free (lst);
}
