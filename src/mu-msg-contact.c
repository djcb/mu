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

#include <glib.h>
#include "mu-msg-contact.h"


MuMsgContact*
mu_msg_contact_new (const char *name, const char* address,
		    MuMsgContactType ctype)
{
	MuMsgContact *ct;
	
	g_return_val_if_fail (ctype == MU_MSG_CONTACT_TYPE_TO ||
			      ctype == MU_MSG_CONTACT_TYPE_FROM ||
			      ctype == MU_MSG_CONTACT_TYPE_CC ||
			      ctype == MU_MSG_CONTACT_TYPE_BCC,
			      NULL);
	
	ct		= g_slice_new (MuMsgContact);
	ct->name	= name ? g_strdup(name) : NULL;
	ct->address	= address ? g_strdup(address) : NULL;
	ct->type	= ctype;

	return ct;
}


void
mu_msg_contact_destroy (MuMsgContact *ct)
{
	if (ct) {
		g_free (ct->name);
		g_free (ct->address);
	}

	g_slice_free (MuMsgContact, ct);
}

		

void
mu_msg_contact_list_foreach (GSList *lst,
			     MuMsgContactForeachFunc func,
			     gpointer user_data)
{
	while (lst && func((MuMsgContact*)lst->data, user_data))
		lst = g_slist_next(lst);
}

	

void
mu_msg_contact_list_free (GSList *lst)
{
	g_slist_foreach (lst, (GFunc)mu_msg_contact_destroy, NULL);
	g_slist_free (lst);
}
