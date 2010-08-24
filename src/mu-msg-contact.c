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
#include <gmime/gmime.h>

#include "mu-msg-priv.h"
#include "mu-msg.h"

#include "mu-msg-contact.h"
#include "mu-util.h"

	

static gboolean
fill_contact (MuMsgContact *contact, InternetAddress *addr,
	      MuMsgContactType ctype)
{
	if (!addr)
		return FALSE;
	
	contact->name = (char*)internet_address_get_name (addr);
	contact->type = ctype;  
	
	/* we only support internet addresses;
	 * if we don't check, g_mime hits an assert
	 */
	contact->address = (char*)internet_address_mailbox_get_addr
		(INTERNET_ADDRESS_MAILBOX(addr));
	
	return TRUE;
}


static void
address_list_foreach (InternetAddressList *addrlist,
		      MuMsgContactType     ctype,
		      MuMsgContactForeachFunc func, 
		      gpointer user_data)
{
	int i;
	
	if (!addrlist)
		return;
	
	for (i = 0; i != internet_address_list_length(addrlist); ++i) {
		
		MuMsgContact contact;
		if (!fill_contact(&contact,
				  internet_address_list_get_address (addrlist, i),
				  ctype)) {
			MU_WRITE_LOG ("ignoring contact");
			continue;
		}
		
		if (!(func)(&contact, user_data))
			break;
	}

	return;
}



static void
get_contacts_from (MuMsg *msg, MuMsgContactForeachFunc func, 
		   gpointer user_data)
{
	InternetAddressList *lst;
	
	/* we go through this whole excercise of trying to get a *list*
	 * of 'From:' address (usually there is only one...), because
	 * internet_address_parse_string has the nice side-effect of
	 * splitting in names and addresses for us */
	lst = internet_address_list_parse_string (
		g_mime_message_get_sender (msg->_mime_msg));
	if (lst) {
		address_list_foreach (lst, MU_MSG_CONTACT_TYPE_FROM,
				      func, user_data);
		g_object_unref (G_OBJECT(lst));
	} 
}


void
mu_msg_contact_foreach (MuMsg *msg, MuMsgContactForeachFunc func, 
			gpointer user_data)
{
	int i;		
	struct { 
		GMimeRecipientType     _gmime_type;
		MuMsgContactType       _type;
	} ctypes[] = {
		{GMIME_RECIPIENT_TYPE_TO,  MU_MSG_CONTACT_TYPE_TO},
		{GMIME_RECIPIENT_TYPE_CC,  MU_MSG_CONTACT_TYPE_CC},
		{GMIME_RECIPIENT_TYPE_BCC, MU_MSG_CONTACT_TYPE_BCC},
	};

	g_return_if_fail (func && msg);

	/* first, get the from address(es) */
	get_contacts_from (msg, func, user_data);

	/* get to, cc, bcc */
	for (i = 0; i != G_N_ELEMENTS(ctypes); ++i) {
		InternetAddressList *addrlist;
		addrlist = g_mime_message_get_recipients (msg->_mime_msg,
							  ctypes[i]._gmime_type);
		address_list_foreach (addrlist, ctypes[i]._type, func, user_data);
	}
}
