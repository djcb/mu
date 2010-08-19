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

#ifndef __MU_MSG_CONTACT_H__
#define __MU_MSG_CONTACT_H__

#include <glib.h>

enum _MuMsgContactType {  /* Reply-To:? */
	MU_MSG_CONTACT_TYPE_TO,
	MU_MSG_CONTACT_TYPE_FROM,
	MU_MSG_CONTACT_TYPE_CC,
	MU_MSG_CONTACT_TYPE_BCC
};
typedef enum _MuMsgContactType MuMsgContactType;


struct _MuMsgContact {
	char             *name;    /* Foo Bar */
	char             *address; /* foo@bar.cuux */
	MuMsgContactType  type;    /*MU_MSG_CONTACT_TYPE_{TO,CC,BCC,FROM}*/  
};
typedef struct _MuMsgContact MuMsgContact;


/**
 * create a new MuMsgContact instance
 * 
 * @param name name of the contact (or NULL)
 * @param addr address of the contact (or NULL)
 * @param ctype a valid contact type
 * 
 * @return a new MuMsgContact instance; use mu_msg_contact_destroy
 * when finished.
 */
MuMsgContact *mu_msg_contact_new (const char *name, const char *addr,
				  MuMsgContactType ctype);

/**
 * destroy a MuMsgContact
 * 
 * @param ct a MuMsgContact, or NULL
 */
void mu_msg_contact_destroy (MuMsgContact *ct);


/**
 * macro to get the name of a contact
 * 
 * @param ct a MuMsgContact
 * 
 * @return the name
 */
#define mu_msg_contact_name(ct)    ((ct)->name)

/**
 * macro to get the address of a contact
 * 
 * @param ct a MuMsgContact
 * 
 * @return the address
 */
#define mu_msg_contact_address(ct) ((ct)->address)

/**
 * macro to get the contact type
 * 
 * @param ct a MuMsgContact
 * 
 * @return the contact type
 */
#define mu_msg_contact_type(ct)    ((ct)->type)


/**
 * callback function
 * 
 * @param contact
 * @param user_data a user provided data pointer
 * 
 * @return TRUE if we should continue the foreach, FALSE otherwise
 */
typedef gboolean  (*MuMsgContactForeachFunc) (MuMsgContact* contact,
					      gpointer user_data);

/**
 * call a function for each MuMsgContact in the list
 * 
 * @param lst a list of MuMsgContact objects
 * @param func a callback function
 * @param user_data user pointer, passed to the callback
 */
void mu_msg_contact_list_foreach (GSList *lst,
				  MuMsgContactForeachFunc func,
				  gpointer user_data);

/**
 * free a list of MuMsgContact objects
 * 
 * @param lst list of MuMsgContact object
 */
void mu_msg_contact_list_free (GSList *lst);



#endif /*__MU_MSG_CONTACT_H__*/
