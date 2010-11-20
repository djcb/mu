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
#include "mu-msg.h"

G_BEGIN_DECLS

enum _MuMsgContactType {  /* Reply-To:? */
	MU_MSG_CONTACT_TYPE_TO = 0,
	MU_MSG_CONTACT_TYPE_FROM,
	MU_MSG_CONTACT_TYPE_CC,
	MU_MSG_CONTACT_TYPE_BCC,
	
	MU_MSG_CONTACT_TYPE_NUM
};
typedef guint MuMsgContactType;

#define mu_msg_contact_type_is_valid(MCT)\
	((MCT) < MU_MSG_CONTACT_TYPE_NUM)

struct _MuMsgContact {
	const char		*name;	/* Foo Bar */
	const char		*address;	/* foo@bar.cuux */
	MuMsgContactType	 type;	/*MU_MSG_CONTACT_TYPE_{ TO,
					 * CC, BCC, FROM}*/  
};
typedef struct _MuMsgContact	 MuMsgContact;

/**
 * create a new MuMsgContact object; note, in many case, this is not
 * needed, any a stack-allocated struct can be uses.
 * 
 * @param name the name of the contact
 * @param address the e-mail address of the contact
 * @param type the type of contact: cc, bcc, from, to
 * 
 * @return a newly allocated MuMsgConcact or NULL in case of
 * error. use mu_msg_contact_destroy to destroy it when it's no longer
 * needed.
 */
MuMsgContact *mu_msg_contact_new (const char *name, const char *address,
				  MuMsgContactType type);

/**
 * destroy a MuMsgConcact object
 * 
 * @param contact a contact object, or NULL
 */
void          mu_msg_contact_destroy (MuMsgContact *contact);

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
 * call a function for each of the contacts in a message 
 *
 * @param msg a valid MuMsgGMime* instance
 * @param func a callback function to call for each contact; when
 * the callback does not return TRUE, it won't be called again
 * @param user_data a user-provide pointer that will be passed to the callback
 * 
 */
void mu_msg_contact_foreach (MuMsg *msg, MuMsgContactForeachFunc func,
			     gpointer user_data);

G_END_DECLS

#endif /*__MU_MSG_CONTACT_H__*/
