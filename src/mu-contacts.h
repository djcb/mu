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

#ifndef __MU_CONTACTS_H__
#define __MU_CONTACTS_H__

#include <glib.h>
#include <time.h>

G_BEGIN_DECLS

struct _MuContacts;
typedef struct _MuContacts MuContacts;


/** 
 * create a new MuContacts object; use mu_contacts_destroy when you no longer need it
 * 
 * @param ccachefile full path to the file with cached list of contacts
 * 
 * @return a new MuContacts* if succeeded, NULL otherwise
 */
MuContacts* mu_contacts_new (const gchar* ccachefile)
          G_GNUC_MALLOC G_GNUC_WARN_UNUSED_RESULT;


/** 
 * add a contacts; if there's a contact with this e-mail address
 * already, it will not updated unless the timestamp of this one is
 * higher and has a non-empty name
 * 
 * @param contacts a contacts object
 * @param name name of the contact (or NULL)
 * @param email e-mail address of the contact
 * @param tstamp timestamp for this address
 * 
 * @return TRUE if succeeded, FALSE otherwise
 */
gboolean mu_contacts_add (MuContacts *contacts, const char* name, const char *email,
			  time_t tstamp);

/** 
 * destroy the Contacts object
 * 
 * @param contacts a contacts object
 */
void mu_contacts_destroy (MuContacts *contacts);


typedef void (*MuContactsForeachFunc) (const char *email, const char *mail,
				       gpointer user_data);

/** 
 * call a function for each contact
 * 
 * @param contacts contacts object 
 * @param func callback function to be called for each
 * @param user_data user data to pass to the callback
 */
void mu_contacts_foreach (MuContacts *contacts, MuContactsForeachFunc func,
			  gpointer user_data);

G_END_DECLS

#endif /*__MU_CONTACTS_H__*/
