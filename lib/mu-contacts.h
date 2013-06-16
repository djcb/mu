/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

/*
** Copyright (C) 2012-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
MuContacts* mu_contacts_new (const gchar *ccachefile)
          G_GNUC_MALLOC G_GNUC_WARN_UNUSED_RESULT;


/**
 * add a contacts; if there's a contact with this e-mail address
 * already, it will not updated unless the timestamp of this one is
 * higher and has a non-empty name
 *
 * @param contacts a contacts object
 * @param email e-mail address of the contact (not NULL)
 * @param name name of the contact (or NULL)
 * @param personal whether the contact is 'personal' (ie., my address
 *        appears in one of the address fields)
 * @param tstamp timestamp for this address
 *
 * @return TRUE if succeeded, FALSE otherwise
 */
gboolean mu_contacts_add (MuContacts *self, const char *email,
			  const char* name, gboolean personal, time_t tstamp);

/**
 * destroy the Contacts object
 *
 * @param contacts a contacts object
 */
void mu_contacts_destroy (MuContacts *self);


/**
 * clear all contacts from the cache
 *
 * @param self a MuContacts instance
 */
void mu_contacts_clear (MuContacts *self);


/**
 * get the path for the contacts cache file
 *
 * @param contacts a contacts object
 *
 * @return the path as a constant string (don't free), or NULL in case
 * of error
 */
const gchar* mu_contacts_get_path (MuContacts *self);


/**
 * return the number of contacts
 *
 * @param self a contacts object
 *
 * @return the number of contacts
 */
size_t mu_contacts_count (MuContacts *self);


/**
 * call called for mu_contacts_foreach; returns the e-mail address,
 * name (which may be NULL) , whether the message is 'personal', the
 * timestamp for the address (when it was last seen), and the
 * frequency (in how many message did this contact participate)
 *
 */
typedef void (*MuContactsForeachFunc) (const char *email, const char *name,
				       gboolean personal,
				       time_t tstamp, unsigned freq,
				       gpointer user_data);

/**
 * call a function for either each contact, or each contact satisfying
 * a regular expression,
 *
 * @param contacts contacts object
 * @param func callback function to be called for each
 * @param user_data user data to pass to the callback
 * @param pattern a regular expression which matches either the e-mail
 * or name, to filter out contacts, or NULL to not do any filtering.
 * @param num receives the number of contacts found, or NULL
 *
 * @return TRUE if the function succeeded, or FALSE if the provide
 * regular expression was invalid (and not NULL)
 */
gboolean mu_contacts_foreach (MuContacts *self, MuContactsForeachFunc func,
			      gpointer user_data, const char* pattern, size_t *num);

G_END_DECLS

#endif /*__MU_CONTACTS_H__*/
