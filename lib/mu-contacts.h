/*
** Copyright (C) 2012-2016 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
 * return the number of contacts
 *
 * @param self a contacts object
 *
 * @return the number of contacts
 */
size_t mu_contacts_count (MuContacts *self);

/**
 * Function called for mu_contacts_foreach; returns the e-mail address, name
 * (which may be NULL) , whether the message is 'personal', the timestamp for
 * the address (when it was last seen), and the frequency (in how many message
 * did this contact participate) and the tstamp (last modification)
 *
 */
typedef void (*MuContactsForeachFunc) (const char *full_address,
                                       const char *email, const char *name,
                                       gboolean personal,
                                       time_t last_seen, unsigned freq,
                                       gint64 tstamp, gpointer user_data);

/**
 * call a function for either each contact, or each contact satisfying
 * a regular expression,
 *
 * @param self contacts object
 * @param func callback function to be called for each
 * @param user_data user data to pass to the callback
 *
 * @return TRUE if the function succeeded, or FALSE if the provide regular
 * expression was invalid (and not NULL)
 */
gboolean mu_contacts_foreach (MuContacts *self,
                              MuContactsForeachFunc func,
                              gpointer user_data);

G_END_DECLS

#endif /*__MU_CONTACTS_H__*/
