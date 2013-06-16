/*
** Copyright (C) 2008-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <glib/gstdio.h>

#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include "test-mu-common.h"
#include "mu-contacts.h"

static gchar*
fill_database (void)
{
	gchar *cmdline, *tmpdir;
	GError *err;

	tmpdir = test_mu_common_get_random_tmpdir();
	cmdline = g_strdup_printf ("%s index --muhome=%s --maildir=%s"
				   " --quiet",
				   MU_PROGRAM, tmpdir, MU_TESTMAILDIR);

	err = NULL;
	if (!g_spawn_command_line_sync (cmdline, NULL, NULL,
					NULL, &err)) {
		g_printerr ("Error: %s\n", err ? err->message : "?");
		g_clear_error (&err);
		g_assert (0);
	}

	g_clear_error (&err);
	g_free (cmdline);

	return tmpdir;
}


struct _Contact {
	char *name, *email;
	gboolean personal;
	time_t tstamp;
};
typedef struct _Contact Contact;

static Contact*
contact_new (const char *email, const char *name,
	     gboolean personal, size_t tstamp)
{
	Contact *contact;

	contact			= g_slice_new (Contact);
	contact->name		= name ? g_strdup (name) :NULL;
	contact->email		= email ? g_strdup (email) : NULL;
	contact->personal	= personal;
	contact->tstamp		= tstamp;

	return contact;

}

static void
contact_destroy (Contact *contact)
{
	if (contact) {
		g_free (contact->name);
		g_free (contact->email);
		g_slice_free (Contact, contact);
	}
}


static void
each_contact (const char *email, const char *name, gboolean personal,
	      time_t tstamp, unsigned freq, GSList **lst)
{
	Contact *contact;

	/* g_print ("[n:%s, e:%s]\n", name, email); */

	contact = contact_new (email, name, personal, tstamp);
	*lst = g_slist_prepend (*lst, contact);
}

static gboolean
has_contact (GSList *lst, const char* name_or_email, gboolean use_name)
{
	while (lst) {
		Contact *c;
		c = (Contact*)lst->data;

		/* g_print ("{n:%s,e:%s}\n", c->name, c->email); */

		if (use_name && g_strcmp0(name_or_email, c->name) == 0)
			return TRUE;
		if (g_strcmp0 (name_or_email, c->email) == 0)
			return TRUE;

		lst = g_slist_next (lst);
	}

	return FALSE;
}



static GSList*
accumulate_contacts (MuContacts *contacts, const gchar *pattern)
{
	GSList *lst;

	lst = NULL;
	g_assert (mu_contacts_foreach (contacts,
				       (MuContactsForeachFunc)each_contact,
				       &lst, pattern, NULL));
	return lst;
}


static void
test_mu_contacts_01 (void)
{
	MuContacts *contacts;
	gchar *muhome, *contactsfile;
	GSList *clist;

	muhome = fill_database ();
	g_assert (muhome != NULL);
	contactsfile = g_strdup_printf ("%s%ccache%ccontacts",
					muhome, G_DIR_SEPARATOR, G_DIR_SEPARATOR);
	/* g_print ("[%s]\n", contactsfile); */

	contacts = mu_contacts_new (contactsfile);
	g_assert (contacts);

	clist = accumulate_contacts (contacts, "Mü");
	g_assert_cmpint (g_slist_length (clist), ==, 1);
	g_assert (has_contact (clist, "Mü", TRUE));
	g_assert (has_contact (clist, "testmu@testmu.xx", FALSE));
	g_slist_foreach (clist, (GFunc)contact_destroy, NULL);
	g_slist_free (clist);

	clist = accumulate_contacts (contacts, "testmu\\.xxx?");
	g_assert_cmpint (g_slist_length (clist), ==, 2);
	g_assert (has_contact (clist, "Mü", TRUE));
	g_assert (has_contact (clist, "testmu@testmu.xx", FALSE));
	g_assert (has_contact (clist, "Helmut Kröger", TRUE));
	g_assert (has_contact (clist, "hk@testmu.xxx", FALSE));

	g_slist_foreach (clist, (GFunc)contact_destroy, NULL);
	g_slist_free (clist);

	mu_contacts_destroy (contacts);

	g_free (contactsfile);
	g_free (muhome);
}


int
main (int argc, char *argv[])
{
	int rv;

	g_test_init (&argc, &argv, NULL);
	g_test_add_func ("/mu-contacts/test-mu-contacts-01", test_mu_contacts_01);

	g_log_set_handler (NULL,
			   G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL| G_LOG_FLAG_RECURSION,
			   (GLogFunc)black_hole, NULL);
	rv = g_test_run ();

	return rv;
}
