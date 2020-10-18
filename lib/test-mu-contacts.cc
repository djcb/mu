/*
** Copyright (C) 2019 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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


#include "config.h"

#include <glib.h>
#include "test-mu-common.h"
#include "mu-contacts.hh"

static void
test_mu_contacts_01()
{
        Mu::Contacts contacts ("");

        g_assert_true (contacts.empty());
        g_assert_cmpuint (contacts.size(), ==, 0);

        contacts.add(std::move(Mu::ContactInfo ("Foo <foo.bar@example.com>",
                                                "foo.bar@example.com", "Foo", false, 12345)));
        g_assert_false (contacts.empty());
        g_assert_cmpuint (contacts.size(), ==, 1);

        contacts.add(std::move(Mu::ContactInfo ("Cuux <cuux.fnorb@example.com>",
                                                "cuux@example.com", "Cuux", false, 54321)));

        g_assert_cmpuint (contacts.size(), ==, 2);

        contacts.add(std::move(Mu::ContactInfo ("foo.bar@example.com",
                                                "foo.bar@example.com", "Foo", false, 77777)));
        g_assert_cmpuint (contacts.size(), ==, 2);

        contacts.add(std::move(Mu::ContactInfo ("Foo.Bar@Example.Com",
                                                "Foo.Bar@Example.Com", "Foo", false, 88888)));
        g_assert_cmpuint (contacts.size(), ==, 2);
        // note: replaces first.

        {
                const auto info = contacts._find("bla@example.com");
                g_assert_false (info);
        }

        {
                const auto info = contacts._find("foo.BAR@example.com");
                g_assert_true (info);

                g_assert_cmpstr(info->email.c_str(), ==, "Foo.Bar@Example.Com");
        }

        contacts.clear();
        g_assert_true (contacts.empty());
        g_assert_cmpuint (contacts.size(), ==, 0);
}

static void
test_mu_contacts_02()
{
        Mu::StringVec personal = {
                "foo@example.com",
                "bar@cuux.org",
                "/bar-.*@fnorb.f./"
        };
        Mu::Contacts contacts{"", personal};

        g_assert_true (contacts.is_personal("foo@example.com"));
        g_assert_true (contacts.is_personal("Bar@CuuX.orG"));
        g_assert_true (contacts.is_personal("bar-123abc@fnorb.fi"));
        g_assert_true (contacts.is_personal("bar-zzz@fnorb.fr"));

        g_assert_false (contacts.is_personal("foo@bar.com"));
        g_assert_false (contacts.is_personal("BÂr@CuuX.orG"));
        g_assert_false (contacts.is_personal("bar@fnorb.fi"));
        g_assert_false (contacts.is_personal("bar-zzz@fnorb.xr"));
}



int
main (int argc, char *argv[])
{
        g_test_init (&argc, &argv, NULL);

        g_test_add_func ("/mu-contacts/01", test_mu_contacts_01);
        g_test_add_func ("/mu-contacts/02", test_mu_contacts_02);

        g_log_set_handler (NULL,
                           (GLogLevelFlags)
                           (G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL| G_LOG_FLAG_RECURSION),
                           (GLogFunc)black_hole, NULL);

        return g_test_run ();
}
