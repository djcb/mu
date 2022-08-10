/*
** Copyright (C) 2008-2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#if HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include <glib.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#include <locale.h>

#include "utils/mu-test-utils.hh"
#include "mu-message-fields.hh"

static void
test_mu_msg_field_body(void)
{
	Field::Id field;

	field = Field::Id::BodyText;

	g_assert_cmpstr(mu_msg_field_name(field), ==, "body");
	g_assert_cmpuint(mu_msg_field_shortcut(field), ==, 'b');
	g_assert_cmpuint(mu_msg_field_xapian_prefix(field), ==, 'B');

	g_assert_cmpuint(mu_msg_field_is_numeric(field), ==, FALSE);
}

static void
test_mu_msg_field_subject(void)
{
	Field::Id field;

	field = Field::Id::Subject;

	g_assert_cmpstr(mu_msg_field_name(field), ==, "subject");
	g_assert_cmpuint(mu_msg_field_shortcut(field), ==, 's');
	g_assert_cmpuint(mu_msg_field_xapian_prefix(field), ==, 'S');

	g_assert_cmpuint(mu_msg_field_is_numeric(field), ==, FALSE);
}

static void
test_mu_msg_field_to(void)
{
	Field::Id field;

	field = Field::Id::To;

	g_assert_cmpstr(mu_msg_field_name(field), ==, "to");
	g_assert_cmpuint(mu_msg_field_shortcut(field), ==, 't');
	g_assert_cmpuint(mu_msg_field_xapian_prefix(field), ==, 'T');

	g_assert_cmpuint(mu_msg_field_is_numeric(field), ==, FALSE);
}

static void
test_mu_msg_field_prio(void)
{
	Field::Id field;

	field = Field::Id::Priority;

	g_assert_cmpstr(mu_msg_field_name(field), ==, "prio");
	g_assert_cmpuint(mu_msg_field_shortcut(field), ==, 'p');
	g_assert_cmpuint(mu_msg_field_xapian_prefix(field), ==, 'P');

	g_assert_cmpuint(mu_msg_field_is_numeric(field), ==, TRUE);
}

static void
test_mu_msg_field_flags(void)
{
	Field::Id field;

	field = Field::Id::Flags;

	g_assert_cmpstr(mu_msg_field_name(field), ==, "flag");
	g_assert_cmpuint(mu_msg_field_shortcut(field), ==, 'g');
	g_assert_cmpuint(mu_msg_field_xapian_prefix(field), ==, 'G');

	g_assert_cmpuint(mu_msg_field_is_numeric(field), ==, TRUE);
}

int
main(int argc, char* argv[])
{
	g_test_init(&argc, &argv, NULL);

	/* mu_msg_str_date */
	g_test_add_func("/mu-msg-fields/mu-msg-field-body", test_mu_msg_field_body);
	g_test_add_func("/mu-msg-fields/mu-msg-field-subject", test_mu_msg_field_subject);
	g_test_add_func("/mu-msg-fields/mu-msg-field-to", test_mu_msg_field_to);
	g_test_add_func("/mu-msg-fields/mu-msg-field-prio", test_mu_msg_field_prio);
	g_test_add_func("/mu-msg-fields/mu-msg-field-flags", test_mu_msg_field_flags);

	/* FIXME: add tests for mu_msg_str_flags; but note the
	 * function simply calls mu_msg_field_str */

	g_log_set_handler(
	    NULL,
	    (GLogLevelFlags)(G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL | G_LOG_FLAG_RECURSION),
	    (GLogFunc)black_hole,
	    NULL);

	return g_test_run();
}
