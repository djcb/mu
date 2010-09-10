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
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#include <locale.h>

#include "test-mu-common.h"
#include "src/mu-msg.h"
#include "src/mu-msg-contact.h"

static gboolean
check_contact_01 (MuMsgContact *contact, int *idx)
{
	switch (*idx) {
	case 0:
		g_assert_cmpstr (mu_msg_contact_name (contact),
				 ==, "Mickey Mouse");
		g_assert_cmpstr (mu_msg_contact_address (contact),
				 ==, "anon@example.com");
		break;
	case 1:
		g_assert_cmpstr (mu_msg_contact_name (contact),
				 ==, "Donald Duck");
		g_assert_cmpstr (mu_msg_contact_address (contact),
				 ==, "gcc-help@gcc.gnu.org");
		break;
	default:
		g_assert_not_reached ();
	}
	++(*idx);

	return TRUE;
}




static void
test_mu_msg_01 (void)
{
	MuMsg *msg;
	gint i;

	msg = mu_msg_new (MU_TESTMAILDIR
			  "cur/1220863042.12663_1.mindcrime!2,S", NULL);

	g_assert_cmpstr (mu_msg_get_to(msg),
			 ==, "Donald Duck <gcc-help@gcc.gnu.org>");
	g_assert_cmpstr (mu_msg_get_subject(msg),
			 ==, "gcc include search order");
	g_assert_cmpstr (mu_msg_get_from(msg),
			 ==, "Mickey Mouse <anon@example.com>");
	g_assert_cmpstr (mu_msg_get_msgid(msg),
			 ==, "3BE9E6535E3029448670913581E7A1A20D852173@"
			 "emss35m06.us.lmco.com");
	g_assert_cmpstr (mu_msg_get_header(msg, "Mailing-List"),
			 ==, "contact gcc-help-help@gcc.gnu.org; run by ezmlm");
	g_assert_cmpuint (mu_msg_get_prio(msg), /* 'klub' */
			  ==, MU_MSG_PRIO_NORMAL);
	g_assert_cmpuint (mu_msg_get_date(msg), 
			  ==, 1217530645);

	i = 0;
	mu_msg_contact_foreach (msg, (MuMsgContactForeachFunc)check_contact_01,
				&i);
	g_assert_cmpint (i,==,2);

	mu_msg_destroy (msg);
}



static gboolean
check_contact_02 (MuMsgContact *contact, int *idx)
{
	switch (*idx) {
	case 0:
		g_assert_cmpstr (mu_msg_contact_name (contact),
				 ==, NULL);
		g_assert_cmpstr (mu_msg_contact_address (contact),
				 ==, "anon@example.com");
		break;
	case 1:
		g_assert_cmpstr (mu_msg_contact_name (contact),
				 ==, NULL);
		g_assert_cmpstr (mu_msg_contact_address (contact),
				 ==, "help-gnu-emacs@gnu.org");
		break;
	default:
		g_assert_not_reached ();
	}
	++(*idx);
	
	return TRUE;
}



static void
test_mu_msg_02 (void)
{
	MuMsg *msg;
	int i;

	msg = mu_msg_new (MU_TESTMAILDIR
			  "cur/1220863087.12663_19.mindcrime!2,S", NULL);

	g_assert_cmpstr (mu_msg_get_to(msg),
			 ==, "help-gnu-emacs@gnu.org");
	g_assert_cmpstr (mu_msg_get_subject(msg),
			 ==, "Re: Learning LISP; Scheme vs elisp.");
	g_assert_cmpstr (mu_msg_get_from(msg),
			 ==, "anon@example.com");
	g_assert_cmpstr (mu_msg_get_msgid(msg),
			 ==, "r6bpm5-6n6.ln1@news.ducksburg.com");
	g_assert_cmpstr (mu_msg_get_header(msg, "Errors-To"),
			 ==, "help-gnu-emacs-bounces+xxxx.klub=gmail.com@gnu.org");
	g_assert_cmpuint (mu_msg_get_prio(msg), /* 'low' */
			  ==, MU_MSG_PRIO_LOW);
	g_assert_cmpuint (mu_msg_get_date(msg), 
			  ==, 1218051515);
	
	i = 0;
	mu_msg_contact_foreach (msg, (MuMsgContactForeachFunc)check_contact_02,
				&i);
	g_assert_cmpint (i,==,2);
	
	mu_msg_destroy (msg);
}

static void
test_mu_msg_03 (void)
{
	MuMsg *msg;
	int i;

	msg = mu_msg_new (MU_TESTMAILDIR
			  "cur/1283599333.1840_11.cthulhu!2,", NULL);

	g_assert_cmpstr (mu_msg_get_to(msg),
			 ==, "Bilbo Baggins <bilbo@anotherexample.com>");
	g_assert_cmpstr (mu_msg_get_subject(msg),
			 ==, "Greetings from Lothlórien");
	g_assert_cmpstr (mu_msg_get_from(msg),
			 ==, "Frodo Baggins <frodo@example.com>");
	g_assert_cmpuint (mu_msg_get_prio(msg), /* 'low' */
			  ==, MU_MSG_PRIO_NORMAL);
	g_assert_cmpuint (mu_msg_get_date(msg),
			  ==, 0);
	g_assert_cmpstr (mu_msg_get_body_text(msg),
			 ==,
			 "\nLet's write some fünkÿ text\nusing umlauts.\n\nFoo.\n");
	
	mu_msg_destroy (msg);
}






static gboolean
ignore_error (const char* log_domain, GLogLevelFlags log_level, const gchar* msg,
	      gpointer user_data)
{
	return FALSE; /* don't abort */
}

static void
shutup (void) {}



int
main (int argc, char *argv[])
{
	g_test_init (&argc, &argv, NULL);

	/* mu_msg_str_date */
	g_test_add_func ("/mu-msg/mu-msg-01",
			 test_mu_msg_01);
	g_test_add_func ("/mu-msg/mu-msg-02",
			 test_mu_msg_02);
	g_test_add_func ("/mu-msg/mu-msg-03",
			 test_mu_msg_03);
	
	/* g_log_set_handler (NULL, */
	/* 		   G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL| G_LOG_FLAG_RECURSION, */
	/* 		   (GLogFunc)shutup, NULL); */
	
	return g_test_run ();
}
