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
#include "src/mu-msg-gmime.h"

static gchar*
get_mailpath (unsigned idx)
{
	const char* mailfile;
	
	switch (idx) {
	case 1:  mailfile = "cur/1220863042.12663_1.mindcrime!2,S"; break;
	case 2:  mailfile = "cur/1220863087.12663_19.mindcrime!2,S"; break;
	default: mailfile = NULL;	
	}

	if (mailfile)
		return g_strdup_printf ("%s/%s", MU_TESTMAILDIR, mailfile);
	else
		return NULL;		
}


static gboolean
each_contact (MuMsgContact *contact, GSList **lst)
{
	char *addr;
	addr = g_strdup_printf ("%s <%s>",
				contact->name ? contact->name : "<none>",
				contact->address ? contact->address : "<none>");
	
	*lst = g_slist_append (*lst, addr);
	return TRUE;
}


static void
test_mu_msg_gmime_01 (void)
{
	char *mfile;
	MuMsgGMime *msg;
	GSList *lst;
	
	mu_msg_gmime_init ();
	mfile = get_mailpath (1);

	msg = mu_msg_gmime_new (mfile, NULL);

	g_assert_cmpstr (mu_msg_gmime_get_to(msg),
			 ==, "gcc-help@gcc.gnu.org");
	g_assert_cmpstr (mu_msg_gmime_get_subject(msg),
			 ==, "gcc include search order");
	g_assert_cmpstr (mu_msg_gmime_get_from(msg),
			 ==, "anon@example.com");
	g_assert_cmpstr (mu_msg_gmime_get_msgid(msg),
			 ==, "3BE9E6535E3029448670913581E7A1A20D852173@"
			 "emss35m06.us.lmco.com");
	g_assert_cmpstr (mu_msg_gmime_get_header(msg, "Mailing-List"),
			 ==, "contact gcc-help-help@gcc.gnu.org; run by ezmlm");
	g_assert_cmpuint (mu_msg_gmime_get_priority(msg), /* 'klub' */
			  ==, MU_MSG_PRIORITY_NORMAL);
	g_assert_cmpuint (mu_msg_gmime_get_date(msg), 
			  ==, 1217530645);
	{
		GSList *lst, *cur;
		lst = NULL;
		mu_msg_gmime_contacts_foreach (msg,
					       (MuMsgGMimeContactsForeachFunc)each_contact,
					       &lst);
		g_assert_cmpuint (g_slist_length(lst),==, 1);
		cur = lst;
		g_assert_cmpstr ((const char*)cur->data, ==,
				 "<none> <anon@example.com>");

		g_slist_foreach (lst, (GFunc)g_free, NULL);
		g_slist_free (lst);
	}
		
	mu_msg_gmime_destroy (msg);
	
	g_free (mfile);
	mu_msg_gmime_uninit ();
}



static void
test_mu_msg_gmime_02 (void)
{
	char *mfile;
	MuMsgGMime *msg;
	GSList *lst;
	
	mu_msg_gmime_init ();
	mfile = get_mailpath (2);

	msg = mu_msg_gmime_new (mfile, NULL);

	g_assert_cmpstr (mu_msg_gmime_get_to(msg),
			 ==, "help-gnu-emacs@gnu.org");
	g_assert_cmpstr (mu_msg_gmime_get_subject(msg),
			 ==, "Re: Learning LISP; Scheme vs elisp.");
	g_assert_cmpstr (mu_msg_gmime_get_from(msg),
			 ==, "anon@example.com");
	g_assert_cmpstr (mu_msg_gmime_get_msgid(msg),
			 ==, "r6bpm5-6n6.ln1@news.ducksburg.com");
	g_assert_cmpstr (mu_msg_gmime_get_header(msg, "Errors-To"),
			 ==, "help-gnu-emacs-bounces+xxxx.klub=gmail.com@gnu.org");
	g_assert_cmpuint (mu_msg_gmime_get_priority(msg), /* 'low' */
			  ==, MU_MSG_PRIORITY_LOW);
	g_assert_cmpuint (mu_msg_gmime_get_date(msg), 
			  ==, 1218051515);
	{
		GSList *lst, *cur;
		lst = NULL;
		mu_msg_gmime_contacts_foreach (msg,
					       (MuMsgGMimeContactsForeachFunc)each_contact,
					       &lst);
		g_assert_cmpuint (g_slist_length(lst),==, 1);
		cur = lst;
		g_assert_cmpstr ((const char*)cur->data, ==,
				 "<none> <anon@example.com>");

		g_slist_foreach (lst, (GFunc)g_free, NULL);
		g_slist_free (lst);
	}
		
	mu_msg_gmime_destroy (msg);
	
	g_free (mfile);
	mu_msg_gmime_uninit ();
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
	g_test_add_func ("/mu-msg-gmime/mu-msg-gmime-01",
			 test_mu_msg_gmime_01);
	g_test_add_func ("/mu-msg-gmime/mu-msg-gmime-02",
			 test_mu_msg_gmime_02);
	
	g_log_set_handler (NULL,
			   G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL| G_LOG_FLAG_RECURSION,
			   (GLogFunc)shutup, NULL);
	
	return g_test_run ();
}
