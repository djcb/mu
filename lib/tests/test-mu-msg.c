/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/
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

#if HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include <glib.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#include <locale.h>

#include "test-mu-common.h"
#include "mu-msg.h"
#include "mu-str.h"


static MuMsg*
get_msg (const char *path)
{
	GError *err;
	MuMsg *msg;

	if (g_test_verbose ())
		g_print (">> %s\n", path);

	err = NULL;
	msg = mu_msg_new_from_file (path, NULL, &err);

	if (!msg) {
		g_printerr ("failed to load %s: %s\n",
			    path, err ? err->message : "something went wrong");
		g_clear_error (&err);
		g_assert (0);
	}

	return msg;
}



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

	msg = get_msg (MU_TESTMAILDIR4 "/1220863042.12663_1.mindcrime!2,S");

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
					 ==,
			 "contact gcc-help-help@gcc.gnu.org; run by ezmlm");
	g_assert_cmpuint (mu_msg_get_prio(msg), /* 'klub' */
			  ==, MU_MSG_PRIO_NORMAL);
	g_assert_cmpuint (mu_msg_get_date(msg),
			  ==, 1217530645);

	i = 0;
	mu_msg_contact_foreach (msg, (MuMsgContactForeachFunc)check_contact_01,
				&i);
	g_assert_cmpint (i,==,2);

	mu_msg_unref (msg);
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

	msg = get_msg (MU_TESTMAILDIR4 "/1220863087.12663_19.mindcrime!2,S");

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
	mu_msg_contact_foreach (msg,
				(MuMsgContactForeachFunc)check_contact_02,
				&i);
	g_assert_cmpint (i,==,2);
	g_assert_cmpuint (mu_msg_get_flags(msg), ==, MU_FLAG_SEEN|MU_FLAG_LIST);

	mu_msg_unref (msg);
}

static void
test_mu_msg_03 (void)
{
	MuMsg *msg;

	msg = get_msg (MU_TESTMAILDIR4 "/1283599333.1840_11.cthulhu!2,");
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
	g_assert_cmpstr (mu_msg_get_body_text(msg, MU_MSG_OPTION_NONE),
			 ==,
			 "\nLet's write some fünkÿ text\nusing umlauts.\n\nFoo.\n");
	g_assert_cmpuint (mu_msg_get_flags(msg),
			  ==, MU_FLAG_UNREAD); /* not seen => unread */

	mu_msg_unref (msg);
}


static void
test_mu_msg_04 (void)
{
	MuMsg *msg;

	msg = get_msg (MU_TESTMAILDIR4 "/mail5");
	g_assert_cmpstr (mu_msg_get_to(msg),
			 ==, "George Custer <gac@example.com>");
	g_assert_cmpstr (mu_msg_get_subject(msg),
			 ==, "pics for you");
	g_assert_cmpstr (mu_msg_get_from(msg),
			 ==, "Sitting Bull <sb@example.com>");
	g_assert_cmpuint (mu_msg_get_prio(msg), /* 'low' */
			  ==, MU_MSG_PRIO_NORMAL);
	g_assert_cmpuint (mu_msg_get_date(msg),
			  ==, 0);
	g_assert_cmpuint (mu_msg_get_flags(msg),
			  ==, MU_FLAG_HAS_ATTACH|MU_FLAG_UNREAD);
	mu_msg_unref (msg);
}


static void
test_mu_msg_multimime (void)
{
	MuMsg *msg;

	msg = get_msg (MU_TESTMAILDIR4 "/multimime!2,FS");
	/* ie., are text parts properly concatenated? */
	g_assert_cmpstr (mu_msg_get_subject(msg),
			 ==, "multimime");
	g_assert_cmpstr (mu_msg_get_body_text(msg, MU_MSG_OPTION_NONE),
			 ==, "abcdef");
	g_assert_cmpuint (mu_msg_get_flags(msg),
			 ==, MU_FLAG_FLAGGED | MU_FLAG_SEEN |
			 MU_FLAG_HAS_ATTACH);
	mu_msg_unref (msg);
}



static void
test_mu_msg_flags (void)
{
	unsigned u;

	struct {
		const char *path;
		MuFlags flags;
	} msgflags [] = {
		{ MU_TESTMAILDIR4 "/multimime!2,FS",
		  MU_FLAG_FLAGGED | MU_FLAG_SEEN |
		  MU_FLAG_HAS_ATTACH },
		{ MU_TESTMAILDIR4 "/special!2,Sabc",
		  MU_FLAG_SEEN }

	};

	for (u = 0; u != G_N_ELEMENTS(msgflags); ++u) {
		MuMsg *msg;
		MuFlags flags;

		g_assert ((msg = get_msg (msgflags[u].path)));
		flags = mu_msg_get_flags (msg);

		if (g_test_verbose())
			g_print ("=> %s [ %s, %u] <=> [ %s, %u]\n",
				 msgflags[u].path,
				 mu_flags_to_str_s(msgflags[u].flags,
						   MU_FLAG_TYPE_ANY),
				 (unsigned)msgflags[u].flags,
				 mu_flags_to_str_s(flags, MU_FLAG_TYPE_ANY),
				 (unsigned)flags);
		g_assert_cmpuint (flags ,==, msgflags[u].flags);
		mu_msg_unref (msg);
	}
}


static void
test_mu_msg_umlaut (void)
{
	MuMsg *msg;

	msg = get_msg (MU_TESTMAILDIR4 "/1305664394.2171_402.cthulhu!2,");
	g_assert_cmpstr (mu_msg_get_to(msg),
			 ==, "Helmut Kröger <hk@testmu.xxx>");
	g_assert_cmpstr (mu_msg_get_subject(msg),
			 ==, "Motörhead");
	g_assert_cmpstr (mu_msg_get_from(msg),
			 ==, "Mü <testmu@testmu.xx>");
	g_assert_cmpuint (mu_msg_get_prio(msg), /* 'low' */
			  ==, MU_MSG_PRIO_NORMAL);
	g_assert_cmpuint (mu_msg_get_date(msg),
			  ==, 0);

	mu_msg_unref (msg);
}


static void
test_mu_msg_references (void)
{
	MuMsg *msg;
	const GSList *refs;

	msg = get_msg (MU_TESTMAILDIR4 "/1305664394.2171_402.cthulhu!2,");
	refs = mu_msg_get_references(msg);

	g_assert_cmpuint (g_slist_length ((GSList*)refs), ==, 4);

	g_assert_cmpstr ((char*)refs->data,==, "non-exist-01@msg.id");
	refs = g_slist_next (refs);
	g_assert_cmpstr ((char*)refs->data,==, "non-exist-02@msg.id");
	refs = g_slist_next (refs);
	g_assert_cmpstr ((char*)refs->data,==, "non-exist-03@msg.id");
	refs = g_slist_next (refs);
	g_assert_cmpstr ((char*)refs->data,==, "non-exist-04@msg.id");
	refs = g_slist_next (refs);

	mu_msg_unref (msg);
}



static void
test_mu_msg_references_dups (void)
{
	MuMsg *msg;
	const GSList *refs;

	msg = get_msg (MU_TESTMAILDIR4 "/1252168370_3.14675.cthulhu!2,S");
	refs = mu_msg_get_references(msg);

	/* make sure duplicate msg-ids are filtered out */

	g_assert_cmpuint (g_slist_length ((GSList*)refs), ==, 6);

	g_assert_cmpstr ((char*)refs->data,==,
			 "439C1136.90504@euler.org");
	refs = g_slist_next (refs);
	g_assert_cmpstr ((char*)refs->data,==,
			 "4399DD94.5070309@euler.org");
	refs = g_slist_next (refs);
	g_assert_cmpstr ((char*)refs->data,==,
			 "20051209233303.GA13812@gauss.org");
	refs = g_slist_next (refs);
	g_assert_cmpstr ((char*)refs->data,==,
			 "439B41ED.2080402@euler.org");
	refs = g_slist_next (refs);
	g_assert_cmpstr ((char*)refs->data,==,
			 "439A1E03.3090604@euler.org");
	refs = g_slist_next (refs);
	g_assert_cmpstr ((char*)refs->data,==,
			 "20051211184308.GB13513@gauss.org");
	refs = g_slist_next (refs);

	mu_msg_unref (msg);
}


static void
test_mu_msg_references_many (void)
{
	MuMsg *msg;
	unsigned u;
	const GSList *refs, *cur;
	const char* expt_refs[] = {
		"e9065dac-13c1-4103-9e31-6974ca232a89@t15g2000prt.googlegroups.com",
		"87hbblwelr.fsf@sapphire.mobileactivedefense.com",
		"pql248-4va.ln1@wilbur.25thandClement.com",
		"ikns6r$li3$1@Iltempo.Update.UU.SE",
		"8762s0jreh.fsf@sapphire.mobileactivedefense.com",
		"ikqqp1$jv0$1@Iltempo.Update.UU.SE",
		"87hbbjc5jt.fsf@sapphire.mobileactivedefense.com",
		"ikr0na$lru$1@Iltempo.Update.UU.SE",
		"tO8cp.1228$GE6.370@news.usenetserver.com",
		"ikr6ks$nlf$1@Iltempo.Update.UU.SE",
		"8ioh48-8mu.ln1@leafnode-msgid.gclare.org.uk"
	};

	msg = get_msg (MU_TESTMAILDIR2 "/bar/cur/181736.eml");
	refs = mu_msg_get_references(msg);

	g_assert_cmpuint (G_N_ELEMENTS(expt_refs), ==,
			  g_slist_length((GSList*)refs));

	for (cur = refs, u = 0; cur; cur = g_slist_next(cur), ++u) {
		if (g_test_verbose())
			g_print ("%u. '%s' =? '%s'\n",
				 u, (char*)cur->data,
				 expt_refs[u]);

		g_assert_cmpstr ((char*)cur->data, ==, expt_refs[u]);
	}

	mu_msg_unref (msg);
}





static void
test_mu_msg_tags (void)
{
	MuMsg *msg;
	const GSList *tags;

	msg = get_msg (MU_TESTMAILDIR4 "/mail1");

	g_assert_cmpstr (mu_msg_get_to(msg),
			 ==, "Julius Caesar <jc@example.com>");
	g_assert_cmpstr (mu_msg_get_subject(msg),
			 ==, "Fere libenter homines id quod volunt credunt");
	g_assert_cmpstr (mu_msg_get_from(msg),
			 ==, "John Milton <jm@example.com>");
	g_assert_cmpuint (mu_msg_get_prio(msg), /* 'low' */
			  ==, MU_MSG_PRIO_HIGH);
	g_assert_cmpuint (mu_msg_get_date(msg),
			  ==, 1217530645);

	tags = mu_msg_get_tags (msg);
	g_assert_cmpstr ((char*)tags->data,==,"Paradise");
	g_assert_cmpstr ((char*)tags->next->data,==,"losT");
	g_assert_cmpstr ((char*)tags->next->next->data,==,"john");
	g_assert_cmpstr ((char*)tags->next->next->next->data,==,"milton");

	g_assert (!tags->next->next->next->next);

	mu_msg_unref (msg);
}


static void
test_mu_msg_comp_unix_programmer (void)
{
	MuMsg *msg;
	char *refs;

	msg = get_msg (MU_TESTMAILDIR4 "/181736.eml");
	g_assert_cmpstr (mu_msg_get_to(msg),
	 		 ==, NULL);
	g_assert_cmpstr (mu_msg_get_subject(msg),
			 ==, "Re: Are writes \"atomic\" to readers of the file?");
	g_assert_cmpstr (mu_msg_get_from(msg),
			 ==, "Jimbo Foobarcuux <jimbo@slp53.sl.home>");
	g_assert_cmpstr (mu_msg_get_msgid(msg),
			 ==, "oktdp.42997$Te.22361@news.usenetserver.com");

	refs = mu_str_from_list (mu_msg_get_references(msg), ',');
	g_assert_cmpstr (refs, ==,
			 "e9065dac-13c1-4103-9e31-6974ca232a89@t15g2000prt"
			 ".googlegroups.com,"
			 "87hbblwelr.fsf@sapphire.mobileactivedefense.com,"
			 "pql248-4va.ln1@wilbur.25thandClement.com,"
			 "ikns6r$li3$1@Iltempo.Update.UU.SE,"
			 "8762s0jreh.fsf@sapphire.mobileactivedefense.com,"
			 "ikqqp1$jv0$1@Iltempo.Update.UU.SE,"
			 "87hbbjc5jt.fsf@sapphire.mobileactivedefense.com,"
			 "ikr0na$lru$1@Iltempo.Update.UU.SE,"
			 "tO8cp.1228$GE6.370@news.usenetserver.com,"
			 "ikr6ks$nlf$1@Iltempo.Update.UU.SE,"
			 "8ioh48-8mu.ln1@leafnode-msgid.gclare.org.uk");
	g_free (refs);

	//"jimbo@slp53.sl.home (Jimbo Foobarcuux)";
	g_assert_cmpuint (mu_msg_get_prio(msg), /* 'low' */
			  ==, MU_MSG_PRIO_NORMAL);
	g_assert_cmpuint (mu_msg_get_date(msg),
			  ==, 1299603860);

	mu_msg_unref (msg);
}

int
main (int argc, char *argv[])
{
	int rv;

	g_test_init (&argc, &argv, NULL);

	/* mu_msg_str_date */
	g_test_add_func ("/mu-msg/mu-msg-01",
			 test_mu_msg_01);
	g_test_add_func ("/mu-msg/mu-msg-02",
			 test_mu_msg_02);
	g_test_add_func ("/mu-msg/mu-msg-03",
			 test_mu_msg_03);
	g_test_add_func ("/mu-msg/mu-msg-04",
			 test_mu_msg_04);
	g_test_add_func ("/mu-msg/mu-msg-multimime",
			 test_mu_msg_multimime);

	g_test_add_func ("/mu-msg/mu-msg-flags",
			 test_mu_msg_flags);

	g_test_add_func ("/mu-msg/mu-msg-tags",
			 test_mu_msg_tags);
	g_test_add_func ("/mu-msg/mu-msg-references",
			 test_mu_msg_references);
	g_test_add_func ("/mu-msg/mu-msg-references_dups",
			 test_mu_msg_references_dups);
	g_test_add_func ("/mu-msg/mu-msg-references_many",
			 test_mu_msg_references_many);

	g_test_add_func ("/mu-msg/mu-msg-umlaut",
			 test_mu_msg_umlaut);
	g_test_add_func ("/mu-msg/mu-msg-comp-unix-programmer",
			 test_mu_msg_comp_unix_programmer);

	g_log_set_handler (NULL,
			   G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL|
			   G_LOG_FLAG_RECURSION,
			   (GLogFunc)black_hole, NULL);

	rv = g_test_run ();

	return rv;
}
