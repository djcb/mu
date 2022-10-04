/*
** Copyright (C) 2008-2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <array>
#include <string>

#include <locale.h>

#include "utils/mu-test-utils.hh"
#include "utils/mu-result.hh"
#include "utils/mu-utils.hh"

#include <message/mu-message.hh>

using namespace Mu;

using ExpectedContacts = const std::vector<std::pair<std::string, std::string>>;

static void
assert_contacts_equal(const Contacts& contacts,
		      const ExpectedContacts& expected)
{
	g_assert_cmpuint(contacts.size(), ==, expected.size());

	size_t n{};
	for (auto&& contact: contacts) {
		if (g_test_verbose())
			g_message("{ \"%s\", \"%s\"},\n", contact.name.c_str(), contact.email.c_str());
		assert_equal(contact.name, expected.at(n).first);
		assert_equal(contact.email, expected.at(n).second);
		++n;
	}
	g_print("\n");
}


static void
test_mu_msg_01(void)
{
	auto msg{Message::make_from_path(MU_TESTMAILDIR4 "/1220863042.12663_1.mindcrime!2,S")
		.value()};

	assert_contacts_equal(msg.to(), {{ "Donald Duck", "gcc-help@gcc.gnu.org" }});
	assert_contacts_equal(msg.from(), {{ "Mickey Mouse", "anon@example.com" }});

	assert_equal(msg.subject(), "gcc include search order");
	assert_equal(msg.message_id(),
		     "3BE9E6535E3029448670913581E7A1A20D852173@"
		     "emss35m06.us.lmco.com");
	assert_equal(msg.header("Mailing-List").value_or(""),
		     "contact gcc-help-help@gcc.gnu.org; run by ezmlm");
	g_assert_true(msg.priority() == Priority::Normal);
	g_assert_cmpuint(msg.date(), ==, 1217530645);

	assert_contacts_equal(msg.all_contacts(), {
			{ "", "gcc-help-owner@gcc.gnu.org"},
			{ "Mickey Mouse", "anon@example.com" },
			{ "Donald Duck", "gcc-help@gcc.gnu.org" }
		});

}

static void
test_mu_msg_02(void)
{
	auto msg{Message::make_from_path(MU_TESTMAILDIR4 "/1220863087.12663_19.mindcrime!2,S")
		.value()};

	assert_equal(msg.to().at(0).email, "help-gnu-emacs@gnu.org");
	assert_equal(msg.subject(), "Re: Learning LISP; Scheme vs elisp.");
	assert_equal(msg.from().at(0).email, "anon@example.com");
	assert_equal(msg.message_id(), "r6bpm5-6n6.ln1@news.ducksburg.com");
	assert_equal(msg.header("Errors-To").value_or(""),
		     "help-gnu-emacs-bounces+xxxx.klub=gmail.com@gnu.org");
	g_assert_true(msg.priority() /* 'low' */
		      == Priority::Low);
	g_assert_cmpuint(msg.date(), ==, 1218051515);
		g_print("flags: %s\n", Mu::to_string(msg.flags()).c_str());
	g_assert_true(msg.flags() == (Flags::Seen|Flags::MailingList));

	assert_contacts_equal(msg.all_contacts(), {
			{ "", "help-gnu-emacs-bounces+xxxx.klub=gmail.com@gnu.org"},
			{ "", "anon@example.com"},
			{ "", "help-gnu-emacs@gnu.org"},
		});

}

static void
test_mu_msg_03(void)
{
	//const GSList* params;

	auto msg{Message::make_from_path(MU_TESTMAILDIR4 "/1283599333.1840_11.cthulhu!2,")
		.value()};

	assert_equal(msg.to().at(0).display_name(), "Bilbo Baggins <bilbo@anotherexample.com>");
	assert_equal(msg.subject(), "Greetings from Lothlórien");
	assert_equal(msg.from().at(0).display_name(), "Frodo Baggins <frodo@example.com>");
	g_assert_true(msg.priority() == Priority::Normal);
	g_assert_cmpuint(msg.date(), ==, 0);
	assert_equal(msg.body_text().value_or(""),
		     "\nLet's write some fünkÿ text\nusing umlauts.\n\nFoo.\n");

	// params = mu_msg_get_body_text_content_type_parameters(msg, MU_MSG_OPTION_NONE);
	// g_assert_cmpuint(g_slist_length((GSList*)params), ==, 2);

	// assert_equal((char*)params->data, "charset");
	// params = g_slist_next(params);
	// assert_equal((char*)params->data, "UTF-8");
	g_assert_true(msg.flags() == (Flags::Unread));
}

static void
test_mu_msg_04(void)
{
	auto msg{Message::make_from_path(MU_TESTMAILDIR4 "/mail5").value()};

	assert_equal(msg.to().at(0).display_name(), "George Custer <gac@example.com>");
	assert_equal(msg.subject(), "pics for you");
	assert_equal(msg.from().at(0).display_name(), "Sitting Bull <sb@example.com>");
	g_assert_true(msg.priority() /* 'low' */
		      == Priority::Normal);
	g_assert_cmpuint(msg.date(), ==, 0);
	g_assert_true(msg.flags() ==
		      (Flags::HasAttachment|Flags::Unread));
	g_assert_true(msg.flags() ==
		      (Flags::HasAttachment|Flags::Unread));
}

static void
test_mu_msg_multimime(void)
{
	auto msg{Message::make_from_path(MU_TESTMAILDIR4 "/multimime!2,FS").value()};

	/* ie., are text parts properly concatenated? */
	assert_equal(msg.subject(), "multimime");
	assert_equal(msg.body_text().value_or(""), "abcdef");
	g_assert_true(msg.flags() == (Flags::HasAttachment|Flags::Flagged|Flags::Seen));
}

static void
test_mu_msg_flags(void)
{
	std::array<std::pair<std::string, Flags>, 2> tests= {{
			{MU_TESTMAILDIR4 "/multimime!2,FS",
			 (Flags::Flagged | Flags::Seen |
			  Flags::HasAttachment)},
			{MU_TESTMAILDIR4 "/special!2,Sabc",
			 (Flags::Seen)}
		}};

	for (auto&& test: tests) {
		 auto msg = Message::make_from_path(test.first);
		 assert_valid_result(msg);
		 g_assert_true(msg->flags() == test.second);
	}
}

static void
test_mu_msg_umlaut(void)
{
	auto msg{Message::make_from_path(MU_TESTMAILDIR4 "/1305664394.2171_402.cthulhu!2,")
		.value()};

	assert_contacts_equal(msg.to(), { { "Helmut Kröger", "hk@testmu.xxx"}});
	assert_contacts_equal(msg.from(), { { "Mü", "testmu@testmu.xx"}});

	assert_equal(msg.subject(), "Motörhead");
	assert_equal(msg.from().at(0).display_name(), "Mü <testmu@testmu.xx>");
	g_assert_true(msg.priority() == Priority::Normal);
	g_assert_cmpuint(msg.date(), ==, 0);
}

static void
test_mu_msg_references(void)
{
	auto msg{Message::make_from_path(MU_TESTMAILDIR4 "/1305664394.2171_402.cthulhu!2,")
		.value()};

	std::array<std::string, 4> expected_refs = {
		"non-exist-01@msg.id",
		"non-exist-02@msg.id",
		"non-exist-03@msg.id",
		"non-exist-04@msg.id"
	};

	assert_equal_seq_str(msg.references(), expected_refs);
	assert_equal(msg.thread_id(), expected_refs[0]);
}

static void
test_mu_msg_references_dups(void)
{
	auto msg{Message::make_from_path(MU_TESTMAILDIR4 "/1252168370_3.14675.cthulhu!2,S")
		.value()};

	std::array<std::string, 6> expected_refs = {
		"439C1136.90504@euler.org",
		"4399DD94.5070309@euler.org",
		"20051209233303.GA13812@gauss.org",
		"439B41ED.2080402@euler.org",
		"439A1E03.3090604@euler.org",
		"20051211184308.GB13513@gauss.org"
	};

	assert_equal_seq_str(msg.references(), expected_refs);
	assert_equal(msg.thread_id(), expected_refs[0]);
}

static void
test_mu_msg_references_many(void)
{
	auto msg{Message::make_from_path(MU_TESTMAILDIR2 "/bar/cur/181736.eml")
		.value()};

	std::array<std::string, 11> expected_refs = {
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

	assert_equal_seq_str(msg.references(), expected_refs);
	assert_equal(msg.thread_id(), expected_refs[0]);
}

static void
test_mu_msg_tags(void)
{
	auto msg{Message::make_from_path(MU_TESTMAILDIR4 "/mail1").value()};

	assert_contacts_equal(msg.to(), {{ "Julius Caesar", "jc@example.com" }});
	assert_contacts_equal(msg.from(), {{ "John Milton", "jm@example.com" }});

	assert_equal(msg.subject(),"Fere libenter homines id quod volunt credunt");

	g_assert_true(msg.priority() == Priority::High);
	g_assert_cmpuint(msg.date(), ==, 1217530645);

	std::array<std::string, 4> expected_tags = {
		"Paradise",
		"losT",
		"john",
		"milton"
	};
	assert_equal_seq_str(msg.tags(), expected_tags);
}

static void
test_mu_msg_comp_unix_programmer(void)
{
	auto msg{Message::make_from_path(MU_TESTMAILDIR4 "/181736.eml").value()};

	g_assert_true(msg.to().empty());
	assert_equal(msg.subject(),
			"Re: Are writes \"atomic\" to readers of the file?");
	assert_equal(msg.from().at(0).display_name(), "Jimbo Foobarcuux <jimbo@slp53.sl.home>");
	assert_equal(msg.message_id(), "oktdp.42997$Te.22361@news.usenetserver.com");

	auto refs = join(msg.references(), ',');
	assert_equal(refs,
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

	//"jimbo@slp53.sl.home (Jimbo Foobarcuux)";
	g_assert_true(msg.priority() == Priority::Normal);
	g_assert_cmpuint(msg.date(), ==, 1299603860);
}

static void
test_mu_str_prio_01(void)
{
	g_assert_true(priority_name(Priority::Low) == "low");
	g_assert_true(priority_name(Priority::Normal) == "normal");
	g_assert_true(priority_name(Priority::High) == "high");
}

G_GNUC_UNUSED static gboolean
ignore_error(const char* log_domain, GLogLevelFlags log_level, const gchar* msg, gpointer user_data)
{
	return FALSE; /* don't abort */
}


int
main(int argc, char* argv[])
{
	int rv;

	g_test_init(&argc, &argv, NULL);

	/* mu_msg_str_date */
	g_test_add_func("/mu-msg/mu-msg-01", test_mu_msg_01);
	g_test_add_func("/mu-msg/mu-msg-02", test_mu_msg_02);
	g_test_add_func("/mu-msg/mu-msg-03", test_mu_msg_03);
	g_test_add_func("/mu-msg/mu-msg-04", test_mu_msg_04);
	g_test_add_func("/mu-msg/mu-msg-multimime", test_mu_msg_multimime);

	g_test_add_func("/mu-msg/mu-msg-flags", test_mu_msg_flags);

	g_test_add_func("/mu-msg/mu-msg-tags", test_mu_msg_tags);
	g_test_add_func("/mu-msg/mu-msg-references", test_mu_msg_references);
	g_test_add_func("/mu-msg/mu-msg-references_dups", test_mu_msg_references_dups);
	g_test_add_func("/mu-msg/mu-msg-references_many", test_mu_msg_references_many);

	g_test_add_func("/mu-msg/mu-msg-umlaut", test_mu_msg_umlaut);
	g_test_add_func("/mu-msg/mu-msg-comp-unix-programmer", test_mu_msg_comp_unix_programmer);

	g_test_add_func("/mu-str/mu-str-prio-01", test_mu_str_prio_01);

	rv = g_test_run();

	return rv;
}
