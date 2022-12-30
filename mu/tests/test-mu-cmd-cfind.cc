/*
**
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

#include "config.h"

#include <glib.h>
#include <glib/gstdio.h>

#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include "utils/mu-test-utils.hh"
#include "mu-store.hh"
#include "mu-query.hh"
#include "utils/mu-utils.hh"

static std::string CONTACTS_CACHE;

using namespace Mu;

static std::string
fill_contacts_cache(const std::string& path)
{
	auto cmdline = format("/bin/sh -c '"
			      "%s --quiet init  --muhome=%s --maildir=%s ; "
			      "%s --quiet index --muhome=%s '",
			      MU_PROGRAM,
			      path.c_str(),
			      MU_TESTMAILDIR,
			      MU_PROGRAM,
			      path.c_str());

	if (g_test_verbose())
		g_print("%s\n", cmdline.c_str());

	GError *err{};
	if (!g_spawn_command_line_sync(cmdline.c_str(), NULL, NULL, NULL, &err)) {
		g_printerr("Error: %s\n", err ? err->message : "?");
		g_assert(0);
	}

	return path;
}

static void
test_mu_cfind_plain(void)
{
	gchar *cmdline, *output, *erroutput;

	cmdline = g_strdup_printf("%s --nocolor cfind --muhome=%s --format=plain "
				  "'testmu\\.xxx?'",
				  MU_PROGRAM,
				  CONTACTS_CACHE.c_str());
	if (g_test_verbose())
		g_print("%s\n", cmdline);

	output = erroutput = NULL;
	g_assert(g_spawn_command_line_sync(cmdline, &output, &erroutput, NULL, NULL));

	/* note, output order is unspecified */
	g_assert(output);
	if (output[0] == 'H')
		g_assert_cmpstr(output,
				==,
				"Helmut Kröger hk@testmu.xxx\n"
				"Mü testmu@testmu.xx\n");
	else
		g_assert_cmpstr(output,
				==,
				"Mü testmu@testmu.xx\n"
				"Helmut Kröger hk@testmu.xxx\n");
	g_free(cmdline);
	g_free(output);
	g_free(erroutput);
}

static void
test_mu_cfind_bbdb(void)
{
	gchar *     cmdline, *output, *erroutput, *expected;
	gchar       today[12];
	struct tm*  tmtoday;
	time_t      now;
	const char* old_tz;

	old_tz = set_tz("Europe/Helsinki");

	cmdline = g_strdup_printf("%s --nocolor cfind --muhome=%s --format=bbdb "
				  "'testmu\\.xxx?'",
				  MU_PROGRAM,
				  CONTACTS_CACHE.c_str());

	output = erroutput = NULL;
	g_assert(g_spawn_command_line_sync(cmdline, &output, &erroutput, NULL, NULL));

#define frm1                                                                                       \
	";; -*-coding: utf-8-emacs;-*-\n"                                                          \
	";;; file-version: 6\n"                                                                    \
	"[\"Helmut\" \"Kröger\" nil nil nil nil (\"hk@testmu.xxx\") "                              \
	"((creation-date . \"%s\") "                                                               \
	"(time-stamp . \"1970-01-01\")) nil]\n"                                                    \
	"[\"Mü\" \"\" nil nil nil nil (\"testmu@testmu.xx\") "                                     \
	"((creation-date . \"%s\") "                                                               \
	"(time-stamp . \"1970-01-01\")) nil]\n"

#define frm2                                                                                       \
	";; -*-coding: utf-8-emacs;-*-\n"                                                          \
	";;; file-version: 6\n"                                                                    \
	"[\"Mü\" \"\" nil nil nil nil (\"testmu@testmu.xx\") "                                     \
	"((creation-date . \"%s\") "                                                               \
	"(time-stamp . \"1970-01-01\")) nil]\n"                                                    \
	"[\"Helmut\" \"Kröger\" nil nil nil nil (\"hk@testmu.xxx\") "                              \
	"((creation-date . \"%s\") "                                                               \
	"(time-stamp . \"1970-01-01\")) nil]\n"

	g_assert(output);

	now     = time(NULL);
	tmtoday = localtime(&now);
	strftime(today, sizeof(today), "%Y-%m-%d", tmtoday);

	expected = g_strdup_printf(output[52] == 'H' ? frm1 : frm2, today, today);

	/* g_print ("\n%s\n", output); */

	g_assert_cmpstr(output, ==, expected);

	g_free(cmdline);
	g_free(output);
	g_free(erroutput);
	g_free(expected);

	set_tz(old_tz);
}

static void
test_mu_cfind_wl(void)
{
	gchar *cmdline, *output, *erroutput;

	cmdline = g_strdup_printf("%s cfind --muhome=%s --format=wl "
				  "'testmu\\.xxx?'",
				  MU_PROGRAM,
				  CONTACTS_CACHE.c_str());

	output = erroutput = NULL;
	g_assert(g_spawn_command_line_sync(cmdline, &output, &erroutput, NULL, NULL));

	g_assert(output);
	if (output[0] == 'h')
		g_assert_cmpstr(output,
				==,
				"hk@testmu.xxx \"HelmutK\" \"Helmut Kröger\"\n"
				"testmu@testmu.xx \"Mü\" \"Mü\"\n");
	else
		g_assert_cmpstr(output,
				==,
				"testmu@testmu.xx \"Mü\" \"Mü\"\n"
				"hk@testmu.xxx \"HelmutK\" \"Helmut Kröger\"\n");

	g_free(cmdline);
	g_free(output);
	g_free(erroutput);
}

static void
test_mu_cfind_mutt_alias(void)
{
	gchar *cmdline, *output, *erroutput;

	cmdline = g_strdup_printf("%s cfind --muhome=%s --format=mutt-alias "
				  "'testmu\\.xxx?'",
				  MU_PROGRAM,
				  CONTACTS_CACHE.c_str());

	output = erroutput = NULL;
	g_assert(g_spawn_command_line_sync(cmdline, &output, &erroutput, NULL, NULL));

	/* both orders are possible... */
	g_assert(output);

	if (output[6] == 'H')
		g_assert_cmpstr(output,
				==,
				"alias HelmutK Helmut Kröger <hk@testmu.xxx>\n"
				"alias Mü Mü <testmu@testmu.xx>\n");
	else
		g_assert_cmpstr(output,
				==,
				"alias Mü Mü <testmu@testmu.xx>\n"
				"alias HelmutK Helmut Kröger <hk@testmu.xxx>\n");

	g_free(cmdline);
	g_free(output);
	g_free(erroutput);
}

static void
test_mu_cfind_mutt_ab(void)
{
	gchar *cmdline, *output, *erroutput;

	cmdline = g_strdup_printf("%s cfind --muhome=%s --format=mutt-ab "
				  "'testmu\\.xxx?'",
				  MU_PROGRAM,
				  CONTACTS_CACHE.c_str());

	if (g_test_verbose())
		g_print("%s\n", cmdline);

	output = erroutput = NULL;
	g_assert(g_spawn_command_line_sync(cmdline, &output, &erroutput, NULL, NULL));
	g_assert(output);

	if (output[39] == 'h')
		g_assert_cmpstr(output,
				==,
				"Matching addresses in the mu database:\n"
				"hk@testmu.xxx\tHelmut Kröger\t\n"
				"testmu@testmu.xx\tMü\t\n");
	else
		g_assert_cmpstr(output,
				==,
				"Matching addresses in the mu database:\n"
				"testmu@testmu.xx\tMü\t\n"
				"hk@testmu.xxx\tHelmut Kröger\t\n");

	g_free(cmdline);
	g_free(output);
	g_free(erroutput);
}

static void
test_mu_cfind_org_contact(void)
{
	gchar *cmdline, *output, *erroutput;

	cmdline = g_strdup_printf("%s cfind --muhome=%s --format=org-contact "
				  "'testmu\\.xxx?'",
				  MU_PROGRAM,
				  CONTACTS_CACHE.c_str());

	output = erroutput = NULL;
	g_assert(g_spawn_command_line_sync(cmdline, &output, &erroutput, NULL, NULL));

	g_assert(output);

	if (output[2] == 'H')
		g_assert_cmpstr(output,
				==,
				"* Helmut Kröger\n"
				":PROPERTIES:\n"
				":EMAIL: hk@testmu.xxx\n"
				":END:\n\n"
				"* Mü\n"
				":PROPERTIES:\n"
				":EMAIL: testmu@testmu.xx\n"
				":END:\n\n");
	else
		g_assert_cmpstr(output,
				==,
				"* Mü\n"
				":PROPERTIES:\n"
				":EMAIL: testmu@testmu.xx\n"
				":END:\n\n"
				"* Helmut Kröger\n"
				":PROPERTIES:\n"
				":EMAIL: hk@testmu.xxx\n"
				":END:\n\n");

	g_free(cmdline);
	g_free(output);
	g_free(erroutput);
}

static void
test_mu_cfind_csv(void)
{
	gchar *cmdline, *output, *erroutput;

	cmdline = g_strdup_printf("%s --nocolor cfind --muhome=%s --format=csv "
				  "'testmu\\.xxx?'",
				  MU_PROGRAM,
				  CONTACTS_CACHE.c_str());

	if (g_test_verbose())
		g_print("%s\n", cmdline);

	output = erroutput = NULL;
	g_assert(g_spawn_command_line_sync(cmdline, &output, &erroutput, NULL, NULL));
	g_assert(output);
	if (output[1] == 'H')
		g_assert_cmpstr(output,
				==,
				"\"Helmut Kröger\",\"hk@testmu.xxx\"\n"
				"\"Mü\",\"testmu@testmu.xx\"\n");
	else
		g_assert_cmpstr(output,
				==,
				"\"Mü\",\"testmu@testmu.xx\"\n"
				"\"Helmut Kröger\",\"hk@testmu.xxx\"\n");
	g_free(cmdline);
	g_free(output);
	g_free(erroutput);
}


static void
test_mu_cfind_json()
{
	gchar *cmdline, *output, *erroutput;

	cmdline = g_strdup_printf("%s --nocolor cfind --muhome=%s --format=json ^a@example\\.com",
				  MU_PROGRAM,
				  CONTACTS_CACHE.c_str());

	if (g_test_verbose())
		g_print("%s\n", cmdline);

	output = erroutput = NULL;
	g_assert(g_spawn_command_line_sync(cmdline, &output, &erroutput, NULL, NULL));
	g_assert(output);

	const auto expected = R"([
  {
    "email"         : "a@example.com",
    "name"          : null,
    "display"       : "a@example.com",
    "last-seen"     : 1463331445,
    "last-seen-iso" : "2016-05-15T16:57:25Z",
    "personal"      : false,
    "frequency"     : 1
  }
]
)";
	g_assert_cmpstr(output, ==, expected);
	g_free(cmdline);
	g_free(output);
	g_free(erroutput);
}

int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	if (!set_en_us_utf8_locale())
		return 0; /* don't error out... */

	TempDir tmpdir{};
	CONTACTS_CACHE = fill_contacts_cache(tmpdir.path());

	g_test_add_func("/mu-cmd-cfind/test-mu-cfind-plain", test_mu_cfind_plain);
	g_test_add_func("/mu-cmd-cfind/test-mu-cfind-bbdb", test_mu_cfind_bbdb);
	g_test_add_func("/mu-cmd-cfind/test-mu-cfind-wl", test_mu_cfind_wl);
	g_test_add_func("/mu-cmd-cfind/test-mu-cfind-mutt-alias", test_mu_cfind_mutt_alias);
	g_test_add_func("/mu-cmd-cfind/test-mu-cfind-mutt-ab", test_mu_cfind_mutt_ab);
	g_test_add_func("/mu-cmd-cfind/test-mu-cfind-org-contact", test_mu_cfind_org_contact);
	g_test_add_func("/mu-cmd-cfind/test-mu-cfind-csv", test_mu_cfind_csv);
	g_test_add_func("/mu-cmd-cfind/test-mu-cfind-json", test_mu_cfind_json);

	return g_test_run();
}
