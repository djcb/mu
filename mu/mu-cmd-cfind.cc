/*
** Copyright (C) 2011-2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
**
** This program is free software; you can redistribute it and/or modify it under
** the terms of the GNU General Public License as published by the Free Software
** Foundation; either version 3, or (at your option) any later version.
**
** This program is distributed in the hope that it will be useful, but WITHOUT
** ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
** FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
** details.
**
** You should have received a copy of the GNU General Public License along with
** this program; if not, write to the Free Software Foundation, Inc., 51
** Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
**
*/

#include "config.h"

#include <string>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "mu-cmd.hh"
#include "mu-contacts.hh"
#include "mu-runtime.hh"

#include "utils/mu-util.h"
#include "utils/mu-utils.hh"
#include "utils/mu-error.hh"
#include "utils/mu-str.h"
#include "utils/mu-date.h"

using namespace Mu;

/**
 * guess the last name for the given name; clearly,
 * this is just a rough guess for setting an initial value.
 *
 * @param name a name
 *
 * @return the last name, as a newly allocated string (free with
 * g_free)
 */
static gchar*
guess_last_name (const char *name)
{
        const gchar *lastsp;

        if (!name)
                return g_strdup ("");

        lastsp = g_strrstr (name, " ");

        return g_strdup (lastsp ? lastsp + 1 : "");
}

/**
 * guess the first name for the given name; clearly,
 * this is just a rough guess for setting an initial value.
 *
 * @param name a name
 *
 * @return the first name, as a newly allocated string (free with
 * g_free)
 */
static gchar*
guess_first_name (const char *name)
{
        const gchar *lastsp;

        if (!name)
                return g_strdup ("");

        lastsp = g_strrstr (name, " ");

        if (lastsp)
                return g_strndup (name, lastsp - name);
        else
                return g_strdup (name);
}

/**
 * guess some nick name for the given name; if we can determine an
 * first name, last name, the nick will be first name + the first char
 * of the last name. otherwise, it's just the first name. clearly,
 * this is just a rough guess for setting an initial value for nicks.
 *
 * @param name a name
 *
 * @return the guessed nick, as a newly allocated string (free with g_free)
 */
static gchar*
cleanup_str (const char* str)
{
        gchar *s;
        const gchar *cur;
        unsigned i;

        if (mu_str_is_empty(str))
                return g_strdup ("");

        s = g_new0 (char, strlen(str) + 1);

        for (cur = str, i = 0; *cur; ++cur) {
                if (ispunct(*cur) || isspace(*cur))
                        continue;
                else
                        s[i++] = *cur;
        }

        return s;
}

static char*
uniquify_nick (const char *nick, GHashTable *nicks)
{
        guint u;

        for (u = 2; u != 1000; ++u) {
                char *cand;
                cand = g_strdup_printf ("%s%u", nick, u);
                if (!g_hash_table_contains (nicks, cand))
                        return cand;
        }

        return g_strdup (nick); /* if all else fails */
}


static gchar*
guess_nick (const char* name, GHashTable *nicks)
{
        gchar *fname, *lname, *nick;
        gchar initial[7];

        fname	  = guess_first_name (name);
        lname	  = guess_last_name (name);

        /* if there's no last name, use first name as the nick */
        if (mu_str_is_empty(fname) || mu_str_is_empty(lname)) {
                g_free (lname);
                nick = fname;
                goto leave;
        }

        memset (initial, 0, sizeof(initial));
        /* couldn't we get an initial for the last name? */
        if (g_unichar_to_utf8 (g_utf8_get_char (lname), initial) == 0) {
                g_free (lname);
                nick = fname;
                goto leave;
        }

        nick = g_strdup_printf ("%s%s", fname, initial);
        g_free (fname);
        g_free (lname);

leave:
        {
                gchar *tmp;
                tmp = cleanup_str (nick);
                g_free (nick);
                nick = tmp;
        }

        if (g_hash_table_contains (nicks, nick)) {
                char *tmp;
                tmp = uniquify_nick (nick, nicks);
                g_free (nick);
                nick = tmp;
        }

        g_hash_table_add (nicks, g_strdup(nick));

        return nick;
}

static void
print_header (const MuConfigFormat format)
{
        switch (format) {
        case MU_CONFIG_FORMAT_BBDB:
                g_print (";; -*-coding: utf-8-emacs;-*-\n"
                         ";;; file-version: 6\n");
                break;
        case MU_CONFIG_FORMAT_MUTT_AB:
                g_print ("Matching addresses in the mu database:\n");
                break;
        default:
                break;
        }
}

static void
each_contact_bbdb (const std::string& email, const std::string& name, time_t tstamp)
{
        char *fname, *lname, *now, *timestamp;

        fname	  = guess_first_name (name.c_str());
        lname	  = guess_last_name (name.c_str());
        now	  = mu_date_str ("%Y-%m-%d", time(NULL));
        timestamp = mu_date_str ("%Y-%m-%d", tstamp);

        g_print ("[\"%s\" \"%s\" nil nil nil nil (\"%s\") "
                 "((creation-date . \"%s\") (time-stamp . \"%s\")) nil]\n",
                 fname, lname, email.c_str(), now, timestamp);

        g_free (now);
        g_free (timestamp);
        g_free (fname);
        g_free (lname);
}


static void
each_contact_mutt_alias (const std::string& email, const std::string& name,
                         GHashTable *nicks)
{
        if (name.empty())
                return;

        char *nick = guess_nick (name.c_str(), nicks);
        mu_util_print_encoded ("alias %s %s <%s>\n",
                               nick, name.c_str(), email.c_str());
        g_free (nick);
}


static void
each_contact_wl (const std::string& email, const std::string& name, GHashTable *nicks)
{
        if (name.empty())
                return;

        char *nick = guess_nick (name.c_str(), nicks);
        mu_util_print_encoded ("%s \"%s\" \"%s\"\n", email.c_str(), nick, name.c_str());
        g_free (nick);
}

static void
print_plain (const std::string& email, const std::string& name, bool color)
{
        if (!name.empty()) {
                if (color)
                        ::fputs (MU_COLOR_MAGENTA, stdout);
                mu_util_fputs_encoded (name.c_str(), stdout);
                ::fputs (" ", stdout);
        }

        if (color)
                ::fputs (MU_COLOR_GREEN, stdout);

        mu_util_fputs_encoded (email.c_str(), stdout);

        if (color)
                fputs (MU_COLOR_DEFAULT, stdout);

        fputs ("\n", stdout);
}

struct ECData {
        MuConfigFormat  format;
        gboolean        color, personal;
        time_t          after;
        GRegex         *rx;
        GHashTable     *nicks;
        size_t          n;
};

static void
each_contact (const Mu::ContactInfo& ci, ECData& ecdata)
{
        if (ecdata.personal && ci.personal)
                return;

        if (ci.tstamp < ecdata.after)
                return;

        if (ecdata.rx &&
            !g_regex_match (ecdata.rx, ci.email.c_str(), (GRegexMatchFlags)0, NULL) &&
            !g_regex_match (ecdata.rx, ci.name.empty() ? "" : ci.name.c_str(), (GRegexMatchFlags)0, NULL))
                return;

        ++ecdata.n;

        switch (ecdata.format) {
        case MU_CONFIG_FORMAT_MUTT_ALIAS:
                each_contact_mutt_alias (ci.email, ci.name, ecdata.nicks);
                break;
        case MU_CONFIG_FORMAT_MUTT_AB:
                mu_util_print_encoded ("%s\t%s\t\n",
                                       ci.email.c_str(), ci.name.c_str());
                break;
        case MU_CONFIG_FORMAT_WL:
                each_contact_wl (ci.email, ci.name, ecdata.nicks);
                break;
        case MU_CONFIG_FORMAT_ORG_CONTACT:
                if (!ci.name.empty())
                        mu_util_print_encoded("* %s\n:PROPERTIES:\n:EMAIL: %s\n:END:\n\n",
                                              ci.name.c_str(), ci.email.c_str());
                break;
        case MU_CONFIG_FORMAT_BBDB:
                each_contact_bbdb (ci.email, ci.name, ci.last_seen);
                break;
        case MU_CONFIG_FORMAT_CSV:
                mu_util_print_encoded("%s,%s\n",
                                      ci.name.empty() ? "" : Mu::quote(ci.name).c_str(),
                                      Mu::quote(ci.email).c_str());
                break;
        case MU_CONFIG_FORMAT_DEBUG: {
                char datebuf[32];
                strftime(datebuf,  sizeof(datebuf), "%F %T",
                         gmtime(&ci.last_seen));
                g_print ("%s\n\tname: %s\n\t%s\n\tpersonal: %s\n\tfreq: %zu\n"
                         "\tlast-seen: %s\n",
                         ci.email.c_str(),
                         ci.name.empty() ? "<none>" : ci.name.c_str(),
                         ci.full_address.c_str(),
                         ci.personal ? "yes" : "no",
                         ci.freq,
                         datebuf);
        } break;
        default:
                print_plain (ci.email, ci.name, ecdata.color);
        }
}


static MuError
run_cmd_cfind (const Mu::Store&       store,
               const char*            pattern,
               gboolean               personal,
               time_t                 after,
               const MuConfigFormat   format,
               gboolean               color,
               GError               **err)
{
        ECData	 ecdata{};

        memset(&ecdata, 0, sizeof(ecdata));

        if (pattern) {
                ecdata.rx = g_regex_new (pattern,
                                         (GRegexCompileFlags)(G_REGEX_CASELESS|G_REGEX_OPTIMIZE),
                                         (GRegexMatchFlags)0, err);
                if (!ecdata.rx)
                        return MU_ERROR_CONTACTS;
        }

        ecdata.personal = personal;
        ecdata.n        = 0;
        ecdata.after    = after;
        ecdata.format	= format;
        ecdata.color	= color;
        ecdata.nicks    = g_hash_table_new_full (g_str_hash, g_str_equal,
                                                 g_free, NULL);

        print_header (format);

        store.contacts().for_each([&](const auto& ci) { each_contact(ci, ecdata); });

        g_hash_table_unref (ecdata.nicks);

        if (ecdata.rx)
                g_regex_unref (ecdata.rx);

        if (ecdata.n == 0) {
                g_printerr ("no matching contacts found\n");
                return MU_ERROR_NO_MATCHES;
        }

        return MU_OK;
}

static gboolean
cfind_params_valid (const MuConfig *opts)
{
        switch (opts->format) {
        case MU_CONFIG_FORMAT_PLAIN:
        case MU_CONFIG_FORMAT_MUTT_ALIAS:
        case MU_CONFIG_FORMAT_MUTT_AB:
        case MU_CONFIG_FORMAT_WL:
        case MU_CONFIG_FORMAT_BBDB:
        case MU_CONFIG_FORMAT_CSV:
        case MU_CONFIG_FORMAT_ORG_CONTACT:
        case MU_CONFIG_FORMAT_DEBUG:
                break;
        default:
                g_printerr ("invalid output format %s\n",
                            opts->formatstr ? opts->formatstr : "<none>");
                return FALSE;
        }

        /* only one pattern allowed */
        if (opts->params[1] && opts->params[2]) {
                g_printerr ("usage: mu cfind [options] [<ptrn>]\n");
                return FALSE;
        }

        return TRUE;
}

MuError
Mu::mu_cmd_cfind (const Mu::Store& store, const MuConfig *opts, GError **err)
{
        g_return_val_if_fail (opts, MU_ERROR_INTERNAL);
        g_return_val_if_fail (opts->cmd == MU_CONFIG_CMD_CFIND,
                              MU_ERROR_INTERNAL);

        if (!cfind_params_valid (opts))
                throw Mu::Error(Mu::Error::Code::InvalidArgument,
                                "invalid parameters");

        auto res = run_cmd_cfind (store,
                                  opts->params[1],
                                  opts->personal,
                                  opts->after,
                                  opts->format,
                                  !opts->nocolor,
                                  err);
        if (res != MU_OK)
                throw Mu::Error(Mu::Error::Code::Internal, err/*consumes*/,
                                "error in cfind");
        return MU_OK;
}
