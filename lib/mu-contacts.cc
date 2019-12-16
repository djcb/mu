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

#include "mu-contacts.hh"

#include <mutex>
#include <unordered_map>
#include <set>
#include <sstream>
#include <functional>
#include <algorithm>

#include <utils/mu-utils.hh>
#include <glib.h>

using namespace Mu;

ContactInfo::ContactInfo (const std::string& _full_address,
                          const std::string& _email,
                          const std::string& _name,
                          bool _personal, time_t _last_seen, size_t _freq):
        full_address{_full_address},
        email{_email},
        name{_name},
        personal{_personal},
        last_seen{_last_seen},
        freq{_freq},
        tstamp{g_get_monotonic_time()} {}


struct EmailHash {
        std::size_t operator()(const std::string& email) const {
                std::size_t djb = 5381; // djb hash
                for (const auto c : email)
                        djb = ((djb << 5) + djb) + g_ascii_tolower(c);
                return djb;
        }
};

struct EmailEqual {
        bool operator()(const std::string& email1, const std::string& email2) const {
                return g_ascii_strcasecmp(email1.c_str(), email2.c_str()) == 0;
        }
};

struct ContactInfoHash {
        std::size_t operator()(const ContactInfo& ci) const {
                std::size_t djb = 5381; // djb hash
                for (const auto c : ci.email)
                        djb = ((djb << 5) + djb) + g_ascii_tolower(c);
                return djb;
        }
};

struct ContactInfoEqual {
        bool operator()(const Mu::ContactInfo& ci1, const Mu::ContactInfo& ci2) const {
                return g_ascii_strcasecmp(ci1.email.c_str(), ci2.email.c_str()) == 0;
        }
};

struct ContactInfoLessThan {
        bool operator()(const Mu::ContactInfo& ci1, const Mu::ContactInfo& ci2) const {

                if (ci1.personal != ci2.personal)
                        return ci1.personal; // personal comes first

                if (ci1.last_seen != ci2.last_seen) // more recent comes first
                        return ci1.last_seen > ci2.last_seen;

                if (ci1.freq != ci2.freq) // more frequent comes first
                        return ci1.freq > ci2.freq;

                return g_ascii_strcasecmp(ci1.email.c_str(), ci2.email.c_str()) < 0;
        }
};

using ContactUMap = std::unordered_map<const std::string, ContactInfo, EmailHash, EmailEqual>;
//using ContactUSet = std::unordered_set<ContactInfo, ContactInfoHash, ContactInfoEqual>;
using ContactSet  = std::set<std::reference_wrapper<const ContactInfo>, ContactInfoLessThan>;

struct Contacts::Private {
        Private(const std::string& serialized):
                contacts_{deserialize(serialized)}
        {}

        ContactUMap deserialize(const std::string&) const;
        std::string serialize() const;

        ContactUMap contacts_;
        std::mutex  mtx_;
};

constexpr auto Separator = "\xff"; // Invalid in UTF-8

ContactUMap
Contacts::Private::deserialize(const std::string& serialized) const
{
        ContactUMap contacts;
        std::stringstream ss{serialized, std::ios_base::in};
        std::string line;

        while (getline (ss, line)) {

                const auto parts = Mu::split (line, Separator);
                if (G_UNLIKELY(parts.size() != 6)) {
                        g_warning ("error: '%s'", line.c_str());
                        continue;
                }

                ContactInfo ci(std::move(parts[0]), // full address
                               parts[1], // email
                               std::move(parts[2]), // name
                               parts[3][0] == '1' ? true : false, // personal
                               (time_t)g_ascii_strtoll(parts[4].c_str(), NULL, 10), // last_seen
                               (std::size_t)g_ascii_strtoll(parts[5].c_str(), NULL, 10)); // freq

                contacts.emplace(std::move(parts[1]), std::move(ci));

        }

        return contacts;
}


Contacts::Contacts (const std::string& serialized) :
        priv_{std::make_unique<Private>(serialized)}
{}

Contacts::~Contacts() = default;

std::string
Contacts::serialize() const
{
        std::lock_guard<std::mutex> l_{priv_->mtx_};
        std::string s;

        for (auto& item: priv_->contacts_) {
                const auto& ci{item.second};
                s += Mu::format("%s%s"
                                 "%s%s"
                                 "%s%s"
                                 "%d%s"
                                 "%" G_GINT64_FORMAT "%s"
                                 "%" G_GINT64_FORMAT "\n",
                                 ci.full_address.c_str(), Separator,
                                 ci.email.c_str(), Separator,
                                 ci.name.c_str(), Separator,
                                 ci.personal ? 1 : 0, Separator,
                                 (gint64)ci.last_seen, Separator,
                                 (gint64)ci.freq);
        }

        return s;
}


// for now, we only care about _not_ having newlines.
static void
wash (std::string& str)
{
        str.erase(std::remove(str.begin(), str.end(), '\n'), str.end());
}


void
Contacts::add (ContactInfo&& ci)
{
        std::lock_guard<std::mutex> l_{priv_->mtx_};

        auto down = g_ascii_strdown (ci.email.c_str(), -1);
        std::string email{down};
        g_free(down);

        auto it = priv_->contacts_.find(email);
        if (it != priv_->contacts_.end()) {
                auto& ci2 = it->second;
                ++ci2.freq;
                if (ci.last_seen > ci2.last_seen) {
                        ci2.last_seen = ci.last_seen;
                        wash(ci.email);
                        ci2.email = std::move(ci.email);
                        if (!ci.name.empty()) {
                                wash(ci.name);
                                ci2.name = std::move(ci.name);
                        }
                }
        }

        wash(ci.name);
        wash(ci.email);
        wash(ci.full_address);

        priv_->contacts_.emplace(
                        ContactUMap::value_type(std::move(email), std::move(ci)));
}


const ContactInfo*
Contacts::_find (const std::string& email) const
{
        std::lock_guard<std::mutex> l_{priv_->mtx_};

        ContactInfo ci{"", email, "", false, 0};
        const auto it = priv_->contacts_.find(ci.email);
        if (it == priv_->contacts_.end())
                return {};
        else
                return &it->second;
}


void
Contacts::clear()
{
        std::lock_guard<std::mutex> l_{priv_->mtx_};

        priv_->contacts_.clear();
}


std::size_t
Contacts::size() const
{
        std::lock_guard<std::mutex> l_{priv_->mtx_};

        return priv_->contacts_.size();
}


void
Contacts::for_each(const EachContactFunc& each_contact) const
{
        std::lock_guard<std::mutex> l_{priv_->mtx_};

        if (!each_contact)
                return; // nothing to do

        // first sort them for 'rank'
        ContactSet sorted;
        for (const auto& item: priv_->contacts_)
                sorted.emplace(item.second);

        for (const auto& ci: sorted)
                each_contact (ci);
}

/// C binding

size_t
mu_contacts_count (const MuContacts *self)
{
        g_return_val_if_fail (self, 0);

        auto myself = reinterpret_cast<const Mu::Contacts*>(self);

        return myself->size();
}

gboolean
mu_contacts_foreach (const MuContacts *self, MuContactsForeachFunc func,
                     gpointer user_data)
{
        g_return_val_if_fail (self, FALSE);
        g_return_val_if_fail (func, FALSE);

        auto myself = reinterpret_cast<const Mu::Contacts*>(self);

        myself->for_each([&](const ContactInfo& ci) {
                 g_return_if_fail (!ci.email.empty());
                 func(ci.full_address.c_str(),
                      ci.email.c_str(),
                      ci.name.empty() ? NULL : ci.name.c_str(),
                      ci.personal,
                      ci.last_seen,
                      ci.freq,
                      ci.tstamp,
                      user_data);
         });

        return TRUE;
}

struct _MuContacts :  public Mu::Contacts {}; /**< c-compat */
