/*
** Copyright (C) 2019-2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <regex>
#include <ctime>

#include <utils/mu-utils.hh>
#include <glib.h>

using namespace Mu;

ContactInfo::ContactInfo(const std::string& _full_address,
                         const std::string& _email,
                         const std::string& _name,
                         bool               _personal,
                         time_t             _last_seen,
                         size_t             _freq)
    : full_address{_full_address}, email{_email}, name{_name}, personal{_personal},
      last_seen{_last_seen}, freq{_freq}, tstamp{g_get_monotonic_time()}
{
}

struct EmailHash {
	std::size_t operator()(const std::string& email) const
	{
		std::size_t djb = 5381; // djb hash
		for (const auto c : email)
			djb = ((djb << 5) + djb) + static_cast<size_t>(g_ascii_tolower(c));
		return djb;
	}
};

struct EmailEqual {
	bool operator()(const std::string& email1, const std::string& email2) const
	{
		return g_ascii_strcasecmp(email1.c_str(), email2.c_str()) == 0;
	}
};

struct ContactInfoHash {
	std::size_t operator()(const ContactInfo& ci) const
	{
		std::size_t djb = 5381; // djb hash
		for (const auto c : ci.email)
			djb = ((djb << 5) + djb) + static_cast<size_t>(g_ascii_tolower(c));
		return djb;
	}
};

struct ContactInfoEqual {
	bool operator()(const Mu::ContactInfo& ci1, const Mu::ContactInfo& ci2) const
	{
		return g_ascii_strcasecmp(ci1.email.c_str(), ci2.email.c_str()) == 0;
	}
};

constexpr auto RecentOffset{15 * 24 * 3600};
struct ContactInfoLessThan {
	ContactInfoLessThan()
	    : recently_{::time({}) - RecentOffset} {}

	bool operator()(const Mu::ContactInfo& ci1, const Mu::ContactInfo& ci2) const
	{
		if (ci1.personal != ci2.personal)
			return ci1.personal; // personal comes first

		if ((ci1.last_seen > recently_) != (ci2.last_seen > recently_))
			return ci1.last_seen > ci2.last_seen;

		if (ci1.freq != ci2.freq) // more frequent comes first
			return ci1.freq > ci2.freq;

		return g_ascii_strcasecmp(ci1.email.c_str(), ci2.email.c_str()) < 0;
	}
	// only sort recently seen contacts by recency; approx 15 days.
	// this changes during the lifetime, but that's all fine.
	const time_t recently_;
};

using ContactUMap = std::unordered_map<const std::string, ContactInfo, EmailHash, EmailEqual>;
// using ContactUSet = std::unordered_set<ContactInfo, ContactInfoHash, ContactInfoEqual>;
using ContactSet = std::set<std::reference_wrapper<const ContactInfo>, ContactInfoLessThan>;

struct Contacts::Private {
	Private(const std::string& serialized, const StringVec& personal)
	    : contacts_{deserialize(serialized)}, dirty_{0}
	{
		make_personal(personal);
	}

	void        make_personal(const StringVec& personal);
	ContactUMap deserialize(const std::string&) const;
	std::string serialize() const;

	ContactUMap contacts_;
	std::mutex  mtx_;

	StringVec               personal_plain_;
	std::vector<std::regex> personal_rx_;

	size_t dirty_;
};

constexpr auto Separator = "\xff"; // Invalid in UTF-8

void
Contacts::Private::make_personal(const StringVec& personal)
{
	for (auto&& p : personal) {
		if (p.empty())
			continue; // invalid

		if (p.size() < 2 || p.at(0) != '/' || p.at(p.length() - 1) != '/')
			personal_plain_.emplace_back(p); // normal address
		else {
			// a regex pattern.
			try {
				const auto rxstr{p.substr(1, p.length() - 2)};
				personal_rx_.emplace_back(std::regex(
				    rxstr,
				    std::regex::basic | std::regex::optimize | std::regex::icase));

			} catch (const std::regex_error& rex) {
				g_warning("invalid personal address regexp '%s': %s",
				          p.c_str(),
				          rex.what());
			}
		}
	}
}

ContactUMap
Contacts::Private::deserialize(const std::string& serialized) const
{
	ContactUMap       contacts;
	std::stringstream ss{serialized, std::ios_base::in};
	std::string       line;

	while (getline(ss, line)) {
		const auto parts = Mu::split(line, Separator);
		if (G_UNLIKELY(parts.size() != 6)) {
			g_warning("error: '%s'", line.c_str());
			continue;
		}

		ContactInfo ci(std::move(parts[0]),                                       // full address
		               parts[1],                                                  // email
		               std::move(parts[2]),                                       // name
		               parts[3][0] == '1' ? true : false,                         // personal
		               (time_t)g_ascii_strtoll(parts[4].c_str(), NULL, 10),       // last_seen
		               (std::size_t)g_ascii_strtoll(parts[5].c_str(), NULL, 10)); // freq

		contacts.emplace(std::move(parts[1]), std::move(ci));
	}

	return contacts;
}

Contacts::Contacts(const std::string& serialized, const StringVec& personal)
    : priv_{std::make_unique<Private>(serialized, personal)}
{
}

Contacts::~Contacts() = default;
std::string
Contacts::serialize() const
{
	std::lock_guard<std::mutex> l_{priv_->mtx_};
	std::string                 s;

	for (auto& item : priv_->contacts_) {
		const auto& ci{item.second};
		s += Mu::format("%s%s"
		                "%s%s"
		                "%s%s"
		                "%d%s"
		                "%" G_GINT64_FORMAT "%s"
		                "%" G_GINT64_FORMAT "\n",
		                ci.full_address.c_str(),
		                Separator,
		                ci.email.c_str(),
		                Separator,
		                ci.name.c_str(),
		                Separator,
		                ci.personal ? 1 : 0,
		                Separator,
		                (gint64)ci.last_seen,
		                Separator,
		                (gint64)ci.freq);
	}

	priv_->dirty_ = 0;

	return s;
}

bool
Contacts::dirty() const
{
	return priv_->dirty_;
}

const ContactInfo
Contacts::add(ContactInfo&& ci)
{
	std::lock_guard<std::mutex> l_{priv_->mtx_};

	++priv_->dirty_;

	auto it = priv_->contacts_.find(ci.email);

	if (it == priv_->contacts_.end()) { // completely new contact

		ci.name         = Mu::remove_ctrl(ci.name);
		ci.full_address = remove_ctrl(ci.full_address);

		auto email{ci.email};
		return priv_->contacts_.emplace(ContactUMap::value_type(email, std::move(ci)))
		    .first->second;

	} else { // existing contact.
		auto& ci_existing{it->second};
		++ci_existing.freq;

		if (ci.last_seen > ci_existing.last_seen) { // update.

			ci_existing.email        = std::move(ci.email);
			ci_existing.name         = Mu::remove_ctrl(ci.name);
			ci_existing.full_address = Mu::remove_ctrl(ci.full_address);

			ci_existing.tstamp    = g_get_monotonic_time();
			ci_existing.last_seen = ci.last_seen;
		}

		return std::move(ci);
	}
}

const ContactInfo*
Contacts::_find(const std::string& email) const
{
	std::lock_guard<std::mutex> l_{priv_->mtx_};

	const auto it = priv_->contacts_.find(email);
	if (it == priv_->contacts_.end())
		return {};
	else
		return &it->second;
}

void
Contacts::clear()
{
	std::lock_guard<std::mutex> l_{priv_->mtx_};

	++priv_->dirty_;

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
	for (const auto& item : priv_->contacts_)
		sorted.emplace(item.second);

	for (const auto& ci : sorted)
		each_contact(ci);
}

bool
Contacts::is_personal(const std::string& addr) const
{
	for (auto&& p : priv_->personal_plain_)
		if (g_ascii_strcasecmp(addr.c_str(), p.c_str()) == 0)
			return true;

	for (auto&& rx : priv_->personal_rx_) {
		std::smatch m; // perhaps cache addr in personal_plain_?
		if (std::regex_match(addr, m, rx))
			return true;
	}

	return false;
}

#ifdef BUILD_TESTS
/*
 * Tests.
 *
 */

#include "test-mu-common.hh"

static void
test_mu_contacts_01()
{
	Mu::Contacts contacts("");

	g_assert_true(contacts.empty());
	g_assert_cmpuint(contacts.size(), ==, 0);

	contacts.add(Mu::ContactInfo("Foo <foo.bar@example.com>",
	                             "foo.bar@example.com",
	                             "Foo",
	                             false,
	                             12345));
	g_assert_false(contacts.empty());
	g_assert_cmpuint(contacts.size(), ==, 1);

	contacts.add(Mu::ContactInfo("Cuux <cuux.fnorb@example.com>",
	                             "cuux@example.com",
	                             "Cuux",
	                             false,
	                             54321));

	g_assert_cmpuint(contacts.size(), ==, 2);

	contacts.add(
	    Mu::ContactInfo("foo.bar@example.com", "foo.bar@example.com", "Foo", false, 77777));
	g_assert_cmpuint(contacts.size(), ==, 2);

	contacts.add(
	    Mu::ContactInfo("Foo.Bar@Example.Com", "Foo.Bar@Example.Com", "Foo", false, 88888));
	g_assert_cmpuint(contacts.size(), ==, 2);
	// note: replaces first.

	{
		const auto info = contacts._find("bla@example.com");
		g_assert_false(info);
	}

	{
		const auto info = contacts._find("foo.BAR@example.com");
		g_assert_true(info);

		g_assert_cmpstr(info->email.c_str(), ==, "Foo.Bar@Example.Com");
	}

	contacts.clear();
	g_assert_true(contacts.empty());
	g_assert_cmpuint(contacts.size(), ==, 0);
}

static void
test_mu_contacts_02()
{
	Mu::StringVec personal = {"foo@example.com", "bar@cuux.org", "/bar-.*@fnorb.f./"};
	Mu::Contacts  contacts{"", personal};

	g_assert_true(contacts.is_personal("foo@example.com"));
	g_assert_true(contacts.is_personal("Bar@CuuX.orG"));
	g_assert_true(contacts.is_personal("bar-123abc@fnorb.fi"));
	g_assert_true(contacts.is_personal("bar-zzz@fnorb.fr"));

	g_assert_false(contacts.is_personal("foo@bar.com"));
	g_assert_false(contacts.is_personal("BÂr@CuuX.orG"));
	g_assert_false(contacts.is_personal("bar@fnorb.fi"));
	g_assert_false(contacts.is_personal("bar-zzz@fnorb.xr"));
}

int
main(int argc, char* argv[])
{
	g_test_init(&argc, &argv, NULL);

	g_test_add_func("/mu-contacts/01", test_mu_contacts_01);
	g_test_add_func("/mu-contacts/02", test_mu_contacts_02);

	g_log_set_handler(
	    NULL,
	    (GLogLevelFlags)(G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL | G_LOG_FLAG_RECURSION),
	    (GLogFunc)black_hole,
	    NULL);

	return g_test_run();
}
#endif /*BUILD_TESTS*/
