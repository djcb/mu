/*
** Copyright (C) 2019-2024 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-contacts-cache.hh"

#include <mutex>
#include <unordered_map>
#include <set>
#include <sstream>
#include <functional>
#include <algorithm>
#include <ctime>

#include <utils/mu-utils.hh>
#include <utils/mu-regex.hh>
#include <glib.h>

using namespace Mu;

struct EmailHash {
	std::size_t operator()(const std::string& email) const {
		return lowercase_hash(email);
	}
};
struct EmailEqual {
	bool operator()(const std::string& email1, const std::string& email2) const {
		return lowercase_hash(email1) == lowercase_hash(email2);
	}
};

using ContactUMap = std::unordered_map<const std::string, Contact, EmailHash, EmailEqual>;
struct ContactsCache::Private {
	Private(Config& config_db)
		:config_db_{config_db},
		 contacts_{deserialize(config_db_.get<Config::Id::Contacts>())},
		 personal_plain_{make_matchers<Config::Id::PersonalAddresses>()},
		 personal_rx_{make_rx_matchers<Config::Id::PersonalAddresses>()},
		 ignored_plain_{make_matchers<Config::Id::IgnoredAddresses>()},
		 ignored_rx_{make_rx_matchers<Config::Id::IgnoredAddresses>()},
		 dirty_{0},
		 email_rx_{unwrap(Regex::make(email_rx_str, G_REGEX_OPTIMIZE))}
		{}

	~Private() { serialize(); }

	ContactUMap deserialize(const std::string&) const;
	void serialize() const;

	bool is_valid_email(const std::string& email) const {
		return email_rx_.matches(email);
	}

	Config&			config_db_;
	ContactUMap		contacts_;
	mutable std::mutex	mtx_;

	const StringVec			personal_plain_;
	const std::vector<Regex>	personal_rx_;

	const StringVec			ignored_plain_;
	const std::vector<Regex>	ignored_rx_;

	mutable size_t	dirty_;
	Regex		email_rx_;

private:
	static bool is_rx(const std::string& p)  {
		return p.size() >= 2 && p.at(0) == '/' && p.at(p.length() - 1) == '/';
	}

	template<Config::Id Id> StringVec make_matchers() const {
		return seq_remove(config_db_.get<Id>(), is_rx);
	}
	template<Config::Id Id> std::vector<Regex> make_rx_matchers() const {
		std::vector<Regex> rxvec;
		for (auto&& p: config_db_.get<Id>()) {

			if (!is_rx(p))
				continue;
			constexpr auto opts{static_cast<GRegexCompileFlags>(G_REGEX_OPTIMIZE|G_REGEX_CASELESS)};

			const auto rxstr{p.substr(1, p.length() - 2)};
			try {
				rxvec.push_back(unwrap(Regex::make(rxstr, opts)));
				mu_debug("match {}: '{}' {}", Config::property<Id>().name,
					 p, rxvec.back());
			} catch (const Error& rex) {
				mu_warning("invalid personal address regexp '{}': {}",
					   p, rex.what());
			}
		}
		return rxvec;
	}

	/* regexp as per:
	 *   https://html.spec.whatwg.org/multipage/input.html#valid-e-mail-address
	 *
	 * "This requirement is a willful violation of RFC 5322, which defines a
	 * syntax for email addresses that is simultaneously too strict (before
	 * the "@" character), too vague (after the "@" character), and too lax
	 * (allowing comments, whitespace characters, and quoted strings in
	 * manners unfamiliar to most users) to be of practical use here."
	 */
	static constexpr auto email_rx_str =
		R"(^[a-zA-Z0-9.!#$%&'*+\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$)";

};

ContactUMap
ContactsCache::Private::deserialize(const std::string& serialized) const
{
	ContactUMap       contacts;
	std::stringstream ss{serialized, std::ios_base::in};
	std::string       line;

	while (getline(ss, line)) {
		const auto parts = Mu::split(line, SepaChar2);
		if (G_UNLIKELY(parts.size() != 5)) {
			mu_warning("error: '{}'", line);
			continue;
		}
		Contact ci(parts[0],                                                  // email
			   std::move(parts[1]),                                       // name
			   (time_t)g_ascii_strtoll(parts[3].c_str(), NULL, 10),       // message_date
			   parts[2][0] == '1' ? true : false,                         // personal
			   (std::size_t)g_ascii_strtoll(parts[4].c_str(), NULL, 10),  // frequency
			   g_get_monotonic_time());                                   // tstamp
		contacts.emplace(std::move(parts[0]), std::move(ci));
	}

	return contacts;
}


void
ContactsCache::Private::serialize() const
{
	if (config_db_.read_only()) {
		if (dirty_ > 0)
			mu_critical("dirty data in read-only ccache!"); // bug
		return;
	}

	std::string     s;
	std::unique_lock lock(mtx_);

	if (dirty_ == 0)
		return; // nothing to do.

	for (auto& item : contacts_) {
		const auto& ci{item.second};
		s += mu_format("{}{}{}{}{}{}{}{}{}\n",
			       ci.email, SepaChar2,
			       ci.name, SepaChar2,
			       ci.personal ? 1 : 0, SepaChar2,
			       ci.message_date, SepaChar2,
			       ci.frequency);
	}
	config_db_.set<Config::Id::Contacts>(s);
	dirty_ = 0;
}

ContactsCache::ContactsCache(Config& config_db)
	: priv_{std::make_unique<Private>(config_db)}
{}

ContactsCache::~ContactsCache() = default;

void
ContactsCache::serialize() const
{
	if (priv_->config_db_.read_only())
		throw std::runtime_error("cannot serialize read-only contacts-cache");

	priv_->serialize();
}

void
ContactsCache::add(Contact&& contact)
{
	/* we do _not_ cache invalid email addresses, so we won't offer them in
	 * completions etc. It should be _rare_, but we've seen cases ( broken
	 * local messages, and various "fake" messages RSS2Imap etc. */
	if (!is_valid(contact.email)) {
		mu_debug("not caching invalid e-mail address '{}'", contact.email);
		return;
	}

	if (is_ignored(contact.email)) {
		/* ignored this address, e.g. 'noreply@example.com */
		return;
	}

	std::lock_guard<std::mutex> l_{priv_->mtx_};

	++priv_->dirty_;

	auto it = priv_->contacts_.find(contact.email);

	if (it == priv_->contacts_.end()) { // completely new contact

		contact.name		 = contact.name;
		if (!contact.personal)
			contact.personal = is_personal(contact.email);
		contact.tstamp		 = g_get_monotonic_time();

		auto email{contact.email};
		// return priv_->contacts_.emplace(ContactUMap::value_type(email, std::move(contact)))
		//     .first->second;
		mu_debug("adding contact {} <{}>", contact.name.c_str(), contact.email.c_str());
		priv_->contacts_.emplace(ContactUMap::value_type(email, std::move(contact)));

	} else {	// existing contact.
		auto& existing{it->second};
		++existing.frequency;
		if (contact.message_date > existing.message_date) {	// update?
			existing.email	      = std::move(contact.email);
			// update name only if new one is not empty.
			if (!contact.name.empty())
				existing.name = std::move(contact.name);
			existing.tstamp	      = g_get_monotonic_time();
			existing.message_date = contact.message_date;
		}
		mu_debug("updating contact {} <{}> ({})",
			 contact.name, contact.email, existing.frequency);
	}
}


void
ContactsCache::add(Contacts&& contacts, bool& personal)
{
	personal = seq_find_if(contacts,[&](auto&& c){
		return is_personal(c.email); }) != contacts.cend();

	for (auto&& contact: contacts) {
		contact.personal = personal;
		add(std::move(contact));
	}
}

const Contact*
ContactsCache::_find(const std::string& email) const
{
	std::lock_guard<std::mutex> l_{priv_->mtx_};

	const auto it = priv_->contacts_.find(email);
	if (it == priv_->contacts_.end())
		return {};
	else
		return &it->second;
}

void
ContactsCache::clear()
{
	std::lock_guard<std::mutex> l_{priv_->mtx_};

	++priv_->dirty_;

	priv_->contacts_.clear();
}

std::size_t
ContactsCache::size() const
{
	std::lock_guard<std::mutex> l_{priv_->mtx_};

	return priv_->contacts_.size();
}


/**
 * This is used for sorting the Contacts in order of relevance. A highly
 * specific algorithm, but the details don't matter _too_ much.
 *
 * This is currently used for the ordering in mu-cfind and auto-completion in
 * mu4e, if the various completion methods don't override it...
 */
constexpr auto RecentOffset{15 * 24 * 3600};
struct ContactLessThan {
	ContactLessThan()
	    : recently_{::time({}) - RecentOffset} {}

	bool operator()(const Mu::Contact& ci1, const Mu::Contact& ci2) const {
		// non-personal is less relevant.
		if (ci1.personal != ci2.personal)
			return ci1.personal < ci2.personal;

		// older is less relevant for recent messages
		if (std::max(ci1.message_date, ci2.message_date) > recently_ &&
		    ci1.message_date != ci2.message_date)
			return ci1.message_date < ci2.message_date;

		// less frequent is less relevant
		if (ci1.frequency != ci2.frequency)
			return ci1.frequency < ci2.frequency;

		// if all else fails, alphabetically
		return ci1.email < ci2.email;
	}
	// only sort recently seen contacts by recency; approx 15 days.
	// this changes during the lifetime, but that's all fine.
	const time_t recently_;
};

using ContactSet = std::set<std::reference_wrapper<const Contact>,
			    ContactLessThan>;

void
ContactsCache::for_each(const EachContactFunc& each_contact) const
{
	std::lock_guard<std::mutex> l_{priv_->mtx_};

	// first sort them for 'rank'
	ContactSet sorted;
	for (const auto& item : priv_->contacts_)
		sorted.emplace(item.second);

	// return in _reverse_ order, so we get the most relevant ones first.
	for (auto it = sorted.rbegin(); it != sorted.rend(); ++it) {
		if (!each_contact(*it))
			break;
	}
}

static bool
address_matches(const std::string& addr, const StringVec& plain, const std::vector<Regex>& regexes)
{
	for (auto&& p : plain)
		if (g_ascii_strcasecmp(addr.c_str(), p.c_str()) == 0)
			return true;

	for (auto&& rx : regexes) {
		if (rx.matches(addr))
			return true;
	}

	return false;
}

bool
ContactsCache::is_personal(const std::string& addr) const
{
	return address_matches(addr, priv_->personal_plain_, priv_->personal_rx_);
}

bool
ContactsCache::is_ignored(const std::string& addr) const
{
	return address_matches(addr, priv_->ignored_plain_, priv_->ignored_rx_);
}

bool
ContactsCache::is_valid(const std::string& addr) const
{
	return priv_->is_valid_email(addr);
}


#ifdef BUILD_TESTS
/*
 * Tests.
 *
 */

#include "utils/mu-test-utils.hh"

static void
test_mu_contacts_cache_base()
{
	MemDb xdb{};
	Config cdb{xdb};
	ContactsCache contacts(cdb);

	g_assert_true(contacts.empty());
	g_assert_cmpuint(contacts.size(), ==, 0);

	contacts.add(Mu::Contact("foo.bar@example.com",
					"Foo", {}, 12345));
	g_assert_false(contacts.empty());
	g_assert_cmpuint(contacts.size(), ==, 1);

	contacts.add(Mu::Contact("cuux@example.com", "Cuux", {},
					54321));

	g_assert_cmpuint(contacts.size(), ==, 2);

	contacts.add(
	    Mu::Contact("foo.bar@example.com", "Foo", {}, 77777));
	g_assert_cmpuint(contacts.size(), ==, 2);

	contacts.add(
	    Mu::Contact("Foo.Bar@Example.Com", "Foo", {}, 88888));
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
test_mu_contacts_cache_personal()
{
	MemDb xdb{};
	Config cdb{xdb};
	cdb.set<Config::Id::PersonalAddresses>
		(StringVec{{"foo@example.com", "bar@cuux.org", "/bar-.*@fnorb.f./"}});
	ContactsCache  contacts{cdb};

	g_assert_true(contacts.is_personal("foo@example.com"));
	g_assert_true(contacts.is_personal("Bar@CuuX.orG"));
	g_assert_true(contacts.is_personal("bar-123abc@fnorb.fi"));
	g_assert_true(contacts.is_personal("bar-zzz@fnorb.fr"));

	g_assert_false(contacts.is_personal("foo@bar.com"));
	g_assert_false(contacts.is_personal("BÂr@CuuX.orG"));
	g_assert_false(contacts.is_personal("bar@fnorb.fi"));
	g_assert_false(contacts.is_personal("bar-zzz@fnorb.xr"));
}

static void
test_mu_contacts_cache_ignored()
{
	MemDb xdb{};
	Config cdb{xdb};
	cdb.set<Config::Id::IgnoredAddresses>
		(StringVec{{"foo@example.com", "bar@cuux.org", "/bar-.*@fnorb.f./"}});
	ContactsCache  contacts{cdb};

	g_assert_true(contacts.is_ignored("foo@example.com"));
	g_assert_true(contacts.is_ignored("Bar@CuuX.orG"));
	g_assert_true(contacts.is_ignored("bar-123abc@fnorb.fi"));
	g_assert_true(contacts.is_ignored("bar-zzz@fnorb.fr"));

	g_assert_false(contacts.is_ignored("foo@bar.com"));
	g_assert_false(contacts.is_ignored("BÂr@CuuX.orG"));
	g_assert_false(contacts.is_ignored("bar@fnorb.fi"));
	g_assert_false(contacts.is_ignored("bar-zzz@fnorb.xr"));

	g_assert_cmpuint(contacts.size(),==,0);
	contacts.add(Mu::Contact{"a@example.com", "a", 123, true, 1000, 0});
	g_assert_cmpuint(contacts.size(),==,1);
	contacts.add(Mu::Contact{"foo@example.com", "b", 123, true, 1000, 0}); // ignored
	contacts.add(Mu::Contact{"bar-123abc@fnorb.fi", "c", 123, true, 1000, 0}); // ignored
	g_assert_cmpuint(contacts.size(),==,1);
	contacts.add(Mu::Contact{"b@example.com", "d", 123, true, 1000, 0});
	g_assert_cmpuint(contacts.size(),==,2);
}



static void
test_mu_contacts_cache_foreach()
{
	MemDb xdb{};
	Config cdb{xdb};
	ContactsCache ccache(cdb);

	ccache.add(Mu::Contact{"a@example.com", "a", 123, true, 1000, 0});
	ccache.add(Mu::Contact{"b@example.com", "b", 456, true, 1000, 0});

	{
		size_t n{};
		g_assert_false(ccache.empty());
		g_assert_cmpuint(ccache.size(),==,2);
		ccache.for_each([&](auto&& contact) { ++n; return false; });
		g_assert_cmpuint(n,==,1);
	}

	{
		size_t n{};
		g_assert_false(ccache.empty());
		g_assert_cmpuint(ccache.size(),==,2);
		ccache.for_each([&](auto&& contact) { ++n; return true; });
		g_assert_cmpuint(n,==,2);
	}

	{
		size_t n{};
		ccache.clear();
		g_assert_true(ccache.empty());
		g_assert_cmpuint(ccache.size(),==,0);
		ccache.for_each([&](auto&& contact) { ++n; return true; });
		g_assert_cmpuint(n,==,0);
	}
}



static void
test_mu_contacts_cache_sort()
{
	auto result_chars = [](const Mu::ContactsCache& ccache)->std::string {
		std::string str;
		if (g_test_verbose())
			fmt::print("contacts-cache:\n");

		ccache.for_each([&](auto&& contact) {
			if (g_test_verbose())
				fmt::print("\t- {}\n", contact.display_name());
			str += contact.name;
			return true;
		});
		return str;
	};

	const auto now{std::time({})};

	// "first" means more relevant

	{ /* recent messages, newer comes first */

		MemDb xdb{};
		Config cdb{xdb};
		ContactsCache ccache(cdb);

		ccache.add(Mu::Contact{"a@example.com", "a", now, true, 1000, 0});
		ccache.add(Mu::Contact{"b@example.com", "b", now-1, true, 1000, 0});
		assert_equal(result_chars(ccache), "ab");
	}

	{ /* non-recent messages, more frequent comes first */

		MemDb xdb{};
		Config cdb{xdb};
		ContactsCache ccache(cdb);

		ccache.add(Mu::Contact{"a@example.com", "a", now-2*RecentOffset, true, 1000, 0});
		ccache.add(Mu::Contact{"b@example.com", "b", now-3*RecentOffset, true, 2000, 0});
		assert_equal(result_chars(ccache), "ba");
	}

	{ /* personal comes first */
		MemDb xdb{};
		Config cdb{xdb};
		ContactsCache ccache(cdb);

		ccache.add(Mu::Contact{"a@example.com", "a", now-5*RecentOffset, true, 1000, 0});
		ccache.add(Mu::Contact{"b@example.com", "b", now, false, 8000, 0});
		assert_equal(result_chars(ccache), "ab");
	}

	{ /* if all else fails, reverse-alphabetically */
		MemDb xdb{};
		Config cdb{xdb};
		ContactsCache ccache(cdb);

		ccache.add(Mu::Contact{"a@example.com", "a", now, false, 1000, 0});
		ccache.add(Mu::Contact{"b@example.com", "b", now, false, 1000, 0});
		g_assert_cmpuint(ccache.size(),==,2);
		assert_equal(result_chars(ccache), "ba");
	}
}

static void
test_mu_contacts_valid_address()
{
	MemDb xdb{};
	Config cdb{xdb};
	ContactsCache ccache(cdb);

	g_assert_true(ccache.is_valid("a@example.com"));
	g_assert_false(ccache.is_valid("a***@@booa@example..com"));
}

int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	g_test_add_func("/contacts-cache/base", test_mu_contacts_cache_base);
	g_test_add_func("/contacts-cache/personal", test_mu_contacts_cache_personal);
	g_test_add_func("/contacts-cache/ignored", test_mu_contacts_cache_ignored);
	g_test_add_func("/contacts-cache/for-each", test_mu_contacts_cache_foreach);
	g_test_add_func("/contacts-cache/sort", test_mu_contacts_cache_sort);
	g_test_add_func("/contacts-cache/valid-address", test_mu_contacts_valid_address);

	return g_test_run();
}
#endif /*BUILD_TESTS*/
