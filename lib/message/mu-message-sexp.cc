/*
** Copyright (C) 2011-2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <string.h>
#include <ctype.h>

#include "mu-message.hh"
#include "mu-query-results.hh"
#include "utils/mu-str.h"
#include "mu-msg.hh"
#include "mu-msg-part.hh"
#include "mu-maildir.hh"
#include "utils/mu-utils.hh"

using namespace Mu;

static void
add_prop_nonempty(Sexp::List& list, std::string&& elm,
		  std::vector<std::string>&& items)
{
	if (items.empty())
		return;

	Sexp::List elms;
	for(auto&& item: items)
		elms.add(Sexp::make_string(std::move(item)));

	list.add_prop(std::move(elm), Sexp::make_list(std::move(elms)));
}

static void
add_prop_nonempty(Sexp::List& list, std::string&& name, std::string&& value)
{
	if (!value.empty())
		list.add_prop(std::move(name),
			      Sexp::make_string(std::move(value)));
}

static Mu::Sexp
make_contact_sexp(const Contact& contact)
{
	return Sexp::make_list(
		/* name */
		Sexp::make_string(contact.name, true/*?nil*/),
		/* dot */
		Sexp::make_symbol("."),
		/* email */
		Sexp::make_string(contact.email));
}

static void
add_list_post(Sexp::List& list, const Message& message)
{
	if (!message.has_mime_message())
		return;

	/* some mailing lists do not set the reply-to; see pull #1278. So for
	 * those cases, check the List-Post address and use that instead */

	GMatchInfo* minfo;
	GRegex*     rx;
	const auto list_post{message.header("List-Post")};
	if (!list_post)
		return;

	rx = g_regex_new("<?mailto:([a-z0-9!@#$%&'*+-/=?^_`{|}~]+)>?",
			 G_REGEX_CASELESS, (GRegexMatchFlags)0, {});
	g_return_if_fail(rx);

	if (g_regex_match(rx, list_post->c_str(), (GRegexMatchFlags)0, &minfo)) {
		auto    address = (char*)g_match_info_fetch(minfo, 1);
		list.add_prop(":list-post", make_contact_sexp(Contact{address}));
		g_free(address);
	}

	g_match_info_free(minfo);
	g_regex_unref(rx);
}

static void
add_contacts(Sexp::List& list, const Message& message)
{
	auto add_contact_type = [&](const Contacts& contacts, std::string&& prop) {
		Sexp::List clist;
		seq_for_each(contacts, [&](auto&& c) { clist.add(make_contact_sexp(c)); });
		if (!clist.empty())
			list.add_prop(std::move(prop),
				      Sexp::make_list(std::move(clist)));
	};

	add_contact_type(message.from(),":from");
	add_contact_type(message.to(),  ":to");
	add_contact_type(message.cc(),  ":cc");
	add_contact_type(message.bcc(), ":bcc");

	// FIXME: reply-to.
}

static void
add_flags(Sexp::List& list, const Message& message)
{
	Sexp::List flaglist;
	const auto flags{message.flags()};
	for (auto&& info: AllMessageFlagInfos)
		if (any_of(flags & info.flag))
			flaglist.add(Sexp::make_symbol_sv(info.name));

	if (!flaglist.empty())
		list.add_prop(":flags", Sexp::make_list(std::move(flaglist)));
}

static void
add_date_and_size(Sexp::List& items, const Message& message)
{
	auto t{message.date()};
	if (t != 0) {
		Sexp::List dlist;
		dlist.add(Sexp::make_number((unsigned)(t >> 16)));
		dlist.add(Sexp::make_number((unsigned)(t & 0xffff)));
		dlist.add(Sexp::make_number(0));
		items.add_prop(":date", Sexp::make_list(std::move(dlist)));
	}

	auto size{message.size()};
	if (size != 0)
		items.add_prop(":size", Sexp::make_number(size));
}

Mu::Sexp::List
Message::to_sexp_list() const
{
	Sexp::List items;

	// if (docid != 0)
	//	items.add_prop(":docid", Sexp::make_number(docid));

	add_prop_nonempty(items, ":subject", subject());
	add_prop_nonempty(items, ":message-id", message_id());
	add_prop_nonempty(items, ":mailing-list", mailing_list());
	add_prop_nonempty(items, ":path", path());
	add_prop_nonempty(items, ":maildir",maildir());

	items.add_prop(":priority",
		       Sexp::make_symbol_sv(priority_name(priority())));

	add_contacts(items, *this);
	add_list_post(items, *this);

	add_prop_nonempty(items, ":references", references());
	add_prop_nonempty(items, ":tags",       tags());

	add_date_and_size(items, *this);
	add_flags(items, *this);

	return items;
}

Mu::Sexp
Message::to_sexp() const
{
	return Sexp::make_list(to_sexp_list());
}
