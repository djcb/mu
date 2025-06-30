/*
** Copyright (C) 2025 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-scm-types.hh"
#include "message/mu-message.hh"
#include <mutex>

using namespace Mu;
using namespace Mu::Scm;

namespace {
static SCM message_type;
static bool initialized;

std::mutex map_lock;

constexpr auto max_message_map_size{512};

struct MessageObject {
	Message message;
	SCM foreign_object{};
};
using MessageMap = std::unordered_map<std::string, MessageObject>;
static MessageMap message_map;
}

static const Message&
to_message(SCM scm, const char *func, int pos)
{
	if (!SCM_IS_A_P(scm, message_type))
		throw ScmError{ScmError::Id::WrongType, func, pos, scm, "mesagestore"};

	return *reinterpret_cast<Message*>(scm_foreign_object_ref(scm, 0));
}

static void
finalize_message(SCM scm)
{
	std::unique_lock lock{map_lock};
	const auto msg = reinterpret_cast<const Message*>(scm_foreign_object_ref(scm, 0));
	//mu_debug("finalizing message @ {}", msg->path());
	if (const auto n = message_map.erase(msg->path()); n != 1)
		mu_warning("huh?! deleted {}", n);
}

static SCM
subr_message_object_make(SCM message_path_scm) try {
	// message objects eat fds, tickle the gc... letting it handle it
	// automatically is not soon enough.
	if (message_map.size() >= 0.8 * max_message_map_size)
		scm_gc();

	std::unique_lock lock{map_lock};

	// qttempt to give an good error message rather then getting something
	// from GMime)
	if (message_map.size() >= max_message_map_size)
		throw ScmError{"make-message", "too many open messages"};

	// if we already have the message in our map, return it.
	auto path{from_scm<std::string>(message_path_scm, "make-message", 1)};
	if (const auto it = message_map.find(path); it != message_map.end())
		return it->second.foreign_object;

	// don't have it yet; attempt to create one
	if (auto res{Message::make_from_path(path)}; !res)
		throw ScmError{"make-message", "failed to create message"};
	else {
		// create a new object, store it in our map and return the foreign ptr.
		std::pair<std::string, MessageObject> item {path, MessageObject{std::move(*res), {}}};
		auto it = message_map.emplace(std::move(item));
		return it.first->second.foreign_object = scm_make_foreign_object_1(
			message_type, const_cast<Message*>(&it.first->second.message));
	}
} catch (const ScmError& err) {
	err.throw_scm();
}

static SCM
subr_message_body(SCM message_scm, SCM html_scm) try {
	const auto& message{to_message(message_scm, "body", 1)};
	const auto html{from_scm<bool>(html_scm, "message-body", 2)};
	if (const auto body{html ? message.body_html() : message.body_text()}; body)
		return to_scm(*body);
	else
		return SCM_BOOL_F;
} catch (const ScmError& err) {
	err.throw_scm();
}

static SCM
subr_message_header(SCM message_scm, SCM field_scm) try {
	const auto& message{to_message(message_scm, "header", 1)};
	const auto field{from_scm<std::string>(field_scm, "message-header", 2)};

	if (const auto val{message.header(field)}; val)
		return to_scm(*val);
	else
		return SCM_BOOL_F;
} catch (const ScmError& err) {
	err.throw_scm();
}

static void
init_subrs()
{
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-function-type"
	scm_c_define_gsubr("message-object-make", 1/*req*/, 0/*opt*/, 0/*rst*/,
			   reinterpret_cast<scm_t_subr>(subr_message_object_make));
	scm_c_define_gsubr("message-body", 2/*req*/, 0/*opt*/, 0/*rst*/,
			   reinterpret_cast<scm_t_subr>(subr_message_body));
	scm_c_define_gsubr("message-header",2/*req*/, 0/*opt*/, 0/*rst*/,
			   reinterpret_cast<scm_t_subr>(subr_message_header));
#pragma GCC diagnostic pop
}


void
Mu::Scm::init_message()
{
	if (initialized)
		return;

	message_type = scm_make_foreign_object_type(
		make_symbol("message"),
		scm_list_1(make_symbol("data")),
		finalize_message);

	init_subrs();
	initialized = true;
}
