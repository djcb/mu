/*
** Copyright (C) 2025-2026 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <algorithm>
#include <atomic>
#include <mutex>
#include <cstdio>

#include "mu-scm-types.hh"
#include "message/mu-message.hh"
#include "message/mu-mime-object.hh"

using namespace Mu;
using namespace Mu::Scm;

namespace {
static SCM message_type;
// weak-value hash table, path -> message foreign-object. Since the values are
// weak, the cache does not keep message objects alive.
static SCM message_cache;
static bool initialized;

std::mutex cache_lock;
// number of live message objects; incremented on creation, decremented
// from the finalizer.
std::atomic<size_t> message_count;

constexpr size_t max_open_messages{512};

// gc-tickling (see subr_cc_message_make): tickle when the live count
// reaches gc_threshold; after each tickle, raise the next tickle-point
// by gc_step (protected by cache_lock).
constexpr size_t gc_threshold{(8 * max_open_messages) / 10};
constexpr size_t gc_step{max_open_messages / 16};
size_t gc_watermark{gc_threshold};
}

static const Message&
to_message(SCM scm, const char *func, int pos)
{
	if (!SCM_IS_A_P(scm, message_type))
		throw ScmError{ScmError::Id::WrongType, func, pos, scm, "message"};

	return *reinterpret_cast<Message*>(scm_foreign_object_ref(scm, 0));
}

static void
finalize_message(SCM scm)
{
	// the foreign object owns its Message; the cache cleans up by itself
	// (weak values), so there is nothing to synchronize with here.
	delete reinterpret_cast<Message*>(scm_foreign_object_ref(scm, 0));
	--message_count;
}

static SCM
subr_cc_message_make(SCM message_path_scm) try {

	constexpr auto func{"cc-message-make"};

	const auto path{from_scm<std::string>(message_path_scm, func, 1)};

	std::unique_lock lock{cache_lock};

	// if we already have a live message object for this path, return it.
	// use a fresh key, so the cache is unaffected by callers mutating
	// their string afterwards.
	SCM key{to_scm(path)};
	if (SCM cached{scm_hash_ref(message_cache, key, SCM_BOOL_F)};
	    scm_is_true(cached))
		return cached;

	// we need to create a new message object; these eat fds, so when
	// nearing the cap, tickle the gc.
	//
	// However, if a script _holds_ references to most of them, collecting
	// cannot lower the count; back off by raising the next tickle-point
	// (and reset it once the count drops below the threshold again), so
	// we avoid gc for every call.
	if (const auto count{message_count.load()}; count < gc_threshold)
		gc_watermark = gc_threshold;
	else if (count >= gc_watermark) {
		scm_gc();
		gc_watermark = std::min(count + gc_step, max_open_messages);
	}

	// attempt to give a good error message rather than getting something
	// from GMime)
	if (message_count >= max_open_messages)
		throw ScmError{func, "too many open messages"};

	// don't have it yet; attempt to create one
	auto res{Message::make_from_path(path)};
	if (!res) {
		mu_printerrln("{}", res.error().what());
		throw ScmError{func, "failed to create message"};
	}

	// the new foreign object owns the Message (finalize_message
	// deletes it); the cache holds only a weak reference.
	SCM msg_scm{scm_make_foreign_object_1(
		message_type, new Message{std::move(*res)})};
	++message_count;
	scm_hash_set_x(message_cache, key, msg_scm);

	return msg_scm;

} catch (const ScmError& err) {
	err.throw_scm();
}

static SCM
subr_cc_message_body(SCM message_scm, SCM html_scm) try {

	constexpr auto func{"cc-message-body"};

	const auto& message{to_message(message_scm, func, 1)};
	const auto html{from_scm<bool>(html_scm, func, 2)};
	if (const auto body{html ? message.body_html() : message.body_text()}; body)
		return to_scm(*body);
	else
		return SCM_BOOL_F;
} catch (const ScmError& err) {
	err.throw_scm();
}

static SCM
subr_cc_message_header(SCM message_scm, SCM field_scm) try {

	constexpr auto func{"cc-message-header"};

	const auto& message{to_message(message_scm, func, 1)};
	const auto field{from_scm<std::string>(field_scm, func, 2)};

	if (const auto val{message.header(field)}; val)
		return to_scm(*val);
	else
		return SCM_BOOL_F;
} catch (const ScmError& err) {
	err.throw_scm();
}

static SCM
subr_cc_message_plist(SCM message_scm) try {

	constexpr auto func{"cc-message-plist"};

	const auto& message{to_message(message_scm, func, 1)};
	// return the serialized (mu4e) message
	const auto plist{message.sexp().to_string()};
	return to_scm(plist);

} catch (const ScmError& err) {
	err.throw_scm();
}



/**
 * Get a list of message's MIME-parts
 *
 * @param message_scm  a Message (foreign-object)
 *
 * @return a list of MIME parts, each is a pair
 *   ( mime-obj . alist )
 * where the mime-obj is the GMimeObject* as a foreign-object,
 * and alist is an association list describing the part.
 */
static SCM
subr_cc_message_parts(SCM message_scm) try {

	constexpr auto func{"cc-message-parts"};

	const auto& message{to_message(message_scm, func, 1)};
	const auto& parts{message.parts()};

	SCM parts_scm{SCM_EOL};
	for (size_t idx = 0; idx != parts.size(); ++idx) {
		auto part{parts[idx]};
		auto mime_part{GMIME_PART(part.mime_object().object())};
		SCM mime_part_scm{to_scm(mime_part)};
		SCM alist_scm{to_scm(idx, parts[idx])};
		SCM item{scm_cons(mime_part_scm, alist_scm)};

		parts_scm = scm_cons(item, parts_scm);
	}

	return scm_reverse_x(parts_scm, SCM_EOL);


} catch (const ScmError& err) {
	err.throw_scm();
}

static void
init_subrs()
{
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-function-type"
	scm_c_define_gsubr("cc-message-make", 1/*req*/, 0/*opt*/, 0/*rst*/,
			   reinterpret_cast<scm_t_subr>(subr_cc_message_make));
	scm_c_define_gsubr("cc-message-body", 2/*req*/, 0/*opt*/, 0/*rst*/,
			   reinterpret_cast<scm_t_subr>(subr_cc_message_body));
	scm_c_define_gsubr("cc-message-header",2/*req*/, 0/*opt*/, 0/*rst*/,
			   reinterpret_cast<scm_t_subr>(subr_cc_message_header));
	scm_c_define_gsubr("cc-message-parts",1/*req*/, 0/*opt*/, 0/*rst*/,
			   reinterpret_cast<scm_t_subr>(subr_cc_message_parts));
	scm_c_define_gsubr("cc-message-plist",1/*req*/, 0/*opt*/, 0/*rst*/,
			   reinterpret_cast<scm_t_subr>(subr_cc_message_plist));
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

	message_cache = scm_make_weak_value_hash_table(
		scm_from_size_t(max_open_messages));

	init_subrs();
	initialized = true;
}
