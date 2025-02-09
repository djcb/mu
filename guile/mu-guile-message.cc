/*
** Copyright (C) 2011-2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <config.h>
#include "mu-guile-message.hh"

#include "message/mu-message.hh"
#include "utils/mu-utils.hh"

#include <glib-object.h>
#include <memory>
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wredundant-decls"
#include <libguile.h>
#pragma GCC diagnostic pop

#include "mu-guile.hh"

#include <mu-store.hh>
#include <mu-query.hh>

using namespace Mu;

/* pseudo field, not in Xapian */
constexpr auto MU_GUILE_MSG_FIELD_ID_TIMESTAMP = Field::id_size() + 1;

/* some symbols */
static SCM	SYMB_PRIO_LOW, SYMB_PRIO_NORMAL, SYMB_PRIO_HIGH;
static std::array<SCM, AllMessageFlagInfos.size()> SYMB_FLAGS;
static SCM	SYMB_CONTACT_TO, SYMB_CONTACT_CC, SYMB_CONTACT_BCC, SYMB_CONTACT_FROM;
static long	MSG_TAG;


using MessageSPtr = std::unique_ptr<Message>;

static gboolean
mu_guile_scm_is_msg(SCM scm)
{
	return SCM_NIMP(scm) && (long)SCM_CAR(scm) == MSG_TAG;
}

static SCM
message_scm_create(Xapian::Document&& doc)
{
	/* placement-new */

	void *scm_mem{scm_gc_malloc(sizeof(Message), "msg")};
	Message* msgp = new(scm_mem)Message(std::move(doc));

	SCM_RETURN_NEWSMOB(MSG_TAG, msgp);
}

static const Message*
message_from_scm(SCM msg_smob)
{
	return reinterpret_cast<Message*>(SCM_CDR(msg_smob));
}

static size_t
message_scm_free(SCM msg_smob)
{
	if (auto msg = message_from_scm(msg_smob); msg)
		msg->~Message();

	return sizeof(Message);
}

static int
message_scm_print(SCM msg_smob, SCM port, scm_print_state* pstate)
{
	scm_puts("#<msg ", port);

	if (auto msg = message_from_scm(msg_smob); msg)
		scm_puts(msg->path().c_str(), port);

	scm_puts(">", port);
	return 1;
}

struct FlagData {
	Flags	flags;
	SCM	lst;
};

#define MU_GUILE_INITIALIZED_OR_ERROR                                            \
	do {                                                                     \
		if (!(mu_guile_initialized())) {                                 \
			mu_guile_error(FUNC_NAME,                                \
				       0,                                        \
				       "mu not initialized; call mu:initialize", \
				       SCM_UNDEFINED);                           \
			return SCM_UNSPECIFIED;                                  \
		}                                                                \
	} while (0)


static SCM
get_flags_scm(const Message& msg)
{
	SCM lst{SCM_EOL};
	const auto flags{msg.flags()};

	for (auto i = 0; i != AllMessageFlagInfos.size(); ++i) {
		const auto& info{AllMessageFlagInfos.at(i)};
		if (any_of(info.flag & flags))
			scm_append_x(scm_list_2(lst, scm_list_1(SYMB_FLAGS.at(i))));
	}

	return lst;
}

static SCM
get_prio_scm(const Message& msg)
{
	switch (msg.priority()) {
	case Priority::Low: return SYMB_PRIO_LOW;
	case Priority::Normal: return SYMB_PRIO_NORMAL;
	case Priority::High: return SYMB_PRIO_HIGH;

	default: g_return_val_if_reached(SCM_UNDEFINED);
	}
}

static SCM
msg_string_list_field(const Message& msg, Field::Id field_id)
{
	SCM           scmlst{SCM_EOL};
	for (auto&& val: msg.document().string_vec_value(field_id)) {
		SCM item;
		item   = scm_list_1(mu_guile_scm_from_string(val));
		scmlst = scm_append_x(scm_list_2(scmlst, item));
	}

	return scmlst;
}

static SCM
msg_contact_list_field(const Message& msg, Field::Id field_id)
{
	return scm_from_utf8_string(
		to_string(msg.document().contacts_value(field_id)).c_str());
}

static SCM
get_body(const Message& msg, bool html)
{
	if (const auto body = html ? msg.body_html() : msg.body_text(); body)
		return mu_guile_scm_from_string(*body);
	else
		return SCM_BOOL_F;
}

SCM_DEFINE(get_field,
	   "mu:c:get-field",
	   2,
	   0,
	   0,
	   (SCM MSG, SCM FIELD),
	   "Get the field FIELD from message MSG.\n")
#define FUNC_NAME s_get_field
{
	SCM_ASSERT(mu_guile_scm_is_msg(MSG), MSG, SCM_ARG1, FUNC_NAME);
	auto msg{message_from_scm(MSG)};
	SCM_ASSERT(msg, MSG, SCM_ARG1, FUNC_NAME);

	SCM_ASSERT(scm_integer_p(FIELD), FIELD, SCM_ARG2, FUNC_NAME);
	const auto field_opt{field_from_number(static_cast<size_t>(scm_to_int(FIELD)))};
	SCM_ASSERT(!!field_opt, FIELD, SCM_ARG2, FUNC_NAME);
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wswitch-enum"
	switch (field_opt->id) {
	case Field::Id::Priority:
		return get_prio_scm(*msg);
	case Field::Id::Flags:
		return get_flags_scm(*msg);
	case Field::Id::BodyText:
		return get_body(*msg, false);
	default:
		break;
	}
#pragma GCC diagnostic pop

	switch (field_opt->type) {
	case Field::Type::String:
		return mu_guile_scm_from_string(msg->document().string_value(field_opt->id));
	case Field::Type::ByteSize:
	case Field::Type::TimeT:
	case Field::Type::Integer:
		return scm_from_uint(msg->document().integer_value(field_opt->id));
	case Field::Type::StringList:
		return msg_string_list_field(*msg, field_opt->id);
	case Field::Type::ContactList:
		return msg_contact_list_field(*msg, field_opt->id);
	default:
		SCM_ASSERT(0, FIELD, SCM_ARG2, FUNC_NAME);
	}

	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static SCM
contacts_to_list(const Message& msg, Option<Field::Id> field_id)
{
	SCM list{SCM_EOL};

	const auto contacts{field_id ?
		msg.document().contacts_value(*field_id) :
		msg.all_contacts()};

	for (auto&& contact: contacts) {
		SCM item{scm_list_1(
				scm_cons(mu_guile_scm_from_string(contact.name),
					 mu_guile_scm_from_string(contact.email)))};
		list = scm_append_x(scm_list_2(list, item));
	}

	return list;
}

SCM_DEFINE(get_contacts,
	   "mu:c:get-contacts",
	   2,
	   0,
	   0,
	   (SCM MSG, SCM CONTACT_TYPE),
	   "Get a list of contact information pairs.\n")
#define FUNC_NAME s_get_contacts
{
	SCM	list;

	MU_GUILE_INITIALIZED_OR_ERROR;

	SCM_ASSERT(mu_guile_scm_is_msg(MSG), MSG, SCM_ARG1, FUNC_NAME);
	auto msg{message_from_scm(MSG)};
	SCM_ASSERT(msg, MSG, SCM_ARG1, FUNC_NAME);

	SCM_ASSERT(scm_symbol_p(CONTACT_TYPE) || scm_is_bool(CONTACT_TYPE),
		   CONTACT_TYPE,
		   SCM_ARG2,
		   FUNC_NAME);

	if (CONTACT_TYPE == SCM_BOOL_F)
		return SCM_UNSPECIFIED; /* nothing to do */

	Option<Field::Id> field_id;
	if (CONTACT_TYPE == SCM_BOOL_T)
		field_id = {}; /* get all */
	else {
		if (scm_is_eq(CONTACT_TYPE, SYMB_CONTACT_TO))
			field_id = Field::Id::To;
		else if (scm_is_eq(CONTACT_TYPE, SYMB_CONTACT_CC))
			field_id = Field::Id::Cc;
		else if (scm_is_eq(CONTACT_TYPE, SYMB_CONTACT_BCC))
			field_id = Field::Id::Bcc;
		else if (scm_is_eq(CONTACT_TYPE, SYMB_CONTACT_FROM))
			field_id = Field::Id::From;
		else {
			mu_guile_error(FUNC_NAME, 0, "invalid contact type", SCM_UNDEFINED);
			return SCM_UNSPECIFIED;
		}
	}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-function-type"
	list = contacts_to_list(*msg, field_id);
#pragma GCC diagnostic pop

	/* explicitly close the file backend, so we won't run out of fds */


	return list;
}
#undef FUNC_NAME

SCM_DEFINE(get_parts,
	   "mu:c:get-parts",
	   1,
	   1,
	   0,
	   (SCM MSG, SCM ATTS_ONLY),
	   "Get the list of mime-parts for MSG. If ATTS_ONLY is #t, only"
	   "get parts that are (look like) attachments. The resulting list has "
	   "elements which are list of the form (index name mime-type size).\n")
#define FUNC_NAME s_get_parts
{
	MU_GUILE_INITIALIZED_OR_ERROR;

	SCM_ASSERT(mu_guile_scm_is_msg(MSG), MSG, SCM_ARG1, FUNC_NAME);
	auto msg{message_from_scm(MSG)};
	SCM_ASSERT(msg, MSG, SCM_ARG1, FUNC_NAME);
	SCM_ASSERT(scm_is_bool(ATTS_ONLY), ATTS_ONLY, SCM_ARG2, FUNC_NAME);

	SCM	attlist          = SCM_EOL;	/* empty list */
	bool	attachments_only = ATTS_ONLY == SCM_BOOL_T ? TRUE : FALSE;

	size_t n{};
	for (auto&& part: msg->parts()) {

		if (attachments_only && !part.is_attachment())
			continue;

		const auto mime_type{part.mime_type()};
		const auto filename{part.cooked_filename()};

		SCM elm = scm_list_5(
			/* msg */
			mu_guile_scm_from_string(msg->path().c_str()),
			/* index */
			scm_from_uint(n++),
			/* filename or #f */
			filename ? mu_guile_scm_from_string(*filename) : SCM_BOOL_F,
			/* mime-type */
			mime_type ? mu_guile_scm_from_string(*mime_type) : SCM_BOOL_F,
			/* size */
			part.size() > 0 ? scm_from_uint(part.size()) : SCM_BOOL_F);

		attlist = scm_cons(elm, attlist);
	}

	/* explicitly close the file backend, so we won't run out of fds */
	msg->unload_mime_message();

	return attlist;
}
#undef FUNC_NAME

SCM_DEFINE(get_header,
	   "mu:c:get-header",
	   2,
	   0,
	   0,
	   (SCM MSG, SCM HEADER),
	   "Get an arbitrary HEADER from MSG.\n")
#define FUNC_NAME s_get_header
{
	MU_GUILE_INITIALIZED_OR_ERROR;

	SCM_ASSERT(mu_guile_scm_is_msg(MSG), MSG, SCM_ARG1, FUNC_NAME);
	auto msg{message_from_scm(MSG)};
	SCM_ASSERT(msg, MSG, SCM_ARG1, FUNC_NAME);

	SCM_ASSERT(scm_is_string(HEADER) || HEADER == SCM_UNDEFINED, HEADER, SCM_ARG2, FUNC_NAME);

	char *header  = scm_to_utf8_string(HEADER);
	SCM val     = mu_guile_scm_from_string(msg->header(header).value_or(""));
	free(header);

	/* explicitly close the file backend, so we won't run of fds */
	msg->unload_mime_message();

	return val;
}
#undef FUNC_NAME
SCM_DEFINE(for_each_message,
	   "mu:c:for-each-message",
	   3,
	   0,
	   0,
	   (SCM FUNC, SCM EXPR, SCM MAXNUM),
	   "Call FUNC for each msg in the message store matching EXPR. EXPR is"
	   "either a string containing a mu search expression or a boolean; in the former "
	   "case, limit the messages to only those matching the expression, in the "
	   "latter case, match /all/ messages if the EXPR equals #t, and match "
	   "none if EXPR equals #f.")
#define FUNC_NAME s_for_each_message
{
	char* expr{};

	MU_GUILE_INITIALIZED_OR_ERROR;

	SCM_ASSERT(scm_procedure_p(FUNC), FUNC, SCM_ARG1, FUNC_NAME);
	SCM_ASSERT(scm_is_bool(EXPR) || scm_is_string(EXPR), EXPR, SCM_ARG2, FUNC_NAME);
	SCM_ASSERT(scm_is_integer(MAXNUM), MAXNUM, SCM_ARG3, FUNC_NAME);

	if (EXPR == SCM_BOOL_F)
		return SCM_UNSPECIFIED; /* nothing to do */

	if (EXPR == SCM_BOOL_T)
		expr = strdup("\"\""); /* note, "" matches *all* messages */
	else
		expr = scm_to_utf8_string(EXPR);

	const auto res = mu_guile_store().run_query(expr,{}, {}, scm_to_int(MAXNUM));
	free(expr);
	if (!res)
		return SCM_UNSPECIFIED;

	for (auto&& mi : *res) {
		if (auto xdoc{mi.document()}; xdoc) {
			scm_call_1(FUNC, message_scm_create(std::move(xdoc.value())));
		}
	}

	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static SCM
register_symbol(const char* name)
{
	SCM scm;

	scm = scm_from_utf8_symbol(name);
	scm_c_define(name, scm);
	scm_c_export(name, NULL);

	return scm;
}

static void
define_symbols(void)
{
	SYMB_CONTACT_TO   = register_symbol("mu:contact:to");
	SYMB_CONTACT_CC   = register_symbol("mu:contact:cc");
	SYMB_CONTACT_FROM = register_symbol("mu:contact:from");
	SYMB_CONTACT_BCC  = register_symbol("mu:contact:bcc");

	SYMB_PRIO_LOW    = register_symbol("mu:prio:low");
	SYMB_PRIO_NORMAL = register_symbol("mu:prio:normal");
	SYMB_PRIO_HIGH   = register_symbol("mu:prio:high");

	for (auto i = 0U; i != AllMessageFlagInfos.size(); ++i) {
		const auto& info{AllMessageFlagInfos.at(i)};
		const auto name = "mu:flag:" + std::string{info.name};
		SYMB_FLAGS[i] = register_symbol(name.c_str());
	}
}
static void
define_vars(void)
{
	field_for_each([](auto&& field){

		auto defvar = [&](auto&& fname, auto&& ffield) {
			const auto name{"mu:field:" + std::string{fname}};
			scm_c_define(name.c_str(), scm_from_uint(field.value_no()));
			scm_c_export(name.c_str(), NULL);
		};

		// define for both name and (if exists) alias.
		if (!field.name.empty())
			defvar(field.name, field);
		if (!field.alias.empty())
			defvar(field.alias, field);
	});

	/* non-Xapian field: timestamp */
	scm_c_define("mu:field:timestamp",
		     scm_from_uint(MU_GUILE_MSG_FIELD_ID_TIMESTAMP));
	scm_c_export("mu:field:timestamp", NULL);

}

void*
mu_guile_message_init(void* data)
{
	MSG_TAG = scm_make_smob_type("message", sizeof(Message));

	scm_set_smob_free(MSG_TAG, message_scm_free);
	scm_set_smob_print(MSG_TAG, message_scm_print);

	define_vars();
	define_symbols();

#ifndef SCM_MAGIC_SNARFER
#include "mu-guile-message.x"
#endif /*SCM_MAGIC_SNARFER*/

	return NULL;
}
