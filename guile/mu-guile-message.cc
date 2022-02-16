/*
** Copyright (C) 2011-2021 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include "mu-guile-message.hh"
#include "mu-message-flags.hh"
#include <config.h>

#include <glib-object.h>
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wredundant-decls"
#include <libguile.h>
#pragma GCC diagnostic pop

#include "mu-guile.hh"

#include <mu-runtime.hh>
#include <mu-store.hh>
#include <mu-query.hh>
#include <mu-msg.hh>
#include <mu-msg-part.hh>

using namespace Mu;

/* pseudo field, not in Xapian */
#define MU_GUILE_MSG_FIELD_ID_TIMESTAMP (MU_MSG_FIELD_ID_NUM + 1)

/* some symbols */
static SCM SYMB_PRIO_LOW, SYMB_PRIO_NORMAL, SYMB_PRIO_HIGH;
static std::array<SCM, AllMessageFlagInfos.size()> SYMB_FLAGS;
static SCM SYMB_CONTACT_TO, SYMB_CONTACT_CC, SYMB_CONTACT_BCC, SYMB_CONTACT_FROM;

struct _MuMsgWrapper {
	MuMsg*   _msg;
	gboolean _unrefme;
};
typedef struct _MuMsgWrapper MuMsgWrapper;
static long                  MSG_TAG;

static gboolean
mu_guile_scm_is_msg(SCM scm)
{
	return SCM_NIMP(scm) && (long)SCM_CAR(scm) == MSG_TAG;
}

SCM
mu_guile_msg_to_scm(MuMsg* msg)
{
	MuMsgWrapper* msgwrap;

	g_return_val_if_fail(msg, SCM_UNDEFINED);

	msgwrap           = (MuMsgWrapper*)scm_gc_malloc(sizeof(MuMsgWrapper), "msg");
	msgwrap->_msg     = msg;
	msgwrap->_unrefme = FALSE;

	SCM_RETURN_NEWSMOB(MSG_TAG, msgwrap);
}

typedef struct {
	MessageFlags		flags;
	SCM			lst;
} FlagData;

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
get_flags_scm(MuMsg* msg)
{
	SCM lst{SCM_EOL};
	const auto flags{mu_msg_get_flags(msg)};

	for (auto i = 0; i != AllMessageFlagInfos.size(); ++i) {
		const auto& info{AllMessageFlagInfos.at(i)};
		if (any_of(info.flag & flags))
			scm_append_x(scm_list_2(lst, scm_list_1(SYMB_FLAGS.at(i))));
	}

	return lst;
}

static SCM
get_prio_scm(MuMsg* msg)
{
	switch (mu_msg_get_prio(msg)) {
	case MessagePriority::Low: return SYMB_PRIO_LOW;
	case MessagePriority::Normal: return SYMB_PRIO_NORMAL;
	case MessagePriority::High: return SYMB_PRIO_HIGH;

	default: g_return_val_if_reached(SCM_UNDEFINED);
	}
}

static SCM
msg_string_list_field(MuMsg* msg, MuMsgFieldId mfid)
{
	SCM           scmlst;
	const GSList* lst;

	lst = mu_msg_get_field_string_list(msg, mfid);

	for (scmlst = SCM_EOL; lst; lst = g_slist_next(lst)) {
		SCM item;
		item   = scm_list_1(mu_guile_scm_from_str((const char*)lst->data));
		scmlst = scm_append_x(scm_list_2(scmlst, item));
	}

	return scmlst;
}

static SCM
get_body(MuMsg* msg, gboolean html)
{
	SCM          data;
	const char*  body;
	MuMsgOptions opts;

	opts = MU_MSG_OPTION_NONE;

	if (html)
		body = mu_msg_get_body_html(msg, opts);
	else
		body = mu_msg_get_body_text(msg, opts);

	if (body)
		data = mu_guile_scm_from_str(body);
	else
		data = SCM_BOOL_F;

	/* explicitly close the file backend, so we won't run of fds */
	mu_msg_unload_msg_file(msg);

	return data;
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
	MuMsgWrapper* msgwrap;
	MuMsgFieldId  mfid;
	msgwrap = (MuMsgWrapper*)SCM_CDR(MSG);

	MU_GUILE_INITIALIZED_OR_ERROR;

	SCM_ASSERT(mu_guile_scm_is_msg(MSG), MSG, SCM_ARG1, FUNC_NAME);
	SCM_ASSERT(scm_integer_p(FIELD), FIELD, SCM_ARG2, FUNC_NAME);

	mfid = scm_to_int(FIELD);
	SCM_ASSERT(mfid < MU_MSG_FIELD_ID_NUM || mfid == MU_GUILE_MSG_FIELD_ID_TIMESTAMP,
	           FIELD,
	           SCM_ARG2,
	           FUNC_NAME);

	switch (mfid) {
	case MU_MSG_FIELD_ID_PRIO: return get_prio_scm(msgwrap->_msg);
	case MU_MSG_FIELD_ID_FLAGS: return get_flags_scm(msgwrap->_msg);

	case MU_MSG_FIELD_ID_BODY_HTML: return get_body(msgwrap->_msg, TRUE);
	case MU_MSG_FIELD_ID_BODY_TEXT: return get_body(msgwrap->_msg, FALSE);

	/* our pseudo-field; we get it from the message file */
	case MU_GUILE_MSG_FIELD_ID_TIMESTAMP:
		return scm_from_uint((unsigned)mu_msg_get_timestamp(msgwrap->_msg));
	default: break;
	}

	switch (mu_msg_field_type(mfid)) {
	case MU_MSG_FIELD_TYPE_STRING:
		return mu_guile_scm_from_str(mu_msg_get_field_string(msgwrap->_msg, mfid));
	case MU_MSG_FIELD_TYPE_BYTESIZE:
	case MU_MSG_FIELD_TYPE_TIME_T:
		return scm_from_uint(mu_msg_get_field_numeric(msgwrap->_msg, mfid));
	case MU_MSG_FIELD_TYPE_INT:
		return scm_from_int(mu_msg_get_field_numeric(msgwrap->_msg, mfid));
	case MU_MSG_FIELD_TYPE_STRING_LIST: return msg_string_list_field(msgwrap->_msg, mfid);
	default: SCM_ASSERT(0, FIELD, SCM_ARG2, FUNC_NAME);
	}
}
#undef FUNC_NAME

struct _EachContactData {
	SCM              lst;
	MuMsgContactType ctype;
};
typedef struct _EachContactData EachContactData;

static void
contacts_to_list(MuMsgContact* contact, EachContactData* ecdata)
{
	SCM item;

	if (ecdata->ctype != MU_MSG_CONTACT_TYPE_ALL &&
	    mu_msg_contact_type(contact) != ecdata->ctype)
		return;

	item = scm_list_1(scm_cons(mu_guile_scm_from_str(mu_msg_contact_name(contact)),
	                           mu_guile_scm_from_str(mu_msg_contact_email(contact))));

	ecdata->lst = scm_append_x(scm_list_2(ecdata->lst, item));
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
	MuMsgWrapper*   msgwrap;
	EachContactData ecdata;

	MU_GUILE_INITIALIZED_OR_ERROR;

	SCM_ASSERT(mu_guile_scm_is_msg(MSG), MSG, SCM_ARG1, FUNC_NAME);
	SCM_ASSERT(scm_symbol_p(CONTACT_TYPE) || scm_is_bool(CONTACT_TYPE),
	           CONTACT_TYPE,
	           SCM_ARG2,
	           FUNC_NAME);

	if (CONTACT_TYPE == SCM_BOOL_F)
		return SCM_UNSPECIFIED; /* nothing to do */
	else if (CONTACT_TYPE == SCM_BOOL_T)
		ecdata.ctype = MU_MSG_CONTACT_TYPE_ALL;
	else {
		if (scm_is_eq(CONTACT_TYPE, SYMB_CONTACT_TO))
			ecdata.ctype = MU_MSG_CONTACT_TYPE_TO;
		else if (scm_is_eq(CONTACT_TYPE, SYMB_CONTACT_CC))
			ecdata.ctype = MU_MSG_CONTACT_TYPE_CC;
		else if (scm_is_eq(CONTACT_TYPE, SYMB_CONTACT_BCC))
			ecdata.ctype = MU_MSG_CONTACT_TYPE_BCC;
		else if (scm_is_eq(CONTACT_TYPE, SYMB_CONTACT_FROM))
			ecdata.ctype = MU_MSG_CONTACT_TYPE_FROM;
		else {
			mu_guile_error(FUNC_NAME, 0, "invalid contact type", SCM_UNDEFINED);
			return SCM_UNSPECIFIED;
		}
	}

	ecdata.lst = SCM_EOL;
	msgwrap    = (MuMsgWrapper*)SCM_CDR(MSG);
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-function-type"
	mu_msg_contact_foreach(msgwrap->_msg, (MuMsgContactForeachFunc)contacts_to_list, &ecdata);
#pragma GCC diagnostic pop

	/* explicitly close the file backend, so we won't run out of fds */
	mu_msg_unload_msg_file(msgwrap->_msg);

	return ecdata.lst;
}
#undef FUNC_NAME

struct _AttInfo {
	SCM      attlist;
	gboolean attachments_only;
};
typedef struct _AttInfo AttInfo;

static void
each_part(MuMsg* msg, MuMsgPart* part, AttInfo* attinfo)
{
	char *mime_type, *filename;
	SCM   elm;

	if (!part->type)
		return;
	if (attinfo->attachments_only && !mu_msg_part_maybe_attachment(part))
		return;

	mime_type = g_strdup_printf("%s/%s", part->type, part->subtype);
	filename  = mu_msg_part_get_filename(part, FALSE);

	elm = scm_list_5(
	    /* msg */
	    mu_guile_scm_from_str(mu_msg_get_path(msg)),
	    /* index */
	    scm_from_uint(part->index),
	    /* filename or #f */
	    filename ? mu_guile_scm_from_str(filename) : SCM_BOOL_F,
	    /* mime-type */
	    mime_type ? mu_guile_scm_from_str(mime_type) : SCM_BOOL_F,
	    /* size */
	    part->size > 0 ? scm_from_uint(part->size) : SCM_BOOL_F);

	g_free(mime_type);
	g_free(filename);

	attinfo->attlist = scm_cons(elm, attinfo->attlist);
}

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
	MuMsgWrapper* msgwrap;
	AttInfo       attinfo;

	MU_GUILE_INITIALIZED_OR_ERROR;

	SCM_ASSERT(mu_guile_scm_is_msg(MSG), MSG, SCM_ARG1, FUNC_NAME);
	SCM_ASSERT(scm_is_bool(ATTS_ONLY), ATTS_ONLY, SCM_ARG2, FUNC_NAME);

	attinfo.attlist          = SCM_EOL; /* empty list */
	attinfo.attachments_only = ATTS_ONLY == SCM_BOOL_T ? TRUE : FALSE;

	msgwrap = (MuMsgWrapper*)SCM_CDR(MSG);
	mu_msg_part_foreach(msgwrap->_msg,
	                    MU_MSG_OPTION_NONE,
	                    (MuMsgPartForeachFunc)each_part,
	                    &attinfo);

	/* explicitly close the file backend, so we won't run of fds */
	mu_msg_unload_msg_file(msgwrap->_msg);

	return attinfo.attlist;
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
	MuMsgWrapper* msgwrap;
	char*         header;
	SCM           val;

	MU_GUILE_INITIALIZED_OR_ERROR;

	SCM_ASSERT(mu_guile_scm_is_msg(MSG), MSG, SCM_ARG1, FUNC_NAME);
	SCM_ASSERT(scm_is_string(HEADER) || HEADER == SCM_UNDEFINED, HEADER, SCM_ARG2, FUNC_NAME);

	msgwrap = (MuMsgWrapper*)SCM_CDR(MSG);
	header  = scm_to_utf8_string(HEADER);
	val     = mu_guile_scm_from_str(mu_msg_get_header(msgwrap->_msg, header));
	free(header);

	/* explicitly close the file backend, so we won't run of fds */
	mu_msg_unload_msg_file(msgwrap->_msg);

	return val;
}
#undef FUNC_NAME

static Mu::Option<Mu::QueryResults>
get_query_results(Mu::Store& store, const char* expr, int maxnum)
{
	return store.run_query(expr, MU_MSG_FIELD_ID_NONE, Mu::QueryFlags::None, maxnum);
}

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
		expr = strdup(""); /* note, "" matches *all* messages */
	else
		expr = scm_to_utf8_string(EXPR);

	const auto res{get_query_results(mu_guile_store(), expr, scm_to_int(MAXNUM))};
	free(expr);
	if (!res)
		return SCM_UNSPECIFIED;

	for (auto&& mi : *res) {
		auto msg{mi.floating_msg()};
		if (msg) {
			auto msgsmob{mu_guile_msg_to_scm(mu_msg_ref(msg))};
			scm_call_1(FUNC, msgsmob);
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

static struct {
	const char* name;
	unsigned    val;
} VAR_PAIRS[] = {

    {"mu:field:bcc", MU_MSG_FIELD_ID_BCC},
    {"mu:field:body-html", MU_MSG_FIELD_ID_BODY_HTML},
    {"mu:field:body-txt", MU_MSG_FIELD_ID_BODY_TEXT},
    {"mu:field:cc", MU_MSG_FIELD_ID_CC},
    {"mu:field:date", MU_MSG_FIELD_ID_DATE},
    {"mu:field:flags", MU_MSG_FIELD_ID_FLAGS},
    {"mu:field:from", MU_MSG_FIELD_ID_FROM},
    {"mu:field:maildir", MU_MSG_FIELD_ID_MAILDIR},
    {"mu:field:message-id", MU_MSG_FIELD_ID_MSGID},
    {"mu:field:path", MU_MSG_FIELD_ID_PATH},
    {"mu:field:prio", MU_MSG_FIELD_ID_PRIO},
    {"mu:field:refs", MU_MSG_FIELD_ID_REFS},
    {"mu:field:size", MU_MSG_FIELD_ID_SIZE},
    {"mu:field:subject", MU_MSG_FIELD_ID_SUBJECT},
    {"mu:field:tags", MU_MSG_FIELD_ID_TAGS},
    {"mu:field:to", MU_MSG_FIELD_ID_TO},

    /* non-Xapian field: timestamp */
    {"mu:field:timestamp", MU_GUILE_MSG_FIELD_ID_TIMESTAMP}};

static void
define_vars(void)
{
	unsigned u;
	for (u = 0; u != G_N_ELEMENTS(VAR_PAIRS); ++u) {
		scm_c_define(VAR_PAIRS[u].name, scm_from_uint(VAR_PAIRS[u].val));
		scm_c_export(VAR_PAIRS[u].name, NULL);
	}
}

static SCM
msg_mark(SCM msg_smob)
{
	MuMsgWrapper* msgwrap;
	msgwrap = (MuMsgWrapper*)SCM_CDR(msg_smob);

	msgwrap->_unrefme = TRUE;

	return SCM_UNSPECIFIED;
}

static size_t
msg_free(SCM msg_smob)
{
	MuMsgWrapper* msgwrap;
	msgwrap = (MuMsgWrapper*)SCM_CDR(msg_smob);

	if (msgwrap->_unrefme)
		mu_msg_unref(msgwrap->_msg);

	return sizeof(MuMsgWrapper);
}

static int
msg_print(SCM msg_smob, SCM port, scm_print_state* pstate)
{
	MuMsgWrapper* msgwrap;
	msgwrap = (MuMsgWrapper*)SCM_CDR(msg_smob);

	scm_puts("#<msg ", port);

	if (msg_smob == SCM_BOOL_F)
		scm_puts("#f", port);
	else
		scm_puts(mu_msg_get_path(msgwrap->_msg), port);

	scm_puts(">", port);

	return 1;
}

void*
mu_guile_message_init(void* data)
{
	MSG_TAG = scm_make_smob_type("msg", sizeof(MuMsgWrapper));

	scm_set_smob_mark(MSG_TAG, msg_mark);
	scm_set_smob_free(MSG_TAG, msg_free);
	scm_set_smob_print(MSG_TAG, msg_print);

	define_vars();
	define_symbols();

#ifndef SCM_MAGIC_SNARFER
#include "mu-guile-message.x"
#endif /*SCM_MAGIC_SNARFER*/

	return NULL;
}
