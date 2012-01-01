/*
** Copyright (C) 2011 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#if HAVE_CONFIG_H
#include <config.h>
#endif /*HAVE_CONFIG_H*/

#include <glib-object.h>
#include <libguile.h>

#include <mu-runtime.h>
#include <mu-store.h>
#include <mu-query.h>
#include <mu-msg.h>
#include <mu-query.h>
#include <mu-runtime.h>

struct _MuMsgWrapper {
	MuMsg   *_msg;
	gboolean _unrefme;
};
typedef struct _MuMsgWrapper MuMsgWrapper;

static long MSG_TAG;


static SCM
mu_guile_util_error (const char *func_name, int status,
		     const char *fmt, SCM args)
{
	scm_error_scm (scm_from_locale_symbol ("MuError"),
		       scm_from_utf8_string (func_name ? func_name : "<nameless>"),
		       scm_from_utf8_string (fmt), args,
		       scm_list_1 (scm_from_int (status)));

	return SCM_UNSPECIFIED;
}

static SCM
mu_guile_util_g_error (const char *func_name, GError *err)
{
	scm_error_scm (scm_from_locale_symbol ("MuError"),
		       scm_from_utf8_string (func_name),
		       scm_from_utf8_string (err ? err->message : "error"),
		       SCM_UNDEFINED, SCM_UNDEFINED);

	return SCM_UNSPECIFIED;
}


static gboolean
mu_guile_scm_is_msg (SCM scm)
{
	return SCM_NIMP(scm) && (long)SCM_CAR(scm) == MSG_TAG;
}


SCM
mu_guile_msg_to_scm (MuMsg *msg)
{
	MuMsgWrapper *msgwrap;

	g_return_val_if_fail (msg, SCM_UNDEFINED);

	msgwrap = scm_gc_malloc (sizeof (MuMsgWrapper), "msg");
	msgwrap->_msg = msg;
	msgwrap->_unrefme = FALSE;

	SCM_RETURN_NEWSMOB (MSG_TAG, msgwrap);
}

SCM_DEFINE_PUBLIC (msg_make_from_file, "mu:msg:make-from-file", 1, 0, 0,
		   (SCM PATH),
		   "Create a message object based on the message in PATH.\n")
#define FUNC_NAME s_msg_make_from_file
{
	MuMsg *msg;
	GError *err;

	SCM_ASSERT (scm_is_string (PATH), PATH, SCM_ARG1, FUNC_NAME);

	err = NULL;
	msg = mu_msg_new_from_file (scm_to_utf8_string (PATH), NULL, &err);

	if (err) {
		mu_guile_util_g_error (FUNC_NAME, err);
		g_error_free (err);
	}

	return msg ? mu_guile_msg_to_scm (msg) : SCM_UNDEFINED;
}
#undef FUNC_NAME


/* SCM_DEFINE_PUBLIC (msg_move, "mu:msg:move-to-maildir", 2, 0, 0, */
/* 		   (SCM MSG, SCM TARGETMDIR), */
/* 		   "Move message to another maildir TARGETMDIR. Note that this the " */
/* 		   "base-level Maildir, ie. /home/user/Maildir/archive, and must" */
/* 		   " _not_ include the 'cur' or 'new' part. mu_msg_move_to_maildir " */
/* 		   "will make sure that the copy is from new/ to new/ and cur/ to " */
/* 		   "cur/. Also note that the target maildir must be on the same " */
/* 		   "filesystem. Returns #t if it worked, #f otherwise.\n") */
/* #define FUNC_NAME s_msg_move */
/* { */
/* 	GError *err; */
/* 	MuMsgWrapper *msgwrap; */
/* 	gboolean rv; */
/* 	MuFlags flags; */

/* 	SCM_ASSERT (mu_guile_scm_is_msg(MSG), MSG, SCM_ARG1, FUNC_NAME); */
/* 	SCM_ASSERT (scm_is_string (TARGETMDIR), TARGETMDIR, SCM_ARG2, FUNC_NAME); */

/* 	msgwrap = (MuMsgWrapper*) SCM_CDR(MSG); */

/* 	err = NULL; */
/* 	flags = mu_msg_get_flags    (msgwrap->_msg); */
/* 	rv = mu_msg_move_to_maildir (msgwrap->_msg, */
/* 				     scm_to_utf8_string (TARGETMDIR), flags, */
/* 				     FALSE, &err); */
/* 	if (!rv && err) { */
/* 		mu_guile_util_g_error (FUNC_NAME, err); */
/* 		g_error_free (err); */
/* 	} */

/* 	return rv ? SCM_BOOL_T : SCM_BOOL_F; */
/* } */
/* #undef FUNC_NAME */


static SCM
scm_from_string_or_null (const char *str)
{
	return str ? scm_from_utf8_string (str) : SCM_BOOL_F;
}


struct _FlagData {
	MuFlags flags;
	SCM lst;
};
typedef struct _FlagData FlagData;


static void
check_flag (MuFlags flag, FlagData *fdata)
{
	if (fdata->flags & flag) {
		SCM item;
		char *flagsym;

		flagsym = g_strconcat ("mu:", mu_flag_name(flag), NULL);
		item = scm_list_1 (scm_from_locale_symbol(flagsym));
		g_free (flagsym);

		fdata->lst = scm_append_x (scm_list_2(fdata->lst, item));
	}
}


static SCM
get_flags_scm (MuMsg *msg)
{
	FlagData fdata;

	fdata.flags = mu_msg_get_flags (msg);
	fdata.lst = SCM_EOL;
	mu_flags_foreach ((MuFlagsForeachFunc)check_flag, &fdata);

	return fdata.lst;
}


static SCM
get_prio_scm (MuMsg *msg)
{
	switch (mu_msg_get_prio (msg)) {
	case MU_MSG_PRIO_LOW:    return scm_from_locale_symbol("mu:low");
	case MU_MSG_PRIO_NORMAL: return scm_from_locale_symbol("mu:normal");
	case MU_MSG_PRIO_HIGH:   return scm_from_locale_symbol("mu:high");
	default: g_return_val_if_reached (SCM_UNDEFINED);
	}
}

static SCM
msg_string_list_field (MuMsg *msg, MuMsgFieldId mfid)
{
	SCM scmlst;
	const GSList *lst;

	lst = mu_msg_get_field_string_list (msg, mfid);

	for (scmlst = SCM_EOL; lst;
	     lst = g_slist_next(lst)) {
		SCM item;
		item = scm_list_1
			(scm_from_string_or_null((const char*)lst->data));
		scmlst = scm_append_x (scm_list_2(scmlst, item));
	}

	return scmlst;
}


SCM_DEFINE_PUBLIC(msg_field, "mu:msg:field", 2, 0, 0,
		  (SCM MSG, SCM FIELD),
		  "Get the field FIELD from message MSG.\n")
#define FUNC_NAME s_msg_field
{
	MuMsgWrapper *msgwrap;
	MuMsgFieldId mfid;
	msgwrap = (MuMsgWrapper*) SCM_CDR(MSG);

	SCM_ASSERT (mu_guile_scm_is_msg(MSG), MSG, SCM_ARG1, FUNC_NAME);
	SCM_ASSERT (scm_integer_p(FIELD), FIELD, SCM_ARG2, FUNC_NAME);

	mfid = scm_to_int (FIELD);
	SCM_ASSERT (mfid < MU_MSG_FIELD_ID_NUM, FIELD, SCM_ARG2, FUNC_NAME);

	switch (mfid) {
	case MU_MSG_FIELD_ID_PRIO:  return get_prio_scm (msgwrap->_msg);
	case MU_MSG_FIELD_ID_FLAGS: return get_flags_scm (msgwrap->_msg);
	default: break;
	}

	switch (mu_msg_field_type (mfid)) {
	case MU_MSG_FIELD_TYPE_STRING:
		return scm_from_string_or_null
			(mu_msg_get_field_string(msgwrap->_msg, mfid));
	case MU_MSG_FIELD_TYPE_BYTESIZE:
	case MU_MSG_FIELD_TYPE_TIME_T:
		return scm_from_uint (
			mu_msg_get_field_numeric (msgwrap->_msg, mfid));
	case MU_MSG_FIELD_TYPE_INT:
		return scm_from_int (
			mu_msg_get_field_numeric (msgwrap->_msg, mfid));
	case MU_MSG_FIELD_TYPE_STRING_LIST:
		return msg_string_list_field (msgwrap->_msg, mfid);
	default:
		SCM_ASSERT (0, FIELD, SCM_ARG2, FUNC_NAME);
	}
}
#undef FUNC_NAME



struct _EachContactData {
	SCM lst;
	MuMsgContactType ctype;
};
typedef struct _EachContactData EachContactData;

static void
contacts_to_list (MuMsgContact *contact, EachContactData *ecdata)
{
	if (ecdata->ctype == MU_MSG_CONTACT_TYPE_ALL ||
	    mu_msg_contact_type (contact) == ecdata->ctype) {

		SCM item;
		const char *addr, *name;

		addr = mu_msg_contact_address (contact);
		name = mu_msg_contact_name (contact);

		item = scm_list_1
			(scm_cons (
				scm_from_string_or_null(name),
				scm_from_string_or_null(addr)));

		ecdata->lst = scm_append_x (scm_list_2(ecdata->lst, item));
	}
}



SCM_DEFINE_PUBLIC (msg_contacts, "mu:msg:contacts", 2, 0, 0,
		   (SCM MSG, SCM CONTACT_TYPE), "Get a list of contact information pairs.\n")
#define FUNC_NAME s_msg_contacts
{
	MuMsgWrapper *msgwrap;
	EachContactData ecdata;

	SCM_ASSERT (mu_guile_scm_is_msg(MSG), MSG, SCM_ARG1, FUNC_NAME);
	SCM_ASSERT (scm_integer_p (CONTACT_TYPE) || scm_is_bool(CONTACT_TYPE),
		    CONTACT_TYPE, SCM_ARG2, FUNC_NAME);

	if (CONTACT_TYPE == SCM_BOOL_F)
		return SCM_UNSPECIFIED; /* nothing to do */
	else if (CONTACT_TYPE == SCM_BOOL_T)
		ecdata.ctype = MU_MSG_CONTACT_TYPE_ALL;
	else {
		MuMsgFieldId mfid;
		mfid = scm_to_uint (CONTACT_TYPE);
		switch (mfid) {
		case MU_MSG_FIELD_ID_TO: ecdata.ctype = MU_MSG_CONTACT_TYPE_TO; break;
		case MU_MSG_FIELD_ID_FROM: ecdata.ctype = MU_MSG_CONTACT_TYPE_FROM; break;
		case MU_MSG_FIELD_ID_CC: ecdata.ctype = MU_MSG_CONTACT_TYPE_CC; break;
		case MU_MSG_FIELD_ID_BCC: ecdata.ctype = MU_MSG_CONTACT_TYPE_BCC; break;
		default: g_return_val_if_reached (SCM_UNDEFINED);
		}
	}

	ecdata.lst = SCM_EOL;
	msgwrap = (MuMsgWrapper*) SCM_CDR(MSG);
	mu_msg_contact_foreach (msgwrap->_msg,
				(MuMsgContactForeachFunc)contacts_to_list,
				&ecdata);
	return ecdata.lst;
}
#undef FUNC_NAME



SCM_DEFINE_PUBLIC (msg_header, "mu:msg:header", 2, 0, 0,
		   (SCM MSG, SCM HEADER), "Get an arbitary HEADER from MSG.\n")
#define FUNC_NAME s_msg_header
{
	MuMsgWrapper *msgwrap;
	const char *header;
	const char *val;

	SCM_ASSERT (mu_guile_scm_is_msg(MSG), MSG, SCM_ARG1, FUNC_NAME);
	SCM_ASSERT (scm_is_string (HEADER)||HEADER==SCM_UNDEFINED,
		    HEADER, SCM_ARG2, FUNC_NAME);

	msgwrap = (MuMsgWrapper*) SCM_CDR(MSG);
	header  =  scm_to_utf8_string (HEADER);
	val     =  mu_msg_get_header(msgwrap->_msg, header);

	return val ? scm_from_string_or_null(val) : SCM_BOOL_F;
}
#undef FUNC_NAME



static SCM
msg_mark (SCM msg_smob)
{
	MuMsgWrapper *msgwrap;
	msgwrap = (MuMsgWrapper*) SCM_CDR(msg_smob);

	msgwrap->_unrefme = TRUE;

	return SCM_UNSPECIFIED;
}

static size_t
msg_free (SCM msg_smob)
{
	MuMsgWrapper *msgwrap;
	msgwrap = (MuMsgWrapper*) SCM_CDR(msg_smob);

	if (msgwrap->_unrefme)
		mu_msg_unref (msgwrap->_msg);

	return sizeof (MuMsgWrapper);
}

static int
msg_print (SCM msg_smob, SCM port, scm_print_state * pstate)
{
	MuMsgWrapper *msgwrap;
	msgwrap = (MuMsgWrapper*) SCM_CDR(msg_smob);

	scm_puts ("#<msg ", port);

	if (msg_smob == SCM_BOOL_F)
		scm_puts ("#f", port);
	else
		scm_puts (mu_msg_get_path(msgwrap->_msg),
			  port);

	scm_puts (">", port);

	return 1;
}


static struct  {
	const char* name;
	unsigned val;
} SYMPAIRS[] = {
	{ "mu:high",		MU_MSG_PRIO_HIGH },
	{ "mu:low",		MU_MSG_PRIO_LOW },
	{ "mu:normal",		MU_MSG_PRIO_NORMAL },

	{ "mu:new",		MU_FLAG_NEW },
	{ "mu:passed",		MU_FLAG_PASSED },
	{ "mu:replied",		MU_FLAG_REPLIED },
	{ "mu:seen",		MU_FLAG_SEEN },
	{ "mu:trashed",		MU_FLAG_TRASHED },
	{ "mu:draft",		MU_FLAG_DRAFT },
	{ "mu:flagged",		MU_FLAG_FLAGGED },
	{ "mu:signed",		MU_FLAG_SIGNED },
	{ "mu:encrypted",	MU_FLAG_ENCRYPTED },
	{ "mu:has-attach",	MU_FLAG_HAS_ATTACH },
	{ "mu:unread",		MU_FLAG_UNREAD },

	/* { "mu:embedded-text",	MU_MSG_FIELD_ID_EMBEDDED_TEXT }, */
	/* { "mu:file",		MU_MSG_FIELD_ID_FILE }, */
	/* { "mu:mime",		MU_MSG_FIELD_ID_MIME }, */
	{ "mu:bcc",		MU_MSG_FIELD_ID_BCC },
	{ "mu:body-html",	MU_MSG_FIELD_ID_BODY_HTML },
	{ "mu:body-txt",	MU_MSG_FIELD_ID_BODY_TEXT },
	{ "mu:cc",		MU_MSG_FIELD_ID_CC },
	{ "mu:date",		MU_MSG_FIELD_ID_DATE },
	{ "mu:flags",		MU_MSG_FIELD_ID_FLAGS },
	{ "mu:from",		MU_MSG_FIELD_ID_FROM },
	{ "mu:maildir",		MU_MSG_FIELD_ID_MAILDIR },
	{ "mu:message-id",	MU_MSG_FIELD_ID_MSGID },
	{ "mu:path",		MU_MSG_FIELD_ID_PATH },
	{ "mu:prio",		MU_MSG_FIELD_ID_PRIO },
	{ "mu:refs",		MU_MSG_FIELD_ID_REFS },
	{ "mu:size",		MU_MSG_FIELD_ID_SIZE },
	{ "mu:subject",		MU_MSG_FIELD_ID_SUBJECT },
	{ "mu:tags",		MU_MSG_FIELD_ID_TAGS },
	{ "mu:to",		MU_MSG_FIELD_ID_TO },
};


static void
define_symbols (void)
{
	unsigned u;

	for (u = 0; u != G_N_ELEMENTS(SYMPAIRS); ++u) {
		scm_c_define (SYMPAIRS[u].name,
			      scm_from_uint (SYMPAIRS[u].val));
		scm_c_export (SYMPAIRS[u].name, NULL);
	}
}


/* gboolean */
/* mu_guile_msg_load_current (const char *path) */
/* { */
/* 	MuMsg *msg; */
/* 	GError *err; */
/* 	SCM msgsmob; */

/* 	err = NULL; */
/* 	msg = mu_msg_new_from_file (path, NULL, &err); */

/* 	if (!msg) { */
/* 		g_printerr ("error creating message for '%s'", path); */
/* 		if (err) { */
/* 			g_printerr (": %s", err->message); */
/* 			g_error_free (err); */
/* 		} */
/* 		g_printerr ("\n"); */
/* 		return FALSE; */
/* 	} */

/* 	msgsmob = mu_guile_msg_to_scm (msg); */
/* 	scm_c_define ("mu:current-msg", msgsmob); */

/* 	return TRUE; */
/* } */


struct _MuData {
	MuQuery *_query;
};
typedef struct _MuData MuData;

static MuData *MU_DATA = NULL;

static gboolean
init_mu (const char *muhome)
{
	MuStore *store;
	MuQuery *query;
	GError *err;

	g_return_val_if_fail (!MU_DATA, FALSE);

	if (!mu_runtime_init (muhome, "guile"))
		return FALSE;

	store = mu_store_new_read_only (mu_runtime_path(MU_RUNTIME_PATH_XAPIANDB),
					&err);
	if (!store) {
		mu_guile_util_g_error (__FUNCTION__, err);
		g_clear_error (&err);
		return FALSE;
	}

	query = mu_query_new (store, &err);
	mu_store_unref (store);
	if (!query) {
		mu_guile_util_g_error (__FUNCTION__, err);
		g_clear_error (&err);
		return FALSE;
	}

	MU_DATA = g_new0 (MuData, 1);
	MU_DATA->_query = query;

	return TRUE;
}

static void
uninit_mu (void)
{
	g_return_if_fail (MU_DATA);

	mu_query_destroy (MU_DATA->_query);
	g_free (MU_DATA);

	MU_DATA = NULL;

	mu_runtime_uninit ();
}


SCM_DEFINE_PUBLIC (mu_initialize, "mu:initialize", 0, 1, 0,
		   (SCM MUHOME),
		   "Initialize mu - needed before you call any of the other "
		   "functions. Optionally, you can provide MUHOME which "
		   "should be an absolute path to your mu home directory "
		   "(typically, the default, ~/.mu, should be just fine)")
#define FUNC_NAME s_mu_initialize
{
	const char *muhome;

	SCM_ASSERT (scm_is_string (MUHOME) || MUHOME == SCM_BOOL_F || SCM_UNBNDP(MUHOME),
		    MUHOME, SCM_ARG1, FUNC_NAME);

	if (MU_DATA)
		return mu_guile_util_error (FUNC_NAME, 0, "Already initialized",
					    SCM_BOOL_F);

	if (SCM_UNBNDP(MUHOME) || MUHOME == SCM_BOOL_F)
		muhome = NULL;
	else
		muhome =  scm_to_utf8_string (MUHOME);

	if (!init_mu (muhome))
		return mu_guile_util_error (FUNC_NAME, 0, "Failed to initialize mu",
					    SCM_BOOL_F);
	/* cleanup when we're exiting */
	g_atexit (uninit_mu);

	return SCM_BOOL_T;
}
#undef FUNC_NAME


SCM_DEFINE_PUBLIC (mu_initialized_p, "mu:initialized?", 0, 0, 0,
		   (void), "Whether mu is initialized or not.\n")
#define FUNC_NAME s_mu_initialized_p
{
	return MU_DATA ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME


static MuMsgIter*
get_query_iter (MuQuery *query, const char* expr)
{
	MuMsgIter *iter;
	GError *err;

	err = NULL;
	iter = mu_query_run (query, expr,
			     FALSE, MU_MSG_FIELD_ID_NONE, TRUE, -1, &err);
	if (!iter) {
		mu_guile_util_g_error ("<internal error>", err);
		g_clear_error (&err);
	}

	return iter;
}


static void
call_func (SCM FUNC, MuMsgIter *iter, const char* func_name)
{
	SCM msgsmob;
	MuMsg *msg;

	msg = mu_msg_iter_get_msg_floating (iter); /* don't unref */

	msgsmob = mu_guile_msg_to_scm (mu_msg_ref(msg));
	scm_call_1 (FUNC, msgsmob);
}


SCM_DEFINE_PUBLIC (for_each_message, "mu:internal:for-each-message", 2, 0, 0,
		   (SCM FUNC, SCM EXPR),
		   "Call FUNC for each message in the message store. EXPR is either a "
"string containing a mu search expression or a boolean; in the former "
"case, limit the messages to only those matching the expression, in the "
"latter case, match /all/ messages if the EXPR equals #t, and match "
"none if EXPR equals #f.")
#define FUNC_NAME s_for_each_message
{
	MuMsgIter *iter;
	const char* expr;

	SCM_ASSERT (scm_procedure_p (FUNC), FUNC, SCM_ARG1, FUNC_NAME);
	SCM_ASSERT (scm_is_bool(EXPR) || scm_is_string (EXPR),
		    EXPR, SCM_ARG2, FUNC_NAME);

	if (!MU_DATA)
		return mu_guile_util_error (FUNC_NAME, 0, "mu not initialized",
					    SCM_UNDEFINED);
	if (EXPR == SCM_BOOL_F)
		return SCM_UNSPECIFIED; /* nothing to do */
	else if (EXPR == SCM_BOOL_T)
		expr = ""; 	/* note, "" matches *all* messages */
	else
		expr = scm_to_utf8_string(EXPR);

	iter = get_query_iter (MU_DATA->_query, expr);
	if (!iter)
		return SCM_UNSPECIFIED;

	while (!mu_msg_iter_is_done(iter)) {
		call_func (FUNC, iter, FUNC_NAME);
		mu_msg_iter_next (iter);
	}

	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


enum _LogType {
	LOG_INFO,
	LOG_WARNING,
	LOG_CRITICAL
};
typedef enum _LogType LogType;


static SCM
write_log (LogType logtype, SCM FRM, SCM ARGS)
#define FUNC_NAME __FUNCTION__
{
	SCM str;

	SCM_ASSERT (scm_is_string(FRM), FRM, SCM_ARG1, "<write_log>");
	SCM_VALIDATE_REST_ARGUMENT(ARGS);

	str = scm_simple_format (SCM_BOOL_F, FRM, ARGS);

	if (scm_is_string (str)) {

		gchar *output;
		output = scm_to_utf8_string (str);

		switch (logtype) {
		case LOG_INFO:     g_message ("%s", output); break;
		case LOG_WARNING:  g_warning ("%s", output); break;
		case LOG_CRITICAL: g_critical ("%s", output); break;
		}
	}

	return SCM_UNSPECIFIED;

#undef FUNC_NAME
}


SCM_DEFINE_PUBLIC (log_info, "mu:log", 1, 0, 1,  (SCM FRM, SCM ARGS),
	    "log some message using a list of ARGS applied to FRM "
	    "(in 'simple-format' notation).\n")
#define FUNC_NAME s_info
{
	return write_log (LOG_INFO, FRM, ARGS);
}
#undef FUNC_NAME

SCM_DEFINE_PUBLIC (log_warning, "mu:warning", 1, 0, 1,  (SCM FRM, SCM ARGS),
	    "log some warning using a list of ARGS applied to FRM (in 'simple-format' "
	    "notation).\n")
#define FUNC_NAME s_warning
{
	return write_log (LOG_WARNING, FRM, ARGS);
}
#undef FUNC_NAME

SCM_DEFINE_PUBLIC (log_critical, "mu:critical", 1, 0, 1,  (SCM FRM, SCM ARGS),
	    "log some critical message using a list of ARGS applied to FRM "
	    "(in 'simple-format' notation).\n")
#define FUNC_NAME s_critical
{
	return write_log (LOG_CRITICAL, FRM, ARGS);
}
#undef FUNC_NAME



void*
mu_guile_init (void *data)
{
	MSG_TAG = scm_make_smob_type ("msg", sizeof(MuMsgWrapper));

	scm_set_smob_mark  (MSG_TAG, msg_mark);
	scm_set_smob_free  (MSG_TAG, msg_free);
	scm_set_smob_print (MSG_TAG, msg_print);

	define_symbols ();

#include "mu-guile.x"

	return NULL;
}
