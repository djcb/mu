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

#include "mu-guile.h"

#include <mu-runtime.h>
#include <mu-store.h>
#include <mu-query.h>
#include <mu-msg.h>
#include <mu-msg-part.h>


struct _MuMsgWrapper {
	MuMsg	*_msg;
	gboolean _unrefme;
};
typedef struct _MuMsgWrapper MuMsgWrapper;
static long MSG_TAG;

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



/* SCM_DEFINE_PUBLIC (msg_make_from_file, "mu:msg:make-from-file", 1, 0, 0, */
/* 		   (SCM PATH), */
/* 		   "Create a message object based on the message in PATH.\n") */
/* #define FUNC_NAME s_msg_make_from_file */
/* { */
/* 	MuMsg *msg; */
/* 	GError *err; */

/* 	SCM_ASSERT (scm_is_string (PATH), PATH, SCM_ARG1, FUNC_NAME); */

/* 	err = NULL; */
/* 	msg = mu_msg_new_from_file (scm_to_utf8_string (PATH), NULL, &err); */

/* 	if (err) { */
/* 		mu_guile_g_error (FUNC_NAME, err); */
/* 		g_error_free (err); */
/* 	} */

/* 	return msg ? mu_guile_msg_to_scm (msg) : SCM_UNDEFINED; */
/* } */
/* #undef FUNC_NAME */


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
/* 		mu_guile_g_error (FUNC_NAME, err); */
/* 		g_error_free (err); */
/* 	} */

/* 	return rv ? SCM_BOOL_T : SCM_BOOL_F; */
/* } */
/* #undef FUNC_NAME */



struct _FlagData {
	MuFlags flags;
	SCM lst;
};
typedef struct _FlagData FlagData;


static void
check_flag (MuFlags flag, FlagData *fdata)
{
	SCM item;
	char *flagsym;

	if (!(fdata->flags & flag))
		return;

	flagsym = g_strconcat ("mu:", mu_flag_name(flag), NULL);
	item    = scm_list_1 (scm_from_utf8_symbol(flagsym));
	g_free (flagsym);

	fdata->lst = scm_append_x (scm_list_2(fdata->lst, item));
}


static SCM
get_flags_scm (MuMsg *msg)
{
	FlagData fdata;

	fdata.flags = mu_msg_get_flags (msg);
	fdata.lst   = SCM_EOL;

	mu_flags_foreach ((MuFlagsForeachFunc)check_flag, &fdata);

	return fdata.lst;
}


static SCM
get_prio_scm (MuMsg *msg)
{
	switch (mu_msg_get_prio (msg)) {

	case MU_MSG_PRIO_LOW:
		return scm_from_utf8_symbol("mu:low");
	case MU_MSG_PRIO_NORMAL:
		return scm_from_utf8_symbol("mu:normal");
	case MU_MSG_PRIO_HIGH:
		return scm_from_utf8_symbol("mu:high");
	default:
		g_return_val_if_reached (SCM_UNDEFINED);
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
			(mu_guile_scm_from_str((const char*)lst->data));
		scmlst = scm_append_x (scm_list_2(scmlst, item));
	}

	return scmlst;
}


SCM_DEFINE_PUBLIC(get_field, "mu:get-field", 2, 0, 0,
		  (SCM MSG, SCM FIELD),
		  "Get the field FIELD from message MSG.\n")
#define FUNC_NAME s_get_field
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

	case MU_MSG_FIELD_ID_BODY_HTML:
	case MU_MSG_FIELD_ID_BODY_TEXT:
	{
		SCM data;
		data = mu_guile_scm_from_str
			(mu_msg_get_field_string (msgwrap->_msg, mfid));
		/* explicitly close the file backend, so we won't run of fds */
		mu_msg_close_file_backend (msgwrap->_msg);
		return data;
	}

	default: break;
	}

	switch (mu_msg_field_type (mfid)) {
	case MU_MSG_FIELD_TYPE_STRING:
		return mu_guile_scm_from_str
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
	SCM item;

	if (ecdata->ctype != MU_MSG_CONTACT_TYPE_ALL &&
	    mu_msg_contact_type (contact) != ecdata->ctype)
		return;

	item = scm_list_1
		(scm_cons
		 (mu_guile_scm_from_str(mu_msg_contact_name (contact)),
		  mu_guile_scm_from_str(mu_msg_contact_address (contact))));

	ecdata->lst = scm_append_x (scm_list_2(ecdata->lst, item));
}



SCM_DEFINE_PUBLIC (get_contacts, "mu:get-contacts", 2, 0, 0,
		   (SCM MSG, SCM CONTACT_TYPE),
		   "Get a list of contact information pairs.\n")
#define FUNC_NAME s_get_contacts
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

	/* explicitly close the file backend, so we won't run of fds */
	mu_msg_close_file_backend (msgwrap->_msg);

	return ecdata.lst;
}
#undef FUNC_NAME

struct _AttInfo {
	SCM      attlist;
	gboolean files_only;
};
typedef struct _AttInfo AttInfo;

static void
each_part (MuMsg *msg, MuMsgPart *part, AttInfo *attinfo)
{
	char *mime_type;
	SCM elm;

	if (!part->type)
		return;
	if (attinfo->files_only && !part->file_name)
		return;

	mime_type = g_strdup_printf ("%s/%s", part->type, part->subtype);

	elm = scm_list_5 (
		/* msg */
		mu_guile_scm_from_str (mu_msg_get_path(msg)),
		/* index */
		scm_from_uint(part->index),
		/* filename or #f */
		part->file_name ?
		mu_guile_scm_from_str (part->file_name) :
		SCM_BOOL_F,
		/* mime-type */
		mime_type ?
		mu_guile_scm_from_str (mime_type):
		SCM_BOOL_F,
		/* size */
		part->size > 0 ?
		scm_from_uint (part->size) :
		SCM_BOOL_F);

	g_free (mime_type);

	attinfo->attlist = scm_cons (elm, attinfo->attlist);
}


SCM_DEFINE_PUBLIC (get_parts, "mu:get-parts", 1, 1, 0,
		   (SCM MSG, SCM FILES_ONLY),
		   "Get the list of mime-parts for MSG. If FILES_ONLY is #t, only"
		   "get parts that file names. The resulting list has elements "
		   "which are list of the form (index name mime-type size).\n")
#define FUNC_NAME s_get_parts
{
	MuMsgWrapper *msgwrap;
	AttInfo attinfo;

	SCM_ASSERT (mu_guile_scm_is_msg(MSG), MSG, SCM_ARG1, FUNC_NAME);
	SCM_ASSERT (scm_is_bool(FILES_ONLY), FILES_ONLY,SCM_ARG2, FUNC_NAME);

	attinfo.attlist    = SCM_EOL; /* empty list */
	attinfo.files_only = FILES_ONLY == SCM_BOOL_T ? TRUE : FALSE;

	msgwrap = (MuMsgWrapper*) SCM_CDR(MSG);
	mu_msg_part_foreach (msgwrap->_msg,
			     (MuMsgPartForeachFunc)each_part,
			     &attinfo);

	/* explicitly close the file backend, so we won't run of fds */
	mu_msg_close_file_backend (msgwrap->_msg);

	return attinfo.attlist;
}
#undef FUNC_NAME


SCM_DEFINE_PUBLIC (save_part, "mu:save-part", 2, 0, 0,
		   (SCM MSGPATH, SCM INDEX),
		   "Create a temporary file containing the attachment; this function "
		   "returns the full path to that temporary file.\n")
#define FUNC_NAME s_save_part
{
	GError *err;
	gchar *filepath, *msgpath;
	unsigned index;
	MuMsg *msg;
	gboolean rv;
	SCM rv_scm;

	SCM_ASSERT (scm_is_string(MSGPATH), MSGPATH, SCM_ARG1, FUNC_NAME);
	SCM_ASSERT (scm_is_integer (INDEX),
		    INDEX,SCM_ARG2, FUNC_NAME);

	index	= scm_to_uint (INDEX);
	msgpath = scm_to_utf8_string (MSGPATH);

	err	= NULL;
	msg	= mu_msg_new_from_file (msgpath, NULL, &err);
	if (!msg) {
		rv_scm = mu_guile_g_error (FUNC_NAME, err);
		goto leave;
	}

	filepath = mu_msg_part_filepath_cache (msg, index);
	if (!filepath) {
		rv_scm = mu_guile_error (FUNC_NAME, 0, "could not get filepath",
					 SCM_UNSPECIFIED);
		goto leave;
	}

	rv = mu_msg_part_save (msg, filepath, index, FALSE, TRUE, &err);
	if (!rv) {
		rv_scm = err ? mu_guile_g_error (FUNC_NAME, err) :
			mu_guile_error (FUNC_NAME, 0, "could not save part",
					SCM_UNSPECIFIED);
		goto leave;
	}

	rv_scm = mu_guile_scm_from_str (filepath);

leave:
	mu_msg_unref (msg);

	g_clear_error (&err);
	g_free (filepath);
	free (msgpath);

	return rv_scm;
}
#undef FUNC_NAME




SCM_DEFINE_PUBLIC (get_header, "mu:get-header", 2, 0, 0,
		   (SCM MSG, SCM HEADER),
		   "Get an arbitary HEADER from MSG.\n")
#define FUNC_NAME s_get_header
{
	MuMsgWrapper *msgwrap;
	char *header;
	const char *val;

	SCM_ASSERT (mu_guile_scm_is_msg(MSG), MSG, SCM_ARG1, FUNC_NAME);
	SCM_ASSERT (scm_is_string (HEADER)||HEADER==SCM_UNDEFINED,
		    HEADER, SCM_ARG2, FUNC_NAME);

	msgwrap = (MuMsgWrapper*) SCM_CDR(MSG);
	header  =  scm_to_utf8_string (HEADER);
	val     =  mu_msg_get_header (msgwrap->_msg, header);
	free (header);

	/* explicitly close the file backend, so we won't run of fds */
	mu_msg_close_file_backend (msgwrap->_msg);

	return mu_guile_scm_from_str(val);
}
#undef FUNC_NAME



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

static void
call_func (SCM FUNC, MuMsgIter *iter, const char* func_name)
{
	SCM msgsmob;
	MuMsg *msg;

	msg = mu_msg_iter_get_msg_floating (iter); /* don't unref */

	msgsmob = mu_guile_msg_to_scm (mu_msg_ref(msg));
	scm_call_1 (FUNC, msgsmob);
}


static MuMsgIter*
get_query_iter (MuQuery *query, const char* expr)
{
	MuMsgIter *iter;
	GError *err;

	err = NULL;
	iter = mu_query_run (query, expr,
			     FALSE, MU_MSG_FIELD_ID_NONE, TRUE, -1, &err);
	if (!iter) {
		mu_guile_g_error ("<internal error>", err);
		g_clear_error (&err);
	}

	return iter;
}


SCM_DEFINE_PUBLIC (for_each_msg_internal, "mu:for-each-msg-internal", 2, 0, 0,
		   (SCM FUNC, SCM EXPR),
"Call FUNC for each msg in the message store matching EXPR. EXPR is "
"either a string containing a mu search expression or a boolean; in the former "
"case, limit the messages to only those matching the expression, in the "
"latter case, match /all/ messages if the EXPR equals #t, and match "
"none if EXPR equals #f. Note -- function for internal use.")
#define FUNC_NAME s_for_each_msg_internal
{
	MuMsgIter *iter;
	char* expr;

	SCM_ASSERT (scm_procedure_p (FUNC), FUNC, SCM_ARG1, FUNC_NAME);
	SCM_ASSERT (scm_is_bool(EXPR) || scm_is_string (EXPR),
		    EXPR, SCM_ARG2, FUNC_NAME);
	if (!mu_guile_initialized())
		return mu_guile_error (FUNC_NAME, 0, "mu not initialized",
					    SCM_UNSPECIFIED);

	if (EXPR == SCM_BOOL_F)
		return SCM_UNSPECIFIED; /* nothing to do */

	if (EXPR == SCM_BOOL_T)
		expr = strdup (""); 	/* note, "" matches *all* messages */
	else
		expr = scm_to_utf8_string(EXPR);

	iter = get_query_iter (mu_guile_instance()->query, expr);
	free (expr);

	if (!iter)
		return SCM_UNSPECIFIED;

	while (!mu_msg_iter_is_done(iter)) {

		call_func (FUNC, iter, FUNC_NAME);
		mu_msg_iter_next (iter);
	}

	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



void*
mu_guile_message_init (void *data)
{
	MSG_TAG = scm_make_smob_type ("msg", sizeof(MuMsgWrapper));

	scm_set_smob_mark  (MSG_TAG, msg_mark);
	scm_set_smob_free  (MSG_TAG, msg_free);
	scm_set_smob_print (MSG_TAG, msg_print);

	define_symbols ();

#include "mu-guile-message.x"

	return NULL;
}
