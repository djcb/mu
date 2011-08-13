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

#include <mu-msg.h>
#include <mu-query.h>
#include <mu-runtime.h>

#include "mu-guile-msg.h"
#include "mu-guile-common.h"

struct _MuMsgWrapper {
	MuMsg   *_msg;
	gboolean _unrefme;
};
typedef struct _MuMsgWrapper MuMsgWrapper;

static long MSG_TAG;

static int
mu_guile_scm_is_msg (SCM scm)
{
	return SCM_NIMP(scm) && (long) SCM_CAR (scm) == MSG_TAG;
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

SCM_DEFINE (msg_make_from_file, "mu:msg:make-from-file", 1, 0, 0,
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
		mu_guile_g_error (FUNC_NAME, err);
		g_error_free (err);
	}
	
	return msg ? mu_guile_msg_to_scm (msg) : SCM_UNDEFINED;
}
#undef FUNC_NAME


SCM_DEFINE (msg_move, "mu:msg:move-to-maildir", 2, 0, 0,
	    (SCM MSG, SCM TARGETMDIR),
	    "Move message to another maildir TARGETMDIR. Note that this the "
	    "base-level Maildir, ie. /home/user/Maildir/archive, and must"
	    " _not_ include the 'cur' or 'new' part. mu_msg_move_to_maildir "
	    "will make sure that the copy is from new/ to new/ and cur/ to "
	    "cur/. Also note that the target maildir must be on the same "
	    "filesystem. Returns #t if it worked, #f otherwise.\n")
#define FUNC_NAME s_msg_move
{
	GError *err;
	MuMsgWrapper *msgwrap;
	gboolean rv;
	MuFlags flags;
	
	SCM_ASSERT (mu_guile_scm_is_msg(MSG), MSG, SCM_ARG1, FUNC_NAME);	
	SCM_ASSERT (scm_is_string (TARGETMDIR), TARGETMDIR, SCM_ARG2, FUNC_NAME);

	msgwrap = (MuMsgWrapper*) SCM_CDR(MSG);

	err = NULL;
	flags = mu_msg_get_flags    (msgwrap->_msg);
	rv = mu_msg_move_to_maildir (msgwrap->_msg,
				     scm_to_utf8_string (TARGETMDIR), flags,
				     &err);
	if (!rv && err) {
		mu_guile_g_error (FUNC_NAME, err);
		g_error_free (err);
	}

	return rv ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME





static SCM
scm_from_string_or_null (const char *str)
{
	return str ? scm_from_utf8_string (str) : SCM_UNSPECIFIED;
}
		

static SCM
msg_str_field (SCM msg_smob, MuMsgFieldId mfid)
{
	MuMsgWrapper *msgwrap;
	msgwrap = (MuMsgWrapper*) SCM_CDR(msg_smob);

	return scm_from_string_or_null (
		mu_msg_get_field_string(msgwrap->_msg, mfid));
}

static gint64
msg_num_field (SCM msg_smob, MuMsgFieldId mfid)
{
	MuMsgWrapper *msgwrap;
	msgwrap = (MuMsgWrapper*) SCM_CDR(msg_smob);

	return mu_msg_get_field_numeric(msgwrap->_msg, mfid);
}


SCM_DEFINE (msg_date, "mu:msg:date", 1, 0, 0,
	    (SCM MSG),
	    "Get the date (time in seconds since epoch) for MSG.\n")
#define FUNC_NAME s_msg_date
{
	SCM_ASSERT (mu_guile_scm_is_msg(MSG), MSG, SCM_ARG1, FUNC_NAME);
	
	return scm_from_unsigned_integer
		(msg_num_field (MSG, MU_MSG_FIELD_ID_DATE));	
}
#undef FUNC_NAME



SCM_DEFINE (msg_size, "mu:msg:size", 1, 0, 0,
	    (SCM MSG),
	    "Get the size in bytes for MSG.\n")
#define FUNC_NAME s_msg_size
{
	SCM_ASSERT (mu_guile_scm_is_msg(MSG), MSG, SCM_ARG1, FUNC_NAME);
	
	return scm_from_unsigned_integer
		(msg_num_field (MSG, MU_MSG_FIELD_ID_SIZE));	
}
#undef FUNC_NAME



SCM_DEFINE (msg_prio, "mu:msg:priority", 1, 0, 0,
	    (SCM MSG),
	    "Get the priority of MSG (low, normal or high).\n")
#define FUNC_NAME s_msg_prio
{
	MuMsgPrio prio;
	MuMsgWrapper *msgwrap;

	SCM_ASSERT (mu_guile_scm_is_msg(MSG), MSG, SCM_ARG1, FUNC_NAME);

	msgwrap = (MuMsgWrapper*) SCM_CDR(MSG);
	
	prio = mu_msg_get_prio (msgwrap->_msg);

	switch (prio) {
	case MU_MSG_PRIO_LOW:    return scm_from_locale_symbol("mu:low");
	case MU_MSG_PRIO_NORMAL: return scm_from_locale_symbol("mu:normal");
	case MU_MSG_PRIO_HIGH:   return scm_from_locale_symbol("mu:high");
	default:
		g_return_val_if_reached (SCM_UNDEFINED);
	}	
}
#undef FUNC_NAME

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


SCM_DEFINE (msg_flags, "mu:msg:flags", 1, 0, 0,
	    (SCM MSG),
	    "Get the flags for MSG (one or or more of new, passed, replied, "
	    "seen, trashed, draft, flagged, unread, signed, encrypted, "
	    "has-attach).\n")
#define FUNC_NAME s_msg_flags
{
	MuMsgWrapper *msgwrap;
	FlagData fdata;
	SCM_ASSERT (mu_guile_scm_is_msg(MSG), MSG, SCM_ARG1, FUNC_NAME);
	
	msgwrap = (MuMsgWrapper*) SCM_CDR(MSG);
	
	fdata.flags = mu_msg_get_flags (msgwrap->_msg);
	fdata.lst = SCM_EOL;
	mu_flags_foreach ((MuFlagsForeachFunc)check_flag,
			      &fdata);

	return fdata.lst;
}
#undef FUNC_NAME


SCM_DEFINE (msg_subject, "mu:msg:subject", 1, 0, 0,
	    (SCM MSG), "Get the subject of MSG.\n")
#define FUNC_NAME s_msg_subject
{
	SCM_ASSERT (mu_guile_scm_is_msg(MSG), MSG, SCM_ARG1, FUNC_NAME);

	return msg_str_field (MSG, MU_MSG_FIELD_ID_SUBJECT);
}
#undef FUNC_NAME


SCM_DEFINE (msg_from, "mu:msg:from", 1, 0, 0,
	    (SCM MSG), "Get the sender of MSG.\n")
#define FUNC_NAME s_msg_from
{
	SCM_ASSERT (mu_guile_scm_is_msg(MSG), MSG, SCM_ARG1, FUNC_NAME);
	
	return msg_str_field (MSG, MU_MSG_FIELD_ID_FROM);
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
	if (mu_msg_contact_type (contact) == ecdata->ctype) {
		SCM item;
		const char *addr, *name;

		addr = mu_msg_contact_address(contact);
		name = mu_msg_contact_name(contact);
		
		item = scm_list_1
			(scm_list_2 (
				scm_from_string_or_null(name),
				scm_from_string_or_null(addr)));
		ecdata->lst = scm_append_x (scm_list_2(ecdata->lst, item));	
	}
}
	

static SCM
contact_list_field (SCM msg_smob, MuMsgFieldId mfid)
{
	MuMsgWrapper *msgwrap;
	EachContactData ecdata;

	ecdata.lst = SCM_EOL;
	
	switch (mfid) {
	case MU_MSG_FIELD_ID_TO: ecdata.ctype = MU_MSG_CONTACT_TYPE_TO; break;
	case MU_MSG_FIELD_ID_CC: ecdata.ctype = MU_MSG_CONTACT_TYPE_CC; break;
	case MU_MSG_FIELD_ID_BCC: ecdata.ctype = MU_MSG_CONTACT_TYPE_BCC; break;
	default: g_return_val_if_reached (SCM_UNDEFINED);
	}
	
	msgwrap = (MuMsgWrapper*) SCM_CDR(msg_smob);
	
	mu_msg_contact_foreach (msgwrap->_msg,
				(MuMsgContactForeachFunc)contacts_to_list,
				&ecdata);
	return ecdata.lst;
}


SCM_DEFINE (msg_to, "mu:msg:to", 1, 0, 0,
	    (SCM MSG), "Get the list of To:-recipients of MSG.\n")
#define FUNC_NAME s_msg_to
{
	SCM_ASSERT (mu_guile_scm_is_msg(MSG), MSG, SCM_ARG1, FUNC_NAME);
	
	return contact_list_field (MSG, MU_MSG_FIELD_ID_TO);
}
#undef FUNC_NAME

	    

SCM_DEFINE (msg_cc, "mu:msg:cc", 1, 0, 0,
	    (SCM MSG), "Get the list of Cc:-recipients of MSG.\n")
#define FUNC_NAME s_msg_cc
{
	SCM_ASSERT (mu_guile_scm_is_msg(MSG), MSG, SCM_ARG1, FUNC_NAME);

	return contact_list_field (MSG, MU_MSG_FIELD_ID_CC);
}
#undef FUNC_NAME


SCM_DEFINE (msg_bcc, "mu:msg:bcc", 1, 0, 0,
	    (SCM MSG), "Get the list of Bcc:-recipients of MSG.\n")
#define FUNC_NAME s_msg_bcc
{
	SCM_ASSERT (mu_guile_scm_is_msg(MSG), MSG, SCM_ARG1, FUNC_NAME);
	
	return contact_list_field (MSG, MU_MSG_FIELD_ID_BCC);
}
#undef FUNC_NAME


SCM_DEFINE (msg_path, "mu:msg:path", 1, 0, 0,
	    (SCM MSG), "Get the filesystem path for MSG.\n")
#define FUNC_NAME s_msg_path
{
	SCM_ASSERT (mu_guile_scm_is_msg(MSG), MSG, SCM_ARG1, FUNC_NAME);
	
	return msg_str_field (MSG, MU_MSG_FIELD_ID_PATH);
}
#undef FUNC_NAME


SCM_DEFINE (msg_maildir, "mu:msg:maildir", 1, 0, 0,
	    (SCM MSG), "Get the maildir where MSG lives.\n")
#define FUNC_NAME s_msg_maildir
{
	SCM_ASSERT (mu_guile_scm_is_msg(MSG), MSG, SCM_ARG1, FUNC_NAME);

	return msg_str_field (MSG, MU_MSG_FIELD_ID_MAILDIR);
}
#undef FUNC_NAME



SCM_DEFINE (msg_msgid, "mu:msg:message-id", 1, 0, 0,
	    (SCM MSG), "Get the MSG's message-id.\n")
#define FUNC_NAME s_msg_msgid
{
	return msg_str_field (MSG, MU_MSG_FIELD_ID_MSGID);
}
#undef FUNC_NAME


SCM_DEFINE (msg_body, "mu:msg:body", 1, 1, 0,
		    (SCM MSG, SCM HTML), "Get the MSG's body. If HTML is #t, "
		    "prefer the html-version, otherwise prefer plain text.\n")
#define FUNC_NAME s_msg_body
{
	MuMsgWrapper *msgwrap;
	gboolean html;
	const char *val;

	SCM_ASSERT (mu_guile_scm_is_msg(MSG), MSG, SCM_ARG1, FUNC_NAME);
	
	msgwrap = (MuMsgWrapper*) SCM_CDR(MSG);
	html = SCM_UNBNDP(HTML) ? FALSE : HTML == SCM_BOOL_T;
	
	if (html)
		val = mu_msg_get_body_html(msgwrap->_msg);
	else
		val = mu_msg_get_body_text(msgwrap->_msg);
		
	return scm_from_string_or_null (val);
}
#undef FUNC_NAME


SCM_DEFINE (msg_header, "mu:msg:header", 1, 1, 0,
		    (SCM MSG, SCM HEADER), "Get an arbitary HEADER from MSG.\n")
#define FUNC_NAME s_msg_header
{
	MuMsgWrapper *msgwrap;
	const char *header;
	const char *val;

	SCM_ASSERT (mu_guile_scm_is_msg(MSG), MSG, SCM_ARG1, FUNC_NAME);
	SCM_ASSERT (scm_is_string (HEADER), HEADER, SCM_ARG2, FUNC_NAME);
	
	msgwrap = (MuMsgWrapper*) SCM_CDR(MSG);
	header  =  scm_to_utf8_string (HEADER);
	val     =  mu_msg_get_header(msgwrap->_msg, header);
	
	return val ? scm_from_string_or_null(val) : SCM_UNDEFINED;
}
#undef FUNC_NAME

static SCM
msg_string_list_field (SCM msg_smob, MuMsgFieldId mfid)
{
	MuMsgWrapper *msgwrap;
	SCM scmlst;
	const GSList *lst;
	
	msgwrap = (MuMsgWrapper*) SCM_CDR(msg_smob);
	lst = mu_msg_get_field_string_list (msgwrap->_msg, mfid);
	
	for (scmlst = SCM_EOL; lst;
	     lst = g_slist_next(lst)) {	
		SCM item;
		item = scm_list_1
			(scm_from_string_or_null((const char*)lst->data));
		scmlst = scm_append_x (scm_list_2(scmlst, item));
	}

	return scmlst;
}


SCM_DEFINE (msg_tags, "mu:msg:tags", 1, 1, 0,
		    (SCM MSG), "Get the list of tags (contents of the "
		    "X-Label:-header) for MSG.\n")
#define FUNC_NAME s_msg_tags
{
	SCM_ASSERT (mu_guile_scm_is_msg(MSG), MSG, SCM_ARG1, FUNC_NAME);
	
	return msg_string_list_field (MSG, MU_MSG_FIELD_ID_TAGS);
}
#undef FUNC_NAME



SCM_DEFINE (msg_refs, "mu:msg:references", 1, 1, 0,
	    (SCM MSG), "Get the list of referenced message-ids "
	    "(contents of the References: and Reply-To: headers).\n")
#define FUNC_NAME s_msg_refs
{
	SCM_ASSERT (mu_guile_scm_is_msg(MSG), MSG, SCM_ARG1, FUNC_NAME);
	
	return msg_string_list_field (MSG, MU_MSG_FIELD_ID_REFS);
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

static scm_sizet
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


static void
define_symbols (void)
{
	/* message priority */
	scm_c_define ("mu:high",	scm_from_int(MU_MSG_PRIO_HIGH));
	scm_c_define ("mu:low",		scm_from_int(MU_MSG_PRIO_LOW));
	scm_c_define ("mu:normal",	scm_from_int(MU_MSG_PRIO_NORMAL));

	/* message flags */
	scm_c_define ("mu:new",		scm_from_int(MU_FLAG_NEW));
	scm_c_define ("mu:passed",	scm_from_int(MU_FLAG_PASSED));
	scm_c_define ("mu:replied",	scm_from_int(MU_FLAG_REPLIED));
	scm_c_define ("mu:seen",	scm_from_int(MU_FLAG_SEEN));
	scm_c_define ("mu:trashed",	scm_from_int(MU_FLAG_TRASHED));
	scm_c_define ("mu:draft",	scm_from_int(MU_FLAG_DRAFT));
	scm_c_define ("mu:flagged",	scm_from_int(MU_FLAG_FLAGGED));

	scm_c_define ("mu:signed",	scm_from_int(MU_FLAG_SIGNED));
	scm_c_define ("mu:encrypted",	scm_from_int(MU_FLAG_ENCRYPTED));
	scm_c_define ("mu:has-attach",	scm_from_int(MU_FLAG_HAS_ATTACH));

	scm_c_define ("mu:unread",	scm_from_int(MU_FLAG_UNREAD));

}


gboolean
mu_guile_msg_load_current (const char *path)
{
	MuMsg *msg;
	GError *err;
	SCM msgsmob;
	
	err = NULL;
	msg = mu_msg_new_from_file (path, NULL, &err);

	if (!msg) {
		g_printerr ("error creating message for '%s'", path);
		if (err) {
			g_printerr (": %s", err->message);
			g_error_free (err);
		}
		g_printerr ("\n");
		return FALSE;
	}
	
	msgsmob = mu_guile_msg_to_scm (msg);
	scm_c_define ("mu:current-msg", msgsmob); 

	return TRUE;
}


void*
mu_guile_msg_init (void *data)
{
	
	MSG_TAG = scm_make_smob_type ("msg", sizeof(MuMsgWrapper));
	
	scm_set_smob_mark  (MSG_TAG, msg_mark);
	scm_set_smob_free  (MSG_TAG, msg_free);
	scm_set_smob_print (MSG_TAG, msg_print);	

	define_symbols ();
	
#include "mu-guile-msg.x"

	return NULL;
}


