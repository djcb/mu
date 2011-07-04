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

#include <libguile.h>
#include <mu-msg.h>

struct _MuMsgWrapper {
	MuMsg *_msg;
	gboolean _unrefme;
};
typedef struct _MuMsgWrapper MuMsgWrapper;

static long MSG_TAG;


static SCM
msg_make_from_file (SCM path)
{
	const char* msgpath;
	MuMsgWrapper *msgwrap;
		
	msgpath = scm_to_utf8_string (path);

	msgwrap = scm_gc_malloc (sizeof (MuMsgWrapper), "msg");
	msgwrap->_msg = mu_msg_new_from_file (msgpath, NULL, NULL);
	msgwrap->_unrefme = FALSE;
	
	SCM_RETURN_NEWSMOB (MSG_TAG, msgwrap);
}


static SCM
msg_str_field (SCM msg_smob, MuMsgFieldId mfid)
{
	const char *val;
	MuMsgWrapper *msgwrap;
	msgwrap = (MuMsgWrapper*) SCM_CDR(msg_smob);

	val = mu_msg_get_field_string(msgwrap->_msg, mfid);
	
	return val ? scm_from_utf8_string(val) : SCM_UNDEFINED;
}

static gint64
msg_num_field (SCM msg_smob, MuMsgFieldId mfid)
{
	MuMsgWrapper *msgwrap;
	msgwrap = (MuMsgWrapper*) SCM_CDR(msg_smob);

	return mu_msg_get_field_numeric(msgwrap->_msg, mfid);
}


static SCM
msg_date (SCM msg_smob)
{
	return scm_from_unsigned_integer
		(msg_num_field (msg_smob, MU_MSG_FIELD_ID_DATE));	
}

static SCM
msg_size (SCM msg_smob)
{
	return scm_from_unsigned_integer
		(msg_num_field (msg_smob, MU_MSG_FIELD_ID_SIZE));	
}


static SCM
msg_prio (SCM msg_smob)
{
	MuMsgPrio prio;
	MuMsgWrapper *msgwrap;
	msgwrap = (MuMsgWrapper*) SCM_CDR(msg_smob);

	prio = mu_msg_get_prio (msgwrap->_msg);

	switch (prio) {
	case MU_MSG_PRIO_LOW: return scm_from_utf8_symbol ("low");
	case MU_MSG_PRIO_NORMAL: return scm_from_utf8_symbol ("normal");
	case MU_MSG_PRIO_HIGH: return scm_from_utf8_symbol ("high");
	default:
		g_return_val_if_reached (SCM_UNDEFINED);
	}	
}


struct _FlagData {
	MuMsgFlags flags;
	SCM lst;
};
typedef struct _FlagData FlagData;


static void
check_flag (MuMsgFlags flag, FlagData *fdata)
{
	if (fdata->flags & flag) {
		SCM item;
		item = scm_list_1 (scm_from_utf8_symbol(mu_msg_flag_name(flag)));
		fdata->lst = scm_append_x (scm_list_2(fdata->lst, item));
	}	
}

static SCM
msg_flags (SCM msg_smob)
{
	MuMsgWrapper *msgwrap;
	FlagData fdata;

	msgwrap = (MuMsgWrapper*) SCM_CDR(msg_smob);
	
	fdata.flags = mu_msg_get_flags (msgwrap->_msg);
	fdata.lst = SCM_EOL;
	mu_msg_flags_foreach ((MuMsgFlagsForeachFunc)check_flag,
			      &fdata);

	return fdata.lst;
}


static SCM
msg_subject (SCM msg_smob)
{
	return msg_str_field (msg_smob, MU_MSG_FIELD_ID_SUBJECT);
}

static SCM
msg_from (SCM msg_smob)
{
	return msg_str_field (msg_smob, MU_MSG_FIELD_ID_FROM);
}


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
				name ? scm_from_utf8_string(name) : SCM_UNDEFINED,
				addr ? scm_from_utf8_string(addr) : SCM_UNDEFINED));

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



static SCM
msg_to (SCM msg_smob)
{
	return contact_list_field (msg_smob, MU_MSG_FIELD_ID_TO);
}


static SCM
msg_cc (SCM msg_smob)
{
	return contact_list_field (msg_smob, MU_MSG_FIELD_ID_CC);
}


static SCM
msg_bcc (SCM msg_smob)
{
	return contact_list_field (msg_smob, MU_MSG_FIELD_ID_BCC);
}

static SCM
msg_path (SCM msg_smob)
{
	return msg_str_field (msg_smob, MU_MSG_FIELD_ID_PATH);
}


static SCM
msg_maildir (SCM msg_smob)
{
	return msg_str_field (msg_smob, MU_MSG_FIELD_ID_MAILDIR);
}

static SCM
msg_msgid (SCM msg_smob)
{
	return msg_str_field (msg_smob, MU_MSG_FIELD_ID_MSGID);
}



static SCM
msg_body (SCM msg_smob, SCM html_smob)
{
	MuMsgWrapper *msgwrap;
	gboolean html;
	const char *val;
	
	msgwrap = (MuMsgWrapper*) SCM_CDR(msg_smob);
	html = SCM_UNBNDP(html_smob) ? FALSE : html_smob == SCM_BOOL_T;
	
	if (html)
		val = mu_msg_get_body_html(msgwrap->_msg);
	else
		val = mu_msg_get_body_text(msgwrap->_msg);
		
	return val ? scm_from_utf8_string (val) : SCM_UNDEFINED;
}


static SCM
msg_header (SCM msg_smob, SCM header_smob)
{
	MuMsgWrapper *msgwrap;
	const char *header;
	const char *val;
	
	msgwrap = (MuMsgWrapper*) SCM_CDR(msg_smob);
	header  =  scm_to_utf8_string (header_smob);
	val     =  mu_msg_get_header(msgwrap->_msg, header);
	
	return val ? scm_from_utf8_string(val) : SCM_UNDEFINED;
}

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
		item = scm_list_1 (scm_from_utf8_string((const char*)lst->data));
		scmlst = scm_append_x (scm_list_2(scmlst, item));
	}

	return scmlst;
}

static SCM
msg_tags (SCM msg_smob)
{
	return msg_string_list_field (msg_smob, MU_MSG_FIELD_ID_TAGS);
}


static SCM
msg_references (SCM msg_smob)
{
	return msg_string_list_field (msg_smob, MU_MSG_FIELD_ID_REFS);
}




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
	scm_c_define ("high",		scm_from_int(MU_MSG_PRIO_HIGH));
	scm_c_define ("low",		scm_from_int(MU_MSG_PRIO_LOW));
	scm_c_define ("normal",		scm_from_int(MU_MSG_PRIO_NORMAL));

	/* message flags */
	scm_c_define ("new",		scm_from_int(MU_MSG_FLAG_NEW));
	scm_c_define ("passed",		scm_from_int(MU_MSG_FLAG_PASSED));
	scm_c_define ("replied",	scm_from_int(MU_MSG_FLAG_REPLIED));
	scm_c_define ("seen",		scm_from_int(MU_MSG_FLAG_SEEN));
	scm_c_define ("trashed",	scm_from_int(MU_MSG_FLAG_TRASHED));
	scm_c_define ("draft",		scm_from_int(MU_MSG_FLAG_DRAFT));
	scm_c_define ("flagged",	scm_from_int(MU_MSG_FLAG_FLAGGED));
	scm_c_define ("unread",		scm_from_int(MU_MSG_FLAG_UNREAD));
	scm_c_define ("signed",		scm_from_int(MU_MSG_FLAG_SIGNED));
	scm_c_define ("encrypted",	scm_from_int(MU_MSG_FLAG_ENCRYPTED));
	scm_c_define ("has-attach",	scm_from_int(MU_MSG_FLAG_HAS_ATTACH));
}



void*
mu_msg_guile_register (void *data)
{
	MSG_TAG = scm_make_smob_type ("msg", sizeof(MuMsgWrapper));
		
	scm_set_smob_mark  (MSG_TAG, msg_mark);
	scm_set_smob_free  (MSG_TAG, msg_free);
	scm_set_smob_print (MSG_TAG, msg_print);	

	define_symbols ();
	
	scm_c_define_gsubr ("make-msg-from-file", 1, 0, 0,
			    &msg_make_from_file);
	
	scm_c_define_gsubr ("from",        1, 0, 0, &msg_from);

	scm_c_define_gsubr ("subject",	   1, 0, 0, &msg_subject);
	scm_c_define_gsubr ("path",	   1, 0, 0, &msg_path);
	scm_c_define_gsubr ("maildir",	   1, 0, 0, &msg_maildir);
	scm_c_define_gsubr ("message-id",  1, 0, 0, &msg_msgid);

	scm_c_define_gsubr ("date",	   1, 0, 0, &msg_date);
	scm_c_define_gsubr ("size",	   1, 0, 0, &msg_size);
	
	scm_c_define_gsubr ("body",        1, 1, 0, &msg_body);
	scm_c_define_gsubr ("header",      2, 0, 0, &msg_header);

	/* lists */
	scm_c_define_gsubr ("tags",        1, 0, 0, &msg_tags);
	scm_c_define_gsubr ("references",  1, 0, 0, &msg_references);
	scm_c_define_gsubr ("to",	   1, 0, 0, &msg_to);
	scm_c_define_gsubr ("cc",	   1, 0, 0, &msg_cc);
	scm_c_define_gsubr ("bcc",	   1, 0, 0, &msg_bcc);

	scm_c_define_gsubr ("priority",	   1, 0, 0, &msg_prio);
	scm_c_define_gsubr ("flags",	   1, 0, 0, &msg_flags);

	
	return NULL;
}


