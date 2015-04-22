/* -*- mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-
**
** Copyright (C) 2008-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 3 of the License, or
** (at your option) any later version.
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
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
#include <stdlib.h>
#include <ctype.h>

#include <gmime/gmime.h>

#include "mu-msg-priv.h" /* include before mu-msg.h */
#include "mu-msg.h"

#include "mu-util.h"
#include "mu-str.h"

#include "mu-maildir.h"

/* note, we do the gmime initialization here rather than in
 * mu-runtime, because this way we don't need mu-runtime for simple
 * cases -- such as our unit tests. Also note that we need gmime init
 * even for the doc backend, as we use the address parsing functions
 * also there. */
static gboolean _gmime_initialized = FALSE;

static void
gmime_init (void)
{
	g_return_if_fail (!_gmime_initialized);

#ifdef GMIME_ENABLE_RFC2047_WORKAROUNDS
	g_mime_init(GMIME_ENABLE_RFC2047_WORKAROUNDS);
#else
	g_mime_init(0);
#endif /* GMIME_ENABLE_RFC2047_WORKAROUNDS */

	_gmime_initialized = TRUE;
}

static void
gmime_uninit (void)
{
	g_return_if_fail (_gmime_initialized);

	g_mime_shutdown();
	_gmime_initialized = FALSE;
}


static MuMsg*
msg_new (void)
{
	MuMsg *self;

	self = g_slice_new0 (MuMsg);
	self->_refcount = 1;

	return self;
}

MuMsg*
mu_msg_new_from_file (const char *path, const char *mdir,
		      GError **err)
{
	MuMsg *self;
	MuMsgFile *msgfile;

	g_return_val_if_fail (path, NULL);

	if (G_UNLIKELY(!_gmime_initialized)) {
		gmime_init ();
		atexit (gmime_uninit);
	}

	msgfile = mu_msg_file_new (path, mdir, err);
	if (!msgfile)
		return NULL;

	self = msg_new ();
	self->_file = msgfile;

	return self;
}


MuMsg*
mu_msg_new_from_doc (XapianDocument *doc, GError **err)
{
	MuMsg *self;
	MuMsgDoc *msgdoc;

	g_return_val_if_fail (doc, NULL);

	if (G_UNLIKELY(!_gmime_initialized)) {
		gmime_init ();
		atexit (gmime_uninit);
	}

	msgdoc = mu_msg_doc_new (doc, err);
	if (!msgdoc)
		return NULL;

	self = msg_new ();
	self->_doc = msgdoc;

	return self;
}


static void
mu_msg_destroy (MuMsg *self)
{
	if (!self)
		return;

	mu_msg_file_destroy (self->_file);
	mu_msg_doc_destroy  (self->_doc);

	{ /* cleanup the strings / lists we stored */
	 	mu_str_free_list (self->_free_later_str);
		g_slist_foreach (self->_free_later_lst,
				 (GFunc)mu_str_free_list, NULL);
		g_slist_free (self->_free_later_lst);
	}

	g_slice_free (MuMsg, self);
}


MuMsg*
mu_msg_ref (MuMsg *self)
{
	g_return_val_if_fail (self, NULL);

	++self->_refcount;

	return self;
}

void
mu_msg_unref (MuMsg *self)
{
	g_return_if_fail (self);
	g_return_if_fail (self->_refcount >= 1);

	if (--self->_refcount == 0)
		mu_msg_destroy (self);
}

static const gchar*
free_later_str (MuMsg *self, gchar *str)
{
	if (str)
		self->_free_later_str =
			g_slist_prepend (self->_free_later_str, str);
	return str;
}

static const GSList*
free_later_lst (MuMsg *self, GSList *lst)
{
	if (lst)
		self->_free_later_lst =
			g_slist_prepend (self->_free_later_lst, lst);
	return lst;
}


/* use this instead of mu_msg_get_path so we don't get into infinite
 * regress...*/
static const char*
get_path (MuMsg *self)
{
	char *val;
	gboolean do_free;

	do_free = TRUE;
	val     = NULL;

	if (self->_doc)
		val = mu_msg_doc_get_str_field
			(self->_doc, MU_MSG_FIELD_ID_PATH);

	/* not in the cache yet? try to get it from the file backend,
	 * in case we are using that */
	if (!val && self->_file)
		val = mu_msg_file_get_str_field
			(self->_file, MU_MSG_FIELD_ID_PATH, &do_free);

	/* shouldn't happen */
	if (!val)
		g_warning ("%s: cannot find path", __func__);

	return free_later_str (self, val);
}


/* for some data, we need to read the message file from disk */
gboolean
mu_msg_load_msg_file (MuMsg *self, GError **err)
{
	const char *path;

	g_return_val_if_fail (self, FALSE);

	if (self->_file)
		return TRUE; /* nothing to do */

	if (!(path = get_path (self))) {
		mu_util_g_set_error (err, MU_ERROR_INTERNAL,
				     "cannot get path for message");
		return FALSE;
	}

	self->_file = mu_msg_file_new (path, NULL, err);

	return  (self->_file != NULL);
}


void
mu_msg_unload_msg_file (MuMsg *msg)
{
	g_return_if_fail (msg);

	mu_msg_file_destroy (msg->_file);
	msg->_file = NULL;
}


static const GSList*
get_str_list_field (MuMsg *self, MuMsgFieldId mfid)
{
	GSList *val;

	val = NULL;

	if (self->_doc && mu_msg_field_xapian_value (mfid))
		val = mu_msg_doc_get_str_list_field (self->_doc, mfid);
	else if (mu_msg_field_gmime (mfid)) {
		/* if we don't have a file object yet, we need to
		 * create it from the file on disk */
		if (!mu_msg_load_msg_file (self, NULL))
			return NULL;
		val = mu_msg_file_get_str_list_field (self->_file, mfid);
	}

	return free_later_lst (self, val);
}


static const char*
get_str_field (MuMsg *self, MuMsgFieldId mfid)
{
	char *val;
	gboolean do_free;

	do_free = TRUE;
	val     = NULL;

	if (self->_doc && mu_msg_field_xapian_value (mfid))
		val = mu_msg_doc_get_str_field (self->_doc, mfid);

	else if (mu_msg_field_gmime (mfid)) {
		/* if we don't have a file object yet, we need to
		 * create it from the file on disk */
		if (!mu_msg_load_msg_file (self, NULL))
			return NULL;
		val = mu_msg_file_get_str_field (self->_file, mfid, &do_free);
	} else
		val = NULL;

	return do_free ? free_later_str (self, val) : val;
}


static gint64
get_num_field (MuMsg *self, MuMsgFieldId mfid)
{
	guint64 val;

	val = -1;
	if (self->_doc && mu_msg_field_xapian_value (mfid))
		val = mu_msg_doc_get_num_field (self->_doc, mfid);
	else {
		/* if we don't have a file object yet, we need to
		 * create it from the file on disk */
		if (!mu_msg_load_msg_file (self, NULL))
			return -1;
		val = mu_msg_file_get_num_field (self->_file, mfid);
	}

	return val;
}


const char*
mu_msg_get_header (MuMsg *self, const char *header)
{
	g_return_val_if_fail (self, NULL);
	g_return_val_if_fail (header, NULL);

	/* if we don't have a file object yet, we need to
	 * create it from the file on disk */
	if (!mu_msg_load_msg_file (self, NULL))
		return NULL;

	return free_later_str
		(self, mu_msg_file_get_header (self->_file, header));
}


time_t
mu_msg_get_timestamp (MuMsg *self)
{
	const char *path;
	struct stat statbuf;

	g_return_val_if_fail (self, 0);

	if (self->_file)
		return self->_file->_timestamp;

	path = mu_msg_get_path (self);
	if (!path || stat (path, &statbuf) < 0)
		return 0;

	return statbuf.st_mtime;
}


const char*
mu_msg_get_path (MuMsg *self)
{
	g_return_val_if_fail (self, NULL);
	return get_str_field (self, MU_MSG_FIELD_ID_PATH);
}


const char*
mu_msg_get_subject  (MuMsg *self)
{
	g_return_val_if_fail (self, NULL);
	return get_str_field (self, MU_MSG_FIELD_ID_SUBJECT);
}

const char*
mu_msg_get_msgid  (MuMsg *self)
{
	g_return_val_if_fail (self, NULL);
	return get_str_field (self, MU_MSG_FIELD_ID_MSGID);
}



const char*
mu_msg_get_mailing_list (MuMsg *self)
{
	g_return_val_if_fail (self, NULL);
	return get_str_field (self, MU_MSG_FIELD_ID_MAILING_LIST);
}


const char*
mu_msg_get_maildir (MuMsg *self)
{
	g_return_val_if_fail (self, NULL);
	return get_str_field (self, MU_MSG_FIELD_ID_MAILDIR);
}


const char*
mu_msg_get_from (MuMsg *self)
{
	g_return_val_if_fail (self, NULL);
	return get_str_field (self, MU_MSG_FIELD_ID_FROM);
}


const char*
mu_msg_get_to (MuMsg *self)
{
	g_return_val_if_fail (self, NULL);
	return get_str_field (self, MU_MSG_FIELD_ID_TO);
}

const char*
mu_msg_get_cc (MuMsg *self)
{
	g_return_val_if_fail (self, NULL);
	return get_str_field (self, MU_MSG_FIELD_ID_CC);
}


const char*
mu_msg_get_bcc (MuMsg *self)
{
	g_return_val_if_fail (self, NULL);
	return get_str_field (self, MU_MSG_FIELD_ID_BCC);
}


time_t
mu_msg_get_date (MuMsg *self)
{
	g_return_val_if_fail (self, (time_t)-1);
	return (time_t)get_num_field (self, MU_MSG_FIELD_ID_DATE);
}


MuFlags
mu_msg_get_flags (MuMsg *self)
{
	g_return_val_if_fail (self, MU_FLAG_NONE);
	return (MuFlags)get_num_field (self, MU_MSG_FIELD_ID_FLAGS);
}

size_t
mu_msg_get_size (MuMsg *self)
{
	g_return_val_if_fail (self, (size_t)-1);
	return (size_t)get_num_field (self, MU_MSG_FIELD_ID_SIZE);
}


MuMsgPrio
mu_msg_get_prio (MuMsg *self)
{
	g_return_val_if_fail (self, MU_MSG_PRIO_NORMAL);
	return (MuMsgPrio)get_num_field (self, MU_MSG_FIELD_ID_PRIO);
}



struct _BodyData {
	GString *gstr;
	gboolean want_html;
};
typedef struct _BodyData BodyData;


static void
accumulate_body (MuMsg *msg, MuMsgPart *mpart, BodyData *bdata)
{
	char *txt;
	gboolean err;

	/* if it looks like an attachment, skip it */
	if (mpart->part_type & MU_MSG_PART_TYPE_ATTACHMENT)
		return;

	if (!GMIME_IS_PART(mpart->data))
		return;

	txt = NULL;
	err = TRUE;

	if (!bdata->want_html &&
	    (mpart->part_type & MU_MSG_PART_TYPE_TEXT_PLAIN))
		txt = mu_msg_mime_part_to_string (
			(GMimePart*)mpart->data, &err);
	else if (bdata->want_html &&
		 (mpart->part_type & MU_MSG_PART_TYPE_TEXT_HTML))
		txt = mu_msg_mime_part_to_string (
			(GMimePart*)mpart->data, &err);
	if (!err && txt)
		bdata->gstr = g_string_append (bdata->gstr, txt);

	g_free (txt);
}


static char*
get_body (MuMsg *self, MuMsgOptions opts, gboolean want_html)
{
	BodyData bdata;

	bdata.want_html = want_html;
	bdata.gstr	= g_string_sized_new (4096);

	mu_msg_part_foreach (self, opts,
			     (MuMsgPartForeachFunc)accumulate_body,
			     &bdata);

	if (bdata.gstr->len == 0) {
		g_string_free (bdata.gstr, TRUE);
		return NULL;
	} else
		return g_string_free (bdata.gstr, FALSE);
}

const char*
mu_msg_get_body_html (MuMsg *self, MuMsgOptions opts)
{
	g_return_val_if_fail (self, NULL);
	return free_later_str (self, get_body (self, opts, TRUE));
}



const char*
mu_msg_get_body_text (MuMsg *self, MuMsgOptions opts)
{
	g_return_val_if_fail (self, NULL);
	return free_later_str (self, get_body (self, opts, FALSE));
}


const GSList*
mu_msg_get_references (MuMsg *self)
{
	g_return_val_if_fail (self, NULL);
	return get_str_list_field (self, MU_MSG_FIELD_ID_REFS);
}


const GSList*
mu_msg_get_tags (MuMsg *self)
{
	g_return_val_if_fail (self, NULL);
	return get_str_list_field (self, MU_MSG_FIELD_ID_TAGS);
}


const char*
mu_msg_get_field_string (MuMsg *self, MuMsgFieldId mfid)
{
	g_return_val_if_fail (self, NULL);
	return get_str_field (self, mfid);
}


const GSList*
mu_msg_get_field_string_list (MuMsg *self, MuMsgFieldId mfid)
{
	g_return_val_if_fail (self, NULL);
	return get_str_list_field (self, mfid);
}



gint64
mu_msg_get_field_numeric (MuMsg *self, MuMsgFieldId mfid)
{
	g_return_val_if_fail (self, -1);
	return get_num_field (self, mfid);
}


static gboolean
fill_contact (MuMsgContact *self, InternetAddress *addr,
	      MuMsgContactType ctype)
{
	if (!addr)
		return FALSE;

	self->name = internet_address_get_name (addr);
	self->type = ctype;

	/* we only support internet mailbox addresses; if we don't
	 * check, g_mime hits an assert
	 */
	if (INTERNET_ADDRESS_IS_MAILBOX(addr))
		self->address = internet_address_mailbox_get_addr
			(INTERNET_ADDRESS_MAILBOX(addr));
	else
		self->address  = NULL;

	/* if there's no address, just a name, it's probably a local
	 * address (without @) */
	if (self->name && !self->address)
		self->address = self->name;

	/* note, the address could NULL e.g. when the recipient is something like
	 * 'Undisclosed recipients'
	 */
	return self->address != NULL;
}


static void
address_list_foreach (InternetAddressList *addrlist, MuMsgContactType ctype,
		      MuMsgContactForeachFunc func, gpointer user_data)
{
	int i, len;

	if (!addrlist)
		return;

	len = internet_address_list_length(addrlist);

	for (i = 0; i != len; ++i) {
		MuMsgContact contact;
		if (!fill_contact(&contact,
				  internet_address_list_get_address (addrlist, i),
				  ctype))
			continue;

		if (!(func)(&contact, user_data))
			break;
	}
}


static void
addresses_foreach (const char* addrs, MuMsgContactType ctype,
		   MuMsgContactForeachFunc func, gpointer user_data)
{
	InternetAddressList *addrlist;

	if (!addrs)
		return;

	addrlist = internet_address_list_parse_string (addrs);
	if (addrlist) {
		address_list_foreach (addrlist, ctype, func, user_data);
		g_object_unref (addrlist);
	}
}


static void
msg_contact_foreach_file (MuMsg *msg, MuMsgContactForeachFunc func,
			  gpointer user_data)
{
	int i;
	struct {
		GMimeRecipientType     _gmime_type;
		MuMsgContactType       _type;
	} ctypes[] = {
		{GMIME_RECIPIENT_TYPE_TO,  MU_MSG_CONTACT_TYPE_TO},
		{GMIME_RECIPIENT_TYPE_CC,  MU_MSG_CONTACT_TYPE_CC},
		{GMIME_RECIPIENT_TYPE_BCC, MU_MSG_CONTACT_TYPE_BCC},
	};

	/* sender */
	addresses_foreach (g_mime_message_get_sender (msg->_file->_mime_msg),
			   MU_MSG_CONTACT_TYPE_FROM, func, user_data);

	/* reply_to */
	addresses_foreach (g_mime_message_get_reply_to (msg->_file->_mime_msg),
			   MU_MSG_CONTACT_TYPE_REPLY_TO, func, user_data);

	/* get to, cc, bcc */
	for (i = 0; i != G_N_ELEMENTS(ctypes); ++i) {
		InternetAddressList *addrlist;
		addrlist = g_mime_message_get_recipients (msg->_file->_mime_msg,
							  ctypes[i]._gmime_type);
		address_list_foreach (addrlist, ctypes[i]._type, func, user_data);
	}
}


static void
msg_contact_foreach_doc (MuMsg *msg, MuMsgContactForeachFunc func,
			 gpointer user_data)
{
	addresses_foreach (mu_msg_get_from (msg),
			   MU_MSG_CONTACT_TYPE_FROM, func, user_data);
	addresses_foreach (mu_msg_get_to (msg),
			   MU_MSG_CONTACT_TYPE_TO, func, user_data);
	addresses_foreach (mu_msg_get_cc (msg),
			   MU_MSG_CONTACT_TYPE_CC, func, user_data);
	addresses_foreach (mu_msg_get_bcc (msg),
			   MU_MSG_CONTACT_TYPE_BCC, func, user_data);
}


void
mu_msg_contact_foreach (MuMsg *msg, MuMsgContactForeachFunc func,
			gpointer user_data)
{
	g_return_if_fail (msg);
	g_return_if_fail (func);

	if (msg->_file)
		msg_contact_foreach_file (msg, func, user_data);
	else if (msg->_doc)
		msg_contact_foreach_doc (msg, func, user_data);
	else
		g_return_if_reached ();
}



static int
cmp_str (const char *s1, const char *s2)
{
	if (s1 == s2)
		return 0;
	else if (!s1)
		return -1;
	else if (!s2)
		return 1;

	/* optimization 1: ascii */
	if (isascii(s1[0]) && isascii(s2[0])) {
		int diff;
		diff = tolower(s1[0]) - tolower(s2[0]);
		if (diff != 0)
			return diff;
	}

	/* utf 8 */
	{
		char *u1, *u2;
		int diff;

		u1 = g_utf8_strdown (s1, -1);
		u2 = g_utf8_strdown (s2, -1);

		diff = g_utf8_collate (u1, u2);

		g_free (u1);
		g_free (u2);

		return diff;
	}
}


static int
cmp_subject (const char* s1, const char *s2)
{
	if (s1 == s2)
		return 0;
	else if (!s1)
		return -1;
	else if (!s2)
		return 1;

	return cmp_str (
		mu_str_subject_normalize (s1),
		mu_str_subject_normalize (s2));
}


int
mu_msg_cmp (MuMsg *m1, MuMsg *m2, MuMsgFieldId mfid)
{
	g_return_val_if_fail (m1, 0);
	g_return_val_if_fail (m2, 0);
	g_return_val_if_fail (mu_msg_field_id_is_valid(mfid), 0);

	/* even though date is a numeric field, we can sort it by its
	 * string repr. in the database, which is much faster */
	if (mfid == MU_MSG_FIELD_ID_DATE ||
	    mu_msg_field_is_string (mfid))
		return cmp_str (get_str_field (m1, mfid),
				get_str_field (m2, mfid));

	if (mfid == MU_MSG_FIELD_ID_SUBJECT)
		return cmp_subject (get_str_field (m1, mfid),
				    get_str_field (m2, mfid));

	/* TODO: note, we cast (potentially > MAXINT to int) */
	if (mu_msg_field_is_numeric (mfid))
		return get_num_field(m1, mfid) - get_num_field(m2, mfid);

	return 0; /* TODO: handle lists */
}


gboolean
mu_msg_is_readable (MuMsg *self)
{
	g_return_val_if_fail (self, FALSE);

	return access (mu_msg_get_path (self), R_OK) == 0 ? TRUE : FALSE;
}



/* we need do to determine the
 *   /home/foo/Maildir/bar
 * from the /bar
 * that we got
 */
char*
get_target_mdir (MuMsg *msg, const char *target_maildir, GError **err)
{
	char *rootmaildir, *rv;
	const char *maildir;
	gboolean not_top_level;

	/* maildir is the maildir stored in the message, e.g. '/foo' */
	maildir = mu_msg_get_maildir(msg);
	if (!maildir) {
		mu_util_g_set_error (err, MU_ERROR_GMIME,
				     "message without maildir");
		return NULL;
	}

	/* the 'rootmaildir' is the filesystem path from root to
	 * maildir, ie.  /home/user/Maildir/foo */
	rootmaildir = mu_maildir_get_maildir_from_path (mu_msg_get_path(msg));
	if (!rootmaildir) {
		mu_util_g_set_error (err, MU_ERROR_GMIME,
				     "cannot determine maildir");
		return NULL;
	}

	/* we do a sanity check: verify that that maildir is a suffix of
	 * rootmaildir;*/
	not_top_level = TRUE;
	if (!g_str_has_suffix (rootmaildir, maildir) &&
	    /* special case for the top-level '/' maildir, and
	     * remember not_top_level */
	    (not_top_level = (g_strcmp0 (maildir, "/") != 0))) {
		g_set_error (err, MU_ERROR_DOMAIN, MU_ERROR_FILE,
			     "path is '%s', but maildir is '%s' ('%s')",
			     rootmaildir, mu_msg_get_maildir(msg),
			     mu_msg_get_path (msg));
		g_free (rootmaildir);
		return NULL;
	}

	/* if we're not at the top-level, remove the final '/' from
	 * the rootmaildir */
	if (not_top_level)
		rootmaildir[strlen(rootmaildir) -
			    strlen (mu_msg_get_maildir(msg))] = '\0';

	rv = g_strconcat (rootmaildir, target_maildir, NULL);
	g_free (rootmaildir);

	return rv;
}


/*
 * move a msg to another maildir, trying to maintain 'integrity',
 * ie. msg in 'new/' will go to new/, one in cur/ goes to cur/. be
 * super-paranoid here...
 */
gboolean
mu_msg_move_to_maildir (MuMsg *self, const char *maildir,
			MuFlags flags, gboolean ignore_dups,
			gboolean new_name, GError **err)
{
	char *newfullpath;
	char *targetmdir;

	g_return_val_if_fail (self, FALSE);
	g_return_val_if_fail (maildir, FALSE);     /* i.e. "/inbox" */

	/* targetmdir is the full path to maildir, i.e.,
	 * /home/foo/Maildir/inbox */
	targetmdir = get_target_mdir (self, maildir, err);
	if (!targetmdir)
		return FALSE;

	newfullpath = mu_maildir_move_message (mu_msg_get_path (self),
					       targetmdir, flags,
					       ignore_dups, new_name,
					       err);
	if (!newfullpath) {
		g_free (targetmdir);
		return FALSE;
	}

	/* clear the old backends */
	mu_msg_doc_destroy  (self->_doc);
	self->_doc = NULL;

	mu_msg_file_destroy (self->_file);

	/* and create a new one */
	self->_file = mu_msg_file_new (newfullpath, maildir, err);
	g_free (targetmdir);

	return self->_file ? TRUE : FALSE;
}
