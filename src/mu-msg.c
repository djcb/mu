/* -*- mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-
**
** Copyright (C) 2008-2011 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
	self->_cache = mu_msg_cache_new ();

	self->_file = NULL;
	self->_doc  = NULL;

	return self;
}

MuMsg*
mu_msg_new_from_file (const char *path, const char *mdir, GError **err)
{
	MuMsg *self;
	MuMsgFile *msgfile;

	g_return_val_if_fail (path, NULL);

	if (G_UNLIKELY(!_gmime_initialized)) {
		gmime_init ();
		g_atexit (gmime_uninit);
	}

	msgfile = mu_msg_file_new (path, mdir, err);
	if (!msgfile)
		return NULL;

	self = msg_new ();
	self->_file = msgfile;

	return self;
}


void
mu_msg_close_file_backend (MuMsg *msg)
{
	g_return_if_fail (msg);

	mu_msg_file_destroy (msg->_file);
	msg->_file = NULL;
}


MuMsg*
mu_msg_new_from_doc (XapianDocument *doc, GError **err)
{
	MuMsg *self;
	MuMsgDoc *msgdoc;

	g_return_val_if_fail (doc, NULL);

	if (G_UNLIKELY(!_gmime_initialized)) {
		gmime_init ();
		g_atexit (gmime_uninit);
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

	mu_msg_cache_destroy (self->_cache);

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


/* use this instead of mu_msg_get_path so we don't get into infinite
 * regress...*/
static const char*
get_path (MuMsg *self)
{
	const char *path;
	char *val;
	gboolean do_free;

	/* try to get the path from the cache */
	path = mu_msg_cache_str (self->_cache, MU_MSG_FIELD_ID_PATH);
	if (path)
		return path;

	/* nothing found yet? try the doc in case we are using that
	 * backend */
	val = NULL;
	if (self->_doc)
		val = mu_msg_doc_get_str_field (self->_doc,
						MU_MSG_FIELD_ID_PATH,
						&do_free);

	/* not in the cache yet? try to get it from the file backend,
	 * in case we are using that */
	if (!val && self->_file)
		val = mu_msg_file_get_str_field (self->_file,
						 MU_MSG_FIELD_ID_PATH,
						 &do_free);

	/* this cannot happen unless there are bugs in mu */
	if (!val) {
		g_warning ("%s: cannot find path", __FUNCTION__);
		return NULL;
	}

	/* we found something */
	return mu_msg_cache_set_str (self->_cache,
				     MU_MSG_FIELD_ID_PATH, val,
				     do_free);
}


/* for some data, we need to read the message file from disk */
static MuMsgFile*
get_msg_file (MuMsg *self)
{
	MuMsgFile *mfile;
	const char *path;
	GError *err;

	if (!(path = get_path (self)))
		return NULL;

	err = NULL;
	mfile = mu_msg_file_new (path, NULL, &err);
	if (!mfile) {
		g_warning ("%s: failed to create MuMsgFile: %s",
			   __FUNCTION__, err->message ? err->message : "?");
		g_error_free (err);
		return NULL;
	}

	return mfile;
}


static const GSList*
get_str_list_field (MuMsg *self, MuMsgFieldId mfid)
{
	gboolean do_free;
	GSList *val;

	/* first we try the cache */
	if (mu_msg_cache_cached (self->_cache, mfid))
		return mu_msg_cache_str_list (self->_cache, mfid);

	/* if it's not in the cache but it is a value retrievable from
	 * the doc backend, use that */
	val = NULL;
	if (self->_doc && mu_msg_field_xapian_value (mfid))
		val = mu_msg_doc_get_str_list_field (self->_doc,
						     mfid, &do_free);
	else {
		/* if we don't have a file object yet, we need to
		 * create it from the file on disk */
		if (!self->_file)
			self->_file = get_msg_file (self);
		if (!self->_file && !(self->_file = get_msg_file (self)))
			return NULL;
		val = mu_msg_file_get_str_list_field (self->_file, mfid,
						      &do_free);
	}

	/* if we get a string that needs freeing, we tell the cache to
	 * mark the string as such, so it will be freed when the cache
	 * is freed (or when the value is overwritten) */
	return mu_msg_cache_set_str_list (self->_cache, mfid, val,
					  do_free);
}




static const char*
get_str_field (MuMsg *self, MuMsgFieldId mfid)
{
	gboolean do_free;
	char *val;

	/* first we try the cache */
	if (mu_msg_cache_cached (self->_cache, mfid))
		return mu_msg_cache_str (self->_cache, mfid);

	/* if it's not in the cache but it is a value retrievable from
	 * the doc backend, use that */
	val = NULL;
	if (self->_doc && mu_msg_field_xapian_value (mfid))
		val = mu_msg_doc_get_str_field (self->_doc, mfid, &do_free);
	else if (mu_msg_field_gmime (mfid)) {
		/* if we don't have a file object yet, we need to
		 * create it from the file on disk */
		if (!self->_file)
			self->_file = get_msg_file (self);
		if (!self->_file && !(self->_file = get_msg_file (self)))
			return NULL;
		val = mu_msg_file_get_str_field (self->_file, mfid, &do_free);
	} else {
		g_warning ("%s: cannot retrieve field", __FUNCTION__);
		return NULL;
	}

	/* if we get a string that needs freeing, we tell the cache to
	 * mark the string as such, so it will be freed when the cache
	 * is freed (or when the value is overwritten) */
	return mu_msg_cache_set_str (self->_cache, mfid, val, do_free);
}


static gint64
get_num_field (MuMsg *self, MuMsgFieldId mfid)
{
	guint64 val;

	/* first try the cache */
	if (mu_msg_cache_cached (self->_cache, mfid))
		return mu_msg_cache_num (self->_cache, mfid);

	/* if it's not in the cache but it is a value retrievable from
	 * the doc backend, use that */
	val = -1;
	if (self->_doc && mu_msg_field_xapian_value (mfid))
		val = mu_msg_doc_get_num_field (self->_doc, mfid);
	else {
		/* if we don't have a file object yet, we need to
		 * create it from the file on disk */
		if (!self->_file)
			self->_file = get_msg_file (self);
		if (!self->_file && !(self->_file = get_msg_file (self)))
			return -1;
		val = mu_msg_file_get_num_field (self->_file, mfid);
	}

	return mu_msg_cache_set_num (self->_cache, mfid, val);
}


const char*
mu_msg_get_header (MuMsg *self, const char *header)
{
	g_return_val_if_fail (self, NULL);
	g_return_val_if_fail (header, NULL);

	/* if we don't have a file object yet, we need to
	 * create it from the file on disk */
	if (!self->_file)
		self->_file = get_msg_file (self);
	if (!self->_file && !(self->_file = get_msg_file (self)))
		return NULL;

	return mu_msg_file_get_header (self->_file, header);
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


const char*
mu_msg_get_body_html (MuMsg *self)
{
	g_return_val_if_fail (self, NULL);
	return get_str_field (self, MU_MSG_FIELD_ID_BODY_HTML);
}


const char*
mu_msg_get_body_text (MuMsg *self)
{
	g_return_val_if_fail (self, NULL);
	return get_str_field (self, MU_MSG_FIELD_ID_BODY_TEXT);
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



MuMsgContact *
mu_msg_contact_new (const char *name, const char *address,
		    MuMsgContactType type)
{
	MuMsgContact *self;

	g_return_val_if_fail (name, NULL);
	g_return_val_if_fail (address, NULL);
	g_return_val_if_fail (!mu_msg_contact_type_is_valid(type),
			      NULL);

	self = g_slice_new (MuMsgContact);

	self->name	  = g_strdup (name);
	self->address = g_strdup (address);
	self->type    = type;

	return self;
}


void
mu_msg_contact_destroy (MuMsgContact *self)
{

	if (!self)
		return;

	g_free ((void*)self->name);
	g_free ((void*)self->address);
	g_slice_free (MuMsgContact, self);
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

	return TRUE;
}


static void
address_list_foreach (InternetAddressList *addrlist, MuMsgContactType ctype,
		      MuMsgContactForeachFunc func, gpointer user_data)
{
	int i;

	for (i = 0; addrlist && i != internet_address_list_length(addrlist);
	     ++i) {

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


void
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

	if (msg->_doc)
		msg_contact_foreach_doc (msg, func, user_data);
	else if (msg->_file)
		msg_contact_foreach_file (msg, func, user_data);
	else
		g_return_if_reached ();
}



static int
cmp_str (const char* s1, const char *s2)
{
	if (s1 == s2)
		return 0;
	else if (!s1)
		return -1;
	else if (!s2)
		return 1;

	return g_utf8_collate (s1, s2);
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

	return g_utf8_collate (
		mu_str_subject_normalize (s1),
		mu_str_subject_normalize (s2));
}


int
mu_msg_cmp (MuMsg *m1, MuMsg *m2, MuMsgFieldId mfid)
{
	g_return_val_if_fail (m1, 0);
	g_return_val_if_fail (m2, 0);
	g_return_val_if_fail (mu_msg_field_id_is_valid(mfid), 0);

	if (mfid == MU_MSG_FIELD_ID_SUBJECT)
		return cmp_subject (get_str_field (m1, mfid),
				    get_str_field (m2, mfid));

	if (mu_msg_field_is_string (mfid))
		return cmp_str (get_str_field (m1, mfid),
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

	return (access (get_str_field (self, MU_MSG_FIELD_ID_PATH), R_OK)
		== 0) ? TRUE : FALSE;
}



/* we need do to determine the
 *   /home/foo/Maildir/bar
 * from the /bar
 * that we got
 */
char*
get_target_mdir (MuMsg *msg, const char *maildir, GError **err)
{
	char *rootmaildir, *rv;

	if (!mu_msg_get_maildir(msg)) {
		g_set_error (err, 0, MU_ERROR_GMIME,
			     "message misses maildir "
			     "field, so cannot be moved");
		return NULL;
	}

	rootmaildir = mu_maildir_get_maildir_from_path (mu_msg_get_path(msg));
	if (!rootmaildir)
		return NULL;

	if (!g_str_has_suffix (rootmaildir, mu_msg_get_maildir(msg))) {
		g_set_error (err, 0, MU_ERROR_FILE,
			     "path is %s, but maildir is %s",
			     mu_msg_get_path(msg), mu_msg_get_maildir(msg));
		g_free (rootmaildir);
		return NULL;
	}

	rootmaildir[strlen(rootmaildir) - strlen (mu_msg_get_maildir(msg))] = '\0';
	rv = g_strconcat (rootmaildir, maildir, NULL);
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
			MuFlags flags, gboolean ignore_dups, GError **err)
{
	char *newfullpath;
	char *targetmdir;

	g_return_val_if_fail (self, FALSE);
	g_return_val_if_fail (maildir, FALSE);     /* i.e. "/inbox" */

	targetmdir = get_target_mdir (self, maildir, err);
	if (!targetmdir)
		return FALSE;

	newfullpath = mu_maildir_move_message (mu_msg_get_path (self),
					       targetmdir, flags,
					       ignore_dups, err);
	g_free (targetmdir);

	/* update the message path and the flags; they may have
	 * changed */
	if (newfullpath) {
		mu_msg_cache_set_str (self->_cache, MU_MSG_FIELD_ID_PATH, newfullpath,
				      TRUE); /* the cache will free the string */
		mu_msg_cache_set_str (self->_cache, MU_MSG_FIELD_ID_MAILDIR,
				      g_strdup(maildir), TRUE);
		/* the cache will free the string */

		/* the contentflags haven't changed, so make sure they persist */
		flags |= mu_msg_get_flags (self) &
			(MU_FLAG_HAS_ATTACH|MU_FLAG_ENCRYPTED|MU_FLAG_SIGNED);
		/* update the pseudo-flag as well */
		if (!(flags & MU_FLAG_NEW) && (flags & MU_FLAG_SEEN))
			flags &= ~MU_FLAG_UNREAD;
		else
			flags |= MU_FLAG_UNREAD;

		mu_msg_cache_set_num (self->_cache, MU_MSG_FIELD_ID_FLAGS, flags);
	}

	return newfullpath ? TRUE : FALSE;
}
