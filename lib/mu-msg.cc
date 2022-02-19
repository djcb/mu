/*
** Copyright (C) 2008-2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <functional>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
#include <stdlib.h>
#include <ctype.h>

#include <gmime/gmime.h>
#include <vector>
#include <array>


#include "gmime/gmime-message.h"
#include "mu-message-contact.hh"
#include "mu-msg-priv.hh" /* include before mu-msg.h */
#include "mu-msg.hh"
#include "utils/mu-str.h"

#include "mu-maildir.hh"

using namespace Mu;

/* note, we do the gmime initialization here rather than in
 * mu-runtime, because this way we don't need mu-runtime for simple
 * cases -- such as our unit tests. Also note that we need gmime init
 * even for the doc backend, as we use the address parsing functions
 * also there. */
static gboolean _gmime_initialized = FALSE;

static void
gmime_init(void)
{
	g_return_if_fail(!_gmime_initialized);

	g_mime_init();
	_gmime_initialized = TRUE;
}

static void
gmime_uninit(void)
{
	g_return_if_fail(_gmime_initialized);

	g_mime_shutdown();
	_gmime_initialized = FALSE;
}

static MuMsg*
msg_new(void)
{
	MuMsg* self;

	self            = g_new0(MuMsg, 1);
	self->_refcount = 1;

	return self;
}

MuMsg*
Mu::mu_msg_new_from_file(const char* path, const char* mdir, GError** err)
{
	MuMsg*     self;
	MuMsgFile* msgfile;
	gint64     start;

	g_return_val_if_fail(path, NULL);

	start = g_get_monotonic_time();

	if (G_UNLIKELY(!_gmime_initialized)) {
		gmime_init();
		atexit(gmime_uninit);
	}

	msgfile = mu_msg_file_new(path, mdir, err);
	if (!msgfile)
		return NULL;

	self        = msg_new();
	self->_file = msgfile;

	g_debug("created message from %s in %" G_GINT64_FORMAT " μs",
	        path,
	        g_get_monotonic_time() - start);

	return self;
}

MuMsg*
Mu::mu_msg_new_from_doc(XapianDocument* doc, GError** err)
{
	MuMsg*    self;
	MuMsgDoc* msgdoc;

	g_return_val_if_fail(doc, NULL);

	if (G_UNLIKELY(!_gmime_initialized)) {
		gmime_init();
		atexit(gmime_uninit);
	}

	msgdoc = mu_msg_doc_new(doc, err);
	if (!msgdoc)
		return NULL;

	self       = msg_new();
	self->_doc = msgdoc;

	return self;
}

static void
mu_msg_destroy(MuMsg* self)
{
	if (!self)
		return;

	mu_msg_file_destroy(self->_file);
	mu_msg_doc_destroy(self->_doc);

	{ /* cleanup the strings / lists we stored */
		mu_str_free_list(self->_free_later_str);
		for (auto cur = self->_free_later_lst; cur; cur = g_slist_next(cur))
			g_slist_free_full((GSList*)cur->data, g_free);
		g_slist_free(self->_free_later_lst);
	}

	g_free(self);
}

MuMsg*
Mu::mu_msg_ref(MuMsg* self)
{
	g_return_val_if_fail(self, NULL);

	++self->_refcount;

	return self;
}

void
Mu::mu_msg_unref(MuMsg* self)
{
	g_return_if_fail(self);
	g_return_if_fail(self->_refcount >= 1);

	if (--self->_refcount == 0)
		mu_msg_destroy(self);
}

static const gchar*
free_later_str(MuMsg* self, gchar* str)
{
	if (str)
		self->_free_later_str = g_slist_prepend(self->_free_later_str, str);
	return str;
}

static const GSList*
free_later_lst(MuMsg* self, GSList* lst)
{
	if (lst)
		self->_free_later_lst = g_slist_prepend(self->_free_later_lst, lst);
	return lst;
}

/* use this instead of mu_msg_get_path so we don't get into infinite
 * regress...*/
static const char*
get_path(MuMsg* self)
{
	char*    val;
	gboolean do_free;

	do_free = TRUE;
	val     = NULL;

	if (self->_doc)
		val = mu_msg_doc_get_str_field(self->_doc, MU_MSG_FIELD_ID_PATH);

	/* not in the cache yet? try to get it from the file backend,
	 * in case we are using that */
	if (!val && self->_file)
		val = mu_msg_file_get_str_field(self->_file, MU_MSG_FIELD_ID_PATH, &do_free);

	/* shouldn't happen */
	if (!val)
		g_warning("%s: message without path?!", __func__);

	return free_later_str(self, val);
}

/* for some data, we need to read the message file from disk */
gboolean
Mu::mu_msg_load_msg_file(MuMsg* self, GError** err)
{
	const char* path;

	g_return_val_if_fail(self, FALSE);

	if (self->_file)
		return TRUE; /* nothing to do */

	if (!(path = get_path(self))) {
		mu_util_g_set_error(err, MU_ERROR_INTERNAL, "cannot get path for message");
		return FALSE;
	}

	self->_file = mu_msg_file_new(path, NULL, err);

	return (self->_file != NULL);
}

void
Mu::mu_msg_unload_msg_file(MuMsg* msg)
{
	g_return_if_fail(msg);

	mu_msg_file_destroy(msg->_file);
	msg->_file = NULL;
}

static const GSList*
get_str_list_field(MuMsg* self, MuMsgFieldId mfid)
{
	GSList* val;

	val = NULL;

	if (self->_doc && mu_msg_field_xapian_value(mfid))
		val = mu_msg_doc_get_str_list_field(self->_doc, mfid);
	else if (mu_msg_field_gmime(mfid)) {
		/* if we don't have a file object yet, we need to
		 * create it from the file on disk */
		if (!mu_msg_load_msg_file(self, NULL))
			return NULL;
		val = mu_msg_file_get_str_list_field(self->_file, mfid);
	}

	return free_later_lst(self, val);
}

static const char*
get_str_field(MuMsg* self, MuMsgFieldId mfid)
{
	char*    val;
	gboolean do_free;

	do_free = TRUE;
	val     = NULL;

	if (self->_doc && mu_msg_field_xapian_value(mfid))
		val = mu_msg_doc_get_str_field(self->_doc, mfid);

	else if (mu_msg_field_gmime(mfid)) {
		/* if we don't have a file object yet, we need to
		 * create it from the file on disk */
		if (!mu_msg_load_msg_file(self, NULL))
			return NULL;
		val = mu_msg_file_get_str_field(self->_file, mfid, &do_free);
	} else
		val = NULL;

	return do_free ? free_later_str(self, val) : val;
}

static gint64
get_num_field(MuMsg* self, MuMsgFieldId mfid)
{
	if (self->_doc && mu_msg_field_xapian_value(mfid))
		return mu_msg_doc_get_num_field(self->_doc, mfid);

	/* if we don't have a file object yet, we need to
	 * create it from the file on disk */
	if (!mu_msg_load_msg_file(self, NULL))
		return -1;

	return mu_msg_file_get_num_field(self->_file, mfid);
}

const char*
Mu::mu_msg_get_header(MuMsg* self, const char* header)
{
	GError* err;

	g_return_val_if_fail(self, NULL);
	g_return_val_if_fail(header, NULL);

	/* if we don't have a file object yet, we need to
	 * create it from the file on disk */
	err = NULL;
	if (!mu_msg_load_msg_file(self, &err)) {
		g_warning("failed to load message file: %s",
		          err ? err->message : "something went wrong");
		return NULL;
	}

	return free_later_str(self, mu_msg_file_get_header(self->_file, header));
}

time_t
Mu::mu_msg_get_timestamp(MuMsg* self)
{
	const char* path;
	struct stat statbuf;

	g_return_val_if_fail(self, 0);

	if (self->_file)
		return self->_file->_timestamp;

	path = mu_msg_get_path(self);
	if (!path || stat(path, &statbuf) < 0)
		return 0;

	return statbuf.st_mtime;
}

const char*
Mu::mu_msg_get_path(MuMsg* self)
{
	g_return_val_if_fail(self, NULL);
	return get_str_field(self, MU_MSG_FIELD_ID_PATH);
}

const char*
Mu::mu_msg_get_subject(MuMsg* self)
{
	g_return_val_if_fail(self, NULL);
	return get_str_field(self, MU_MSG_FIELD_ID_SUBJECT);
}

const char*
Mu::mu_msg_get_msgid(MuMsg* self)
{
	g_return_val_if_fail(self, NULL);
	return get_str_field(self, MU_MSG_FIELD_ID_MSGID);
}

const char*
Mu::mu_msg_get_mailing_list(MuMsg* self)
{
	const char* ml;
	char*       decml;

	g_return_val_if_fail(self, NULL);

	ml = get_str_field(self, MU_MSG_FIELD_ID_MAILING_LIST);
	if (!ml)
		return NULL;

	decml = g_mime_utils_header_decode_text(NULL, ml);
	if (!decml)
		return NULL;

	return free_later_str(self, decml);
}

const char*
Mu::mu_msg_get_maildir(MuMsg* self)
{
	g_return_val_if_fail(self, NULL);
	return get_str_field(self, MU_MSG_FIELD_ID_MAILDIR);
}

const char*
Mu::mu_msg_get_from(MuMsg* self)
{
	g_return_val_if_fail(self, NULL);
	return get_str_field(self, MU_MSG_FIELD_ID_FROM);
}

const char*
Mu::mu_msg_get_to(MuMsg* self)
{
	g_return_val_if_fail(self, NULL);
	return get_str_field(self, MU_MSG_FIELD_ID_TO);
}

const char*
Mu::mu_msg_get_cc(MuMsg* self)
{
	g_return_val_if_fail(self, NULL);
	return get_str_field(self, MU_MSG_FIELD_ID_CC);
}

const char*
Mu::mu_msg_get_bcc(MuMsg* self)
{
	g_return_val_if_fail(self, NULL);
	return get_str_field(self, MU_MSG_FIELD_ID_BCC);
}

time_t
Mu::mu_msg_get_date(MuMsg* self)
{
	g_return_val_if_fail(self, (time_t)-1);
	return (time_t)get_num_field(self, MU_MSG_FIELD_ID_DATE);
}

MessageFlags
Mu::mu_msg_get_flags(MuMsg* self)
{
	g_return_val_if_fail(self, MessageFlags::None);
	return static_cast<MessageFlags>(get_num_field(self, MU_MSG_FIELD_ID_FLAGS));
}

size_t
Mu::mu_msg_get_size(MuMsg* self)
{
	g_return_val_if_fail(self, (size_t)-1);
	return (size_t)get_num_field(self, MU_MSG_FIELD_ID_SIZE);
}

Mu::MessagePriority
Mu::mu_msg_get_prio(MuMsg* self)
{
	g_return_val_if_fail(self, MessagePriority{});

	return message_priority_from_char(
	    static_cast<char>(get_num_field(self, MU_MSG_FIELD_ID_PRIO)));
}

struct _BodyData {
	GString* gstr;
	gboolean want_html;
};
typedef struct _BodyData BodyData;

static void
accumulate_body(MuMsg* msg, MuMsgPart* mpart, BodyData* bdata)
{
	char*      txt;
	GMimePart* mimepart;
	gboolean   has_err, is_plain, is_html;

	if (!GMIME_IS_PART(mpart->data))
		return;
	if (mpart->part_type & MU_MSG_PART_TYPE_ATTACHMENT)
		return;

	mimepart = (GMimePart*)mpart->data;
	is_html  = mpart->part_type & MU_MSG_PART_TYPE_TEXT_HTML;
	is_plain = mpart->part_type & MU_MSG_PART_TYPE_TEXT_PLAIN;

	txt     = NULL;
	has_err = TRUE;
	if ((bdata->want_html && is_html) || (!bdata->want_html && is_plain))
		txt = mu_msg_mime_part_to_string(mimepart, &has_err);

	if (!has_err && txt)
		bdata->gstr = g_string_append(bdata->gstr, txt);

	g_free(txt);
}

static char*
get_body(MuMsg* self, MuMsgOptions opts, gboolean want_html)
{
	BodyData bdata;

	bdata.want_html = want_html;
	bdata.gstr      = g_string_sized_new(4096);

	/* wipe out some irrelevant options */
	opts &= ~MU_MSG_OPTION_VERIFY;
	opts &= ~MU_MSG_OPTION_EXTRACT_IMAGES;

	mu_msg_part_foreach(self, opts, (MuMsgPartForeachFunc)accumulate_body, &bdata);

	if (bdata.gstr->len == 0) {
		g_string_free(bdata.gstr, TRUE);
		return NULL;
	} else
		return g_string_free(bdata.gstr, FALSE);
}

typedef struct {
	GMimeContentType* ctype;
	gboolean          want_html;
} ContentTypeData;

static void
find_content_type(MuMsg* msg, MuMsgPart* mpart, ContentTypeData* cdata)
{
	GMimePart* wanted;

	if (!GMIME_IS_PART(mpart->data))
		return;

	/* text-like attachments are included when in text-mode */

	if (!cdata->want_html && (mpart->part_type & MU_MSG_PART_TYPE_TEXT_PLAIN))
		wanted = (GMimePart*)mpart->data;
	else if (!(mpart->part_type & MU_MSG_PART_TYPE_ATTACHMENT) && cdata->want_html &&
	         (mpart->part_type & MU_MSG_PART_TYPE_TEXT_HTML))
		wanted = (GMimePart*)mpart->data;
	else
		wanted = NULL;

	if (wanted)
		cdata->ctype = g_mime_object_get_content_type(GMIME_OBJECT(wanted));
}

static const GSList*
get_content_type_parameters(MuMsg* self, MuMsgOptions opts, gboolean want_html)
{
	ContentTypeData cdata;

	cdata.want_html = want_html;
	cdata.ctype     = NULL;

	/* wipe out some irrelevant options */
	opts &= ~MU_MSG_OPTION_VERIFY;
	opts &= ~MU_MSG_OPTION_EXTRACT_IMAGES;

	mu_msg_part_foreach(self, opts, (MuMsgPartForeachFunc)find_content_type, &cdata);

	if (cdata.ctype) {
		GSList*           gslist;
		GMimeParamList*   paramlist;
		const GMimeParam* param;
		int               i, len;

		gslist    = NULL;
		paramlist = g_mime_content_type_get_parameters(cdata.ctype);
		len       = g_mime_param_list_length(paramlist);

		for (i = 0; i < len; ++i) {
			param  = g_mime_param_list_get_parameter_at(paramlist, i);
			gslist = g_slist_prepend(gslist, g_strdup(param->name));
			gslist = g_slist_prepend(gslist, g_strdup(param->value));
		}

		return free_later_lst(self, g_slist_reverse(gslist));
	}
	return NULL;
}

const GSList*
Mu::mu_msg_get_body_text_content_type_parameters(MuMsg* self, MuMsgOptions opts)
{
	g_return_val_if_fail(self, NULL);
	return get_content_type_parameters(self, opts, FALSE);
}

const char*
Mu::mu_msg_get_body_html(MuMsg* self, MuMsgOptions opts)
{
	g_return_val_if_fail(self, NULL);
	return free_later_str(self, get_body(self, opts, TRUE));
}

const char*
Mu::mu_msg_get_body_text(MuMsg* self, MuMsgOptions opts)
{
	g_return_val_if_fail(self, NULL);
	return free_later_str(self, get_body(self, opts, FALSE));
}

const GSList*
Mu::mu_msg_get_references(MuMsg* self)
{
	g_return_val_if_fail(self, NULL);
	return get_str_list_field(self, MU_MSG_FIELD_ID_REFS);
}

const GSList*
Mu::mu_msg_get_tags(MuMsg* self)
{
	g_return_val_if_fail(self, NULL);
	return get_str_list_field(self, MU_MSG_FIELD_ID_TAGS);
}

const char*
Mu::mu_msg_get_field_string(MuMsg* self, MuMsgFieldId mfid)
{
	g_return_val_if_fail(self, NULL);
	return get_str_field(self, mfid);
}

const GSList*
Mu::mu_msg_get_field_string_list(MuMsg* self, MuMsgFieldId mfid)
{
	g_return_val_if_fail(self, NULL);
	return get_str_list_field(self, mfid);
}

gint64
Mu::mu_msg_get_field_numeric(MuMsg* self, MuMsgFieldId mfid)
{
	g_return_val_if_fail(self, -1);
	return get_num_field(self, mfid);
}


static Mu::MessageContacts
get_all_contacts(MuMsg *self)
{
	MessageContacts contacts;

	for (auto&& mtype : { MessageContact::Type::From, MessageContact::Type::To,
			      MessageContact::Type::Cc, MessageContact::Type::ReplyTo,
			      MessageContact::Type::Bcc}) {
		auto type_contacts{mu_msg_get_contacts(self, mtype)};
		
		contacts.reserve(contacts.size() + type_contacts.size());
		contacts.insert(contacts.end(), type_contacts.begin(), type_contacts.end());
	}

	return contacts;
}
			
Mu::MessageContacts
Mu::mu_msg_get_contacts(MuMsg *self, MessageContact::Type mtype)
{
	typedef const char*(*AddressFunc)(MuMsg*);
	using	AddressInfo = std::pair<GMimeAddressType, AddressFunc>;

	g_return_val_if_fail(self, MessageContacts{});

	if (mtype == MessageContact::Type::Unknown)
		return get_all_contacts(self);
	
	const auto info = std::invoke([&]()->AddressInfo {
		switch (mtype) {
		case MessageContact::Type::From:
			return { GMIME_ADDRESS_TYPE_FROM, mu_msg_get_from };
		case MessageContact::Type::To:
			return { GMIME_ADDRESS_TYPE_TO, mu_msg_get_to };
		case MessageContact::Type::Cc:
			return { GMIME_ADDRESS_TYPE_CC, mu_msg_get_cc };
		case MessageContact::Type::ReplyTo:
			return { GMIME_ADDRESS_TYPE_REPLY_TO, {} };
		case MessageContact::Type::Bcc:
			return { GMIME_ADDRESS_TYPE_BCC, mu_msg_get_bcc };
		default:
			throw std::logic_error("bug");
		}
	});

	const auto mdate{mu_msg_get_date(self)};
	if (self->_file) {
		if (auto&& lst{g_mime_message_get_addresses(
					self->_file->_mime_msg, info.first)}; lst)
			return make_message_contacts(lst, mtype, mdate);
	} else if (info.second) {
		if (auto&& lst_str{info.second(self)}; lst_str) 
			return make_message_contacts(lst_str, mtype, mdate);
	}
	
	return {};
		
}


gboolean
Mu::mu_msg_is_readable(MuMsg* self)
{
	g_return_val_if_fail(self, FALSE);

	return access(mu_msg_get_path(self), R_OK) == 0 ? TRUE : FALSE;
}


/*
 * move a msg to another maildir, trying to maintain 'integrity',
 * ie. msg in 'new/' will go to new/, one in cur/ goes to cur/. be
 * super-paranoid here...
 */
bool
Mu::mu_msg_move_to_maildir(MuMsg*		self,
			   const std::string&	root_maildir_path,
                           const std::string&	target_maildir,
			   MessageFlags		flags,
                           bool			ignore_dups,
                           bool			new_name,
                           GError**		err)
{
	g_return_val_if_fail(self, false);


	const auto srcpath{mu_msg_get_path(self)};
	const auto dstpath{mu_maildir_determine_target(srcpath,
						       root_maildir_path,
						       target_maildir,
						       flags,
						       new_name)};
	if (!dstpath)
		return false;

	if (!mu_maildir_move_message(srcpath, *dstpath, ignore_dups))
		return false;

	/* clear the old backends */
	mu_msg_doc_destroy(self->_doc);
	self->_doc = NULL;
	mu_msg_file_destroy(self->_file);

	/* and create a new one */
	self->_file = mu_msg_file_new(dstpath->c_str(), target_maildir.c_str(), err);

	return !!self->_file;
}


static void
cleanup_contact(char* contact)
{
	char *c, *c2;

	/* replace "'<> with space */
	for (c2 = contact; *c2; ++c2)
		if (*c2 == '"' || *c2 == '\'' || *c2 == '<' || *c2 == '>')
			*c2 = ' ';

	/* remove everything between '()' if it's after the 5th pos;
	 * good to cleanup corporate contact address spam... */
	c = g_strstr_len(contact, -1, "(");
	if (c && c - contact > 5)
		*c = '\0';

	g_strstrip(contact);
}

/* this is still somewhat simplistic... */
const char*
Mu::mu_str_display_contact_s(const char* str)
{
	static gchar contact[255];
	gchar *      c, *c2;

	str = str ? str : "";
	g_strlcpy(contact, str, sizeof(contact));

	/* we check for '<', so we can strip out the address stuff in
	 * e.g. 'Hello World <hello@world.xx>, but only if there is
	 * something alphanumeric before the <
	 */
	c = g_strstr_len(contact, -1, "<");
	if (c != NULL) {
		for (c2 = contact; c2 < c && !(isalnum(*c2)); ++c2)
			;
		if (c2 != c) /* apparently, there was something,
		              * so we can remove the <... part*/
			*c = '\0';
	}

	cleanup_contact(contact);

	return contact;
}

char*
Mu::mu_str_display_contact(const char* str)
{
	g_return_val_if_fail(str, NULL);

	return g_strdup(mu_str_display_contact_s(str));
}
