/*
** Copyright (C) 2012-2021 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <array>

#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
#include <stdlib.h>
#include <ctype.h>
#include <inttypes.h>

#include <gmime/gmime.h>
#include "mu-maildir.hh"
#include "mu-store.hh"
#include "mu-msg-priv.hh"

#include "utils/mu-util.h"
#include "utils/mu-str.h"

using namespace Mu;

static gboolean
init_file_metadata(MuMsgFile* self, const char* path, const char* mdir, GError** err);
static gboolean init_mime_msg(MuMsgFile* msg, const char* path, GError** err);

MuMsgFile*
Mu::mu_msg_file_new(const char* filepath, const char* mdir, GError** err)
{
	MuMsgFile* self;

	g_return_val_if_fail(filepath, NULL);

	self = g_new0(MuMsgFile, 1);

	if (!init_file_metadata(self, filepath, mdir, err)) {
		mu_msg_file_destroy(self);
		return NULL;
	}

	if (!init_mime_msg(self, filepath, err)) {
		mu_msg_file_destroy(self);
		return NULL;
	}

	return self;
}

void
Mu::mu_msg_file_destroy(MuMsgFile* self)
{
	if (!self)
		return;

	g_clear_object(&self->_mime_msg);

	g_free(self->_path);
	g_free(self->_maildir);
	g_free(self->_sha1);

	g_free(self);
}

static gboolean
init_file_metadata(MuMsgFile* self, const char* path, const gchar* mdir, GError** err)
{
	struct stat statbuf;

	if (access(path, R_OK) != 0) {
		mu_util_g_set_error(err,
		                    MU_ERROR_FILE,
		                    "cannot read file %s: %s",
		                    path,
		                    g_strerror(errno));
		return FALSE;
	}

	if (stat(path, &statbuf) < 0) {
		mu_util_g_set_error(err,
		                    MU_ERROR_FILE,
		                    "cannot stat %s: %s",
		                    path,
		                    g_strerror(errno));
		return FALSE;
	}

	if (!S_ISREG(statbuf.st_mode)) {
		mu_util_g_set_error(err, MU_ERROR_FILE, "not a regular file: %s", path);
		return FALSE;
	}

	self->_timestamp = statbuf.st_mtime;
	self->_size      = (size_t)statbuf.st_size;
	self->_path      = g_canonicalize_filename(path, NULL);
	self->_maildir   = g_strdup(mdir ? mdir : "");

	return TRUE;
}

static char*
calculate_sha1(FILE* file)
{
	std::array<uint8_t, 4096> buf{};
	char*                     sha1{};
	GChecksum*                checksum{g_checksum_new(G_CHECKSUM_SHA256)};

	while (true) {
		const auto n = ::fread(buf.data(), 1, buf.size(), file);
		if (n == 0)
			break;
		g_checksum_update(checksum, buf.data(), n);
	}

	if (::ferror(file))
		g_warning("error reading file");
	else
		sha1 = g_strdup(g_checksum_get_string(checksum));

	g_checksum_free(checksum);

	return sha1;
}

static GMimeStream*
get_mime_stream(MuMsgFile* self, const char* path, GError** err)
{
	FILE*        file;
	GMimeStream* stream;

	file = fopen(path, "r");
	if (!file) {
		g_set_error(err,
		            MU_ERROR_DOMAIN,
		            MU_ERROR_FILE,
		            "cannot open %s: %s",
		            path,
		            g_strerror(errno));
		return NULL;
	}

	stream = g_mime_stream_file_new(file);
	if (!stream) {
		g_set_error(err,
		            MU_ERROR_DOMAIN,
		            MU_ERROR_GMIME,
		            "cannot create mime stream for %s",
		            path);
		fclose(file);
		return NULL;
	}

	self->_sha1 = calculate_sha1(file);
	if (!self->_sha1) {
		::fclose(file);
		g_set_error(err,
		            MU_ERROR_DOMAIN,
		            MU_ERROR_FILE,
		            "failed to get sha-1 for %s",
		            path);
		return NULL;
	}

	return stream;
}

static gboolean
init_mime_msg(MuMsgFile* self, const char* path, GError** err)
{
	GMimeStream* stream;
	GMimeParser* parser;

	stream = get_mime_stream(self, path, err);
	if (!stream)
		return FALSE;

	parser = g_mime_parser_new_with_stream(stream);
	g_object_unref(stream);
	if (!parser) {
		g_set_error(err,
		            MU_ERROR_DOMAIN,
		            MU_ERROR_GMIME,
		            "cannot create mime parser for %s",
		            path);
		return FALSE;
	}

	self->_mime_msg = g_mime_parser_construct_message(parser, NULL);
	g_object_unref(parser);
	if (!self->_mime_msg) {
		g_set_error(err,
		            MU_ERROR_DOMAIN,
		            MU_ERROR_GMIME,
		            "message seems invalid, ignoring (%s)",
		            path);
		return FALSE;
	}

	return TRUE;
}

static char*
get_recipient(MuMsgFile* self, GMimeAddressType atype)
{
	char*                recip;
	InternetAddressList* recips;

	recips = g_mime_message_get_addresses(self->_mime_msg, atype);

	/* FALSE --> don't encode */
	recip = (char*)internet_address_list_to_string(recips, NULL, FALSE);

	if (recip && !g_utf8_validate(recip, -1, NULL)) {
		g_debug("invalid recipient in %s\n", self->_path);
		mu_str_asciify_in_place(recip); /* ugly... */
	}

	if (mu_str_is_empty(recip)) {
		g_free(recip);
		return NULL;
	}

	if (recip)
		mu_str_remove_ctrl_in_place(recip);

	return recip;
}

/*
 * let's try to guess the mailing list from some other
 * headers in the mail
 */
static gchar*
get_fake_mailing_list_maybe(MuMsgFile* self)
{
	const char* hdr;

	hdr = g_mime_object_get_header(GMIME_OBJECT(self->_mime_msg), "X-Feed2Imap-Version");
	if (!hdr)
		return NULL;

	/* looks like a feed2imap header; guess the source-blog
	 * from the msgid */
	{
		const char *msgid, *e;
		msgid = g_mime_message_get_message_id(self->_mime_msg);
		if (msgid && (e = strchr(msgid, '-')))
			return g_strndup(msgid, e - msgid);
	}

	return NULL;
}

static gchar*
get_mailing_list(MuMsgFile* self)
{
	char *      dechdr, *res;
	const char *hdr, *b, *e;

	hdr = g_mime_object_get_header(GMIME_OBJECT(self->_mime_msg), "List-Id");
	if (mu_str_is_empty(hdr))
		return get_fake_mailing_list_maybe(self);

	dechdr = g_mime_utils_header_decode_phrase(NULL, hdr);
	if (!dechdr)
		return NULL;

	e = NULL;
	b = strchr(dechdr, '<');
	if (b)
		e = strchr(b, '>');

	if (b && e)
		res = g_strndup(b + 1, e - b - 1);
	else
		res = g_strdup(dechdr);

	g_free(dechdr);

	return res;
}

static gboolean
looks_like_attachment(GMimeObject* part)
{
	GMimeContentDisposition* disp;
	GMimeContentType*        ctype;
	const char*              dispstr;
	guint                    u;
	const struct {
		const char* type;
		const char* sub_type;
	} att_types[] = {{"image", "*"},
	                 {"audio", "*"},
	                 {"application", "*"},
	                 {"application", "x-patch"}};

	disp = g_mime_object_get_content_disposition(part);

	if (!GMIME_IS_CONTENT_DISPOSITION(disp))
		return FALSE;

	dispstr = g_mime_content_disposition_get_disposition(disp);

	if (g_ascii_strcasecmp(dispstr, "attachment") == 0)
		return TRUE;

	/* we also consider patches, images, audio, and non-pgp-signature
	 * application attachments to be attachments... */
	ctype = g_mime_object_get_content_type(part);

	if (g_mime_content_type_is_type(ctype, "*", "pgp-signature"))
		return FALSE; /* don't consider as a signature */

	if (g_mime_content_type_is_type(ctype, "text", "*")) {
		if (g_mime_content_type_is_type(ctype, "*", "plain") ||
		    g_mime_content_type_is_type(ctype, "*", "html"))
			return FALSE;
		else
			return TRUE;
	}

	for (u = 0; u != G_N_ELEMENTS(att_types); ++u)
		if (g_mime_content_type_is_type(ctype, att_types[u].type, att_types[u].sub_type))
			return TRUE;

	return FALSE;
}

static void
msg_cflags_cb(GMimeObject* parent, GMimeObject* part, MuFlags* flags)
{
	if (GMIME_IS_MULTIPART_SIGNED(part))
		*flags |= MU_FLAG_SIGNED;

	/* FIXME: An encrypted part might be signed at the same time.
	 *        In that case the signed flag is lost. */
	if (GMIME_IS_MULTIPART_ENCRYPTED(part))
		*flags |= MU_FLAG_ENCRYPTED;

	/* smime */
	if (GMIME_IS_APPLICATION_PKCS7_MIME(part)) {
		GMimeApplicationPkcs7Mime *pkcs7;
		pkcs7 = GMIME_APPLICATION_PKCS7_MIME(part);
		if (pkcs7) {
			switch(pkcs7->smime_type) {
			case GMIME_SECURE_MIME_TYPE_ENVELOPED_DATA:
				*flags |= MU_FLAG_ENCRYPTED;
				break;
			case GMIME_SECURE_MIME_TYPE_SIGNED_DATA:
				*flags |= MU_FLAG_SIGNED;
				break;
			default:
				break;
			}
		}
	}

	if (*flags & MU_FLAG_HAS_ATTACH)
		return;

	if (!GMIME_IS_PART(part))
		return;

	if (*flags & MU_FLAG_HAS_ATTACH)
		return;

	if (looks_like_attachment(part))
		*flags |= MU_FLAG_HAS_ATTACH;
}


static MuFlags
get_content_flags(MuMsgFile* self)
{
	MuFlags flags;
	char*   ml;

	flags = MU_FLAG_NONE;

	if (GMIME_IS_MESSAGE(self->_mime_msg)) {
		/* toplevel */
		msg_cflags_cb(NULL, GMIME_OBJECT(self->_mime_msg), &flags);
		/* parts */
		mu_mime_message_foreach(self->_mime_msg,
		                        FALSE, /* never decrypt for this */
		                        (GMimeObjectForeachFunc)msg_cflags_cb,
		                        &flags);
	}

	ml = get_mailing_list(self);
	if (ml) {
		flags |= MU_FLAG_LIST;
		g_free(ml);
	}

	return flags;
}

static MuFlags
get_flags(MuMsgFile* self)
{
	MuFlags flags;

	g_return_val_if_fail(self, MU_FLAG_INVALID);

	flags = mu_maildir_get_flags_from_path(self->_path);
	flags |= get_content_flags(self);

	/* pseudo-flag --> unread means either NEW or NOT SEEN, just
	 * for searching convenience */
	if ((flags & MU_FLAG_NEW) || !(flags & MU_FLAG_SEEN))
		flags |= MU_FLAG_UNREAD;

	return flags;
}

static size_t
get_size(MuMsgFile* self)
{
	g_return_val_if_fail(self, 0);
	return self->_size;
}

static MessagePriority
parse_prio_str(const char* priostr)
{
	int i;
	struct {
		const char*     _str;
		MessagePriority _prio;
	} str_prio[] = {{"high", MessagePriority::High},
	                {"1", MessagePriority::High},
	                {"2", MessagePriority::High},

	                {"normal", MessagePriority::Normal},
	                {"3", MessagePriority::Normal},

	                {"low", MessagePriority::Low},
	                {"list", MessagePriority::Low},
	                {"bulk", MessagePriority::Low},
	                {"4", MessagePriority::Low},
	                {"5", MessagePriority::Low}};

	for (i = 0; i != G_N_ELEMENTS(str_prio); ++i)
		if (g_ascii_strcasecmp(priostr, str_prio[i]._str) == 0)
			return str_prio[i]._prio;

	/* e.g., last-fm uses 'fm-user'... as precedence */
	return MessagePriority::Normal;
}

static MessagePriority
get_prio(MuMsgFile* self)
{
	GMimeObject* obj;
	const char*  priostr;

	g_return_val_if_fail(self, MessagePriority::Normal);

	obj = GMIME_OBJECT(self->_mime_msg);

	priostr = g_mime_object_get_header(obj, "Precedence");
	if (!priostr)
		priostr = g_mime_object_get_header(obj, "X-Priority");
	if (!priostr)
		priostr = g_mime_object_get_header(obj, "Importance");
	if (!priostr)
		return MessagePriority::Normal;
	else
		return parse_prio_str(priostr);
}

/* NOTE: buffer will be *freed* or returned unchanged */
static char*
convert_to_utf8(GMimePart* part, char* buffer)
{
	GMimeContentType* ctype;
	const char*       charset;

	ctype = g_mime_object_get_content_type(GMIME_OBJECT(part));
	g_return_val_if_fail(GMIME_IS_CONTENT_TYPE(ctype), NULL);

	/* of course, the charset specified may be incorrect... */
	charset = g_mime_content_type_get_parameter(ctype, "charset");
	if (charset) {
		char* utf8;
		if ((utf8 = mu_str_convert_to_utf8(buffer, g_mime_charset_iconv_name(charset)))) {
			g_free(buffer);
			buffer = utf8;
		}
	} else if (!g_utf8_validate(buffer, -1, NULL)) {
		/* if it's already utf8, nothing to do otherwise: no
		   charset at all, or conversion failed; ugly * hack:
		   replace all non-ascii chars with '.' */
		mu_str_asciify_in_place(buffer);
	}

	return buffer;
}

static gchar*
stream_to_string(GMimeStream* stream, size_t buflen)
{
	char*   buffer;
	ssize_t bytes;

	buffer = g_new(char, buflen + 1);
	g_mime_stream_reset(stream);

	/* we read everything in one go */
	bytes = g_mime_stream_read(stream, buffer, buflen);
	if (bytes < 0) {
		g_warning("%s: failed to read from stream", __func__);
		g_free(buffer);
		return NULL;
	}

	buffer[bytes] = '\0';

	return buffer;
}

gchar*
Mu::mu_msg_mime_part_to_string(GMimePart* part, gboolean* err)
{
	GMimeDataWrapper* wrapper;
	GMimeStream*      stream;
	ssize_t           buflen;
	char*             buffer;

	buffer = NULL;
	stream = NULL;

	g_return_val_if_fail(err, NULL);

	*err = TRUE; /* guilty until proven innocent */
	g_return_val_if_fail(GMIME_IS_PART(part), NULL);

	wrapper = g_mime_part_get_content(part);
	if (!wrapper) {
		/* this happens with invalid mails */
		g_debug("failed to create data wrapper");
		goto cleanup;
	}

	stream = g_mime_stream_mem_new();
	if (!stream) {
		g_warning("failed to create mem stream");
		goto cleanup;
	}

	buflen = g_mime_data_wrapper_write_to_stream(wrapper, stream);
	if (buflen <= 0) { /* empty buffer, not an error */
		*err = FALSE;
		goto cleanup;
	}

	buffer = stream_to_string(stream, (size_t)buflen);

	/* convert_to_utf8 will free the old 'buffer' if needed */
	buffer = convert_to_utf8(part, buffer);
	*err   = FALSE;

cleanup:
	if (G_IS_OBJECT(stream))
		g_object_unref(stream);

	return buffer;
}

static gboolean
contains(GSList* lst, const char* str)
{
	for (; lst; lst = g_slist_next(lst))
		if (g_strcmp0((char*)lst->data, str) == 0)
			return TRUE;
	return FALSE;
}

/*
 * NOTE: this will get the list of references with the oldest parent
 * at the beginning */
static GSList*
get_references(MuMsgFile* self)
{
	GSList*     msgids;
	unsigned    u;
	const char* headers[] = {"References", "In-reply-to", NULL};

	for (msgids = NULL, u = 0; headers[u]; ++u) {
		char*            str;
		GMimeReferences* mime_refs;
		int              i, refs_len;

		str = mu_msg_file_get_header(self, headers[u]);
		if (!str)
			continue;

		mime_refs = g_mime_references_parse(NULL, str);
		g_free(str);

		refs_len = g_mime_references_length(mime_refs);
		for (i = 0; i < refs_len; ++i) {
			const char* msgid;
			msgid = g_mime_references_get_message_id(mime_refs, i);

			/* don't include duplicates */
			if (msgid && !contains(msgids, msgid))
				/* explicitly ensure it's utf8-safe,
				 * as GMime does not ensure that */
				msgids = g_slist_prepend(msgids, g_strdup((msgid)));
		}
		g_mime_references_free(mime_refs);
	}

	/* reverse, because we used g_slist_prepend for performance
	 * reasons */
	return g_slist_reverse(msgids);
}

/* see: http://does-not-exist.org/mail-archives/mutt-dev/msg08249.html */
static GSList*
get_tags(MuMsgFile* self)
{
	GSList*  lst;
	unsigned u;
	struct {
		const char* header;
		char        sepa;
	} tagfields[] = {{"X-Label", ' '}, {"X-Keywords", ','}, {"Keywords", ' '}};

	for (lst = NULL, u = 0; u != G_N_ELEMENTS(tagfields); ++u) {
		gchar* hdr;
		hdr = mu_msg_file_get_header(self, tagfields[u].header);
		if (hdr) {
			GSList* hlst;
			hlst = mu_str_to_list(hdr, tagfields[u].sepa, TRUE);

			if (lst)
				(g_slist_last(lst))->next = hlst;
			else
				lst = hlst;

			g_free(hdr);
		}
	}

	return lst;
}

static char*
cleanup_maybe(const char* str, gboolean* do_free)
{
	char* s;

	if (!str)
		return NULL;

	if (!g_utf8_validate(str, -1, NULL)) {
		if (*do_free)
			s = mu_str_asciify_in_place((char*)str);
		else {
			*do_free = TRUE;
			s        = mu_str_asciify_in_place(g_strdup(str));
		}
	} else
		s = (char*)str;

	mu_str_remove_ctrl_in_place(s);

	return s;
}

G_GNUC_CONST static GMimeAddressType
address_type(MuMsgFieldId mfid)
{
	switch (mfid) {
	case MU_MSG_FIELD_ID_BCC: return GMIME_ADDRESS_TYPE_BCC;
	case MU_MSG_FIELD_ID_CC: return GMIME_ADDRESS_TYPE_CC;
	case MU_MSG_FIELD_ID_TO: return GMIME_ADDRESS_TYPE_TO;
	case MU_MSG_FIELD_ID_FROM: return GMIME_ADDRESS_TYPE_FROM;
	default: g_return_val_if_reached((GMimeAddressType)-1);
	}
}

static gchar*
get_msgid(MuMsgFile* self, gboolean* do_free)
{
	const char* msgid{g_mime_message_get_message_id(self->_mime_msg)};
	if (msgid && strlen(msgid) < Store::MaxTermLength) {
		*do_free = FALSE;
		return (char*)msgid;
	}
	// if there's no valid message-id, synthesize one;
	// based on the contents so it stays valid if moved around.
	*do_free = TRUE;
	return g_strdup_printf("%s@mu", self->_sha1);
}

char*
Mu::mu_msg_file_get_str_field(MuMsgFile* self, MuMsgFieldId mfid, gboolean* do_free)
{
	g_return_val_if_fail(self, NULL);
	g_return_val_if_fail(mu_msg_field_is_string(mfid), NULL);

	*do_free = FALSE; /* default */

	switch (mfid) {
	case MU_MSG_FIELD_ID_BCC:
	case MU_MSG_FIELD_ID_CC:
	case MU_MSG_FIELD_ID_FROM:
	case MU_MSG_FIELD_ID_TO: *do_free = TRUE; return get_recipient(self, address_type(mfid));

	case MU_MSG_FIELD_ID_PATH: return self->_path;

	case MU_MSG_FIELD_ID_MAILING_LIST: *do_free = TRUE; return (char*)get_mailing_list(self);

	case MU_MSG_FIELD_ID_SUBJECT:
		return (char*)cleanup_maybe(g_mime_message_get_subject(self->_mime_msg), do_free);

	case MU_MSG_FIELD_ID_MSGID: return get_msgid(self, do_free);

	case MU_MSG_FIELD_ID_MAILDIR: return self->_maildir;

	case MU_MSG_FIELD_ID_BODY_TEXT: /* use mu_msg_get_body_text */
	case MU_MSG_FIELD_ID_BODY_HTML: /* use mu_msg_get_body_html */
	case MU_MSG_FIELD_ID_EMBEDDED_TEXT:
		g_warning("%s is not retrievable through: %s", mu_msg_field_name(mfid), __func__);
		return NULL;

	default: g_return_val_if_reached(NULL);
	}
}

GSList*
Mu::mu_msg_file_get_str_list_field(MuMsgFile* self, MuMsgFieldId mfid)
{
	g_return_val_if_fail(self, NULL);
	g_return_val_if_fail(mu_msg_field_is_string_list(mfid), NULL);

	switch (mfid) {
	case MU_MSG_FIELD_ID_REFS: return get_references(self);
	case MU_MSG_FIELD_ID_TAGS: return get_tags(self);
	default: g_return_val_if_reached(NULL);
	}
}

gint64
Mu::mu_msg_file_get_num_field(MuMsgFile* self, const MuMsgFieldId mfid)
{
	g_return_val_if_fail(self, -1);
	g_return_val_if_fail(mu_msg_field_is_numeric(mfid), -1);

	switch (mfid) {
	case MU_MSG_FIELD_ID_DATE: {
		GDateTime* dt;
		dt = g_mime_message_get_date(self->_mime_msg);
		return dt ? g_date_time_to_unix(dt) : 0;
	}

	case MU_MSG_FIELD_ID_FLAGS: return (gint64)get_flags(self);

	case MU_MSG_FIELD_ID_PRIO: return (gint64)get_prio(self);

	case MU_MSG_FIELD_ID_SIZE: return (gint64)get_size(self);

	default: g_return_val_if_reached(-1);
	}
}

char*
Mu::mu_msg_file_get_header(MuMsgFile* self, const char* header)
{
	const gchar* hdr;

	g_return_val_if_fail(self, NULL);
	g_return_val_if_fail(header, NULL);

	/* sadly, g_mime_object_get_header may return non-ascii;
	 * so, we need to ensure that
	 */
	hdr = g_mime_object_get_header(GMIME_OBJECT(self->_mime_msg), header);

	return hdr ? mu_str_utf8ify(hdr) : NULL;
}

struct _ForeachData {
	GMimeObjectForeachFunc user_func;
	gpointer               user_data;
	gboolean               decrypt;
};
typedef struct _ForeachData ForeachData;

static void
foreach_cb(GMimeObject* parent, GMimeObject* part, ForeachData* fdata)
{
	/* invoke the callback function */
	fdata->user_func(parent, part, fdata->user_data);

	/* maybe iterate over decrypted parts */
	if (fdata->decrypt && GMIME_IS_MULTIPART_ENCRYPTED(part)) {
		GMimeObject* dec;
		dec = mu_msg_crypto_decrypt_part(GMIME_MULTIPART_ENCRYPTED(part),
		                                 MU_MSG_OPTION_NONE,
		                                 NULL,
		                                 NULL,
		                                 NULL);
		if (!dec)
			return;

		if (GMIME_IS_MULTIPART(dec))
			g_mime_multipart_foreach((GMIME_MULTIPART(dec)),
			                         (GMimeObjectForeachFunc)foreach_cb,
			                         fdata);
		else
			foreach_cb(parent, dec, fdata);

		g_object_unref(dec);
	}
}

void
Mu::mu_mime_message_foreach(GMimeMessage*          msg,
                            gboolean               decrypt,
                            GMimeObjectForeachFunc func,
                            gpointer               user_data)
{
	ForeachData fdata;

	g_return_if_fail(GMIME_IS_MESSAGE(msg));
	g_return_if_fail(func);

	fdata.user_func = func;
	fdata.user_data = user_data;
	fdata.decrypt   = decrypt;

	g_mime_message_foreach(msg, (GMimeObjectForeachFunc)foreach_cb, &fdata);
}
