/*
** Copyright (C) 2022 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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


#include "mu-message.hh"
#include "mu-maildir.hh"

#include <utils/mu-util.h>
#include <utils/mu-utils.hh>
#include <utils/mu-error.hh>

#include <atomic>
#include <mutex>
#include <cstdlib>

#include <glib.h>
#include <gmime/gmime.h>

#include "gmime/gmime-message.h"

using namespace Mu;

/* note, we do the gmime initialization here rather than in mu-runtime, because this way
 * we don't need mu-runtime for simple cases -- such as our unit tests. Also note that we
 * need gmime init even for the doc backend, as we use the address parsing functions also
 * there. */
static bool
gmime_maybe_init(void)
{
	static std::atomic_bool gmime_initialized = false;

	if (gmime_initialized)
		return true;

	static std::mutex lock;
	g_debug("initializing gmime %u.%u.%u",
		gmime_major_version,
		gmime_minor_version,
		gmime_micro_version);

	g_mime_init();
	std::atexit([] {
		g_debug("shutting down gmime");
		g_mime_shutdown();
		gmime_initialized = false;
	});

	return true;
}

static GMimeMessage*
make_mime_message(const std::string& path, GError** err)
{
	GMimeStream *stream{g_mime_stream_file_open(path.c_str(), "r", err)};
	if (!stream)
		return {};

	GMimeParser *parser{g_mime_parser_new_with_stream(stream)};
	g_object_unref(stream);
	if (!parser) {
		g_set_error(err,MU_ERROR_DOMAIN, MU_ERROR_GMIME,
			    "cannot create mime parser for %s", path.c_str());
		return {};
	}

	GMimeMessage *mime_msg{g_mime_parser_construct_message(parser, NULL)};
	g_object_unref(parser);
	if (!mime_msg) {
		g_set_error(err, MU_ERROR_DOMAIN, MU_ERROR_GMIME,
			    "message seems invalid, ignoring (%s)", path.c_str());
		return {};
	}

	return mime_msg;
}

static void fill_document(Document& doc, GMimeMessage *mime_msg);


Message::Message(const std::string& path, const std::string& mdir)
{
	gmime_maybe_init();

	/*
	 * sanity checks.
	 */

	if (!g_path_is_absolute(path.c_str()))
		throw Error(Error::Code::File, "path '%s' is not absolute", path.c_str());

	if (::access(path.c_str(), R_OK) != 0)
		throw Error(Error::Code::File, "file @ '%s' is not readable", path.c_str());

	struct stat statbuf{};
	if (::stat(path.c_str(), &statbuf) < 0)
		throw Error(Error::Code::File, "cannot stat %s: %s", path.c_str(),
			    g_strerror(errno));

	if (!S_ISREG(statbuf.st_mode))
		throw Error(Error::Code::File, "not a regular file: %s", path);

	/*
	 * let's get the mime message
	 */
	GError *err{};
	mime_msg_ = make_mime_message(path, &err);
	if (!mime_msg_)
		throw Error(Error::Code::File, &err, "invalid message");

	doc_.add(Field::Id::Path,
		 Mu::from_gchars(g_canonicalize_filename(path.c_str(), NULL)));
	doc_.add(Field::Id::Maildir, mdir);
	doc_.add(Field::Id::Size, static_cast<int64_t>(statbuf.st_size));

	// rest of the fields
	//fill_fields(doc_, mime_msg_);
}

Message::~Message()
{
	g_clear_object(&mime_msg_);
}


Message&
Message::operator=(const Message& rhs) {

	if (this != &rhs) {
		doc_ = rhs.doc_;
		g_clear_object(&mime_msg_);
		if (rhs.mime_msg_)
			mime_msg_ = g_object_ref(rhs.mime_msg_);
	}

	return *this;
}


Message&
Message::operator=(Message&& rhs)
{
	if (this != &rhs) {
		doc_ = std::move(rhs.doc_);
		rhs.doc_ = {};

		g_clear_object(&mime_msg_);
		mime_msg_ = rhs.mime_msg_;
		rhs.mime_msg_ = {};
	}

	return *this;
}


static Priority
parse_prio_str(const char* priostr)
{
	int i;
	struct {
		const char*     _str;
		Priority _prio;
	} str_prio[] = {{"high", Priority::High},
			{"1", Priority::High},
			{"2", Priority::High},

			{"normal", Priority::Normal},
			{"3", Priority::Normal},

			{"low", Priority::Low},
			{"list", Priority::Low},
			{"bulk", Priority::Low},
			{"4", Priority::Low},
			{"5", Priority::Low}};

	for (i = 0; i != G_N_ELEMENTS(str_prio); ++i)
		if (g_ascii_strcasecmp(priostr, str_prio[i]._str) == 0)
			return str_prio[i]._prio;

	/* e.g., last-fm uses 'fm-user'... as precedence */
	return Priority::Normal;
}

static Priority
get_priority(GMimeMessage *mime_msg)
{
	auto obj{GMIME_OBJECT(mime_msg)};
	auto priostr = g_mime_object_get_header(obj, "Precedence");
	if (!priostr)
		priostr = g_mime_object_get_header(obj, "X-Priority");
	if (!priostr)
		priostr = g_mime_object_get_header(obj, "Importance");
	if (!priostr)
		return Priority::Normal;
	else
		return parse_prio_str(priostr);
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
msg_cflags_cb(GMimeObject* parent, GMimeObject* part, Flags* flags)
{
	if (GMIME_IS_MULTIPART_SIGNED(part))
		*flags |= Flags::Signed;

	/* FIXME: An encrypted part might be signed at the same time.
	 *        In that case the signed flag is lost. */
	if (GMIME_IS_MULTIPART_ENCRYPTED(part))
		*flags |= Flags::Encrypted;

	/* smime */
	if (GMIME_IS_APPLICATION_PKCS7_MIME(part)) {
		GMimeApplicationPkcs7Mime *pkcs7;
		pkcs7 = GMIME_APPLICATION_PKCS7_MIME(part);
		if (pkcs7) {
			switch(pkcs7->smime_type) {
			case GMIME_SECURE_MIME_TYPE_ENVELOPED_DATA:
				*flags |= Flags::Encrypted;
				break;
			case GMIME_SECURE_MIME_TYPE_SIGNED_DATA:
				*flags |= Flags::Signed;
				break;
			default:
				break;
			}
		}
	}

	if (any_of(*flags & Flags::HasAttachment))
		return;

	if (!GMIME_IS_PART(part))
		return;

	if (looks_like_attachment(part))
		*flags |= Flags::HasAttachment;
}

static Flags
get_content_flags(GMimeMessage *mime_msg)
{
	Flags flags{Flags::None};

	/* toplevel */
	msg_cflags_cb(NULL, GMIME_OBJECT(mime_msg), &flags);
	/* parts */
	// mu_mime_message_foreach(mime_msg,
	//			FALSE, /* never decrypt for this */
	//			(GMimeObjectForeachFunc)msg_cflags_cb,
	//			&flags);


	// char *ml{get_mailing_list(self)};
	// if (ml) {
	//	flags |= Flags::MailingList;
	//	g_free(ml);
	// }

	return flags;
}

static Flags
get_flags(GMimeMessage *mime_msg, const std::string& path)
{
	auto flags{mu_maildir_flags_from_path(path)
		.value_or(Flags::None)};
	flags |= get_content_flags(mime_msg);

	/* pseudo-flag --> unread means either NEW or NOT SEEN, just
	 * for searching convenience */
	if (any_of(flags & Flags::New) ||
	    none_of(flags & Flags::Seen))
		flags |= Flags::Unread;

	return flags;
}

static void
fill_document(Document& doc, GMimeMessage *mime_msg)
{

	//const auto contacts{mu_msg_get_contacts(msg)};
	const auto path{doc.string_value(Field::Id::Path)};

	// auto add_str=[&](Document& doc, Field::Id field_id, const char *str) {
	//	if (str)
	//		doc.add(field_id, std::string(str));
	// };

	field_for_each([&](auto&& field) {

		if (!field.is_indexable_term() && !field.is_normal_term() && !field.is_value())
			return;
		// else if (field.is_contact())
		//	doc.add(field.id, contacts);
		else if (field.id == Field::Id::Priority)
			doc.add(get_priority(mime_msg));
		// else if (field.id == Field::Id::Flags)
		//	doc.add(get_flags(mime_
		else if (field.id == Field::Id::ThreadId) {
			// refs contains a list of parent messages, with the
			// oldest one first until the last one, which is the
			// direct parent of the current message. of course, it
			// may be empty.
			//
			// NOTE: there may be cases where the list is truncated;
			// we happily ignore that case.
			// const auto refs{mu_msg_get_references(msg)};
			// const auto thread_id{refs ? (const char*)refs->data : mu_msg_get_msgid(msg)};
			// doc.add(Field::Id::ThreadId, std::string(thread_id));
		}
		// else if (field.id == Field::Id::BodyText)
		//	add_str(doc, field.id, mu_msg_get_body_text(msg, MU_MSG_OPTION_NONE));
		// else if (field.id == Field::Id::BodyHtml)
		//	add_str(doc, field.id, mu_msg_get_body_html(msg, MU_MSG_OPTION_NONE));
		// else if (field.id == Field::Id::EmbeddedText || field.id == Field::Id::File) {
		//	/* handle with MIME */
		// } else if (field.id == Field::Id::Mime)
		//	mu_msg_part_foreach(msg, MU_MSG_OPTION_RECURSE_RFC822,
		//			    (MuMsgPartForeachFunc)each_part, &doc);
		// else if (field.is_numerical())
		//	doc.add(field.id, mu_msg_get_field_numeric(msg, field.id));
		// else if (field.is_string())
		//	add_str(doc, field.id, mu_msg_get_field_string(msg, field.id));
		// else if (field.is_string_list()) {
		//	std::vector<std::string> vec;
		//	auto vals{mu_msg_get_field_string_list(msg, field.id)};
		//	while (vals) {
		//		vec.emplace_back ((const char*)vals->data);
		//		vals = g_slist_next((GList*)vals);
		//	}
		//	doc.add(field.id, vec);
		else {
			g_warning("unhandled field %*s", STR_V(field.name));
		}
	});

	//contacts_cache_.add(std::move(contacts));
}


std::string
Message::header(const std::string& header_field) const
{
	if (!mime_msg_)
		return {};

	const char *hdr = g_mime_object_get_header(GMIME_OBJECT(mime_msg_),
						   header_field.c_str());
	if (!hdr)
		return {};

	if (!g_utf8_validate(hdr, -1, {})) {
		char *hdr_u{g_strdup(hdr)};
		for (auto c = hdr_u; c && *c; ++c) {
			if ((!isprint(*c) && !isspace (*c)) || !isascii(*c))
				*c = '.';
		}
		return from_gchars(std::move(hdr_u));
	}

	return hdr;
}
