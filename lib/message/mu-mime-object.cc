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


#include "mu-mime-object.hh"
#include "gmime/gmime-message.h"
#include "utils/mu-utils.hh"
#include <mutex>
#include <fcntl.h>


using namespace Mu;



/* note, we do the gmime initialization here rather than in mu-runtime, because this way
 * we don't need mu-runtime for simple cases -- such as our unit tests. Also note that we
 * need gmime init even for the doc backend, as we use the address parsing functions also
 * there. */

void
Mu::init_gmime(void)
{
	// fast path.
	static bool gmime_initialized = false;
	if (gmime_initialized)
		return;

	static std::mutex gmime_lock;
	std::lock_guard lock (gmime_lock);
	if (gmime_initialized)
		return; // already

	g_debug("initializing gmime %u.%u.%u",
		gmime_major_version,
		gmime_minor_version,
		gmime_micro_version);

	g_mime_init();
	gmime_initialized = true;

	std::atexit([] {
		g_debug("shutting down gmime");
		g_mime_shutdown();
		gmime_initialized = false;
	});
}



/*
 * MimeObject
 */

Option<std::string>
MimeObject::header(const std::string& hdr) const noexcept
{
	const char *val{g_mime_object_get_header(self(), hdr.c_str())};
	if (!val)
		return Nothing;
	if (!g_utf8_validate(val, -1, {}))
		return utf8_clean(hdr);
	else
		return val;
}


Option<std::string>
MimeObject::object_to_string() const noexcept
{
	GMimeStream *stream{g_mime_stream_mem_new()};
	if (!stream) {
		g_warning("failed to create mem stream");
		return Nothing;
	}

	const auto written = g_mime_object_write_to_stream(self(), {}, stream);
	if (written < 0) {
		g_warning("failed to write object to stream");
		return Nothing;
	}

	std::string buffer;
	buffer.resize(written + 1);
	g_mime_stream_reset(stream);

	auto bytes{g_mime_stream_read(stream, buffer.data(), written)};
	g_object_unref(stream);
	if (bytes < 0)
		return Nothing;

	buffer.data()[written]='\0';
	buffer.resize(written);

	return buffer;
}


/*
 * MimeMessage
 */



static Result<MimeMessage>
make_from_stream(GMimeStream* &&stream/*consume*/)
{
	GMimeParser *parser{g_mime_parser_new_with_stream(stream)};
	g_object_unref(stream);
	if (!parser)
		return Err(Error::Code::Message, "cannot create mime parser");

	GMimeMessage *gmime_msg{g_mime_parser_construct_message(parser, NULL)};
	g_object_unref(parser);
	if (!gmime_msg)
		return Err(Error::Code::Message, "message seems invalid");

	auto mime_msg{MimeMessage{std::move(G_OBJECT(gmime_msg))}};
	g_object_unref(gmime_msg);

	return Ok(std::move(mime_msg));
}

Result<MimeMessage>
MimeMessage::make_from_file(const std::string& path)
{
	GError* err{};
	if (auto&& stream{g_mime_stream_file_open(path.c_str(), "r", &err)}; !stream)
		return Err(Error::Code::Message, &err,
			   "failed to open stream for %s", path.c_str());
	else
		return make_from_stream(std::move(stream));
}

Result<MimeMessage>
MimeMessage::make_from_string(const std::string& text)
{
	if (auto&& stream{g_mime_stream_mem_new_with_buffer(
				text.c_str(), text.length())}; !stream)
		return Err(Error::Code::Message,
			   "failed to open stream for string");
	else
		return make_from_stream(std::move(stream));
}

Option<int64_t>
MimeMessage::date() const noexcept
{
	GDateTime *dt{g_mime_message_get_date(self())};
	if (!dt)
		return Nothing;
	else
		return g_date_time_to_unix(dt);
}

Mu::Contacts
MimeMessage::addresses(AddressType atype) const noexcept
{
	auto addrs{g_mime_message_get_addresses(
			self(), static_cast<GMimeAddressType>(atype))};
	if (!addrs)
		return {};


	const auto msgtime{date().value_or(0)};
	const auto opt_field_id = std::invoke(
		[&]()->Option<Field::Id>{
			switch(atype) {
			case AddressType::To:
				return Field::Id::To;
			case AddressType::From:
				return Field::Id::From;
			case AddressType::Bcc:
				return Field::Id::Bcc;
			case AddressType::Cc:
				return Field::Id::Cc;
			default:
				return Nothing;
			}
		});

	Contacts contacts;
	auto lst_len{internet_address_list_length(addrs)};
	contacts.reserve(lst_len);
	for (auto i = 0; i != lst_len; ++i) {

		auto&& addr{internet_address_list_get_address(addrs, i)};
		const auto name{internet_address_get_name(addr)};

		if (G_UNLIKELY(!INTERNET_ADDRESS_IS_MAILBOX(addr)))
			continue;

		const auto email{internet_address_mailbox_get_addr (
				INTERNET_ADDRESS_MAILBOX(addr))};
		if (G_UNLIKELY(!email))
			continue;

		contacts.push_back(Contact{email, name ? name : "",
				opt_field_id, msgtime});
	}

	return contacts;
}



std::vector<std::string>
MimeMessage::references() const noexcept
{
	constexpr std::array<const char*, 2> ref_headers = {
		"References", "In-reply-to",
	};

	// is ref already in the list?
	auto is_dup = [](auto&& seq, const std::string& ref) {
		return seq_find_if(seq, [&](auto&& str) { return ref == str; })
			== seq.cend();
	};

	std::vector<std::string> refs;
	for (auto&& ref_header: ref_headers) {

		auto hdr{header(ref_header)};
		if (!hdr)
			continue;

		GMimeReferences *mime_refs{g_mime_references_parse({}, hdr->c_str())};
		refs.reserve(refs.size() + g_mime_references_length(mime_refs));

		for (auto i = 0; i != g_mime_references_length(mime_refs); ++i) {

			if (auto&& msgid{g_mime_references_get_message_id(mime_refs, i)}; !msgid)
				continue; // invalid
			else if (is_dup(refs, msgid))
				continue; // skip dups
			else
				refs.emplace_back(msgid);
		}
		g_mime_references_free(mime_refs);
	}

	return refs;
}


void
MimeMessage::for_each(const ForEachFunc& func) const noexcept
{
	struct CallbackData { const ForEachFunc& func; };
	CallbackData cbd{func};

	g_mime_message_foreach(
		self(),
		[] (GMimeObject *parent, GMimeObject *part, gpointer user_data) {
			auto cbd{reinterpret_cast<CallbackData*>(user_data)};
			cbd->func(MimeObject{parent}, MimeObject{part});
		}, &cbd);
}



/*
 * MimePart
 */
size_t
MimePart::size() const noexcept
{
	auto wrapper{g_mime_part_get_content(self())};
	if (!wrapper) {
		g_warning("failed to get content wrapper");
		return 0;
	}

	auto stream{g_mime_data_wrapper_get_stream(wrapper)};
	if (!stream) {
		g_warning("failed to get stream");
		return 0;
	}

	return static_cast<size_t>(g_mime_stream_length(stream));
}

Option<std::string>
MimePart::to_string() const noexcept
{
	GMimeDataWrapper *wrapper{g_mime_part_get_content(self())};
	if (!wrapper) { /* this happens with invalid mails */
		g_debug("failed to create data wrapper");
		return Nothing;
	}

	GMimeStream *stream{g_mime_stream_mem_new()};
	if (!stream) {
		g_warning("failed to create mem stream");
		return Nothing;
	}


	ssize_t buflen{g_mime_data_wrapper_write_to_stream(wrapper, stream)};
	if (buflen <= 0) { /* empty buffer, not an error */
		g_object_unref(stream);
		return Nothing;
	}

	std::string buffer;
	buffer.resize(buflen + 1);
	g_mime_stream_reset(stream);

	auto bytes{g_mime_stream_read(stream, buffer.data(), buflen)};
	g_object_unref(stream);
	if (bytes < 0)
		return Nothing;

	buffer.data()[bytes]='\0';
	buffer.resize(buflen);

	return buffer;
}


Result<size_t>
MimePart::to_file(const std::string& path, bool overwrite) const noexcept
{
	GMimeDataWrapper *wrapper{g_mime_part_get_content(self())};
	if (!wrapper)  /* this happens with invalid mails */
		return Err(Error::Code::File, "failed to create data wrapper");


	GError *err{};
	GMimeStream *stream{g_mime_stream_fs_open(
			path.c_str(),
			O_WRONLY | O_CREAT | O_TRUNC |(overwrite ? 0 : O_EXCL),
			S_IRUSR|S_IWUSR,
			&err)};
	if (!stream)
		return Err(Error::Code::File, &err,
			   "failed to open '%s'", path.c_str());

	ssize_t written{g_mime_data_wrapper_write_to_stream(wrapper, stream)};
	g_object_unref(stream);
	if (written < 0) {
		return Err(Error::Code::File, &err,
			   "failed to write to '%s'", path.c_str());
	}

	return Ok(static_cast<size_t>(written));
}
