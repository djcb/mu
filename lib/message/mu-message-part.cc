/*
** Copyright (C) 2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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


#include "mu-message-part.hh"
#include "mu-mime-object.hh"
#include "utils/mu-utils.hh"
#include "utils/mu-utils-file.hh"
#include <string>

using namespace Mu;

MessagePart::MessagePart(const Mu::MimeObject& obj):
	mime_obj{std::make_unique<Mu::MimeObject>(obj)}
{}

MessagePart::MessagePart(const MessagePart& other):
	MessagePart(*other.mime_obj)
{}

MessagePart::~MessagePart() = default;

const MimeObject&
MessagePart::mime_object() const noexcept
{
	return *mime_obj;
}

static std::string
cook(const std::string& fname, const std::vector<char>& forbidden)
{
	std::string clean;
	clean.reserve(fname.length());

	for (auto& c: basename(fname))
		if (seq_some(forbidden,[&](char fc){return ::iscntrl(c) || c == fc;}))
			clean += '-';
		else
			clean += c;

	if (clean[0] == '.' && (clean == "." || clean == ".."))
		return "-";
	else
		return clean;
}

static std::string
cook_minimal(const std::string& fname)
{
	return cook(fname, { '/' });
}

static std::string
cook_full(const std::string& fname)
{
	auto cooked = cook(fname, { '/', ' ', '\\', ':' });
	if (cooked.size() > 1 && cooked[0] == '-')
		cooked.erase(0, 1);

	return cooked;
}

Option<std::string>
MessagePart::cooked_filename(bool minimal) const noexcept
{
	auto&& cooker{minimal ? cook_minimal : cook_full};

	// a MimePart... use the name if there is one.
	if (mime_object().is_part())
		return MimePart{mime_object()}.filename().map(cooker);

	// MimeMessagepart. Construct a name based on subject.
	if (mime_object().is_message_part()) {
		auto msg{MimeMessagePart{mime_object()}.get_message()};
		if (!msg)
			return Nothing;
		else
			return msg->subject()
				.map(cooker)
				.value_or("no-subject") + ".eml";
	}

	return Nothing;
}

Option<std::string>
MessagePart::raw_filename() const noexcept
{
	if (!mime_object().is_part())
		return Nothing;
	else
		return MimePart{mime_object()}.filename();
}



Option<std::string>
MessagePart::mime_type() const noexcept
{
	if (const auto ctype{mime_object().content_type()}; ctype)
		return ctype->media_type() + "/" + ctype->media_subtype();
	else
		return Nothing;
}

Option<std::string>
MessagePart::content_description() const noexcept
{
	if (!mime_object().is_part())
		return Nothing;
	else
		return MimePart{mime_object()}.content_description();
}

size_t
MessagePart::size() const noexcept
{
	if (!mime_object().is_part())
		return 0;
	else
		return MimePart{mime_object()}.size();
}

bool
MessagePart::is_attachment() const noexcept
{
	if (!mime_object().is_part())
		return false;
	else
		return MimePart{mime_object()}.is_attachment();
}


Option<std::string>
MessagePart::to_string() const noexcept
{
	if (mime_object().is_part())
		return MimePart{mime_object()}.to_string();
	else
		return mime_object().to_string_opt();
}

Result<size_t>
MessagePart::to_file(const std::string& path, bool overwrite) const noexcept
{
	if (mime_object().is_part())
		return MimePart{mime_object()}.to_file(path, overwrite);
	else if (mime_object().is_message_part()) {
		if (auto&& msg{MimeMessagePart{mime_object()}.get_message()}; !msg)
			return Err(Error::Code::Message, "failed to get message-part");
		else
			return msg->to_file(path, overwrite);
	} else
		return mime_object().to_file(path, overwrite);
}

bool
MessagePart::is_signed() const noexcept
{
	return mime_object().is_multipart_signed();
}

bool
MessagePart::is_encrypted() const noexcept
{
	return mime_object().is_multipart_encrypted();
}

bool /* heuristic */
MessagePart::looks_like_attachment() const noexcept
{
	auto matches=[](const MimeContentType& ctype,
			const std::initializer_list<std::pair<const char*, const char*>>& ctypes) {
		return std::find_if(ctypes.begin(), ctypes.end(), [&](auto&& item){
			return ctype.is_type(item.first, item.second); }) != ctypes.end();
	};

	const auto ctype{mime_object().content_type()};
	if (!ctype)
		return false; // no content-type: not an attachment.

	// we consider some parts _not_ to be attachments regardless of disposition
	if  (matches(*ctype,{{"application", "pgp-keys"}}))
		return false;

	// we consider some parts to be attachments regardless of disposition
	if  (matches(*ctype,{{"image", "*"},
			     {"audio", "*"},
			     {"application", "*"},
			     {"application", "x-patch"}}))
		return true;

	// otherwise, rely on the disposition
	return is_attachment();
}



#ifdef BUILD_TESTS
#include "utils/mu-test-utils.hh"

static void
test_cooked_full()
{
	std::array<std::pair<std::string, std::string>, 4> cases = {{
		{ "/hello/world/foo", "foo" },
		{ "foo:/\n/bar", "bar"},
		{ "Aap Noot Mies", "Aap-Noot-Mies"},
		{ "..", "-"}
	}};

	for (auto&& test: cases)
		assert_equal(cook_full(test.first), test.second);
}

static void
test_cooked_minimal()
{
	std::array<std::pair<std::string, std::string>, 4> cases = {{
		{ "/hello/world/foo", "foo" },
		{ "foo:/\n/bar", "bar"},
		{ "Aap Noot Mies.doc", "Aap Noot Mies.doc"},
		{ "..", "-"}
	}};

	for (auto&& test: cases)
		assert_equal(cook_minimal(test.first), test.second);
}

int
main(int argc, char* argv[])
{
	mu_test_init(&argc, &argv);

	g_test_add_func("/message/message-part/cooked-full", test_cooked_full);
	g_test_add_func("/message/message-part/cooked-minimal", test_cooked_minimal);

	return g_test_run();
}
#endif /*BUILD_TESTS*/
