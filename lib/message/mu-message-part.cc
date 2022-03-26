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


#include "mu-message-part.hh"
#include "mu-mime-object.hh"
#include "utils/mu-utils.hh"

using namespace Mu;

MessagePart::MessagePart(const Mu::MimeObject& obj):
	mime_obj{std::make_unique<Mu::MimeObject>(obj)}
{}

MessagePart::MessagePart(const MessagePart& other):
	MessagePart(*other.mime_obj)
{}

MessagePart::~MessagePart() = default;


Option<std::string>
MessagePart::filename() const noexcept
{
	if (!mime_obj->is_part())
		return Nothing;
	else
		return MimePart(*mime_obj).filename();
}

Option<std::string>
MessagePart::mime_type() const noexcept
{
	if (const auto ctype{mime_obj->content_type()}; ctype)
		return ctype->media_type() + "/" + ctype->media_subtype();
	else
		return Nothing;
}

size_t
MessagePart::size() const noexcept
{
	if (!mime_obj->is_part())
		return 0;
	else
		return MimePart(*mime_obj).size();
}


Option<std::string>
MessagePart::to_string() const noexcept
{
	if (mime_obj->is_part())
		return MimePart(*mime_obj).to_string();
	else
		return mime_obj->object_to_string();
}



Result<size_t>
MessagePart::to_file(const std::string& path, bool overwrite) const noexcept
{
	if (!mime_obj->is_part())
		return Err(Error::Code::InvalidArgument,
			   "not a part");
	else
		return MimePart(*mime_obj).to_file(path, overwrite);
}
