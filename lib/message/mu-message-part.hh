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


#ifndef MU_MESSAGE_PART_HH__
#define MU_MESSAGE_PART_HH__

#include <string>
#include <memory>
#include <utils/mu-option.hh>
#include <utils/mu-result.hh>

namespace Mu {

class MimeObject; // forward declaration; don't want to include for build-time
		  // reasons.

class MessagePart {
public:
	/**
	 * Construct MessagePart from a MimeObject
	 *
	 * @param obj
	 */
	MessagePart(const MimeObject& obj);

	/**
	 * Copy CTOR
	 *
	 * @param other
	 */
	MessagePart(const MessagePart& other);

	/**
	 * DTOR
	 *
	 */
	~MessagePart();

	/**
	 * Get the underlying MimeObject; you need to include mu-mime-object.hh
	 * to do anything useful with it.
	 *
	 * @return reference to the mime-object
	 */
	const MimeObject& mime_object() const noexcept;

	/**
	 * Filename for the mime-part file. This is a "cooked" filename with
	 * unallowed characters removed. If there's no filename specified,
	 * construct one (such as in the case of a MimeMessagePart).
	 *
	 * @param minimal if true, only perform *minimal* cookiing, where we
	 * only remove forward-slashes.
	 *
	 * @see raw_filename()
	 *
	 * @return the name
	 */
	Option<std::string> cooked_filename(bool minimal=false) const noexcept;

	/**
	 * Name for the mime-part file, i.e., MimePart::filename
	 *
	 * @return the filename or Nothing if there is none
	 */
	Option<std::string> raw_filename() const noexcept;

	/**
	 * Mime-type for the mime-part (e.g. "text/plain")
	 *
	 * @return the mime-part or Nothing if there is none
	 */
	Option<std::string> mime_type() const noexcept;


	/**
	 * Get the content description for this part, or Nothing
	 *
	 * @return the content description
	 */
	Option<std::string> content_description() const noexcept;

	/**
	 * Get the length of the (unencoded) MIME-part.
	 *
	 * @return the size
	 */
	size_t size() const noexcept;

	/**
	 * Does this part have an "attachment" disposition? Otherwise it is
	 * "inline". Note that does *not* map 1:1 to a message's HasAttachment
	 * flag (which uses looks_like_attachment())
	 *
	 * @return true or false.
	 */
	bool is_attachment() const noexcept;


	/**
	 * Does this part appear to be an attachment from an end-users point of
	 * view? This uses some heuristics to guess. Some parts for which
	 * is_attachment() is true may not "really" be attachments, and
	 * vice-versa
	 *
	 * @return true or false.
	 */
	bool looks_like_attachment() const noexcept;

	/**
	 * Is this part signed?
	 *
	 * @return true or false
	 */
	bool is_signed() const noexcept;


	/**
	 * Is this part encrypted?
	 *
	 * @return true or false
	 */
	bool is_encrypted() const noexcept;


	/**
	 * Write (decoded) mime-part contents to string
	 *
	 * @return a string or nothing if there is no contemt
	 */
	Option<std::string> to_string() const noexcept;

	/**
	 * Write (decoded) mime part to a file
	 *
	 * @param path path to file
	 * @param overwrite whether to possibly overwrite
	 *
	 * @return size of file or or an error.
	 */
	Result<size_t> to_file(const std::string& path, bool overwrite) const noexcept;

	struct Private;
private:
	const std::unique_ptr<MimeObject> mime_obj;
};

} // namespace Mu

#endif /* MU_MESSAGE_PART_HH__ */
