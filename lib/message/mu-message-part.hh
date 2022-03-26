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


#ifndef MU_MESSAGE_PART_HH__
#define MU_MESSAGE_PART_HH__

#include <string>
#include <memory>
#include <utils/mu-option.hh>
#include <utils/mu-result.hh>

namespace Mu {

struct MimeObject; // forward declaration

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
	 * Filename for the mime-part
	 *
	 * @return the filename or Nothing if there is none
	 */
	Option<std::string> filename() const noexcept;

	/**
	 * Mime-type for the mime-part (e.g. "text/plain")
	 *
	 * @return the mime-part or Nothing if there is none
	 */
	Option<std::string> mime_type() const noexcept;

	/**
	 * Get the length of the (unencoded) MIME-part.
	 *
	 * @return the size
	 */
	size_t size() const noexcept;

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
	std::unique_ptr<MimeObject> mime_obj;
};

} // namespace Mu

#endif /* MU_MESSAGE_PART_HH__ */
