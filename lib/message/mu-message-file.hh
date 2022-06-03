/*
** Copyright (C) 2022 Dirk-Jan C. Binnema <djcb.bulk@gmail.com>
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

#ifndef MU_MESSAGE_FILE_HH__
#define MU_MESSAGE_FILE_HH__

#include "mu-flags.hh"
#include <utils/mu-result.hh>

namespace Mu {

/*
 * The file-components, ie.
 *     1631819685.fb7b279bbb0a7b66.evergrey:2,RS
 *     => {
 *       "1631819685.fb7b279bbb0a7b66.evergrey",
 *       ':',
 *       "2,",
 *       "RS"
 *     }
 */
struct FileParts {
	std::string	base;	/**< basename */
	char		separator; /**< separator */
	std::string	flags_suffix; /**< suffix (with flags) */
};

/**
 * Get the file-parts for some message-file
 *
 * @param file path to some message file (does not have to exist)
 *
 * @return FileParts for the message file
 */
FileParts message_file_parts(const std::string& file);


struct DirFile {
	std::string	dir;
	std::string	file;
	bool		is_new;
};

/**
 * Get information about the message file componemts
 *
 * @param path message path
 *
 * @return the components for the message file or an error.
 */
Result<DirFile> base_message_dir_file(const std::string& path);



/**
 * Get the Maildir flags from the full path of a mailfile. The flags are as
 * specified in http://cr.yp.to/proto/maildir.html, plus Flag::New for new
 * messages, ie the ones that live in new/. The flags are logically OR'ed. Note
 * that the file does not have to exist; the flags are based on the path only.
 *
 * @param pathname of a mailfile; it does not have to refer to an
 * actual message
 *
 * @return the message flags or an error
 */
Result<Flags> flags_from_path(const std::string& pathname);

/**
 * get the maildir for a certain message path, ie, the path *before*
 * cur/ or new/ and *after* the root.
 *
 * @param path path for some message
 * @param root filesystem root for the maildir
 *
 * @return the maildir or an Error
 */
Result<std::string> maildir_from_path(const std::string& path,
				      const std::string& root);
} // Mu


#endif /* MU_MESSAGE_FILE_HH__ */
