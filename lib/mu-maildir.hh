/*
** Copyright (C) 2008-2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef MU_MAILDIR_HH__
#define MU_MAILDIR_HH__

#include <string>
#include <utils/mu-result.hh>

#include <glib.h>
#include <time.h>
#include <sys/types.h> /* for mode_t */
#include <message/mu-message.hh>

namespace Mu {

/**
 * Create a new maildir. if parts of the maildir already exists, those will
 * simply be ignored.
 *
 * IOW, if you try to create the same maildir twice, the second will simply be a
 * no-op (without any errors). Note, if the function fails 'halfway', it will
 * *not* try to remove the parts the were created. it *will* create any parent
 * dirs that are not yet existent.
 *
 * @param path the path (missing components will be created, as in 'mkdir -p').
 * must be non-empty
 * @param mode the file mode (e.g., 0755)
 * @param noindex add a .noindex file to the maildir, so it will be excluded
 * from indexing by 'mu index'
 *
 * @return a valid result (!!result) or an Error
 */
Result<void> maildir_mkdir(const std::string& path, mode_t mode=0700,
			   bool noindex=false);

/**
 * Create a symbolic link to a mail message
 *
 * @param src the full path to the source message
 * @param targetpath the path to the target maildir; ie., *not*
 * MyMaildir/cur, but just MyMaildir/. The function will figure out
 * the correct subdir then.
 * @param unique_names whether to create unique names; should be true unless
 * for tests.
 *
 * @return a valid result (!!result) or an Error
 */
Result<void> maildir_link(const std::string& src, const std::string& targetpath,
			  bool unique_names=true);
/**
 * Recursively delete all the symbolic links in a directory tree
 *
 * @param dir top dir
 *
 * @return a valid result (!!result) or an Error
 */
Result<void> maildir_clear_links(const std::string& dir);

/**
 * Move a message file to another maildir. If the target exists, it is overwritten.
 *
 * @param oldpath an absolute file system path to an existing message in an
 * actual maildir
 * @param newpath the absolute full path to the target file
 * @param assume_remote assume the target is on a different file-system,
 * and hence rename() won't work and we need another method
 *
 * @return a valid result or an Error
 */
Result<void> maildir_move_message(const std::string& oldpath,
				  const std::string& newpath,
				  bool               assume_remote = false);

/**
 * Determine the target path for a to-be-moved message; i.e. this does not
 * actually move the message, only calculate the path.
 *
 * @param old_path an absolute file system path to an existing message in an
 * actual maildir
 * @param root_maildir_path the absolute file system path under which
 * all maildirs live.
 * @param target_maildir the target maildir; note that this the base-level
 * Maildir, ie. /home/user/Maildir/archive, and must _not_ include the
 * 'cur' or 'new' part. Note that the target maildir must be on the
 * same filesystem. Can be empty if the message should not be moved to
 * a different maildir; note that this may still involve a
 * move to another directory (say, from new/ to cur/)
 * @param flags to set for the target (influences the filename, path).
 * Any non-Maildir/File flags are ignored.
 * @param new_name whether to change the basename of the file
 *
 * @return Full path name of the target file or an Error
 */
Result<std::string>
maildir_determine_target(const std::string&	old_path,
			 const std::string&	root_maildir_path,
			 const std::string&	target_maildir,
			 Flags			newflags,
			 bool			new_name);

} // namespace Mu

#endif /*MU_MAILDIR_HH__*/
