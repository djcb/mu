/*
** Copyright (C) 2008-2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <glib.h>
#include <time.h>
#include <sys/types.h>          /* for mode_t */
#include <utils/mu-util.h>
#include <mu-flags.hh>

namespace Mu {
/**
 * create a new maildir. if parts of the maildir already exists, those
 * will simply be ignored. IOW, if you try to create the same maildir
 * twice, the second will simply be a no-op (without any errors).
 * Note, if the function fails 'halfway', it will *not* try to remove
 * the parts the were created. it *will* create any parent dirs that
 * are not yet existent.
 *
 *
 * @param path the path (missing components will be created, as in 'mkdir -p')
 * @param mode the file mode (e.g., 0755)
 * @param noindex add a .noindex file to the maildir, so it will be excluded
 * from indexing by 'mu index'
 * @param err if function returns FALSE, receives error
 * information. err may be NULL.
 *
 * @return TRUE if creation succeeded (or already existed), FALSE otherwise
 */
gboolean mu_maildir_mkdir (const char* path, mode_t mode, gboolean noindex,
			   GError **err);


/**
 * create a symbolic link to a mail message
 *
 * @param src the full path to the source message
 * @param targetpath the path to the target maildir; ie., *not*
 * MyMaildir/cur, but just MyMaildir/. The function will figure out
 * the correct subdir then.
 * @param err if function returns FALSE, err may contain extra
 * information. if err is NULL, does nothing
 *
 * @return
 */
gboolean mu_maildir_link   (const char* src, const char *targetpath,
			    GError **err);

/**
 * recursively delete all the symbolic links in a directory tree
 *
 * @param dir top dir
 * @param err if function returns FALSE, err may contain extra
 * information. if err is NULL, does nothing
 *
 * @return TRUE if it worked, FALSE in case of error
 */
gboolean mu_maildir_clear_links (const char* dir, GError **err);

/**
 * whether the directory path ends in '/cur/' or '/new/'
 *
 * @param path some path
 */
gboolean mu_maildir_is_leaf_dir (const char *path);


/**
 * get the Maildir flags from the full path of a mailfile. The flags
 * are as specified in http://cr.yp.to/proto/maildir.html, plus
 * MU_MSG_FLAG_NEW for new messages, ie the ones that live in
 * new/. The flags are logically OR'ed. Note that the file does not
 * have to exist; the flags are based on the path only.
 *
 * @param pathname of a mailfile; it does not have to refer to an
 * actual message
 *
 * @return the flags, or MU_MSG_FILE_FLAG_UNKNOWN in case of error
 */
MuFlags mu_maildir_get_flags_from_path (const char* pathname);

/**
 * get the new pathname for a message, based on the old path and the
 * new flags and (optionally) a new maildir. Note that
 * setting/removing the MU_FLAG_NEW will change the directory in which
 * a message lives. The flags are as specified in
 * http://cr.yp.to/proto/maildir.html, plus MU_FLAG_NEW for new
 * messages, ie the ones that live in new/. The flags are logically
 * OR'ed. Note that the file does not have to exist; the flags are
 * based on the path only.
 *
 *
 * @param oldpath the old (current) full path to the message
 * (including the filename)
 * @param new_mdir the new maildir for this message, or NULL to keep
 * it in the current one. The maildir is the absolute file system
 * path, without the 'cur' or 'new'
 * @param new_flags the new flags for this message
 * @param new_name whether to create a new unique name, or keep the
 * old one
 *
 * @return a new path name; use g_free when done with. NULL in case of
 * error.
 */
char* mu_maildir_get_new_path (const char *oldpath, const char *new_mdir,
			       MuFlags new_flags, gboolean new_name)
	G_GNUC_WARN_UNUSED_RESULT;

/**
 * get the maildir for a certain message path, ie, the path *before*
 * cur/ or new/
 *
 * @param path path for some message
 *
 * @return the maildir (free with g_free), or NULL in case of error
 */
char* mu_maildir_get_maildir_from_path (const char* path)
	G_GNUC_WARN_UNUSED_RESULT;


/**
 * move a message file to another maildir; the function returns the full path to
 * the new message. if the target file already exists, it is overwritten.
 *
 * @param msgpath an absolute file system path to an existing message in an
 * actual maildir
 * @param targetmdir the target maildir; note that this the base-level
 * Maildir, ie. /home/user/Maildir/archive, and must _not_ include the
 * 'cur' or 'new' part. Note that the target maildir must be on the
 * same filesystem. If you specify NULL for targetmdir, only the flags
 * of the message are affected; note that this may still involve a
 * moved to another directory (say, from new/ to cur/)
 * @param flags to set for the target (influences the filename, path)
 * @param ignore_dups whether to silently ignore the src=target case
 * (and return TRUE)
 * @param new_name whether to create a new unique name, or keep the
 * old one
 * @param err receives error information
 *
 * @return return the full path name of the target file (g_free) if
 * the move succeeded, NULL otherwise
 */
char* mu_maildir_move_message (const char* oldpath, const char* targetmdir,
                               MuFlags newflags, gboolean ignore_dups,
                               gboolean new_name, GError **err)
	G_GNUC_WARN_UNUSED_RESULT;

} // namespace Mu

#endif /*MU_MAILDIR_HH__*/
