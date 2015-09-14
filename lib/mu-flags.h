/*
** Copyright (C) 2011-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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


#ifndef __MU_FLAGS_H__
#define __MU_FLAGS_H__

#include <glib.h>

G_BEGIN_DECLS

enum _MuFlags {
	MU_FLAG_NONE            = 0,

	/* next 6 are seen in the file-info part of maildir message
	 * file names, ie., in a name like "1234345346:2,<fileinfo>",
	 * <fileinfo> consists of zero or more of the following
	 * characters (in ascii order) */
	MU_FLAG_DRAFT		= 1 << 0,
	MU_FLAG_FLAGGED		= 1 << 1,
	MU_FLAG_PASSED		= 1 << 2,
	MU_FLAG_REPLIED		= 1 << 3,
	MU_FLAG_SEEN		= 1 << 4,
	MU_FLAG_TRASHED		= 1 << 5,

	/* decides on cur/ or new/ in the maildir */
	MU_FLAG_NEW		= 1 << 6,

	/* content flags -- not visible in the filename, but used for
	 * searching */
	MU_FLAG_SIGNED		= 1 << 7,
	MU_FLAG_ENCRYPTED	= 1 << 8,
	MU_FLAG_HAS_ATTACH	= 1 << 9,

	/* pseudo-flag, only for queries, so we can search for
	 * flag:unread, which is equivalent to 'flag:new OR NOT
	 * flag:seen' */
	MU_FLAG_UNREAD          = 1 << 10,

	/* other content flags */
	MU_FLAG_LIST            = 1 << 11
};
typedef enum _MuFlags MuFlags;

#define MU_FLAG_INVALID ((MuFlags)-1)

enum _MuFlagType {
	MU_FLAG_TYPE_MAILFILE    = 1 << 0,
	MU_FLAG_TYPE_MAILDIR     = 1 << 1,
	MU_FLAG_TYPE_CONTENT     = 1 << 2,
	MU_FLAG_TYPE_PSEUDO      = 1 << 3
};
typedef enum _MuFlagType MuFlagType;

#define MU_FLAG_TYPE_ANY ((MuFlagType)-1)
#define MU_FLAG_TYPE_INVALID ((MuFlagType)-1)


/**
 * Get the type of flag (mailfile, maildir, pseudo or content)
 *
 * @param flag a MuFlag
 *
 * @return the flag type or MU_FLAG_TYPE_INVALID in case of error
 */
MuFlagType mu_flag_type (MuFlags flag) G_GNUC_CONST;


/**
 * Get the flag character
 *
 * @param flag a MuFlag (single)
 *
 * @return the character, or 0 if it's not a valid flag
 */
char mu_flag_char (MuFlags flag) G_GNUC_CONST;


/**
 * Get the flag name
 *
 * @param flag a single MuFlag
 *
 * @return the name (don't free) as string or NULL in case of error
 */
const char* mu_flag_name (MuFlags flag) G_GNUC_CONST;


/**
 * Get the string representation of an OR'ed set of flags
 *
 * @param flags MuFlag (OR'ed)
 * @param types allowable types (OR'ed) for the result; the rest is ignored
 *
 * @return The string representation (static, don't free), or NULL in
 * case of error
 */
const char* mu_flags_to_str_s (MuFlags flags, MuFlagType types);


/**
 * Get the (OR'ed) flags corresponding to a string representation
 *
 * @param str the file info string
 * @param types the flag types to accept (other will be ignored)
 * @param ignore invalid if TRUE, ignore invalid flags, otherwise return
 * MU_FLAG_INVALID if an invalid flag is encountered
 *
 * @return the (OR'ed) flags
 */
MuFlags mu_flags_from_str (const char *str, MuFlagType types,
			   gboolean ignore_invalid);


/**
 * return the concatenation of all non-standard file flags in str
 * (ie., characters other than DFPRST) as a newly allocated string.
 *
 * @param str the file info string
 *
 * @return concatenation of all non-standard flags, as a string; free
 * with g_free when done. If there are no such flags, return NULL.
 */
char* mu_flags_custom_from_str (const char *str) G_GNUC_WARN_UNUSED_RESULT;


/**
 * Update #oldflags with the flags in #str, where #str consists of the
 * the normal flag characters, but prefixed with either '+' or '-',
 * which means resp. "add this flag" or "remove this flag" from
 * oldflags.  So, e.g. "-N+S" would unset the NEW flag and set the
 * SEEN flag, without affecting other flags.
 *
 * @param str the string representation
 * @param old flags to update
 * @param types the flag types to accept (other will be ignored)
 *
 * @return
 */
MuFlags mu_flags_from_str_delta (const char *str, MuFlags oldflags,
				 MuFlagType types);


typedef void (*MuFlagsForeachFunc) (MuFlags flag, gpointer user_data);

/**
 * call a function for each available flag
 *
 * @param func a function to call
 * @param user_data a user pointer to pass to the function
 */
void mu_flags_foreach (MuFlagsForeachFunc func, gpointer user_data);

G_END_DECLS

#endif /*__MU_FLAGS_H__*/
