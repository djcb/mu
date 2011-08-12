/*
** Copyright (C) 2011  Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
	
	MU_FLAG_DRAFT		= 1 << 0,
	MU_FLAG_FLAGGED		= 1 << 1,   
	MU_FLAG_PASSED		= 1 << 2,
	MU_FLAG_REPLIED		= 1 << 3,
	MU_FLAG_SEEN		= 1 << 4,
	MU_FLAG_TRASHED		= 1 << 5,
	
	MU_FLAG_NEW		= 1 << 6,
	MU_FLAG_SIGNED		= 1 << 7,
	MU_FLAG_ENCRYPTED	= 1 << 8,
	MU_FLAG_HAS_ATTACH	= 1 << 9
};
typedef enum _MuFlags MuFlags;

#define MU_FLAG_INVALID ((unsigned)-1)

enum _MuFlagType {
	MU_FLAG_TYPE_MAILFILE    = 1 << 0,
	MU_FLAG_TYPE_MAILDIR     = 1 << 1,
	MU_FLAG_TYPE_CONTENT     = 1 << 2
};
typedef enum _MuFlagType MuFlagType;

#define MU_FLAG_TYPE_ANY ((MuFlags)-1)
#define MU_FLAG_TYPE_INVALID ((MuFlagType)-1)


/**
 * Get the type of flag (mailfile, maildir, pseudo or content)
 * 
 * @param flag a MuFlag
 * 
 * @return the flag type or MU_FLAG_TYPE_INVALID in case of error
 */
MuFlagType mu_flag_type (MuFlags flag);


/**
 * Get the flag character
 * 
 * @param flag a MuFlag (single)
 * 
 * @return the character, or 0 in case of error
 */
char mu_flag_char (MuFlags flag);


/**
 * Get the flag name 
 * 
 * @param flag a single MuFlag
 * 
 * @return the name (don't free) as string or NULL in case of error
 */
const char* mu_flag_name (MuFlags flag);


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
 * @param str the string representation
 * @param types the flag types to acceps (other will be ignored)
 * 
 * @return the (OR'ed) flags
 */
MuFlags mu_flags_from_str (const char *str, MuFlagType types);


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
