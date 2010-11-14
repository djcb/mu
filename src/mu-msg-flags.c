/* 
** Copyright (C) 2008-2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <glib.h>
#include <string.h>

#include "mu-util.h"
#include "mu-msg-flags.h"


static const MuMsgFlags ALL_FLAGS[] = {
	MU_MSG_FLAG_NEW,
	MU_MSG_FLAG_PASSED,
	MU_MSG_FLAG_REPLIED,
	MU_MSG_FLAG_SEEN,
	MU_MSG_FLAG_TRASHED,
	MU_MSG_FLAG_DRAFT,
	MU_MSG_FLAG_FLAGGED,
	MU_MSG_FLAG_SIGNED,
	MU_MSG_FLAG_ENCRYPTED,
	MU_MSG_FLAG_HAS_ATTACH	
};


MuMsgFlags
mu_msg_flag_from_char (char k)
{
	switch (k) {
	case 'N': return MU_MSG_FLAG_NEW;
		
	case 'P': return MU_MSG_FLAG_PASSED;
	case 'R': return MU_MSG_FLAG_REPLIED;
	case 'S': return MU_MSG_FLAG_SEEN;
	case 'T': return MU_MSG_FLAG_TRASHED;
	case 'D': return MU_MSG_FLAG_DRAFT;
	case 'F': return MU_MSG_FLAG_FLAGGED;

	case 'Z': return MU_MSG_FLAG_SIGNED;
	case 'X': return MU_MSG_FLAG_ENCRYPTED;
	case 'A': return MU_MSG_FLAG_HAS_ATTACH;
		
	default:
		g_warning ("%s: unknown flag %c", __FUNCTION__, k);
		return MU_MSG_FLAG_NONE;
	}
}

char
mu_msg_flag_to_char (MuMsgFlags flag)
{
	switch (flag) {
	case MU_MSG_FLAG_NEW: return 'N';
	case MU_MSG_FLAG_PASSED: return 'P';
	case MU_MSG_FLAG_REPLIED: return 'R';
	case MU_MSG_FLAG_SEEN: return 'S';
	case MU_MSG_FLAG_TRASHED: return 'T';
	case MU_MSG_FLAG_DRAFT: return 'D';
	case MU_MSG_FLAG_FLAGGED: return 'F';
		
	case MU_MSG_FLAG_SIGNED: return 'Z';
	case MU_MSG_FLAG_ENCRYPTED: return 'X';
	case MU_MSG_FLAG_HAS_ATTACH: return 'A';
		
	default:
		g_warning ("%s: unknown flag 0x%x", __FUNCTION__, flag);
		return 0;
	}
}


const char*
mu_msg_flags_to_str_s (MuMsgFlags flags)
{
	int i, j;
	static char buf[16]; /* more than enough */
	
	for (i = j = 0; i != G_N_ELEMENTS(ALL_FLAGS); ++i) {
		if (flags & ALL_FLAGS[i]) {
			char k;
			if ((k = mu_msg_flag_to_char (ALL_FLAGS[i])) == 0)
				return NULL;
			buf[j++] = k;
		}
	}
	buf[j] = '\0';
	
	return buf;
}

MuMsgFlags
mu_msg_flags_from_str (const char* str)
{
	MuMsgFlags flags;
	
	for (flags = MU_MSG_FLAG_NONE; str && *str; ++str) {
		MuMsgFlags flag;
		if ((flag = mu_msg_flag_to_char (*str)) == 0) {
			flags = 0;
			break;
		}
		flags |= flag;
	}
	
	return flags;
}


/*
 * is this a 'new' msg or a 'cur' msg?; if new, we return
 * (in info) a ptr to the info part 
 */
enum _MsgType {
	MSG_TYPE_CUR,
	MSG_TYPE_NEW,
	MSG_TYPE_OTHER
};
typedef enum _MsgType MsgType;


static MsgType
check_msg_type (const char* path, char **info)
{
	char *dir, *file; 
	MsgType mtype;

	/* try to find the info part */
	/* note that we can use either the ':' or '!' as separator;
	 * the former is the official, but as it does not work on e.g. VFAT
	 * file systems, some Maildir implementations use the latter instead
	 * (or both). For example, Tinymail/modest does this. The python
	 * documentation at http://docs.python.org/lib/mailbox-maildir.html
	 * mentions the '!' as well as a 'popular choice'
	 */
	dir =  g_path_get_dirname (path);
	file = g_path_get_basename (path);
	
	if (!(*info = strrchr(file, ':')))
		*info = strrchr (file, '!'); /* Tinymail */
	if (*info)
		++(*info); /* skip the ':' or '!' */
	
	if (g_str_has_suffix (dir, G_DIR_SEPARATOR_S "cur")) {
		if (!*info)
			g_debug ("'cur' file, but no info part: %s", path);
		mtype = MSG_TYPE_CUR;
	} else if (g_str_has_suffix (dir, G_DIR_SEPARATOR_S "new")) {
		if (*info)
			g_debug ("'new' file, ignoring info part: %s", path);
		mtype = MSG_TYPE_NEW;
	} else 
		mtype = MSG_TYPE_OTHER; /* file has been added explicitly as
					   a single message */
	if (*info)
		*info = g_strdup (*info);

	g_free (dir);
	g_free (file);
	
	return mtype;
}

MuMsgFlags
mu_msg_flags_from_file (const char* path)
{
	MuMsgFlags flags;
	MsgType mtype;
	char *info = NULL;

	g_return_val_if_fail (path, MU_MSG_FLAG_NONE);
	g_return_val_if_fail (!g_str_has_suffix(path,G_DIR_SEPARATOR_S),
			      MU_MSG_FLAG_NONE);
	
	mtype = check_msg_type (path, &info);
	
	/* we ignore any flags for a new message */
	if (mtype == MSG_TYPE_NEW) {
		g_free (info);
		return MU_MSG_FLAG_NEW;
	}
	
	flags = 0;
	if (mtype == MSG_TYPE_CUR || mtype == MSG_TYPE_OTHER) {
		char *cursor = info;
		/* only support the "2," format */
		if (cursor && cursor[0]=='2' && cursor[1]==',') {
			cursor += 2; /* jump past 2, */
			while (*cursor) {
				switch (*cursor) {
				case 'P': flags |= MU_MSG_FLAG_PASSED; break;
				case 'T': flags |= MU_MSG_FLAG_TRASHED; break;
				case 'R': flags |= MU_MSG_FLAG_REPLIED; break;
				case 'S': flags |= MU_MSG_FLAG_SEEN; break;
				case 'D': flags |= MU_MSG_FLAG_DRAFT; break;
				case 'F': flags |= MU_MSG_FLAG_FLAGGED; break;
				}
				++cursor;
			}
		}
	} 
	g_free (info);
	
	return flags;
}
