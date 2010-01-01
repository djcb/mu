/* 
** Copyright (C) 2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-msg-flags.h"


static struct {
	char       kar;
	MuMsgFlags flag;
	gboolean   file_flag;
} FLAG_CHARS[] = {
	{'D', MU_MSG_FLAG_DRAFT, TRUE},
	{'F', MU_MSG_FLAG_FLAGGED, TRUE},
	{'N', MU_MSG_FLAG_NEW, TRUE},
	{'P', MU_MSG_FLAG_PASSED, TRUE},
	{'R', MU_MSG_FLAG_REPLIED, TRUE},
	{'S', MU_MSG_FLAG_SEEN, TRUE},
	{'T', MU_MSG_FLAG_TRASHED, TRUE},
	{'a', MU_MSG_FLAG_HAS_ATTACH, FALSE},
	{'s', MU_MSG_FLAG_SIGNED, FALSE},
	{'x', MU_MSG_FLAG_ENCRYPTED, FALSE}
};


MuMsgFlags
mu_msg_flags_from_str (const char* str)
{
	MuMsgFlags flags = 0;	
	while (str[0]) {
		int i;
		MuMsgFlags oneflag = MU_MSG_FLAG_UNKNOWN;
		for (i = 0; i != G_N_ELEMENTS(FLAG_CHARS); ++i) {
			if (str[0] == FLAG_CHARS[i].kar) {
				oneflag = FLAG_CHARS[i].flag;
				break;
			}
		}
		if (oneflag == MU_MSG_FLAG_UNKNOWN)
			return MU_MSG_FLAG_UNKNOWN;
		else
			flags |= oneflag;

		++str;
	}
 
	return flags;
}

MuMsgFlags
mu_msg_flags_from_char (char c)
{
	char str[2];

	str[0] = c;
	str[1] = '\0';

	return mu_msg_flags_from_str (str);
}


const char*
mu_msg_flags_to_str_s  (MuMsgFlags flags)
{
	int i = 0, j = 0;
	static char buf[G_N_ELEMENTS(FLAG_CHARS) + 1];
	
	for (i = 0; i != G_N_ELEMENTS(FLAG_CHARS); ++i)
		if (flags & FLAG_CHARS[i].flag)
			buf[j++] = FLAG_CHARS[i].kar;
	buf[j] = '\0';
	return buf;
}


gboolean
mu_msg_flags_is_file_flag (MuMsgFlags flag)
{
	int i = 0;
	
	for (i = 0; i != G_N_ELEMENTS(FLAG_CHARS); ++i)
		if (flag == FLAG_CHARS[i].flag)
			return FLAG_CHARS[i].file_flag;

	return FALSE;
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
			g_message ("'cur' file, but no info part: %s", path);
		mtype = MSG_TYPE_CUR;
	} else if (g_str_has_suffix (dir, G_DIR_SEPARATOR_S "new")) {
		/* if (*info) */
		/* 	g_message ("'new' file, ignoring info part: %s", path); */
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

	g_return_val_if_fail (path, MU_MSG_FLAG_UNKNOWN);
	g_return_val_if_fail (!g_str_has_suffix(path,G_DIR_SEPARATOR_S),
			      MU_MSG_FLAG_UNKNOWN);
	
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
				MuMsgFlags oneflag = 
					mu_msg_flags_from_char (*cursor);
				/* ignore anything but file flags */
				if (mu_msg_flags_is_file_flag(oneflag))
					flags |= oneflag;			
				++cursor;
			}
		}
	} 
	g_free (info);
	return flags;
}

