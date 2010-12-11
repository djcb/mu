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

/* hopefully, the should get us a sane PATH_MAX */
#include <limits.h>
/* not all systems provide PATH_MAX in limits.h */
#ifndef PATH_MAX
#include <sys/param.h>
#ifndef PATH_MAX
#define PATH_MAX MAXPATHLEN
#endif				/*!PATH_MAX */
#endif				/*PATH_MAX */

#include <string.h>
#include "mu-msg-file.h"

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
check_msg_type (const char *path, char **info)
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

		*info = NULL;
		dir = g_path_get_dirname(path);
		file = g_path_get_basename(path);
		
		if (!(*info = strrchr(file, ':')))
				*info = strrchr(file, '!');	/* Tinymail */
		if (*info)
				++(*info);	/* skip the ':' or '!' */

		if (g_str_has_suffix(dir, G_DIR_SEPARATOR_S "cur")) {
				if (!*info)
						g_debug("'cur' file, but no info part: %s", path);
				mtype = MSG_TYPE_CUR;
		} else if (g_str_has_suffix(dir, G_DIR_SEPARATOR_S "new")) {
				if (*info)
						g_debug("'new' file, ignoring info part: %s", path);
				mtype = MSG_TYPE_NEW;
		} else
				mtype = MSG_TYPE_OTHER;	/* file has been added explicitly as
										   a single message */
		if (*info)
				*info = g_strdup(*info);

		g_free(dir);
		g_free(file);

		return mtype;
}


MuMsgFlags
mu_msg_file_get_flags_from_path (const char *path)
{
		MuMsgFlags flags;
		MsgType mtype;
		char *info = NULL;

		g_return_val_if_fail (path, MU_MSG_FLAG_NONE);
		g_return_val_if_fail (!g_str_has_suffix(path, G_DIR_SEPARATOR_S),
							  MU_MSG_FLAG_NONE);

		mtype = check_msg_type (path, &info);
		if (mtype == MSG_TYPE_NEW) {	/* we ignore any new-msg flags */
				g_free(info);
				return MU_MSG_FLAG_NEW;
		}

		flags = MU_MSG_FLAG_NONE;
		if (mtype == MSG_TYPE_CUR || mtype == MSG_TYPE_OTHER) {
				char *cursor = info;
				/* only support the "2," format */
				if (cursor && cursor[0] == '2' && cursor[1] == ',') {
						cursor += 2;	/* jump past 2, */
						for (; *cursor; ++cursor) {
								switch (*cursor) {
								case 'P':
										flags |= MU_MSG_FLAG_PASSED;
										break;
								case 'T':
										flags |= MU_MSG_FLAG_TRASHED;
										break;
								case 'R':
										flags |= MU_MSG_FLAG_REPLIED;
										break;
								case 'S':
										flags |= MU_MSG_FLAG_SEEN;
										break;
								case 'D':
										flags |= MU_MSG_FLAG_DRAFT;
										break;
								case 'F':
										flags |= MU_MSG_FLAG_FLAGGED;
										break;
								}
						}
				}
		}
		g_free(info);

		return flags;
}


static const char*
get_flags_str_s (MuMsgFlags flags)
{
		int i;
		static char flagstr[7]; 
		
		/* now, determine the flags to use */
		if (flags & MU_MSG_FLAG_DRAFT)
				flagstr[i++] = 'D';
		if (flags & MU_MSG_FLAG_FLAGGED)
				flagstr[i++] = 'F';
		if (flags & MU_MSG_FLAG_PASSED)
				flagstr[i++] = 'P';
		if (flags & MU_MSG_FLAG_REPLIED)
				flagstr[i++] = 'R';
		if (flags & MU_MSG_FLAG_SEEN)
				flagstr[i++] = 'S';
		if (flags & MU_MSG_FLAG_TRASHED)
				flagstr[i++] = 'T';

		flagstr[i] = '\0';

		return flagstr;
}


/*
 * take an exising message path, and return a new path, based on whether it should be in
 * 'new' or 'cur'; ie.
 *
 * /home/user/Maildir/foo/bar/cur/abc:2,F  and flags == MU_MSG_FLAG_NEW
 *     => /home/user/Maildir/foo/bar/new
 * and 
 * /home/user/Maildir/foo/bar/new/abc  and flags == MU_MSG_FLAG_REPLIED
 *    => /home/user/Maildir/foo/bar/cur
 *
 * so only difference is whether MuMsgFlags matches MU_MSG_FLAG_NEW is set or not
 * 
 */
static char*
get_new_dir_name (const char* oldpath, MuMsgFlags flags)
{
		char *newpath, *dirpart;
		
		/* g_path_get_dirname is not explicit about whether it ends in
		 * a dir-separator (\ or /), so we need to check both */
		const char* cur4 = G_DIR_SEPARATOR_S "cur";
		const char* cur5 = G_DIR_SEPARATOR_S "cur" G_DIR_SEPARATOR_S;
		const char* new4 = G_DIR_SEPARATOR_S "new";
		const char* new5 = G_DIR_SEPARATOR_S "new" G_DIR_SEPARATOR_S;
		
		g_return_val_if_fail (oldpath, NULL);
		/* if MU_MSG_FLAG_NEW is set, it must be the only flag */
		g_return_val_if_fail (flags & MU_MSG_FLAG_NEW ?
							  flags == MU_MSG_FLAG_NEW : TRUE, NULL);
		
		newpath = g_path_get_dirname (oldpath);
		if (g_str_has_suffix (newpath, cur4) || g_str_has_suffix (newpath, new4))
				dirpart = &newpath[strlen(newpath) - strlen(cur4)];
		else if (g_str_has_suffix (newpath, cur5) || g_str_has_suffix (newpath, new5))
				dirpart = &newpath[strlen(newpath) - strlen(cur5)];
		else {
				g_warning ("invalid maildir path: %s", oldpath);
				g_free (newpath);
				return NULL;
		}

		/* now, copy the desired dir part behind this */
		if (flags & MU_MSG_FLAG_NEW) 
				memcpy (dirpart, new4, strlen(new4) + 1);
		else
				memcpy (dirpart, cur4, strlen(cur4) + 1);
		
		return newpath;
}

/*
 * get a new filename for the message, based on the new flags; if the
 * message has MU_MSG_FLAG_NEW, it will loose its flags
 *
 */ 
static char*
get_new_file_name (const char *oldpath, MuMsgFlags flags)
{
		gchar *newname, *sep;
		
		/* if MU_MSG_FLAG_NEW is set, it must be the only flag */
		g_return_val_if_fail (flags & MU_MSG_FLAG_NEW ?
							  flags == MU_MSG_FLAG_NEW : TRUE, NULL);
		
		/* the normal separator is ':', but on e.g. vfat, '!' is seen
		 * as well */
		newname		= g_path_get_basename (oldpath);
		if (!newname) {
				g_warning ("invalid path: '%s'", oldpath);
				return NULL;
		}

		if (!(sep = g_strrstr (newname, ":")) &&
			!(sep = g_strrstr (newname, "!"))) {
				g_warning ("not a valid msg file name: '%s'", oldpath);
				g_free (newname);
				return NULL;
		}

		if (flags & MU_MSG_FLAG_NEW)
				sep[0] = '\0'; /* remove all, including ':' or '!' */
		else {
				gchar *tmp;
				sep[1] = '\0'; /* remove flags, but keep ':' or '!' */
				sep[flags & MU_MSG_FLAG_NEW ? 0 : 1] = '\0';
				tmp = newname;
				newname = g_strdup_printf ("%s2,%s", newname, get_flags_str_s (flags));
				g_free (tmp);
		}

		return newname;
}

char*
mu_msg_file_get_path_from_flags (const char *oldpath, MuMsgFlags newflags)
{
		char *newname, *newdir, *newpath;
		
		g_return_val_if_fail (oldpath, NULL);
		g_return_val_if_fail (newflags != MU_MSG_FLAG_NONE, NULL);
		/* if MU_MSG_FLAG_NEW is set, it must be the only flag */
		g_return_val_if_fail (newflags & MU_MSG_FLAG_NEW ?
							  newflags == MU_MSG_FLAG_NEW : TRUE, NULL);
		

		newname = get_new_file_name (oldpath, newflags);
		if (!newname)
				return NULL;

		newdir = get_new_dir_name (oldpath, newflags);
		if (!newdir) {
				g_free (newname);
				return NULL;
		}
				
		newpath = g_strdup_printf ("%s%c%s", newdir, G_DIR_SEPARATOR, newname);
		g_free (newname);
		g_free (newdir);
		
		return newpath;
}
