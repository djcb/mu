/*
** Copyright (C) 2011-2012  <djcb@djcbsoftware.nl>
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

#include <string.h>
#include "mu-flags.h"

struct _FlagInfo {
	MuFlags		 flag;
	char		 kar;
	const char	*name;
	MuFlagType	 flag_type;
};
typedef struct _FlagInfo FlagInfo;

static const FlagInfo FLAG_INFO[] = {

	/* NOTE: order of this is significant, due to optimizations
	 * below */

	{ MU_FLAG_DRAFT,      'D', "draft",     MU_FLAG_TYPE_MAILFILE },
	{ MU_FLAG_FLAGGED,    'F', "flagged",   MU_FLAG_TYPE_MAILFILE },
	{ MU_FLAG_PASSED,     'P', "passed",    MU_FLAG_TYPE_MAILFILE },
	{ MU_FLAG_REPLIED,    'R', "replied",   MU_FLAG_TYPE_MAILFILE },
	{ MU_FLAG_SEEN,       'S', "seen",      MU_FLAG_TYPE_MAILFILE },
	{ MU_FLAG_TRASHED,    'T', "trashed",   MU_FLAG_TYPE_MAILFILE },

	{ MU_FLAG_NEW,        'N', "new",       MU_FLAG_TYPE_MAILDIR  },

	{ MU_FLAG_SIGNED,     'z', "signed",    MU_FLAG_TYPE_CONTENT  },
	{ MU_FLAG_ENCRYPTED,  'x', "encrypted", MU_FLAG_TYPE_CONTENT  },
	{ MU_FLAG_HAS_ATTACH, 'a', "attach",    MU_FLAG_TYPE_CONTENT  },
	{ MU_FLAG_LIST,       'l', "list",      MU_FLAG_TYPE_CONTENT  },


	{ MU_FLAG_UNREAD,     'u', "unread",    MU_FLAG_TYPE_PSEUDO  }
};


MuFlagType
mu_flag_type (MuFlags flag)
{
	unsigned u;

	for (u = 0; u != G_N_ELEMENTS (FLAG_INFO); ++u)
		if (FLAG_INFO[u].flag == flag)
			return FLAG_INFO[u].flag_type;

	return MU_FLAG_TYPE_INVALID;
}


char
mu_flag_char (MuFlags flag)
{
	unsigned u;

	for (u = 0; u != G_N_ELEMENTS (FLAG_INFO); ++u)
		if (FLAG_INFO[u].flag == flag)
			return FLAG_INFO[u].kar;
	return 0;
}



static MuFlags
mu_flag_from_char (char kar)
{
	unsigned u;

	for (u = 0; u != G_N_ELEMENTS (FLAG_INFO); ++u)
		if (FLAG_INFO[u].kar == kar)
			return FLAG_INFO[u].flag;

	return MU_FLAG_INVALID;
}


const char*
mu_flag_name (MuFlags flag)
{
	unsigned u;

	for (u = 0; u != G_N_ELEMENTS (FLAG_INFO); ++u)
		if (FLAG_INFO[u].flag == flag)
			return FLAG_INFO[u].name;

	return NULL;
}


const char*
mu_flags_to_str_s (MuFlags flags, MuFlagType types)
{
	unsigned u,v;
	static char str[sizeof(FLAG_INFO) + 1];

	for (u = 0, v = 0; u != G_N_ELEMENTS(FLAG_INFO); ++u)
		if (flags & FLAG_INFO[u].flag &&
		    types & FLAG_INFO[u].flag_type)
			str[v++] = FLAG_INFO[u].kar;
	str[v] = '\0';

	return str;
}


MuFlags
mu_flags_from_str (const char *str, MuFlagType types,
		   gboolean ignore_invalid)
{
	const char	*cur;
	MuFlags		 flag;

	g_return_val_if_fail (str, MU_FLAG_INVALID);

	for (cur = str, flag = MU_FLAG_NONE; *cur; ++cur) {

		MuFlags f;

		f = mu_flag_from_char (*cur);

		if (f == MU_FLAG_INVALID) {
			if (ignore_invalid)
				continue;
			return MU_FLAG_INVALID;
		}

		if (mu_flag_type (f) & types)
			flag |= f;
	}

	return flag;
}



char*
mu_flags_custom_from_str (const char *str)
{
	char *custom;
	const char* cur;
	unsigned u;

	g_return_val_if_fail (str, NULL);

	for (cur = str, u = 0, custom = NULL; *cur; ++cur) {

		MuFlags flag;
		flag = mu_flag_from_char (*cur);

		/* if it's a valid file flag, ignore it */
		if (flag != MU_FLAG_INVALID &&
		    mu_flag_type (flag) == MU_FLAG_TYPE_MAILFILE)
			continue;

		/* otherwise, add it to our custom string */
		if (!custom)
			custom = g_new0 (char, strlen(str) + 1);
		custom[u++] = *cur;
	}

	return custom;
}



void
mu_flags_foreach (MuFlagsForeachFunc func, gpointer user_data)
{
	unsigned u;

	g_return_if_fail (func);

	for (u = 0; u != G_N_ELEMENTS(FLAG_INFO); ++u)
		func (FLAG_INFO[u].flag, user_data);
}


MuFlags
mu_flags_from_str_delta (const char *str, MuFlags oldflags,
			 MuFlagType types)
{
	const char	*cur;
	MuFlags		 newflags;

	g_return_val_if_fail (str, MU_FLAG_INVALID);

	for (cur = str, newflags = oldflags; *cur; ++cur) {

		MuFlags f;
		if (*cur == '+' || *cur == '-') {
			f = mu_flag_from_char (cur[1]);
			if (f == 0)
				goto error;
			if (*cur == '+')
				newflags  |= f;
			else
				newflags  &= ~f;
			++cur;
			continue;
		}

		goto error;
	}

	return newflags;
error:
	g_warning ("invalid flag string");
	return MU_FLAG_INVALID;

}
