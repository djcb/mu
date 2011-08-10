/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

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


/* put these in alpha-order, so output strings are ordered correctly
 * (that's the way the Maildir spec teaches us)
 */
static const MuMsgFlags ALL_FLAGS[] = {
	/* a */ MU_MSG_FLAG_HAS_ATTACH, /* non-maildir flag */
	/* D */ MU_MSG_FLAG_DRAFT,
	/* F */ MU_MSG_FLAG_FLAGGED,
	/* N */ MU_MSG_FLAG_NEW,         
	/* P */ MU_MSG_FLAG_PASSED,
	/* R */ MU_MSG_FLAG_REPLIED,
	/* S */ MU_MSG_FLAG_SEEN,
	/* T */ MU_MSG_FLAG_TRASHED,
	/* U */ MU_MSG_FLAG_UNREAD,    /* pseudo flag */
	/* x */ MU_MSG_FLAG_ENCRYPTED, /* non-maildir flag */
	/* z */ MU_MSG_FLAG_SIGNED     /* non-maildir flag */
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
		
	/* NEW OR NOT SEEN */
	case 'U': return MU_MSG_FLAG_UNREAD;
		
	case 'z': return MU_MSG_FLAG_SIGNED;
	case 'x': return MU_MSG_FLAG_ENCRYPTED;
	case 'a': return MU_MSG_FLAG_HAS_ATTACH;
		
	default:
		g_return_val_if_reached (MU_MSG_FLAG_NONE);
	}
}

MuMsgFlags
mu_msg_flag_from_file_char (char k)
{
	switch (k) {
	case 'D': return MU_MSG_FLAG_DRAFT;
	case 'F': return MU_MSG_FLAG_FLAGGED;
	case 'P': return MU_MSG_FLAG_PASSED;
	case 'R': return MU_MSG_FLAG_REPLIED;
	case 'S': return MU_MSG_FLAG_SEEN;
	case 'T': return MU_MSG_FLAG_TRASHED;
	default: return 0;
	}
}


const char*
mu_msg_flag_name (MuMsgFlags flag)
{
	switch (flag) {
		
	/* real maildir flags */
	case MU_MSG_FLAG_DRAFT:		return "draft";
	case MU_MSG_FLAG_FLAGGED:	return "flagged";
	case MU_MSG_FLAG_NEW:		return "new";
	case MU_MSG_FLAG_PASSED:	return "passed";
	case MU_MSG_FLAG_REPLIED:	return "replied";
	case MU_MSG_FLAG_SEEN:		return "seen";
	case MU_MSG_FLAG_TRASHED:	return "trashed";

		/* ie., NEW or NOT SEEN */
	case MU_MSG_FLAG_UNREAD:	return "unread";
		
	case MU_MSG_FLAG_SIGNED:	return "signed";
	case MU_MSG_FLAG_ENCRYPTED:	return "encrypted";
	case MU_MSG_FLAG_HAS_ATTACH:	return "attach";
		
	default:
		g_return_val_if_reached (NULL);
	}
}

char
mu_msg_flag_char (MuMsgFlags flag)
{
	g_return_val_if_fail (flag != MU_MSG_FLAG_NONE, 0);
	
	switch (flag) {
		
	case MU_MSG_FLAG_NEW:		return 'N';
	case MU_MSG_FLAG_PASSED:	return 'P';
	case MU_MSG_FLAG_REPLIED:	return 'R';
	case MU_MSG_FLAG_SEEN:		return 'S';
	case MU_MSG_FLAG_TRASHED:	return 'T';
	case MU_MSG_FLAG_DRAFT:		return 'D';
	case MU_MSG_FLAG_FLAGGED:	return 'F';

	/* NEW OR NOT SEEN */
	case MU_MSG_FLAG_UNREAD:        return 'U';
		
	case MU_MSG_FLAG_SIGNED:	return 'z';
	case MU_MSG_FLAG_ENCRYPTED:	return 'x';
	case MU_MSG_FLAG_HAS_ATTACH:	return 'a';
		
	default:
		g_return_val_if_reached (0);
	}
}

const char*
mu_msg_flags_str_s (MuMsgFlags flags)
{
	int i, j;
	static char buf[16]; /* more than enough */
	
	for (i = j = 0; i != G_N_ELEMENTS(ALL_FLAGS); ++i) {
		if (flags & ALL_FLAGS[i]) {
			char k;
			if ((k = mu_msg_flag_char (ALL_FLAGS[i])) == 0)
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
		if ((flag = mu_msg_flag_from_char (*str)) == 0) {
			flags = 0;
			break;
		}
		flags |= flag;
	}
	
	return flags;
}


void
mu_msg_flags_foreach (MuMsgFlagsForeachFunc func, gpointer user_data)
{
	int i;
	
	g_return_if_fail (func);
	
	for (i = 0; i != G_N_ELEMENTS(ALL_FLAGS); ++i)
		func (ALL_FLAGS[i], user_data);
}



