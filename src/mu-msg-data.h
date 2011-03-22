/*
** Copyright (C) 2011 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef __MU_MSG_DATA_H__
#define __MU_MSG_DATA_H__

#include <time.h>
#include <glib.h>
#include <mu-msg-flags.h>
#include <mu-msg-prio.h>

G_BEGIN_DECLS

/* data structure for saving the data in a MuMsgIter */
struct _MuMsgData {

	char		*cc;
	char		*from;
	char		*maildir;
	char		*msgid;
	char		*path;
	char		*subject;
	char		*to;
	
	size_t		 size;
	time_t		 date;
	MuMsgFlags	 flags;
	MuMsgPrio	 prio;	
};
typedef struct _MuMsgData MuMsgData;


/** 
 * create an _unitialized_ MuMsgData structure, ie. you still need to
 * initialize the struct members. Free with mu_msg_data_destroy
 * 
 * 
 * @return a newly allocated MuMsgData struct
 */
MuMsgData* mu_msg_data_new (void) G_GNUC_WARN_UNUSED_RESULT;

/** 
 * free the MuMsgData structure
 * 
 * @param mdata a mu msg data structure, or NULL
 */
void mu_msg_data_destroy (MuMsgData *mdata);


/** 
 * (deep)copy a MuMsgData structure
 * 
 * @param mdata a mu msg data structure or NULL 
 * 
 * @return a copy, or NULL (free with mu_msg_data_destroy)
 */
MuMsgData* mu_msg_data_copy (MuMsgData *mdata);


G_END_DECLS

#endif /*__MU_MSG_DATA_H__*/
