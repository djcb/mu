/* 
** Copyright (C) 2008-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 3 of the License, or
** (at your option) any later version.
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

#ifndef __MU_MSG_PRIO_H__
#define __MU_MSG_PRIO_H__

#include <glib.h>

G_BEGIN_DECLS

enum _MuMsgPrio {
	MU_MSG_PRIO_LOW	   = 'l',
	MU_MSG_PRIO_NORMAL = 'n',
	MU_MSG_PRIO_HIGH   = 'h'
};
typedef enum _MuMsgPrio MuMsgPrio;

static const MuMsgPrio MU_MSG_PRIO_NONE = (MuMsgPrio)0;


/**
 * get a printable name for the message priority
 * (ie., MU_MSG_PRIO_LOW=>"low" etc.)
 * 
 * @param prio a message priority
 * 
 * @return a printable name for this priority
 */
const char* mu_msg_prio_name (MuMsgPrio prio) G_GNUC_CONST;


/**
 * get the MuMsgPriority corresponding to a one-character shortcut
 * ('l'=>MU_MSG_PRIO_, 'n'=>MU_MSG_PRIO_NORMAL or
 * 'h'=>MU_MSG_PRIO_HIGH)
 * 
 * @param k a character 
 * 
 * @return a message priority
 */
MuMsgPrio mu_msg_prio_from_char (char k) G_GNUC_CONST;


/**
 * get the one-character shortcut corresponding to a message priority
 * ('l'=>MU_MSG_PRIO_, 'n'=>MU_MSG_PRIO_NORMAL or
 * 'h'=>MU_MSG_PRIO_HIGH)
 * 
 * @param prio a mesage priority
 * 
 * @return a shortcut character or 0 in case of error
 */
char mu_msg_prio_char (MuMsgPrio prio) G_GNUC_CONST;

typedef void (*MuMsgPrioForeachFunc) (MuMsgPrio prio, gpointer user_data);
/**
 * call a function for each message priority
 * 
 * @param func a callback function
 * @param user_data a user pointer to pass to the callback
 */
void mu_msg_prio_foreach (MuMsgPrioForeachFunc func, gpointer user_data);



G_END_DECLS

#endif /*__MU_MSG_PRIO_H__*/
