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

#ifndef __MU_MSG_PART_H__
#define __MU_MSG_PART_H__

#include <glib.h>

struct _MuMsgPart {

	/* index of this message */
	unsigned         index;

	/* cid */
	char             *content_id;
	
	/* content-type: type/subtype, ie. text/plain */
	char             *type;    
	char             *subtype;
	/* full content-type, e.g. image/jpeg  */
	/* char             *content_type; */

	/* the file name (if any) */
	char             *file_name;

	/* usually, "attachment" or "inline"; use
	 * mu_msg_part_is_(attachment|inline)
	 * to test */
	char             *disposition;
	
	/* size of the part; or 0 if unknown */
	size_t		 *size;	

	gpointer         data; /* opaque data */
	
	/* if TRUE, mu_msg_part_destroy will free the member vars
	 * as well*/
	gboolean          own_members;
};
typedef struct _MuMsgPart MuMsgPart;

/**
 * macro to get the file name for this mime-part
 * 
 * @param pi a MuMsgPart instance
 * 
 * @return the file name
 */
#define mu_msg_part_file_name(pi)    ((pi)->file_name)

/**
 * macro to get the file name for this mime-part
 * 
 * @param pi a MuMsgPart instance
 * 
 * @return the file name
 */
#define  mu_msg_part_content_type(pi) ((pi)->content_type)




typedef void (*MuMsgPartForeachFunc) (MuMsgPart *part, gpointer data);
/**
 * call a function for each of the contacts in a message 
 *
 * @param msg a valid MuMsg* instance
 * @param func a callback function to call for each contact; when
 * the callback does not return TRUE, it won't be called again
 * @param user_data a user-provide pointer that will be passed to the callback
 * 
 */
void mu_msg_msg_part_foreach (MuMsg *msg,
			      MuMsgPartForeachFunc func,
			      gpointer user_data);


#endif /*__MU_MSG_PART_H__*/
