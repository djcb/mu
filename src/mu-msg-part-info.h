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

#ifndef __MU_MSG_PART_INFO_H__
#define __MU_MSG_PART_INFO_H__

#include <glib.h>

struct _MuMsgPartInfo {

	/* index of this message */
	guint             index;

	/* cid */
	char             *content_id;
	
	/* content-type: type/subtype, ie. text/plain */
	char             *type;    
	char             *subtype;
	/* full content-type, e.g. image/jpeg  */
	char             *content_type;

	/* the file name (if any) */
	char             *file_name;

	/* usually, "attachment" or "inline"; use
	 * mu_msg_part_info_is_(attachment|inline)
	 * to test */
	char             *disposition;
	
	/* size of the part; or 0 if unknown */
	size_t		 *size;	
};
typedef struct _MuMsgPartInfo MuMsgPartInfo;


/**
 * create a new MuMsgPartInfo instance; this just allocates the
 * memory, you'll have to fill it yourself
 *
 * 
 * @return a new MuMsgPartInfo instance; use mu_msg_part_info_destroy
 * when finished.
 */
MuMsgPartInfo *mu_msg_part_info_new (void);

/**
 * destroy a MuMsgPartInfo object
 * 
 * @param ct a MuMsgPartInfo object, or NULL
 * @param destroy_members TRUE if members should be destroyed as well,
 * FALSE otherwise
 */
void mu_msg_part_info_destroy (MuMsgPartInfo *pinfo,
			       gboolean destroy_members);


/**
 * macro to get the file name for this mime-part
 * 
 * @param pi a MuMsgPartInfo instance
 * 
 * @return the file name
 */
#define mu_msg_part_info_file_name(pi)    ((pi)->file_name)

/**
 * macro to get the file name for this mime-part
 * 
 * @param pi a MuMsgPartInfo instance
 * 
 * @return the file name
 */
#define  mu_msg_part_content_type(pi) ((pi)->content_type)
 

/**
 * callback function
 * 
 * @param a msg part info object
 * @param user_data a user provided data pointer
 * 
 * @return TRUE if we should continue the foreach, FALSE otherwise
 */
typedef gboolean  (*MuMsgPartInfoForeachFunc) (MuMsgPartInfo* part,
					       gpointer user_data);

/**
 * call a function for each MuMsgPartInfo (MIME-part) in the list
 * 
 * @param lst a list of MuMsgPartInfo objects
 * @param func a callback function
 * @param user_data user pointer, passed to the callback
 */
void mu_msg_part_info_list_foreach (GSList *lst,
				    MuMsgPartInfoForeachFunc func,
				    gpointer user_data);

/**
 * free a list of MuMsgPartInfo objects
 * 
 * @param lst list of MuMsgPartInfo object
 */
void mu_msg_part_info_list_free (GSList *lst, gboolean destroy_members);

#endif /*__MU_MSG_PART_INFO_H__*/
