/* 
** Copyright (C) 2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef __MU_MSG_H__
#define __MU_MSG_H__

#include "mu-msg-flags.h"
#include "mu-msg-fields.h"

/* what kind of message is this; use by the indexer */
enum _MuMsgStatus {
	MU_MSG_STATUS_NEW,	  /* message is new */
	MU_MSG_STATUS_UPDATE,	  /* message is to be updated */
	MU_MSG_STATUS_CLEANUP,	  /* message is to be cleaned up from db */
	MU_MSG_STATUS_CLEANED_UP, /* message has been cleaned up from db */
	MU_MSG_STATUS_EXISTS,	  /* message exists (will not be cleaned up) */
	MU_MSG_STATUS_UPTODATE	  /* message is up-to-date */
};
typedef enum _MuMsgStatus MuMsgStatus;

enum _MuMsgPriority {  
	MU_MSG_PRIORITY_NONE    = 0,

	MU_MSG_PRIORITY_LOW     = 1,
	MU_MSG_PRIORITY_NORMAL  = 2,
	MU_MSG_PRIORITY_HIGH    = 3
};
typedef enum _MuMsgPriority MuMsgPriority;

enum _MuMsgContactType {  /* Reply-To:? */
	MU_MSG_CONTACT_TYPE_TO,
	MU_MSG_CONTACT_TYPE_FROM,
	MU_MSG_CONTACT_TYPE_CC,
	MU_MSG_CONTACT_TYPE_BCC
};
typedef enum _MuMsgContactType MuMsgContactType;

struct _MuMsgContact {
	const char          *name;    /* Foo Bar */
	const char          *address; /* foo@bar.cuux */
	MuMsgContactType     type;    /*MU_MSG_CONTACT_TYPE_{TO,CC,BCC,FROM}*/  
};
typedef struct _MuMsgContact MuMsgContact;


#endif /*__MU_MSG_H__*/
