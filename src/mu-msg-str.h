/* 
** Copyright (C) 2008 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef __MU_MSG_STR_H__
#define __MU_MSG_STR_H__

#include <time.h>
#include <sys/types.h>

#include "mu-msg.h"
#include "mu-msg-flags.h"

/** 
 * get a display string for a given time_t;
 * use the preferred date/time for the current locale
 * (ie., '%c' in strftime).
 * 
 * mu_msg_str_date_s returns a ptr to a static buffer,
 * while mu_msg_str_date returns dynamically allocated
 * memory that must be freed after use.
 *
 * @param t the time as time_t
 * 
 * @return a string representation of the time; see above
 * for what to do with it
 */
const char* mu_msg_str_date_s (time_t t) G_GNUC_CONST;
char*       mu_msg_str_date   (time_t t);


/** 
 * get a display size for a given off_t;
 * uses M for sizes > 1000*1000, k for smaller sizes
 * 
 * mu_msg_str_size_s returns a ptr to a static buffer,
 * while mu_msg_str_size returns dynamically allocated
 * memory that must be freed after use.
 *
 * @param t the size as an off_t
 * 
 * @return a string representation of the size; see above
 * for what to do with it
 */
const char* mu_msg_str_size_s  (size_t s) G_GNUC_CONST;
char*       mu_msg_str_size    (size_t s);

/** 
 * get a display string for a given set of flags, OR'ed in 
 * @param flags; one character per flag:
 * D=draft,F=flagged,N=new,P=passed,R=replied,S=seen,T=trashed 
 * a=has-attachment,s=signed, x=encrypted
 * 
 * mu_msg_str_file_flags_s  returns a ptr to a static buffer,
 * while mu_msg_str_file_flags returns dynamically allocated
 * memory that must be freed after use.
 *
 * @param flags file flags
 * 
 * @return a string representation of the flags; see above
 * for what to do with it
 */
const char* mu_msg_str_flags_s  (MuMsgFlags flags) G_GNUC_CONST;
char*       mu_msg_str_flags    (MuMsgFlags flags);


/** 
 * get a display string for a message priority; either
 * High,Low or Normal
 *
 * @param flags file flags
 * 
 * @return a string representation of the priority; see above
 * for what to do with it, or NULL in case of error
 */
const char* mu_msg_str_prio  (MuMsgPriority prio) G_GNUC_CONST;


#endif /*__MU_MSG_STR_H__*/
