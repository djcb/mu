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

#ifndef __MU_GUILE_MSG_H__
#define __MU_GUILE_MSG_H__

#include <libguile.h>
#include <mu-msg.h>

#ifdef __cplusplus
extern "C" {
#endif /*__cplusplus*/

typedef void* MuGuileFunc (void*);
	
/**
 * register MuMsg-related functions/smobs with guile; use with
 * scm_with_guile
 *
 * @param data
 */
void *mu_guile_msg_init (void *data);


/**
 * set 'mu:msg:current in the guile env
 * 
 * @param path path to a message
 * 
 * @return TRUE if it worked, FALSE otherwise
 */	
gboolean mu_guile_msg_load_current (const char *path);
	
	
/**
 * create an SCM for the MuMsg*
 * 
 * @param msg a MuMsg instance
 * 
 * @return an SCM for the msg
 */	
SCM  mu_guile_msg_to_scm (MuMsg *msg);
	
#ifdef __cplusplus
}
#endif /*__cplusplus*/


#endif /*__MU_GUILE_MSG_H__*/
