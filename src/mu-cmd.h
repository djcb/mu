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


#ifndef __MU_CMD_H__
#define __MU_CMD_H__

#include <glib.h>
#include "mu-config.h"

G_BEGIN_DECLS

enum _MuCmd {
	MU_CMD_INDEX,
	MU_CMD_FIND,
	MU_CMD_CLEANUP,
	MU_CMD_MKDIR,
	MU_CMD_VIEW,
	MU_CMD_HELP,
	
	MU_CMD_UNKNOWN
};
typedef enum _MuCmd MuCmd;

/**
 * try to execute whatever is specified on the command line 
 * 
 * @param config a config structure with the command line params
 * 
 * @return TRUE if it succeeded, FALSE otherwise
 */
gboolean mu_cmd_execute (MuConfigOptions *config);



/**
 * execute the 'mkdir' command
 * 
 * @param opts configuration options
 * 
 * @return TRUE if the command succeeded, FALSE otherwise
 */
gboolean mu_cmd_mkdir (MuConfigOptions *opts);


/**
 * execute the 'view' command
 * 
 * @param opts configuration options
 * 
 * @return TRUE if the command succeeded, FALSE otherwise
 */
gboolean mu_cmd_view (MuConfigOptions *opts);


/**
 * execute the 'index' command
 * 
 * @param opts configuration options
 * 
 * @return TRUE if the command succeede, FALSE otherwise
 */
gboolean mu_cmd_index   (MuConfigOptions *opts);


/**
 * execute the 'cleanup' command
 * 
 * @param opts configuration options
 * 
 * @return TRUE if the command succeede, FALSE otherwise
 */
gboolean mu_cmd_cleanup (MuConfigOptions *opts);


/**
 * execute the 'find' command
 * 
 * @param opts configuration options
 * 
 * @return TRUE if the command succeede, FALSE otherwise
 */
gboolean mu_cmd_find (MuConfigOptions *opts);


G_END_DECLS

#endif /*__MU_CMD_H__*/
