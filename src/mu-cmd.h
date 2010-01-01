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

enum _MuCmd {
	MU_CMD_INDEX,
	MU_CMD_QUERY,
	MU_CMD_MKDIR,
	MU_CMD_LINK,
	MU_CMD_HELP,
	
	MU_CMD_UNKNOWN
};
typedef enum _MuCmd MuCmd;

/** 
 * determine the Mu command from a string
 * (as given on the cmdline)
 * 
 * @param cmd string for a command
 * 
 * @return the MuCmd or MU_CMD_UNKNOWN
 */
MuCmd  mu_cmd_from_string (const char* cmd);


/** 
 * check the command line parameters
 * 
 * @param cmd the command
 * @param opts options
 * 
 * @return TRUE if there are no errors, FALSE otherwise
 */
gboolean mu_cmd_check_parameters (MuCmd cmd, MuConfigOptions *opts);

#endif /*__MU_CMD_H__*/
