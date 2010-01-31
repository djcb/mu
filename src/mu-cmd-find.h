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

#ifndef __MU_CMD_FIND_H__
#define __MU_CMD_FIND_H__

#include <glib.h>
#include "mu-config.h"

/** 
 * execute the 'find' command
 * 
 * @param opts configuration options
 * 
 * @return TRUE if the command succeede, FALSE otherwise
 */
gboolean mu_cmd_find (MuConfigOptions *opts);


/** 
 * execute the 'view' command
 * 
 * @param opts configuration options
 * 
 * @return TRUE if the command succeede, FALSE otherwise
 */
gboolean mu_cmd_view (MuConfigOptions *opts);


#endif /*__MU_CMD_FIND_H__*/
