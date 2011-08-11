/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

/*
** Copyright (C) 2008-2011 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <mu-config.h>

G_BEGIN_DECLS

/**
 * execute the 'mkdir' command
 * 
 * @param opts configuration options
 * 
 * @return MU_EXITCODE_OK (0) if the command succeeded,
 * MU_EXITCODE_ERROR otherwise
 */
MuError mu_cmd_mkdir (MuConfig *opts);


/**
 * execute the 'view' command
 * 
 * @param opts configuration options
 * 
 * @return MU_EXITCODE_OK (0) if the command succeeded,
 * MU_EXITCODE_ERROR otherwise
 */
MuError mu_cmd_view (MuConfig *opts);


/**
 * execute the 'index' command
 * 
 * @param opts configuration options
 * 
 * @return MU_EXITCODE_OK (0) if the command succeeded,
 * MU_EXITCODE_ERROR otherwise
 */
MuError mu_cmd_index   (MuConfig *opts);


/**
 * execute the 'cleanup' command
 * 
 * @param opts configuration options
 * 
 * @return MU_EXITCODE_OK (0) if the command succeeds,
 * MU_EXITCODE_ERROR otherwise
 */
MuError mu_cmd_cleanup (MuConfig *opts);

/**
 * execute the 'find' command
 * 
 * @param opts configuration options
 * 
 * @return MU_EXITCODE_OK (0) if the command succeeds and
 * >MU_EXITCODE_OK (0) results, MU_EXITCODE_NO_MATCHES if the command
 * succeeds but there no matches, MU_EXITCODE_ERROR for all other errors
 */
MuError mu_cmd_find (MuConfig *opts);


/**
 * execute the 'extract' command
 * 
 * @param opts configuration options
 * 
 * @return MU_EXITCODE_OK (0) if the command succeeds,
 * MU_EXITCODE_ERROR otherwise
 */
MuError mu_cmd_extract (MuConfig *opts);


/**
 * execute the 'mv' command
 * 
 * @param opts configuration options
 * 
 * @return MU_EXITCODE_OK (0) if the command succeeds,
 * MU_EXITCODE_ERROR otherwise
 */
MuError mu_cmd_mv (MuConfig *opts);




/**
 * execute the cfind command
 * 
 * @param opts configuration options
 * 
 * @return MU_EXITCODE_OK (0) if the command succeeds,
 * MU_EXITCODE_ERROR otherwise
 */
MuError mu_cmd_cfind (MuConfig *opts);


/**
 * execute the add command
 * 
 * @param opts configuration options
 * 
 * @return MU_EXITCODE_OK (0) if the command succeeds,
 * MU_EXITCODE_ERROR otherwise
 */
MuError mu_cmd_add (MuConfig *opts);

/**
 * execute the remove command
 * 
 * @param opts configuration options
 * 
 * @return MU_EXITCODE_OK (0) if the command succeeds,
 * MU_EXITCODE_ERROR otherwise
 */
MuError mu_cmd_remove (MuConfig *opts);

G_END_DECLS

#endif /*__MU_CMD_H__*/
