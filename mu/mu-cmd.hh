/*
** Copyright (C) 2008-2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <mu-config.hh>
#include <mu-store.hh>

G_BEGIN_DECLS

/**
 * execute the 'find' command
 *
 * @param store store object to use
 * @param opts configuration options
 * @param err receives error information, or NULL
 *
 * @return MU_OK (0) if the command succeeds and
 * >MU_OK (0) results, MU_EXITCODE_NO_MATCHES if the command
 * succeeds but there no matches, some error code for all other errors
 */
MuError mu_cmd_find (MuStore* store, const MuConfig *opts,
                     GError **err);

/**
 * execute the 'extract' command
 *
 * @param opts configuration options
 * @param err receives error information, or NULL
 *
 * @return MU_OK (0) if the command succeeds,
 * some error code otherwise
 */
MuError mu_cmd_extract (const MuConfig *opts, GError **err);


/**
 * execute the 'script' command
 *
 * @param opts configuration options
 * @param err receives error information, or NULL
 *
 * @return MU_OK (0) if the command succeeds,
 * some error code otherwise
 */
MuError mu_cmd_script (const MuConfig *opts, GError **err);

/**
 * execute the cfind command
 *
 * @param store store object to use
 * @param opts configuration options
 * @param err receives error information, or NULL
 *
 * @return MU_OK (0) if the command succeeds,
 * some error code otherwise
 */
MuError mu_cmd_cfind (const Mu::Store& store, const MuConfig *opts,
                      GError **err);

/**
 * execute some mu command, based on 'opts'
 *
 * @param opts configuration option
 * @param err receives error information, or NULL
 *
 * @return MU_OK if all went wall, some error code otherwise
 */
MuError mu_cmd_execute (const MuConfig *opts, GError **err);

/**
 * execute the 'index' command
 *
 * @param store store object to use
 * @param opts configuration options
 * @param err receives error information, or NULL
 *
 * @return MU_OK (0) if the command succeeded,
 * some error code otherwise
 */
MuError mu_cmd_index (Mu::Store& store, const MuConfig *opt, GError **err);

/**
 * execute the server command
 * @param opts configuration options
 * @param err receives error information, or NULL
 *
 * @return MU_OK (0) if the command succeeds, some error code otherwise
 */
MuError mu_cmd_server (const MuConfig *opts, GError **err);

G_END_DECLS

#endif /*__MU_CMD_H__*/
