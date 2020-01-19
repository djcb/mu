/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

/*
** Copyright (C) 2008-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <mu-store.hh>

G_BEGIN_DECLS

/**
 * execute the 'mkdir' command
 *
 * @param opts configuration options
 * @param err receives error information, or NULL
 *
 * @return MU_OK (0) if the command succeeded,
 * some error code otherwise
 */
MuError mu_cmd_mkdir (MuConfig *opts, GError **err);


/**
 * execute the 'view' command
 *
 * @param opts configuration options
 * @param err receives error information, or NULL
 *
 * @return MU_OK (0) if the command succeeded,
 * some error code otherwise
 */
MuError mu_cmd_view (MuConfig *opts, GError **err);


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
MuError mu_cmd_index   (MuStore *store, MuConfig *opt, GError **err);


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
MuError mu_cmd_find (MuStore *store, MuConfig *opts, GError **err);


/**
 * execute the 'extract' command
 *
 * @param opts configuration options
 * @param err receives error information, or NULL
 *
 * @return MU_OK (0) if the command succeeds,
 * some error code otherwise
 */
MuError mu_cmd_extract (MuConfig *opts, GError **err);


/**
 * execute the 'mv' command
 *
 * @param opts configuration options
 * @param err receives error information, or NULL
 *
 * @return MU_OK (0) if the command succeeds,
 * some error code otherwise
 */
MuError mu_cmd_mv (MuConfig *opts, GError **err);


/**
 * execute the 'script' command
 *
 * @param opts configuration options
 * @param err receives error information, or NULL
 *
 * @return MU_OK (0) if the command succeeds,
 * some error code otherwise
 */
MuError mu_cmd_script (MuConfig *opts, GError **err);

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
MuError mu_cmd_cfind (MuStore *store, MuConfig *opts, GError **err);


/**
 * execute the add command
 *
 * @param store store object to use
 * @param opts configuration options
 * @param err receives error information, or NULL
 *
 * @return MU_OK (0) if the command succeeds,
 * some error code otherwise
 */
MuError mu_cmd_add (MuStore *store, MuConfig *opts, GError **err);

/**
 * execute the remove command
 *
 * @param store store object to use
 * @param opts configuration options
 * @param err receives error information, or NULL
 *
 * @return MU_OK (0) if the command succeeds,
 * some error code otherwise
 */
MuError mu_cmd_remove (MuStore *store, MuConfig *opts, GError **err);

/**
 * execute the tickle command
 *
 * @param store store object to use
 * @param opts configuration options
 * @param err receives error information, or NULL
 *
 * @return MU_OK (0) if the command succeeds,
 * some error code otherwise
 */
MuError mu_cmd_tickle (MuStore *store, MuConfig *opts, GError **err);


/**
 * execute the server command
 * @param store store object to use
 * @param opts configuration options
  *
 * @return MU_OK (0) if the command succeeds,
 * some error code otherwise
 */
MuError mu_cmd_server (MuStore *store, MuConfig *opts, GError**/*unused*/);
MuError mu_cmd_server2 (MuStore *store, MuConfig *opts, GError**/*unused*/);

/**
 * execute the verify command (to verify signatures)
 * @param store store object to use
 * @param opts configuration options
 * @param err receives error information, or NULL
 *
 * @return MU_OK (0) if the command succeeds,
 * some error code otherwise
 */
MuError mu_cmd_verify (MuConfig *opts, GError **err);


/**
 * execute some mu command, based on 'opts'
 *
 * @param opts configuration option
 * @param err receives error information, or NULL
 *
 * @return MU_OK if all went wall, some error code otherwise
 */
MuError mu_cmd_execute (MuConfig *opts, GError **err);

G_END_DECLS

#endif /*__MU_CMD_H__*/
