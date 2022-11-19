/*
** Copyright (C) 2008-2022-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef MU_CMD_HH__
#define MU_CMD_HH__

#include <glib.h>
#include <mu-store.hh>
#include <utils/mu-result.hh>

#include "mu-options.hh"

namespace Mu {


/**
 * Get message options from (sub)command options
 *
 * @param cmdopts (sub) command options
 *
 * @return message options
 */
template<typename CmdOpts>
constexpr Message::Options
message_options(const CmdOpts& cmdopts)
{
	Message::Options mopts{};

	if (cmdopts.decrypt)
		mopts |= Message::Options::Decrypt;
	if (cmdopts.auto_retrieve)
		mopts |= Message::Options::RetrieveKeys;

	return mopts;
}



/**
 * execute the 'find' command
 *
 * @param store store object to use
 * @param opts configuration options
 *
 * @return Ok() or some error
 */
Result<void> mu_cmd_find(const Store& store, const Options& opts);

/**
 * execute the 'extract' command
 *
 * @param opts configuration options
 *
 * @return Ok() or some error
 */
Result<void> mu_cmd_extract(const Options& opts);

/**
 * execute the 'fields' command
 *
 * @param opts configuration options
 *
 * @return Ok() or some error
 */
Result<void> mu_cmd_fields(const Options& opts);

/**
 * execute the 'script' command
 *
 * @param opts configuration options
 * @param err receives error information, or NULL
 *
 * @return Ok() or some error
 */
Result<void> mu_cmd_script(const Options& opts);

/**
 * execute the cfind command
 *
 * @param store store object to use
 * @param opts configuration options
 *
 * @return Ok() or some error
 */
Result<void> mu_cmd_cfind(const Store& store, const Options& opts);

/**
 * execute the 'index' command
 *
 * @param store store object to use
 * @param opts configuration options
 *
 * @return Ok() or some error
 */
Result<void> mu_cmd_index(Store& store, const Options& opt);

/**
 * execute the server command
 * @param opts configuration options
 * @param err receives error information, or NULL
 *
 * @return Ok() or some error
 */
Result<void> mu_cmd_server(const Options& opts);

/**
 * execute some mu command, based on 'opts'
 *
 * @param opts configuration option
 * @param err receives error information, or NULL
 *
 * @return Ok() or some error
 */
Result<void> mu_cmd_execute(const Options& opts);

} // namespace Mu

#endif /*__MU_CMD_H__*/
