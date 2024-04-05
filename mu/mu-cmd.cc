/*
** Copyright (C) 2010-2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "config.h"

#include <iostream>
#include <iomanip>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

#include "mu-options.hh"
#include "mu-cmd.hh"
#include "mu-maildir.hh"
#include "mu-contacts-cache.hh"
#include "message/mu-message.hh"
#include "message/mu-mime-object.hh"

#include "utils/mu-error.hh"
#include "utils/mu-utils-file.hh"
#include "utils/mu-utils.hh"

#include <thirdparty/tabulate.hpp>

using namespace Mu;


static Result<void>
cmd_fields(const Options& opts)
{
	mu_printerrln("the 'mu fields' command has been superseded by 'mu info'; try:\n"
		      "  mu info fields\n");
	return Ok();
}


static Result<void>
cmd_find(const Options& opts)
{
	auto store{Store::make(opts.runtime_path(RuntimePath::XapianDb))};
	if (!store)
		return Err(store.error());
	else
		return mu_cmd_find(*store, opts);
}


static void
show_usage(void)
{
	mu_println("usage: mu command [options] [parameters]");
	mu_println("where command is one of index, find, cfind, view, mkdir, "
		"extract, add, remove, script, verify or server");
	mu_println("see the mu, mu-<command> or mu-easy manpages for "
		   "more information");
}


using ReadOnlyStoreFunc = std::function<Result<void>(const Store&, const Options&)>;
using WritableStoreFunc = std::function<Result<void>(Store&, const Options&)>;

static Result<void>
with_readonly_store(const ReadOnlyStoreFunc& func, const Options& opts)
{
	auto store{Store::make(opts.runtime_path(RuntimePath::XapianDb))};
	if (!store)
		return Err(store.error());

	return func(store.value(), opts);
}

static Result<void>
with_writable_store(const WritableStoreFunc func, const Options& opts)
{
	auto store{Store::make(opts.runtime_path(RuntimePath::XapianDb),
			       Store::Options::Writable)};
	if (!store)
		return Err(store.error());

	return func(store.value(), opts);
}

Result<void>
Mu::mu_cmd_execute(const Options& opts) try {

	if (!opts.sub_command)
		return Err(Error::Code::Internal, "missing subcommand");

	switch (*opts.sub_command) {
	case Options::SubCommand::Help:
		return Ok(); /* already handled in mu-options.cc */
	/*
	 * no store needed
	 */
	case Options::SubCommand::Fields:
		return cmd_fields(opts);
	case Options::SubCommand::Mkdir:
		return mu_cmd_mkdir(opts);
	case Options::SubCommand::Script:
		return mu_cmd_script(opts);
	case Options::SubCommand::View:
		return mu_cmd_view(opts);
	case Options::SubCommand::Verify:
		return mu_cmd_verify(opts);
	case Options::SubCommand::Extract:
		return mu_cmd_extract(opts);
	/*
	 * read-only store
	 */

	case Options::SubCommand::Cfind:
		return with_readonly_store(mu_cmd_cfind, opts);
	case Options::SubCommand::Find:
		return cmd_find(opts);
	case Options::SubCommand::Info:
		return with_readonly_store(mu_cmd_info, opts);

	/* writable store */

	case Options::SubCommand::Add:
		return with_writable_store(mu_cmd_add, opts);
	case Options::SubCommand::Remove:
		return with_writable_store(mu_cmd_remove, opts);
	case Options::SubCommand::Move:
		return with_writable_store(mu_cmd_move, opts);

	/*
	 * commands instantiate store themselves
	 */
	case Options::SubCommand::Index:
		return mu_cmd_index(opts);
	case Options::SubCommand::Init:
		return mu_cmd_init(opts);
	case Options::SubCommand::Server:
		return mu_cmd_server(opts);

	default:
		show_usage();
		return Ok();
	}

} catch (const Mu::Error& er) {
	return Err(er);
} catch (const std::runtime_error& re) {
	return Err(Error::Code::Internal, "runtime-error: {}", re.what());
} catch (const std::exception& ex) {
	return Err(Error::Code::Internal, "error: {}", ex.what());
} catch (...) {
	return Err(Error::Code::Internal, "caught exception");
}
