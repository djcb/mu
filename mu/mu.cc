/*
** Copyright (C) 2008-2021 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <config.h>

#include <glib.h>
#include <glib-object.h>
#include <locale.h>

#include "mu-config.hh"
#include "mu-cmd.hh"
#include "mu-runtime.hh"
#include "utils/mu-utils.hh"

using namespace Mu;

static void
show_version(void)
{
	const char* blurb = "mu (mail indexer/searcher) version " VERSION "\n"
			    "Copyright (C) 2008-2021 Dirk-Jan C. Binnema\n"
			    "License GPLv3+: GNU GPL version 3 or later "
			    "<http://gnu.org/licenses/gpl.html>.\n"
			    "This is free software: you are free to change "
			    "and redistribute it.\n"
			    "There is NO WARRANTY, to the extent permitted by law.";

	g_print("%s\n", blurb);
}

static int
handle_result(const Result<void>& res, MuConfig* conf)
{
	if (res)
		return 0;

	using Color = MaybeAnsi::Color;
	MaybeAnsi col{conf ? !conf->nocolor : false};

	// show the error and some help, but not if it's only a softerror.
	if (!res.error().is_soft_error()) {
		std::cerr << col.fg(Color::Red) << "error" << col.reset() << ": "
			  << col.fg(Color::BrightYellow)
			  << res.error().what() << "something went wrong" << "\n";
	} else
		std::cerr <<  col.fg(Color::BrightBlue) << res.error().what() << '\n';

	std::cerr << col.fg(Color::Green);

	// perhaps give some useful hint on how to solve it.
	switch (res.error().code()) {
	case Error::Code::InvalidArgument:
		if (conf && mu_config_cmd_is_valid(conf->cmd))
			mu_config_show_help(conf->cmd);
		break;
	case Error::Code::SchemaMismatch:
		std::cerr << "Please (re)initialize mu with 'mu init' "
			  << "see mu-init(1) for details\n";
		break;
	default:
		break; /* nothing to do */
	}

	std::cerr << col.reset();

	return res.error().exit_code();
}

int
main(int argc, char* argv[])
{
	int rv{};
	MuConfig *conf{};
	GError*   err{};

	using Color = MaybeAnsi::Color;
	MaybeAnsi col{conf ? !conf->nocolor : false};

	setlocale(LC_ALL, "");

	conf = mu_config_init(&argc, &argv, &err);
	if (!conf) {
		std::cerr << col.fg(Color::Red) << "error" << col.reset() << ": "
			  << col.fg(Color::BrightYellow)
			  << (err ? err->message : "something went wrong") << "\n";
		rv = 1;
		goto cleanup;
	} else if (conf->version) {
		show_version();
		goto cleanup;
	} else if (conf->cmd == MU_CONFIG_CMD_NONE) /* nothing to do */
		goto cleanup;
	else if (!mu_runtime_init(conf->muhome, PACKAGE_NAME, conf->debug)) {
		std::cerr << col.fg(Color::Red) << "error initializing mu\n"
			  << col.reset();
		rv = 2;
	} else
		rv = handle_result(mu_cmd_execute(conf), conf);

cleanup:
	g_clear_error(&err);
	mu_config_uninit(conf);
	mu_runtime_uninit();

	return rv;
}
