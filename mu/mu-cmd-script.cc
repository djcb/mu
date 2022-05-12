/*
** Copyright (C) 2012-2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#if HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <dirent.h>
#include <errno.h>

#include "mu-cmd.hh"
#include "mu-script.hh"
#include "mu-runtime.hh"

#include "utils/mu-util.h"
#include "utils/mu-str.h"

#define MU_GUILE_EXT          ".scm"
#define MU_GUILE_DESCR_PREFIX ";; INFO: "

#define COL(C) ((color) ? C : "")

using namespace Mu;

static void
print_script(const char* name,
	     const char* oneline,
	     const char* descr,
	     gboolean    color,
	     gboolean    verbose)
{
	g_print("%s%s%s%s%s%s%s%s",
		verbose ? "\n" : "  * ",
		COL(MU_COLOR_GREEN),
		name,
		COL(MU_COLOR_DEFAULT),
		oneline ? ": " : "",
		COL(MU_COLOR_BLUE),
		oneline ? oneline : "",
		MU_COLOR_DEFAULT);

	if (verbose && descr)
		g_print("%s%s%s", COL(MU_COLOR_MAGENTA), descr, COL(MU_COLOR_DEFAULT));
}

static gboolean
print_scripts(GSList* scripts, gboolean color, gboolean verbose, const char* rxstr, GError** err)
{
	GSList*     cur;
	const char* verb;

	if (!scripts) {
		g_print("No scripts available\n");
		return TRUE; /* not an error */
	}

	verb = verbose ? "" : " (use --verbose for details)";

	if (rxstr)
		g_print("Available scripts matching '%s'%s:\n", rxstr, verb);
	else
		g_print("Available scripts%s:\n", verb);

	for (cur = scripts; cur; cur = g_slist_next(cur)) {
		MuScriptInfo* msi;
		const char *  descr, *oneline, *name;

		msi     = (MuScriptInfo*)cur->data;
		name    = mu_script_info_name(msi);
		oneline = mu_script_info_one_line(msi);
		descr   = mu_script_info_description(msi);

		/* if rxstr is provide, only consider matching scriptinfos */
		if (rxstr && !mu_script_info_matches_regex(msi, rxstr, err)) {
			if (err && *err)
				return FALSE;
			continue;
		}

		print_script(name, oneline, descr, color, verbose);
	}

	return TRUE;
}

static char*
get_userpath(const char* muhome)
{
	if (muhome)
		return g_build_path(G_DIR_SEPARATOR_S, muhome, "scripts", NULL);
	else
		return g_build_path(G_DIR_SEPARATOR_S,
				    g_get_user_data_dir(),
				    "mu",
				    "scripts",
				    NULL);
}

static GSList*
get_script_info_list(const char* muhome, GError** err)
{
	GSList *scripts, *userscripts, *last;
	char*   userpath;

	scripts = mu_script_get_script_info_list(MU_SCRIPTS_DIR,
						 MU_GUILE_EXT,
						 MU_GUILE_DESCR_PREFIX,
						 err);

	if (err && *err)
		return NULL;

	userpath = get_userpath(muhome);

	/* is there are userdir for scripts? */
	if (!mu_util_check_dir(userpath, TRUE, FALSE)) {
		g_free(userpath);
		return scripts;
	}

	/* append it to the list we already have */
	userscripts =
	    mu_script_get_script_info_list(userpath, MU_GUILE_EXT, MU_GUILE_DESCR_PREFIX, err);
	g_free(userpath);

	/* some error, return nothing */
	if (err && *err) {
		mu_script_info_list_destroy(userscripts);
		mu_script_info_list_destroy(scripts);
		return NULL;
	}

	/* append the user scripts */
	last = g_slist_last(scripts);
	if (last) {
		last->next = userscripts;
		return scripts;
	} else
		return userscripts; /* apparently, scripts was NULL */
}

Mu::Result<void>
Mu::mu_cmd_script(const MuConfig* opts)
{
	GError         *err{};
	MuScriptInfo*	msi;
	GSList*		scripts;

	if (!mu_util_supports(MU_FEATURE_GUILE))
		return Err(Error::Code::InvalidArgument,
			   "<script> sub-command not available (requires guile)");

	scripts = get_script_info_list(opts->muhome, &err);
	if (err)
		goto leave;

	if (g_strcmp0(opts->cmdstr, "script") == 0) {
		print_scripts(scripts, !opts->nocolor, opts->verbose,
			      opts->script_params[0], &err);
		goto leave;
	}

	msi = mu_script_find_script_with_name(scripts, opts->script);
	if (!msi) {
		mu_util_g_set_error(&err, MU_ERROR_SCRIPT_NOT_FOUND,
				    "command or script not found");
		goto leave;
	}

	/* do it! */
	mu_script_guile_run(msi, mu_runtime_path(MU_RUNTIME_PATH_CACHE),
			    opts->script_params, &err);
leave:
	/* this won't be reached, unless there is some error */
	mu_script_info_list_destroy(scripts);

	if (err)
		return Err(Error::Code::InvalidArgument, &err,
			   "error running script");
	else
		return Ok();
}
