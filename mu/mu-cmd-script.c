/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

/*
** Copyright (C) 2012 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include "mu-cmd.h"
#include "mu-util.h"
#include "mu-str.h"
#include "mu-script.h"

#define MU_GUILE_EXT          ".scm"
#define MU_GUILE_DESCR_PREFIX ";; DESCRIPTION: "

static void
print_stats (GSList *scripts)
{
	GSList *cur;

	if (!scripts) {
		g_print ("No statistics available\n");
		return;
	}

	g_print ("Available statistics "
		 "(use with --stat=<stastistic>):\n");
	for (cur = scripts; cur; cur = g_slist_next (cur)) {

		MuScriptInfo *msi;
		const char* descr;

		msi   = (MuScriptInfo*)cur->data;
		descr = mu_script_info_description (msi);

		g_print ("\t%s%s%s\n",
			 mu_script_info_name (msi),
			 descr ? ": " : "",
			 descr ? descr : "");
	}
}


static GSList*
get_script_info_list (const char *muhome, GError **err)
{
	GSList *scripts, *userscripts, *last;
	char *userpath;

	scripts = mu_script_get_script_info_list (MU_STATSDIR, MU_GUILE_EXT,
						  MU_GUILE_DESCR_PREFIX,
						  err);
	if (err && *err)
		return NULL;

	userpath = g_strdup_printf ("%s%c%s",
				    muhome, G_DIR_SEPARATOR,
				    "scripts" G_DIR_SEPARATOR_S "stats");

	/* is there are userdir for scripts? */
	if (!mu_util_check_dir (userpath, TRUE, FALSE))
		return scripts;

	/* append it to the list we already have */
	userscripts = mu_script_get_script_info_list (userpath, MU_GUILE_EXT,
						      MU_GUILE_DESCR_PREFIX,
						      err);
	/* some error, return nothing */
	if (err && *err) {
		mu_script_info_list_destroy (scripts);
		return NULL;
	}

	/* append the user scripts */
	last = g_slist_last (scripts);
	if (last) {
		last->next = userscripts;
		return scripts;
	} else
		return userscripts; /* apparently, scripts was NULL */
}



static gboolean
check_params (MuConfig *opts, GError **err)
{
	if (!mu_util_supports (MU_FEATURE_GUILE | MU_FEATURE_GNUPLOT)) {
		mu_util_g_set_error (err, MU_ERROR_IN_PARAMETERS,
				     "the 'stats' command is not supported");
		return FALSE;
	}

	return TRUE;
}


MuError
mu_cmd_stats (MuConfig *opts, GError **err)
{
	MuScriptInfo *msi;
	GSList *scripts;
	gchar *query;

	g_return_val_if_fail (opts, MU_ERROR_INTERNAL);
	g_return_val_if_fail (opts->cmd == MU_CONFIG_CMD_STATS,
			      MU_ERROR_INTERNAL);

	query = NULL;

	if (!check_params (opts, err))
		return MU_ERROR;

	scripts = get_script_info_list (opts->muhome, err);
	if (err && *err)
		goto leave;

	if (!opts->stat) {
		print_stats (scripts);
		goto leave;
	}

	msi = mu_script_find_script_with_name (scripts, opts->stat);
	if (!msi) {
		mu_util_g_set_error (err, MU_ERROR_IN_PARAMETERS,
				     "script not found");
		goto leave;
	}

	query =  (opts->params[1]) ?
		mu_str_quoted_from_strv ((const gchar**)&opts->params[1]) :
		NULL;

	/* do it! */
	mu_script_guile_run (msi, opts->muhome, query ? query : "",
			     opts->textonly, err);
leave:
	/* this won't be reached, unless there is some error */
	mu_script_info_list_destroy (scripts);
	g_free (query);

	return (err && *err) ? MU_ERROR : MU_OK;
}
