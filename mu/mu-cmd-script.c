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

static MuError
list_stats (GError **err)
{
	GSList *scripts;
	scripts = mu_script_get_script_info_list (MU_STATSDIR,
						  MU_GUILE_EXT,
						  MU_GUILE_DESCR_PREFIX,
						  err);
	if (err && *err)
		return MU_ERROR;

	if (!scripts)
		g_print ("No statistics available\n");
	else {
		GSList *cur;
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

	mu_script_info_list_destroy (scripts);

	return MU_OK;

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

	if (!check_params (opts, err))
		return MU_ERROR;

	if (!opts->stat)
		return list_stats (err);

	scripts = mu_script_get_script_info_list (MU_STATSDIR,
						  MU_GUILE_EXT,
						  MU_GUILE_DESCR_PREFIX,
						  err);
	if (err && *err)
		return MU_ERROR;

	msi = mu_script_find_script_with_name (scripts, opts->stat);
	if (!msi) {
		mu_script_info_list_destroy (scripts);
		mu_util_g_set_error (err, MU_ERROR_IN_PARAMETERS,
				     "script not found");
		return MU_ERROR_IN_PARAMETERS;
	}

	if (opts->params[1])
		query = mu_str_quoted_from_strv
			((const gchar**)&opts->params[1]);
	else
		query = NULL;

	/* do it! */
	mu_script_guile_run (msi, opts->muhome, query ? query : "",
			     opts->textonly, err);

	/* this won't be reached, unless there is some error */
	mu_script_info_list_destroy (scripts);
	g_free (query);

	return MU_ERROR;
}
