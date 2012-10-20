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

#ifdef BUILD_GUILE
#include <libguile.h>
#endif /*BUILD_GUILE*/

#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <dirent.h>
#include <errno.h>

#include "mu-cmd.h"
#include "mu-util.h"
#include "mu-str.h"

struct _NamePath {
	char *name; /* name of the script (ie., basename with ext)*/
	char *path; /* full path of script */
};
typedef struct _NamePath NamePath;

static NamePath*
namepath_new (char *name, const char *path)
{
	NamePath *np;

	np = g_new0 (NamePath, 1);

	np->name = g_strdup (name);
	np->path = g_strdup (path);

	return np;
}

static void
namepath_destroy (NamePath *np)
{
	if (!np)
		return;

	g_free (np->name);
	g_free (np->path);
	g_free (np);
}

static int
namepath_cmp (NamePath *np1, NamePath *np2)
{
	return strcmp (np1->name, np2->name);
}

static void
script_namepaths_destroy (GSList *namepaths)
{
	g_slist_foreach (namepaths, (GFunc)namepath_destroy, NULL);
	g_slist_free    (namepaths);
}


static GSList*
get_script_namepaths (const char *path, GError **err)
{
	DIR *dir;
	GSList *lst;
	struct dirent *dentry;

	dir = opendir (path);
	if (!dir) {
		mu_util_g_set_error (err, MU_ERROR_FILE_CANNOT_OPEN,
				     "failed to open '%s': %s",
				     path, strerror(errno));
		return NULL;
	}

	/* create a list of names, paths */
	lst = NULL;
	while ((dentry = readdir (dir))) {
		const char* scmext = ".scm";
		char *name, *fullpath;

		/* only consider scm files */
		if (!g_str_has_suffix (dentry->d_name, scmext))
			continue;

		name = g_strndup (dentry->d_name,
				  strlen(dentry->d_name) - strlen(scmext));
		fullpath = g_strdup_printf ("%s%c%s", path, G_DIR_SEPARATOR,
					    dentry->d_name);

		lst = g_slist_prepend (lst, namepath_new (name, fullpath));

		g_free (name);
		g_free (fullpath);
	}

	closedir (dir);

	return g_slist_sort (lst, (GCompareFunc)namepath_cmp);
}


static char*
find_script_path (const char *script, GError **err)
{
	GSList *scripts, *cur;
	char *path;

	scripts = get_script_namepaths (MU_STATSDIR, err);
	if (err && *err)
		return NULL;

	for (cur = scripts, path = NULL; cur ; cur = g_slist_next (cur))
		if (g_strcmp0(((NamePath*)cur->data)->name, script) == 0) {
			path = g_strdup (((NamePath*)cur->data)->path);
			break;
		}

	script_namepaths_destroy (scripts);

	if (!path)
		mu_util_g_set_error (err, MU_ERROR_IN_PARAMETERS,
				     "statistic '%s' not found",
				     script);
	return path;
}

#ifdef BUILD_GUILE

static void
do_it (void *closure, int argc, char **argv)
{
	scm_shell (argc, argv);
}


static void
run_guile_script (MuConfig *opts, GError **err)
{
	char *expr, *query, *scriptpath;
	char *argv[] = {
		"guile", "-l", NULL, "-c", NULL, NULL
	};

	scriptpath = find_script_path (opts->stat, err);
	if (!scriptpath)
		return;
	else
		argv[2] = scriptpath;

	if (opts->params[1])
		query =	mu_str_quoted_from_strv
			((const gchar**)&opts->params[1]);
	else
		query = NULL;

	expr = g_strdup_printf (
		"(main '(\"dummy\" \"--muhome=%s\" %s %s))",
		opts->muhome,
		opts->textonly ? "\"--textonly\"" : "",
		query ? query : "");

	g_free (query);
	argv[4] = expr;
	scm_boot_guile (5, argv, do_it, 0);

	/* never reached but let's be correct(TM)*/
	g_free (expr);
	g_free (scriptpath);
}

#else
static void
run_guile_script (MuConfig *opts, GError **err)
{
	g_return_if_reached ();
}
#endif /*!BUILD_GUILE*/


static MuError
list_stats (GError **err)
{
	GSList *scripts;

	scripts = get_script_namepaths (MU_STATSDIR, err);
	if (err && *err)
		return MU_ERROR;

	if (!scripts)
		g_print ("No statistics available\n");
	else {
		GSList *cur;
		g_print ("Available statistics "
			 "(use with --stat=<stastistic):\n");
		for (cur = scripts; cur; cur = g_slist_next (cur))
			g_print ("\t%s\n", ((NamePath*)cur->data)->name);
	}

	script_namepaths_destroy (scripts);

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
	g_return_val_if_fail (opts, MU_ERROR_INTERNAL);
	g_return_val_if_fail (opts->cmd == MU_CONFIG_CMD_STATS,
			      MU_ERROR_INTERNAL);

	if (!check_params (opts, err))
		return MU_ERROR;

	if (!opts->stat)
		return list_stats (err);

	run_guile_script (opts, err);

	return MU_OK;
}
