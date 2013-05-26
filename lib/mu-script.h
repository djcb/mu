/*
** Copyright (C) 2012-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef __MU_SCRIPT_H__
#define __MU_SCRIPT_H__

#include <glib.h>

G_BEGIN_DECLS

/* Opaque structure with information about a script */
struct _MuScriptInfo;
typedef struct _MuScriptInfo MuScriptInfo;

/**
 * get the name of the script (sans-extension, if some extension was
 * provided to mu_script_get_scripts)
 *
 * @param msi a MuScriptInfo structure
 *
 * @return the name
 */
const char* mu_script_info_name (MuScriptInfo *msi);


/**
 * get the full filesystem path of the script
 *
 * @param msi a MuScriptInfo structure
 *
 * @return the path
 */
const char* mu_script_info_path (MuScriptInfo *msi);

/**
 * get a one-line description for the script
 *
 * @param msi a MuScriptInfo structure
 *
 * @return the description, or NULL if there was none
 */
const char* mu_script_info_one_line (MuScriptInfo *msi);

/**
 * get a full description for the script
 *
 * @param msi a MuScriptInfo structure
 *
 * @return the description, or NULL if there was none
 */
const char* mu_script_info_description (MuScriptInfo *msi);

/**
 * check whether either the name or one-line description of a
 * MuScriptInfo matches regular expression rxstr
 *
 * @param msi a MuScriptInfo
 * @param rxstr a regular expression string
 * @param err receives error information
 *
 * @return TRUE if it matches, FALSE if not or in case of error
 */
gboolean mu_script_info_matches_regex (MuScriptInfo *msi, const char *rxstr,
				       GError **err);

/**
 * Get the list of all scripts in path with extension ext
 *
 * @param path a file system path
 * @param ext an extension (e.g., ".scm"), or NULL
 * @param prefix for the one-line description
 *        (e.g., ";; DESCRIPTION: "), or NULL
 * @param err receives error information, if any
 *
 * @return a list of Mu
 */
GSList *mu_script_get_script_info_list (const char *path, const char *ext,
					const char *descprefix, GError **err);

/**
 * destroy a list of MuScriptInfo* objects
 *
 * @param scriptslst a list of MuScriptInfo* objects
 */
void mu_script_info_list_destroy (GSList *lst);


/**
 * find the MuScriptInfo object for the first script with a certain
 * name, or return NULL if not found.
 *
 * @param lst a list of MuScriptInfo* objects
 * @param name the name to search for
 *
 * @return a MuScriptInfo* object, or NULL if not found.
 */
MuScriptInfo* mu_script_find_script_with_name (GSList *lst, const char *name);


/**
 * run the guile script at path
 *
 * @param msi MuScriptInfo object for the script
 * @param muhome path to the mu home dir
 * @param args NULL-terminated array of strings (argv for the script)
 * @param err receives error information
 *
 * @return FALSE in case of error -- otherwise, this function will
 * _not return_
 */
gboolean mu_script_guile_run (MuScriptInfo *msi, const char *muhome,
			      const char **args, GError **err);

G_END_DECLS

#endif /*__MU_SCRIPT_H__*/
