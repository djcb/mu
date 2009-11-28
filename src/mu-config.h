/*
** Copyright (C) 2008, 2009 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef __MU_CONFIG_H__
#define __MU_CONFIG_H__

#include <glib.h>
#include "mu-msg-fields.h"

/* struct with all configuration options for mu; it will be filled
 * from the config file, and/or command line arguments */

struct _MuConfigOptions {
	
	/* general options */
	gboolean	 quiet; /* don't give any output */
	gboolean	 debug; /* spew out debug info */
	const char	*muhome;/* the House of Mu */
	gboolean	version; /* request mu version */
	gboolean	log_stderr; /*log to stderr (instead of logfile)*/
	gboolean	log_append; /* append to log (instead of overwriting)*/
	
	/* options for indexing */
	const char      *maildir;	/* where the mails are */
	gboolean         cleanup;	/* cleanup deleted mails form db */
	gboolean         reindex;	/* re-index existing mails */
	
	/* options for querying */
	gboolean         xquery;       /* give the Xapian query instead of
					      search results */
	const char      *fields;	/* fields to show in output */
	
	const char      *sortfield_str;	/* field to sort by (string) */

	/* FIXME: clean up this mess */
	gboolean        sortdir_descending ;	/* sort descending? */
	gboolean        sortdir_ascending;
	gboolean        ascending_flag, descending_flag;
};
typedef struct _MuConfigOptions MuConfigOptions;


/** 
 * set default values for the configuration options
 * 
 * @param opts 
 */
void mu_config_set_defaults (MuConfigOptions *opts);


/**
 * get the general options option group
 * 
 * @param opts the MuConfigOptions to fill from this option group
 * 
 * @return a new option group; *DON'T* unref when added to an optioncontext
 */
GOptionGroup* mu_config_options_group_mu (MuConfigOptions *opts);

/** 
 * get the index-options option group 
 * 
 * @param opts the MuConfigOptions to fill from this option group
 * 
 * @return a new option group; *DON'T* unref when added to an optioncontext
 */
GOptionGroup* mu_config_options_group_index (MuConfigOptions *opts);

/** 
 * get the query-options option group 
 * 
 * @param opts the MuConfigOptions to fill from this option group
 * 
 * @return a new option group; *DON'T* unref when added to an optioncontext
 */
GOptionGroup* mu_config_options_group_query (MuConfigOptions *opts);


#endif /*__MU_CONFIG_H__*/
