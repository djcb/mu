/*
** Copyright (C) 2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <sys/types.h> /* for mode_t */

#include "mu-msg-fields.h"

G_BEGIN_DECLS

/* struct with all configuration options for mu; it will be filled
 * from the config file, and/or command line arguments */

struct _MuConfigOptions {
	
	/* general options */
	gboolean	 quiet;         /* don't give any output */
	gboolean	 debug;         /* spew out debug info */
	char		*muhome;        /* the House of Mu */
	gboolean	 version;	/* request mu version */
	gboolean	 log_stderr;	/* log to stderr (not logfile) */
	gchar**	         params;	/* parameters (for querying) */
	
	/* options for indexing */
	char	        *maildir;	/* where the mails are */
	gboolean         nocleanup;	/* don't cleanup deleted mails from db */
	gboolean         reindex;	/* re-index existing mails */
	gboolean         rebuild;       /* empty the database before indexing */
	gboolean         autoupgrade;   /* automatically upgrade db
					 * when needed */
	/* options for querying */
	gboolean         xquery;        /* give the Xapian query instead of
					   search results */
	char		*fields;	/* fields to show in output */	
	char	        *sortfield;	/* field to sort by (string) */
	gboolean        descending;	/* sort descending? */
	unsigned        summary_len;    /* max # of lines of msg in summary */
	char            *bookmark;      /* use bookmark */
	/* output to a maildir with symlinks */
	char            *linksdir;      /* maildir to output symlinks */
	gboolean	clearlinks;     /* clear a linksdir before filling */
	mode_t		dirmode;	/* mode for the created maildir */

	/* options for extracting parts */
	gboolean        *save_all;           /* extract all parts */
	gboolean	*save_attachments;   /* extract all attachment parts */
	gchar           *parts;              /* comma-sep'd list of parts to save */
	char            *targetdir;          /* where to save the attachments */
	gboolean        overwrite;           /* should we overwrite same-named files */
};
typedef struct _MuConfigOptions MuConfigOptions;


/** 
 * set default values for the configuration options; when you call
 * mu_config_init, you should also call mu_config_uninit when the data
 * is no longer needed.
 * 
 * @param opts options 
 */
gboolean mu_config_init (MuConfigOptions *opts, int *argcp, char ***argvp);


/** 
 * free the MuOptionsCOnfig structure; the the muhome and maildir
 * members are heap-allocated, so must be freed.
 * 
 * @param opts 
 */
void  mu_config_uninit (MuConfigOptions *opts);


G_END_DECLS

#endif /*__MU_CONFIG_H__*/
