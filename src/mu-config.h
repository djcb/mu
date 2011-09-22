/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

/*
** Copyright (C) 2008-2011 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <mu-msg-fields.h>
#include <mu-util.h>

G_BEGIN_DECLS

/* env var; if non-empty, color are enabled for some commands */
#define MU_COLORS "MU_COLORS"


enum _MuConfigFormat {
	MU_CONFIG_FORMAT_UNKNOWN = 0,

	/* for cfind, find, view */
	MU_CONFIG_FORMAT_PLAIN,		/* plain output */

	/* for cfind */
	MU_CONFIG_FORMAT_MUTT_ALIAS,	/* mutt alias style */
	MU_CONFIG_FORMAT_MUTT_AB,	/* mutt ext abook */
	MU_CONFIG_FORMAT_WL,		/* Wanderlust abook */
	MU_CONFIG_FORMAT_CSV,		/* comma-sep'd values */
	MU_CONFIG_FORMAT_ORG_CONTACT,	/* org-contact */
	MU_CONFIG_FORMAT_BBDB,		/* BBDB */

	/* for find, view */
	MU_CONFIG_FORMAT_SEXP,		/* output sexps (emacs) */

	/* for find */
	MU_CONFIG_FORMAT_LINKS,		/* output as symlinks */
	MU_CONFIG_FORMAT_XML,		/* output xml */
	MU_CONFIG_FORMAT_XQUERY		/* output the xapian query */
};
typedef enum _MuConfigFormat MuConfigFormat;


enum _MuConfigCmd {
	MU_CONFIG_CMD_UNKNOWN = 0,

	MU_CONFIG_CMD_INDEX,
	MU_CONFIG_CMD_FIND,
	MU_CONFIG_CMD_MKDIR,
	MU_CONFIG_CMD_VIEW,
	MU_CONFIG_CMD_EXTRACT,
	MU_CONFIG_CMD_CFIND,
	MU_CONFIG_CMD_MV,
	MU_CONFIG_CMD_ADD,
	MU_CONFIG_CMD_REMOVE,
	MU_CONFIG_CMD_SERVER,

	MU_CONFIG_CMD_NONE
};
typedef enum _MuConfigCmd MuConfigCmd;


/* struct with all configuration options for mu; it will be filled
 * from the config file, and/or command line arguments */

struct _MuConfig {

	MuConfigCmd	cmd;           /* the command, or
					 * MU_CONFIG_CMD_NONE */
	const char	*cmdstr;       /* cmd string, for user
					* info */

	/* general options */
	gboolean	quiet;		/* don't give any output */
	gboolean	debug;		/* spew out debug info */
	char		*muhome;	/* the House of Mu */
	gboolean	version;	/* request mu version */
	gboolean	log_stderr;	/* log to stderr (not logfile) */
	gchar**	        params;		/* parameters (for querying) */
	gboolean        color;          /* use ansi-colors in some output */

	/* options for indexing */
	char	        *maildir;	/* where the mails are */
	gboolean        nocleanup;	/* don't cleanup del'd mails from db */
	gboolean        reindex;	/* re-index existing mails */
	gboolean        rebuild;	/* empty the database before indexing */
	gboolean        autoupgrade;    /* automatically upgrade db
					 * when needed */
	int             xbatchsize;     /* batchsize for xapian
					 * commits, or 0 for
					 * default */
	int		max_msg_size;   /* maximum size for message files */

	/* options for querying 'find' (and view-> 'summary') */
	char		*fields;	/* fields to show in output */
	char	        *sortfield;	/* field to sort by (string) */
	gboolean	 reverse;	/* sort in revers order (z->a) */
	gboolean	 threads;       /* show message threads */
	gboolean	 summary;	/* include a summary? */
	char            *bookmark;	/* use bookmark */
	char		*formatstr;     /* output type for find
					 * (plain,links,xml,json,sexp)
					 * and view (plain, sexp) and cfind
					 */
	MuConfigFormat   format;        /* the decoded formatstr */
	char		*exec;		/* command to execute on the
					 * files for the matched
					 * messages */
	gboolean        include_unreadable; /* don't ignore messages
					     * without a disk file */
	/* options for mv */
	char            *flagstr;        /* message flags to set for
					  * the target */
	gboolean	print_target;    /* should be print the
					  * target file path on
					  * stdout */
	gboolean        ignore_dups;      /* silently ignore the
					   * src=target case */
	/* options for view */
	gboolean         terminator;      /* add separator \f between
					   * multiple messages in mu
					   * view */

	/* output to a maildir with symlinks */
	char            *linksdir;	/* maildir to output symlinks */
	gboolean	 clearlinks;	/* clear a linksdir before filling */
	mode_t		 dirmode;	/* mode for the created maildir */

	/* options for extracting parts */
	gboolean	*save_all;	/* extract all parts */
	gboolean	*save_attachments; /* extract all attachment parts */
	gchar		*parts;		/* comma-sep'd list of parts
					 * to save /  open */
	char		*targetdir;	/* where to save the attachments */
	gboolean	 overwrite;	/* should we overwrite same-named files */
	gboolean         play;          /* after saving, try to 'play'
					 * (open) the attmnt using xdgopen */
};
typedef struct _MuConfig MuConfig;

/**
 * create a new mu config object
 *
 * set default values for the configuration options; when you call
 * mu_config_init, you should also call mu_config_uninit when the data
 * is no longer needed.
 *
 * @param opts options
 */
MuConfig *mu_config_new (int *argcp, char ***argvp)
      G_GNUC_WARN_UNUSED_RESULT;
/**
 * free the MuOptionsConfig structure; the the muhome and maildir
 * members are heap-allocated, so must be freed.
 *
 * @param opts a MuConfig struct, or NULL
 */
void mu_config_destroy (MuConfig *opts);

/**
 * execute the command / options in this config
 *
 * @param opts the commands/options
 *
 * @return a value denoting the success/failure of the execution;
 * MU_ERROR_NONE (0) for success, non-zero for a failure. This is to used for
 * the exit code of the process
 */
MuError mu_config_execute (MuConfig *opts);


/**
 * count the number of non-option parameters
 *
 * @param conf a MuConfig instance
 *
 * @return the number of non-option parameters, or 0 in case of error
 */
guint mu_config_param_num (MuConfig *conf);



G_END_DECLS

#endif /*__MU_CONFIG_H__*/
