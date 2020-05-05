/*
** Copyright (C) 2008-2020 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <mu-msg.h>
#include <utils/mu-util.h>

G_BEGIN_DECLS

/* env var; if non-empty, color are disabled */
#define MU_NOCOLOR "MU_NOCOLOR"


typedef enum {
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
	MU_CONFIG_FORMAT_DEBUG,

	/* for find, view */
	MU_CONFIG_FORMAT_SEXP,		/* output sexps (emacs) */
	MU_CONFIG_FORMAT_JSON,		/* output JSON */

	/* for find */
	MU_CONFIG_FORMAT_LINKS,		/* output as symlinks */
	MU_CONFIG_FORMAT_XML,		/* output xml */
	MU_CONFIG_FORMAT_XQUERY,	/* output the xapian query */
	MU_CONFIG_FORMAT_MQUERY,	/* output the mux query */

	MU_CONFIG_FORMAT_EXEC		/* execute some command */
} MuConfigFormat;

typedef enum {
	MU_CONFIG_CMD_UNKNOWN = 0,

	MU_CONFIG_CMD_ADD,
	MU_CONFIG_CMD_CFIND,
	MU_CONFIG_CMD_EXTRACT,
	MU_CONFIG_CMD_FIND,
	MU_CONFIG_CMD_HELP,
	MU_CONFIG_CMD_INDEX,
        MU_CONFIG_CMD_INFO,
        MU_CONFIG_CMD_INIT,
	MU_CONFIG_CMD_MKDIR,
	MU_CONFIG_CMD_REMOVE,
	MU_CONFIG_CMD_SCRIPT,
	MU_CONFIG_CMD_SERVER,
	MU_CONFIG_CMD_TICKLE,
	MU_CONFIG_CMD_VERIFY,
	MU_CONFIG_CMD_VIEW,

	MU_CONFIG_CMD_NONE
} MuConfigCmd;


#define mu_config_cmd_is_valid(C)					\
	((C) > MU_CONFIG_CMD_UNKNOWN && (C) < MU_CONFIG_CMD_NONE)


/* struct with all configuration options for mu; it will be filled
 * from the config file, and/or command line arguments */

struct _MuConfig {

	MuConfigCmd	 cmd;           /* the command, or
					 * MU_CONFIG_CMD_NONE */
	char		*cmdstr;        /* cmd string, for user
					 * info */
	/* general options */
	gboolean	 quiet;	/* don't give any output */
	gboolean	 debug;	/* spew out debug info */
	gchar		*muhome;	/* the House of Mu */
	gboolean	 version;	/* request mu version */
	gboolean	 log_stderr;	/* log to stderr (not logfile) */
	gchar**		 params;	/* parameters (for querying) */
	gboolean	 nocolor;        /* don't use use ansi-colors
					 * in some output */
	gboolean	 verbose;	/* verbose output */

        /* options for init */
        gchar	        *maildir;	/* where the mails are */
        char**          my_addresses;   /* 'my e-mail address', for mu
                                         * cfind; can be use multiple
                                         * times */
	/* options for indexing */

	gboolean        nocleanup;	/* don't cleanup del'd mails from db */
	gboolean        rebuild;	/* empty the database before indexing */
	gboolean        lazycheck;      /* don't check dirs with up-to-date
					 * timestamps */
	int		max_msg_size;   /* maximum size for message files */

	/* options for querying 'find' (and view-> 'summary') */
	gchar		*fields;	/* fields to show in output */
	gchar	        *sortfield;	/* field to sort by (string) */
	int		 maxnum;	/* max # of entries to print */
	gboolean	 reverse;	/* sort in revers order (z->a) */
	gboolean	 threads;       /* show message threads */

	gboolean	 summary;	/* OBSOLETE: use summary_len */
	int	         summary_len;   /* max # of lines for summary */

	gchar           *bookmark;	/* use bookmark */
	gchar		*formatstr;     /* output type for find
					 * (plain,links,xml,json,sexp)
					 * and view (plain, sexp) and cfind
					 */
	MuConfigFormat   format;        /* the decoded formatstr */
	gchar		*exec;		/* command to execute on the
					 * files for the matched
					 * messages */
	gboolean        skip_dups;        /* if there are multiple
					 * messages with the same
					 * msgid, show only the first
					 * one */
	gboolean        include_related; /* included related messages
					  * in results */
	/* for find and cind */
	time_t            after;          /* only show messages or
					   * addresses last seen after
					   * T */
	/* options for crypto
	 * ie, 'view', 'extract' */
	gboolean	 auto_retrieve;	  /* assume we're online */
	gboolean	 use_agent;	  /* attempt to use the gpg-agent */
	gboolean	 decrypt;         /* try to decrypt the
					   * message body, if any */
	gboolean	 verify;          /* try to crypto-verify the
					   * message */

	/* options for view */
	gboolean         terminator;      /* add separator \f between
					   * multiple messages in mu
					   * view */

	/* options for cfind (and 'find' --> "after") */
	gboolean          personal;       /* only show 'personal' addresses */
	/* also 'after' --> see above */

	/* output to a maildir with symlinks */
	gchar            *linksdir;	/* maildir to output symlinks */
	gboolean	 clearlinks;	/* clear a linksdir before filling */
	mode_t		 dirmode;	/* mode for the created maildir */

	/* options for extracting parts */
	gboolean	save_all;	/* extract all parts */
	gboolean	save_attachments; /* extract all attachment parts */
	gchar		*parts;		/* comma-sep'd list of parts
					 * to save /  open */
	gchar		*targetdir;	/* where to save the attachments */
	gboolean	 overwrite;	/* should we overwrite same-named files */
	gboolean         play;          /* after saving, try to 'play'
					 * (open) the attmnt using xdgopen */
	/* for server */
	gboolean        commands;        /* dump documentations for server
					  * commands */
        gchar          *eval;            /* command to evaluate */

        /* options for mu-script */
	gchar           *script;         /* script to run */
	const char      **script_params; /* parameters for scripts */
};
typedef struct _MuConfig MuConfig;

/**
 * initialize a mu config object
 *
 * set default values for the configuration options; when you call
 * mu_config_init, you should also call mu_config_uninit when the data
 * is no longer needed.
 *
 * Note that this is _static_ data, ie., mu_config_init will always
 * return the same pointer
 *
 * @param argcp: pointer to argc
 * @param argvp: pointer to argv
 * @param err: receives error information
 */
MuConfig *mu_config_init (int *argcp, char ***argvp, GError **err)
      G_GNUC_WARN_UNUSED_RESULT;
/**
 * free the MuConfig structure
 *
 * @param opts a MuConfig struct, or NULL
 */
void mu_config_uninit (MuConfig *conf);


/**
 * execute the command / options in this config
 *
 * @param opts a MuConfig struct
 *
 * @return a value denoting the success/failure of the execution;
 * MU_ERROR_NONE (0) for success, non-zero for a failure. This is to used for
 * the exit code of the process
 *
 */
MuError mu_config_execute (MuConfig *conf);

/**
 * count the number of non-option parameters
 *
 * @param opts a MuConfig struct
 *
 * @return the number of non-option parameters, or 0 in case of error
 */
size_t mu_config_param_num (MuConfig *conf);


/**
 * determine MuMsgOptions for command line args
 *
 * @param opts a MuConfig struct
 *
 * @return the corresponding MuMsgOptions
 */
MuMsgOptions mu_config_get_msg_options (MuConfig *opts);


/**
 * print help text for the current command
 *
 * @param cmd the command to show help for
 */
void mu_config_show_help (MuConfigCmd cmd);

G_END_DECLS

#endif /*__MU_CONFIG_H__*/
