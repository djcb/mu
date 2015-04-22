/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

/*
** Copyright (C) 2008-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 3 of the License, or
** (at your option) any later version.
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

#ifndef __MU_UTIL_H__
#define __MU_UTIL_H__

#include <glib.h>
#include <stdio.h>
#include <dirent.h>
#include <sys/stat.h> /* for mode_t */

/* hopefully, this should get us a sane PATH_MAX */
#include <limits.h>
/* not all systems provide PATH_MAX in limits.h */
#ifndef PATH_MAX
#include <sys/param.h>
#ifndef PATH_MAX
#define PATH_MAX MAXPATHLEN
#endif /*!PATH_MAX*/
#endif /*PATH_MAX*/

G_BEGIN_DECLS

/**
 * get the expanded path; ie. perform shell expansion on the path. the
 * path does not have to exist
 *
 * @param path path to expand
 *
 * @return the expanded path as a newly allocated string, or NULL in
 * case of error
 */
char* mu_util_dir_expand (const char* path)
    G_GNUC_MALLOC G_GNUC_WARN_UNUSED_RESULT;

/**
 * guess the maildir; first try $MAILDIR; if it is unset or
 * non-existant, try ~/Maildir if both fail, return NULL
 *
 * @return full path of the guessed Maildir, or NULL; must be freed (gfree)
 */
char* mu_util_guess_maildir (void)
   G_GNUC_MALLOC G_GNUC_WARN_UNUSED_RESULT;


/**
 * guess the place of the mu homedir (typically, ~/.mu). Note, this
 * directory does not necessarily exist. mu_util_check_dir can be used
 * to check that
 *
 * @return the guessed mu homedir, which needs to be freed with g_free
 * when no longer needed.
 */
gchar* mu_util_guess_mu_homedir (void)
    G_GNUC_MALLOC G_GNUC_WARN_UNUSED_RESULT;

/**
 * if path exists, check that's a read/writeable dir; otherwise try to
 * create it (with perms 0700)
 *
 * @param path path to the dir
 * @param mode to set for the dir (as per chmod(1))
 * @param nowarn, if TRUE, don't write warnings (if any) to stderr
 *
 * @return TRUE if a read/writeable directory `path' exists after
 * leaving this function, FALSE otherwise
 */
gboolean mu_util_create_dir_maybe (const gchar *path, mode_t mode,
				   gboolean nowarn) G_GNUC_WARN_UNUSED_RESULT;

/**
 * check whether path is a directory, and optionally, if it's readable
 * and/or writeable
 *
 * @param path dir path
 * @param readable check for readability
 * @param writeable check for writability
 *
 * @return TRUE if dir exist and has the specified properties
 */
gboolean mu_util_check_dir (const gchar* path, gboolean readable,
			    gboolean writeable)
    G_GNUC_WARN_UNUSED_RESULT;


/**
 * get our the cache directory, typically, /tmp/mu-<userid>/
 *
 * @return the cache directory; don't free
 */
const char* mu_util_cache_dir (void) G_GNUC_CONST;



/**
 * create a writeable file and return its file descriptor (which
 * you'll need to close(2) when done with it.)
 *
 * @param path the full path of the file to create
 * @param the mode to open (ie. 0644 or 0600 etc., see chmod(3)
 * @param overwrite should we allow for overwriting existing files?
 *
 * @return a file descriptor, or -1 in case of error. If it's a file
 * system error, 'errno' may contain more info. use 'close()' when done
 * with the file descriptor
 */
int mu_util_create_writeable_fd (const char* path, mode_t mode,
				 gboolean overwrite)
    G_GNUC_WARN_UNUSED_RESULT;


/**
 * check if file is local, ie. on the local file system. this means
 * that it's either having a file URI, *or* that it's an existing file
 *
 * @param path a path
 *
 * @return TRUE if the file is local, FALSE otherwise
 */
gboolean mu_util_is_local_file (const char* path);


/**
 * is the current locale utf-8 compatible?
 *
 * @return TRUE if it's utf8 compatible, FALSE otherwise
 */
gboolean mu_util_locale_is_utf8 (void) G_GNUC_CONST;

/**
 * write a string (assumed to be in utf8-format) to a stream,
 * converted to the current locale
 *
 * @param str a string
 * @param stream a stream
 *
 * @return TRUE if printing worked, FALSE otherwise
 */
gboolean mu_util_fputs_encoded (const char *str, FILE *stream);

/**
 * print a formatted string (assumed to be in utf8-format) to stdout,
 * converted to the current locale
 *
 * @param a standard printf() format string, followed by a parameter list
 *
 * @return TRUE if printing worked, FALSE otherwise
 */
gboolean mu_util_print_encoded (const char *frm, ...) G_GNUC_PRINTF(1,2);

/**
 * print a formatted string (assumed to be in utf8-format) to stderr,
 * converted to the current locale
 *
 * @param a standard printf() format string, followed by a parameter list
 *
 * @return TRUE if printing worked, FALSE otherwise
 */
gboolean mu_util_printerr_encoded (const char *frm, ...) G_GNUC_PRINTF(1,2);


/**
 * read a password from stdin (without echoing), and return it.
 *
 * @param prompt the prompt text before the password
 *
 * @return the password (free with g_free), or NULL
 */
char* mu_util_read_password (const char *prompt)
	   G_GNUC_MALLOC G_GNUC_WARN_UNUSED_RESULT;

/**
 * Try to 'play' (ie., open with it's associated program) a file. On
 * MacOS, the the program 'open' is used for this; on other platforms
 * 'xdg-open' to do the actual opening. In addition you can set it to another program
 * by setting the MU_PLAY_PROGRAM environment variable
 *
 * @param path full path of the file to open
 * @param allow_local allow local files (ie. with file:// prefix or fs paths)
 * @param allow_remote allow URIs (ie., http, mailto)
 * @param err receives error information, if any
 *
 * @return TRUE if it succeeded, FALSE otherwise
 */
gboolean mu_util_play (const char *path, gboolean allow_local,
		       gboolean allow_remote, GError **err);

/**
 * Check whether program prog exists in PATH
 *
 * @param prog a program (executable)
 *
 * @return TRUE if it exists and is executable, FALSE otherwise
 */
gboolean mu_util_program_in_path (const char *prog);



enum _MuFeature {
	MU_FEATURE_GUILE     = 1 << 0,  /* do we support Guile 2.0? */
	MU_FEATURE_GNUPLOT   = 1 << 1,  /* do we have gnuplot installed? */
	MU_FEATURE_CRYPTO    = 1 << 2   /* do we support crypto (Gmime >= 2.6) */
};
typedef enum _MuFeature MuFeature;


/**
 * Check whether mu supports some particular feature
 *
 * @param feature a feature (multiple features can be logical-or'd together)
 *
 * @return TRUE if the feature is supported, FALSE otherwise
 */
gboolean mu_util_supports (MuFeature feature);



/**
 * Get an error-query for mu, to be used in `g_set_error'. Recent
 * version of Glib warn when using 0 for the error-domain in
 * g_set_error.
 *
 *
 * @return an error quark for mu
 */
GQuark mu_util_error_quark (void) G_GNUC_CONST;
#define MU_ERROR_DOMAIN (mu_util_error_quark())


/**
 * convert a string array in to a string, with the elements separated
 * by ' '
 *
 * @param params a non-NULL, NULL-terminated string array
 *
 * @return a newly allocated string
 */
gchar* mu_util_str_from_strv (const gchar **params)
	G_GNUC_MALLOC G_GNUC_WARN_UNUSED_RESULT;

/*
 * for OSs with out support for direntry->d_type, like Solaris
 */
#ifndef DT_UNKNOWN
enum {
	DT_UNKNOWN = 0,
# define DT_UNKNOWN     DT_UNKNOWN
	DT_FIFO = 1,
# define DT_FIFO        DT_FIFO
	DT_CHR = 2,
# define DT_CHR         DT_CHR
	DT_DIR = 4,
# define DT_DIR         DT_DIR
	DT_BLK = 6,
# define DT_BLK         DT_BLK
	DT_REG = 8,
# define DT_REG         DT_REG
	DT_LNK = 10,
# define DT_LNK         DT_LNK
	DT_SOCK = 12,
# define DT_SOCK        DT_SOCK
	DT_WHT = 14
# define DT_WHT         DT_WHT
};
#endif /*DT_UNKNOWN*/


/**
 * get the d_type (as in direntry->d_type) for the file at path, using
 * lstat(3)
 *
 * @param path full path
 *
 * @return DT_REG, DT_DIR, DT_LNK, or DT_UNKNOWN (other values are not
 * supported currently )
 */
unsigned char mu_util_get_dtype_with_lstat (const char *path);


/**
 * we need this when using Xapian::Document* from C
 *
 */
typedef gpointer XapianDocument;

/**
 * we need this when using Xapian::Enquire* from C
 *
 */
typedef gpointer XapianEnquire;


/* print a warning for a GError, and free it */
#define MU_HANDLE_G_ERROR(GE)							\
	do {									\
		if (!(GE))							\
			g_warning ("%s:%u: an error occured in %s",		\
				   __FILE__, __LINE__, __func__);		\
		else {								\
			g_warning ("error %u: %s", (GE)->code, (GE)->message);  \
			g_error_free ((GE));					\
		}								\
	} while (0)


/**
 *
 * don't repeat these catch blocks everywhere...
 *
 */

#define MU_STORE_CATCH_BLOCK_RETURN(GE,R)				\
        catch (const MuStoreError& merr) {				\
		mu_util_g_set_error ((GE),				\
			     merr.mu_error(), "%s",			\
			     merr.what().c_str());			\
		return (R);						\
	  }								\


#define MU_XAPIAN_CATCH_BLOCK						\
	 catch (const Xapian::Error &xerr) {				\
                g_critical ("%s: xapian error '%s'",			\
			__func__, xerr.get_msg().c_str());		\
        } catch (...) {							\
                g_critical ("%s: caught exception", __func__);	\
        }

#define MU_XAPIAN_CATCH_BLOCK_G_ERROR(GE,E)					\
	  catch (const Xapian::DatabaseLockError &xerr) {			\
		mu_util_g_set_error ((GE),					\
			     MU_ERROR_XAPIAN_CANNOT_GET_WRITELOCK,		\
			     "%s: xapian error '%s'",				\
			     __func__, xerr.get_msg().c_str());		\
	} catch (const Xapian::DatabaseCorruptError &xerr) {			\
		mu_util_g_set_error ((GE),					\
				MU_ERROR_XAPIAN_CORRUPTION,			\
			     "%s: xapian error '%s'",				\
			     __func__, xerr.get_msg().c_str());		\
	} catch (const Xapian::DatabaseError &xerr) {				\
		  mu_util_g_set_error ((GE),MU_ERROR_XAPIAN,			\
			     "%s: xapian error '%s'",				\
			     __func__, xerr.get_msg().c_str());		\
	} catch (const Xapian::Error &xerr) {					\
		mu_util_g_set_error ((GE),(E),					\
			     "%s: xapian error '%s'",				\
			     __func__, xerr.get_msg().c_str());		\
        } catch (...) {								\
			mu_util_g_set_error ((GE),(MU_ERROR_INTERNAL),		\
			     "%s: caught exception", __func__);		\
	 }


#define MU_XAPIAN_CATCH_BLOCK_RETURN(R)					\
	  catch (const Xapian::Error &xerr) {				\
                g_critical ("%s: xapian error '%s'",			\
			   __func__, xerr.get_msg().c_str());	\
		return (R);						\
        } catch (...) {							\
                g_critical ("%s: caught exception", __func__);	\
		return (R);						\
        }

#define MU_XAPIAN_CATCH_BLOCK_G_ERROR_RETURN(GE,E,R)			\
	  catch (const Xapian::Error &xerr) {				\
		mu_util_g_set_error ((GE),(E),				\
			     "%s: xapian error '%s'",			\
			   __func__, xerr.get_msg().c_str());	\
		return (R);						\
        } catch (...) {							\
		if ((GE)&&!(*(GE)))					\
			mu_util_g_set_error ((GE),			\
				     (MU_ERROR_INTERNAL),		\
			     "%s: caught exception", __func__);	\
		return (R);						\
        }

/* the name of the (leaf) dir which has the xapian database */
#define MU_XAPIAN_DIR_NAME    "xapian"

/* name of the bookmark file */
#define MU_BOOKMARK_FILENAME "bookmarks"

/* metadata key for the xapian 'schema' version */
#define MU_STORE_VERSION_KEY "db_version"


/**
 * log something in the log file; note, we use G_LOG_LEVEL_INFO
 * for such messages
 */
#define MU_WRITE_LOG(...)					     \
	G_STMT_START {						     \
		g_log (G_LOG_DOMAIN,				     \
		       G_LOG_LEVEL_INFO,			     \
		       __VA_ARGS__);				     \
	} G_STMT_END



#define MU_G_ERROR_CODE(GE) ((GE)&&(*(GE))?(*(GE))->code:MU_ERROR)


enum _MuError {
	/* no error at all! */
        MU_OK                                 = 0,

	/* generic error */
	MU_ERROR                              = 1,
	MU_ERROR_IN_PARAMETERS                = 2,
	MU_ERROR_INTERNAL                     = 3,
	MU_ERROR_NO_MATCHES                   = 4,

	MU_ERROR_SCRIPT_NOT_FOUND             = 8,

	/* general xapian related error */
	MU_ERROR_XAPIAN                       = 11,

	/* (parsing) error in the query */
	MU_ERROR_XAPIAN_QUERY                 = 13,
	/* xapian dir is not accessible */
	MU_ERROR_XAPIAN_DIR_NOT_ACCESSIBLE    = 14,
	/* database version is not up-to-date */
	MU_ERROR_XAPIAN_VERSION_MISMATCH      = 15,
	/* missing data for a document */
	MU_ERROR_XAPIAN_MISSING_DATA          = 16,
	/* database corruption */
	MU_ERROR_XAPIAN_CORRUPTION            = 17,
	/* can't get write lock */
	MU_ERROR_XAPIAN_CANNOT_GET_WRITELOCK  = 18,
	/* database is empty */
	MU_ERROR_XAPIAN_IS_EMPTY              = 19,
	/* could not write */
	MU_ERROR_XAPIAN_STORE_FAILED	      = 20,
	/* could not remove */
	MU_ERROR_XAPIAN_REMOVE_FAILED	      = 21,
	/* database was modified; reload */
	MU_ERROR_XAPIAN_MODIFIED              = 22,

	/* GMime related errors */

	/* gmime parsing related error */
	MU_ERROR_GMIME                        = 30,

	/* contacts related errors */
	MU_ERROR_CONTACTS                     = 50,
	MU_ERROR_CONTACTS_CANNOT_RETRIEVE     = 51,

	/* crypto related errors */
	MU_ERROR_CRYPTO		              = 60,


	/* File errors */
	/* generic file-related error */
	MU_ERROR_FILE                         = 70,
	MU_ERROR_FILE_INVALID_NAME            = 71,
	MU_ERROR_FILE_CANNOT_LINK             = 72,
	MU_ERROR_FILE_CANNOT_OPEN             = 73,
	MU_ERROR_FILE_CANNOT_READ             = 74,
	MU_ERROR_FILE_CANNOT_EXECUTE          = 75,
	MU_ERROR_FILE_CANNOT_CREATE           = 76,
	MU_ERROR_FILE_CANNOT_MKDIR            = 77,
	MU_ERROR_FILE_STAT_FAILED             = 78,
	MU_ERROR_FILE_READDIR_FAILED          = 79,
	MU_ERROR_FILE_INVALID_SOURCE          = 80,
	MU_ERROR_FILE_TARGET_EQUALS_SOURCE    = 81,
	MU_ERROR_FILE_CANNOT_WRITE            = 82,
	MU_ERROR_FILE_CANNOT_UNLINK           = 83,

	/* not really an error, used in callbacks */
	MU_STOP                               = 99
};
typedef enum _MuError MuError;


/**
 * set an error if it's not already set, and return FALSE
 *
 * @param err errptr, or NULL
 * @param errcode error code
 * @param frm printf-style format, followed by paremeters
 *
 * @return FALSE
 */
gboolean mu_util_g_set_error (GError **err, MuError errcode, const char *frm, ...)
	G_GNUC_PRINTF(3,4);


/**
 * calculate a 64-bit hash for the given string, based on a
 * combination of the DJB and BKDR hash functions
 *
 * @param a string
 *
 * @return the hash as a static string, which stays valid until this
 * function is called again.
 */
const char* mu_util_get_hash (const char* str);




#define MU_COLOR_RED		"\x1b[31m"
#define MU_COLOR_GREEN		"\x1b[32m"
#define MU_COLOR_YELLOW		"\x1b[33m"
#define MU_COLOR_BLUE		"\x1b[34m"
#define MU_COLOR_MAGENTA	"\x1b[35m"
#define MU_COLOR_CYAN		"\x1b[36m"
#define MU_COLOR_DEFAULT	"\x1b[0m"

G_END_DECLS

#endif /*__MU_UTIL_H__*/
