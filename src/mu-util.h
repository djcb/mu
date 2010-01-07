/*
** Copyright (C) 2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include "config.h"

G_BEGIN_DECLS

/** 
 * get the expanded path; ie. perform shell expansion on the path
 *
 * @param path path to expand
 * 
 * @return the expanded path as a newly allocated string, or NULL in
 * case of error
 */
char*       mu_util_dir_expand (const char* path);



/** 
 * guess the maildir; first try MAILDIR, then try ~/Maildir
 * if both fail, return NULL
 * 
 * @return full path of the guessed Maildir, or NULL; must be freed (gfree)
 */
char*       mu_util_guess_maildir (void);



/** 
 * if path exists, check that's a read/writeable dir; otherwise try to
 * create it (with perms 0700)
 * 
 * @param path path to the dir 
 * 
 * @return TRUE if a read/writeable directory `path' exists after
 * leaving this function, FALSE otherwise
 */
gboolean mu_util_create_dir_maybe (const gchar *path);


/** 
 * check whether path is readable and/or writeable
 *  
 * @param path dir path
 * @param readable check for readability
 * @param writeable check fro writability
 * 
 * @return TRUE if dir exist and has the specified properties
 */
gboolean mu_util_check_dir (const gchar* path, gboolean readable,
			    gboolean writeable);



/** 
 * 
 * don't repeat these catch blocks everywhere...
 * 
 */
#define MU_XAPIAN_CATCH_BLOCK						\
	catch (const Xapian::Error &err) {				\
                g_warning ("%s: caught xapian exception '%s'",		\
			__FUNCTION__, err.get_msg().c_str());		\
        } catch (...) {							\
                g_warning ("%s: caught exception", __FUNCTION__);	\
        }


#define MU_XAPIAN_CATCH_BLOCK_RETURN(R)					\
	catch (const Xapian::Error &err) {				\
                g_warning ("%s: caught xapian exception '%s'",		\
			   __FUNCTION__, err.get_msg().c_str());	\
		return (R);						\
        } catch (...) {							\
                g_warning ("%s: caught exception", __FUNCTION__);	\
		return (R);						\
        }

/* the name of the (leaf) dir which has the xapian database */
#define MU_XAPIAN_DIR_NAME "xapian-" MU_XAPIAN_DB_VERSION

/** 
 * log something in the log file; note, we use G_LOG_LEVEL_INFO
 * for such messages
 */
#ifdef G_HAVE_GNUC_VARARGS
#define MU_WRITE_LOG(format...)					     \
	G_STMT_START {						     \
		g_log (G_LOG_DOMAIN,				     \
		       G_LOG_LEVEL_INFO,			     \
		       format);					     \
	} G_STMT_END
#else
#define MU_WRITE_LOG(x)
#endif /*G_HAVE_GNUC_VARARGS*/

G_END_DECLS

#endif /*__MU_UTIL_H__*/
