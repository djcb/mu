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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include "mu-msg.h"
#include "mu-maildir.h"
#include "mu-index.h"
#include "mu-msg-iter.h"

#include "mu-util.h"
#include "mu-output-link.h"


/* create a linksdir if it not exist yet; if it already existed,
 * remove old links if opts->clearlinks was specified */
gboolean
mu_output_link_create_dir (const char *linksdir, gboolean clearlinks)
{
		GError *err;
		
		g_return_val_if_fail (linksdir, FALSE);

		err = NULL;
		if (access (linksdir, F_OK) != 0) {
				if (!mu_maildir_mkdir (linksdir, 0700, TRUE, &err))
						goto fail;
		} else if (clearlinks)
				if (!mu_maildir_clear_links (linksdir, &err))
						goto fail;
		
		return TRUE;
		
fail:
		if (err) {
				g_warning ("%s", err->message ? err->message : "unknown error");
				g_error_free (err);
		}
		return FALSE;
		
}

gboolean
mu_output_link_row (MuMsgIter *iter, const char* linksdir)
{
	const char *path;
	GError *err;
	
	g_return_val_if_fail (iter, FALSE);
	g_return_val_if_fail (linksdir, FALSE);	
	g_return_val_if_fail (!mu_msg_iter_is_done (iter), FALSE);
	
	path = mu_msg_iter_get_field (iter, MU_MSG_FIELD_ID_PATH);
	if (!path)
			return FALSE;
	
	/* this might happen  if the database is not up-to-date, not an error */
	if (access (path, R_OK) != 0) {
			if (errno == ENOENT)
				g_warning ("cannot find source message %s: "
					   "the database is not up-to-date", path);
		else
			g_warning ("cannot read source message %s: %s", path,
				   strerror (errno));
		return FALSE;
	}

	err = NULL;
	if (!mu_maildir_link (path, linksdir, &err)) {
			if (err) {
					g_warning ("%s", err->message ? err->message : "unknown error");
					g_error_free (err);
			}
			return FALSE;
	}
		 
	return TRUE;
}

