/*
** Copyright (C) 2008-2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef __MU_OUTPUT_LINK_H__
#define __MU_OUTPUT_LINK_H__

/**
 * create a target maildir to store the links if it does not exist yet
 * 
 * @param linksdir path to the toplevel Maildir to create
 * @param clearlinks if TRUE, clear any existing links in the target maildir
 * 
 * @return TRUE if succeeded, FALSE otherwise
 */
gboolean mu_output_link_create_dir (const char *linksdir, gboolean clearlinks);

/**
 *  create a symlink for for a message. the target dir should already
 *  exist, use mu_output_link_create_dir if needed.
 * 
 * @param iter iterator pointing to a message row
 * @param fields the fields to print (see MuMsgFields)
 * @param summary_len number of lines to include in message summary
 * 
 * @return TRUE if the printing succeeded, FALSE in case of error
 */
gboolean mu_output_link_row (MuMsgIter *iter, const char *linksdir);

#endif /*__MU_OUTPUT_LINK_H__*/
