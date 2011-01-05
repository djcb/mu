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

#ifndef __MU_OUTPUT_H__
#define __MU_OUTPUT_H__

#include <glib.h>
#include <mu-msg-iter.h>

G_BEGIN_DECLS

/**
 * output the search results (MsgIter) as plain text rows to standard
 * output
 * 
 * @param iter iterator pointing to a message row
 * @param fields the fields to print (see MuMsgFields)
 * @param summary_len number of lines to include in message summary
 * @param count output param to receive the number of messages found, or NULL
 * 
 * @return TRUE if the printing succeeded, FALSE in case of error
 */
gboolean mu_output_plain (MuMsgIter *iter, const char *fields,
			  size_t summary_len, size_t *count);

/**
 * output the search results (MsgIter) as a maildir of symlinks
 * 
 * @param iter iterator pointing to a message row
 * @param path of the output maildir; if the directory does not exist yet, it will be created
 * @param clearlinks; remove any existing links in the target directory
 * @param count output param to receive the number of messages found, or NULL
 * 
 * @return TRUE if the linking succeeded, FALSE in case of error
 */
gboolean mu_output_links (MuMsgIter *iter, const char *linksdir,
			  gboolean clearlinks, size_t *count);

/**
 * output the search results (MsgIter) as XML to standard
 * output
 * 
 * @param iter iterator pointing to a message row
 * @param count output param to receive the number of messages found, or NULL
 * 
 * @return TRUE if the printing succeeded, FALSE in case of error
 */
gboolean mu_output_xml (MuMsgIter *iter, size_t *count);

/**
 * output the search results (MsgIter) as JSON to standard
 * output
 * 
 * @param iter iterator pointing to a message row
 * @param count output param to receive the number of messages found, or NULL
 * 
 * @return TRUE if the printing succeeded, FALSE in case of error
 */
gboolean mu_output_json (MuMsgIter *iter, size_t *count);

/**
 * output the search results (MsgIter) as s-expressions to standard
 * output
 * 
 * @param iter iterator pointing to a message row
 * @param count output param to receive the number of messages found, or NULL
 * 
 * @return TRUE if the printing succeeded, FALSE in case of error
 */
gboolean mu_output_sexp (MuMsgIter *iter, size_t *count);



G_END_DECLS

#endif /*__MU_OUTPUT_H__*/
