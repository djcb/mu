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

#ifndef __MU_OUTPUT_PLAIN_H__
#define __MU_OUTPUT_PLAIN_H__

/**
 * print a row (message) to standard output
 * 
 * @param iter iterator pointing to a message row
 * @param fields the fields to print (see MuMsgFields)
 * @param summary_len number of lines to include in message summary
 * 
 * @return TRUE if the printing succeeded, FALSE in case of error
 */
gboolean mu_output_plain_row (MuMsgIter *iter, const char *fields,
			      size_t summary_len);

#endif /*__MU_OUTPUT_PLAIN_H__*/
