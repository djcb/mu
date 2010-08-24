/*
** Copyright (C) 22010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef __MU_MSG_XAPIAN_PRIV_HH__
#define __MU_MSG_XAPIAN_PRIV_HH__

#include <xapian.h>


/** 
 * create a new MuMsgIterXapian -- basically, an iterator over the search
 * results
 * 
 * @param enq a Xapian::Enquiry providing access to search results 
 * @param batchsize how many results to retrieve at once
 * 
 * @return a new MuMsgIterXapian, or NULL in case of error
 */
MuMsgIterXapian *mu_msg_iter_xapian_new
   (const Xapian::Enquire& enq, size_t batchsize)G_GNUC_WARN_UNUSED_RESULT;

#endif /*__MU_MSG_XAPIAN_PRIV_HH__*/
