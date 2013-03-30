/*
** Copyright (C) 2011-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef __MU_GUILE_MESSAGE_H__
#define __MU_GUILE_MESSAGE_H__

#include <glib.h>

G_BEGIN_DECLS

/**
 * Initialize this mu guile module.
 *
 * @param data
 *
 * @return
 */
void* mu_guile_message_init (void *data);


G_END_DECLS

#endif /*__MU_GUILE_MESSAGE_H__*/
