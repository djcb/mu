/*
** Copyright (C) 2012-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <glib.h>

#ifndef __MU_DATE_H__
#define __MU_DATE_H__

G_BEGIN_DECLS


/**
 * @addtogroup MuDate
 * Date-related functions
 * @{
 */

/**
 * get a string for a given time_t
 *
 * mu_date_str_s returns a ptr to a static buffer,
 * while mu_date_str returns dynamically allocated
 * memory that must be freed after use.
 *
 * @param frm the format of the string (in strftime(3) format)
 * @param t the time as time_t
 *
 * @return a string representation of the time; see above for what to
 * do with it. Length is max. 128 bytes, inc. the ending \0.  if the
 * format is too long, the value will be truncated. in practice this
 * should not happen.
 */
const char* mu_date_str_s (const char* frm, time_t t) G_GNUC_CONST;
char*       mu_date_str   (const char* frm, time_t t) G_GNUC_WARN_UNUSED_RESULT;

/**
 * get a display string for a given time_t; if the given is less than
 * 24h from the current time, we display the time, otherwise the date,
 * using the preferred date/time for the current locale
 *
 * mu_str_display_date_s returns a ptr to a static buffer,
 *
 * @param t the time as time_t
 *
 * @return a string representation of the time/date
 */
const char* mu_date_display_s (time_t t);

G_END_DECLS

#endif /*__MU_DATE_H__*/
