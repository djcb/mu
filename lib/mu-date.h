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
 * do with it. Lenght is max. 128 bytes, inc. the ending \0.  if the
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

/**
 *
 * parse strings like 1h, 3w, 2m to mean '1 hour before now', '3 weeks
 * before now' and '2 * 30 days before now'
 *
 * the format is <n>(h|d|w|m|y), where <n> is an integer > 0, and
 * h=hour, d=day, w=week, m=30 days, year=365 days. function returns
 * *now* minus this value as time_t (UTC)
 *
 * if the number cannot be parsed, return (time_t)-1
 *
 * @param str a str
 *
 * @return the time_t of the point in time indicated by 'now' minus
 * the value, or (time_t)-1 otherwise
 */
time_t mu_date_parse_hdwmy (const char* str);



/**
 * complete a date (a string of the form YYYYMMDDHHMMSS with [0..14]
 * of the rightmost characters missing) to the the full YYYYMMDDHHMMSS.
 *
 * if is_begin is TRUE, add to the 'floor' (e.g,
 * 20110101=>20110101000000), otherwise go to the 'ceiling',
 * e.g. 2009=>20091231235050)
 *
 * @param date a date string (assumed to have the beginning of the
 * date, this is not checked
 * @param is_begin if TRUE go to floor (as described), otherwise to
 * the ceiling
 *
 * @return mu_date_complete: return a newly allocated string (free
 * with g_free) with the full, 14-char date; mu_date_complete_s:
 * return a statically allocated string. NOT REENTRANT.
 */
char*       mu_date_complete (const char *date, gboolean is_begin);
const char* mu_date_complete_s (const char *date, gboolean is_begin);



/**
 *
 *
 * @param datespec
 * @param is_begin
 *
 * @return
 */
const char* mu_date_interpret_s (const char *datespec, gboolean is_begin);
char* mu_date_interpret (const char *datespec, gboolean is_begin);



/**
 * convert a date of the form 'YYYYMMDDHHMMSS' into time_t
 *
 * @param date a date str of the form 'YYYYMMDDHHMMSS'
 * @param local if TRUE, source is assumed to bin in local time, UTC otherwise
 *
 * @return the corresponding time_t, or (time_t)-1 in case of error
 */
time_t mu_date_str_to_time_t (const char* date, gboolean local);




/**
 * convert a time_t value into a date string of the form
 * 'YYYYMMDDHHMMSS'; assume UTC
 *
 * @param t a time_t value
 * @param local if TRUE, convert to local time, otherwise use UTC
 *
 * @return mu_date_time_t_to_str_s: a static string (don't modify,
 * non-reentrant) of the form 'YYYYMMDDHHMMSS'; mu_date_time_t_to_str:
 * return a newly allocated string with the same.
 */
const char* mu_date_time_t_to_str_s (time_t t, gboolean local);
char* mu_date_time_t_to_str (time_t t, gboolean local);

/** @} */

G_END_DECLS

#endif /*__MU_DATE_H__*/
