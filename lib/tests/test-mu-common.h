/*
** Copyright (C) 2008-2013 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#ifndef __TEST_MU_COMMON_H__
#define __TEST_MU_COMMON_H__

#include <glib.h>

G_BEGIN_DECLS

/**
 * get a dir name for a random temporary directory to do tests
 *
 * @return a random dir name, g_free when it's no longer needed
 */
char* test_mu_common_get_random_tmpdir (void);



/**
 * set the output to /dev/null
 *
 */
void black_hole (void);

/**
 * set the timezone
 *
 * @param tz timezone
 *
 * @return the old timezone
 */
const char* set_tz (const char* tz);


/**
 * switch the locale to en_US.utf8, return TRUE if it succeeds
 *
 * @return TRUE if the switch succeeds, FALSE otherwise
 */
gboolean  set_en_us_utf8_locale (void);

G_END_DECLS

#endif /*__TEST_MU_COMMON_H__*/
