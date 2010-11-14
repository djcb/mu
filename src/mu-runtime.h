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
#ifndef __MU_RUNTIME_H__
#define __MU_RUNTIME_H__

#include <glib.h>
#include "mu-config.h"

G_BEGIN_DECLS

gboolean mu_runtime_init              (const char* muhome);
gboolean mu_runtime_init_from_cmdline (int *pargc, char ***pargv);
void mu_runtime_uninit (void);

const char* mu_runtime_mu_home_dir     (void);
const char* mu_runtime_xapian_dir      (void);
const char* mu_runtime_bookmarks_file  (void);
MuConfigOptions* mu_runtime_config_options (void);

G_END_DECLS

#endif /*__MU_RUNTIME_H__*/
