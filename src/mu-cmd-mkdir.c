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

#include "config.h"

#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>

#include "mu-maildir.h"
#include "mu-cmd.h"

#include "mu-util.h"

gboolean
mu_cmd_mkdir (MuConfigOptions *opts)
{
	int i;

	g_return_val_if_fail (opts, FALSE);
	g_return_val_if_fail (mu_cmd_equals (opts, "mkdir"), FALSE);
	
	if (!opts->params[1]) {
		g_warning (
			"usage: mu mkdir [-u,--mode=<mode>] "
			"<dir> [more dirs]");
		return FALSE;
	}
	
	i = 1;
	while (opts->params[i]) {
		if (!mu_maildir_mkmdir (opts->params[i], opts->dirmode,
					FALSE))
			return FALSE;
		++i;
	}

	return TRUE;
}
