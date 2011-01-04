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

#if HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>

#include "mu-maildir.h"
#include "mu-cmd.h"
#include "mu-util.h"

MuExitCode
mu_cmd_mkdir (MuConfig *opts)
{
	int i;

	g_return_val_if_fail (opts, MU_EXITCODE_ERROR);
	g_return_val_if_fail (opts->cmd == MU_CONFIG_CMD_MKDIR,
			      MU_EXITCODE_ERROR);
	
	if (!opts->params[1]) {
		g_printerr ("usage: mu mkdir [-u,--mode=<mode>] "
			    "<dir> [more dirs]");
		return MU_EXITCODE_ERROR;
	}
	
	i = 1;
	while (opts->params[i]) {
		GError *err;
		err = NULL;
		if (!mu_maildir_mkdir (opts->params[i], opts->dirmode,
				       FALSE, &err))
			if (err && err->message) {
				g_printerr ("mu: %s", err->message);
				g_error_free (err);
			}
		return 1;
		++i;
	}

	return MU_EXITCODE_OK;
}
