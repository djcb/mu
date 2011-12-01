/* -*-mode: c++; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8-*- */

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /*HAVE_CONFIG_H*/

#include <glib.h>

#include "mu-cmd.h"
#include "mu-runtime.h"

static void
handle_error (GError *err)
{
	const char *advise;

	if (!err)
		return; /* nothing to do */

	advise = NULL;

	switch (err->code) {

	case MU_ERROR_XAPIAN_CANNOT_GET_WRITELOCK:
		advise = "maybe mu is already running?";
		break;

	case MU_ERROR_XAPIAN_CORRUPTION:
	case MU_ERROR_XAPIAN_NOT_UP_TO_DATE:
		advise = "please try 'mu index --rebuild'";
		break;
	case MU_ERROR_XAPIAN_IS_EMPTY:
		advise = "please try 'mu index'";
		break;
	default:
		break; /* nothing to do */
	}

	g_warning ("%s", err->message);
	if (advise)
		g_message ("%s", advise);
}


int
main (int argc, char *argv[])
{
	GError *err;
	MuError rv;

	if (!mu_runtime_init_from_cmdline (&argc, &argv, "mu"))
		return 1;

	err = NULL;
	rv = mu_cmd_execute (mu_runtime_config(), &err);

	handle_error (err);
	g_clear_error (&err);

	mu_runtime_uninit ();
	return rv;
}
