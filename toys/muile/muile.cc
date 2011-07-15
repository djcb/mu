/*
** Copyright (C) 2011 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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

#include <mu-runtime.h>

#include <libguile.h>
#include <libmuguile/mu-guile-msg.h>
#include <libmuguile/mu-guile-store.h>


int
main (int argc, char *argv[])
{
	mu_runtime_init ("/home/djcb/.mu");
		
	scm_with_guile (&mu_guile_msg_init, NULL);
	scm_with_guile (&mu_guile_store_init, NULL);
	
	scm_shell (argc, argv);

	mu_runtime_uninit ();
	
	return 0;
}
