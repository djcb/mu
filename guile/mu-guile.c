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

#if HAVE_CONFIG_H
#include <config.h>
#endif /*HAVE_CONFIG_H*/

#include <mu-runtime.h>
#include "mu-guile-util.h"


static gboolean initialized = FALSE;

SCM_DEFINE_PUBLIC (init_mu, "mu:init", 0, 1, 0,
		   (SCM MUHOME),
		   "Initialize mu - needed before you call any of the other "
		   "functions. Optionally, you can provide MUHOME which "
		   "should be an absolute path to your mu home directory "
		   "(typically, the default, ~/.mu, should be just fine)")
#define FUNC_NAME s_init_mu
{
	const char *muhome;
	static gboolean initialized = FALSE;

	SCM_ASSERT (scm_is_string (MUHOME) || SCM_UNBNDP(MUHOME),
		    MUHOME, SCM_ARG1, FUNC_NAME);

	if (initialized)
		return mu_guile_util_error (FUNC_NAME, 0, "Already initialized",
					    SCM_UNSPECIFIED);

	muhome = SCM_UNBNDP(MUHOME) ? NULL : scm_to_utf8_string (MUHOME);

	if (!mu_runtime_init (muhome, "mu-guile"))
		return mu_guile_util_error (FUNC_NAME, 0, "Failed to initialize mu",
					    SCM_UNSPECIFIED);
	initialized = TRUE;

	/* cleanup when we're exiting */
	g_atexit (mu_runtime_uninit);

	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE_PUBLIC (init_p, "mu:init?", 0, 0, 0,
		   (void), "Whether mu is initialized or not.\n")
#define FUNC_NAME s_init_p
{
	return initialized ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME


/* C function so we can cheaply check from other C-based code */
gboolean
mu_guile_initialized (void)
{
	return initialized;
}


void*
mu_guile_init (void *data)
{
#include "mu-guile.x"

	return NULL;
}
