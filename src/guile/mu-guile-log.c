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

#include "mu-guile-util.h"
#include "mu-guile-log.h"

enum _LogType {
	LOG_INFO,
	LOG_WARNING,
	LOG_CRITICAL
};
typedef enum _LogType LogType;


static SCM
write_log (LogType logtype, SCM FRM, SCM ARGS)
#define FUNC_NAME __FUNCTION__
{
	SCM str;

	SCM_ASSERT (scm_is_string(FRM), FRM, SCM_ARG1, "<write_log>");
	SCM_VALIDATE_REST_ARGUMENT(ARGS);

	str = scm_simple_format (SCM_BOOL_F, FRM, ARGS);

	if (scm_is_string (str)) {

		gchar *output;
		output = scm_to_utf8_string (str);

		switch (logtype) {
		case LOG_INFO:     g_message ("%s", output); break;
		case LOG_WARNING:  g_warning ("%s", output); break;
		case LOG_CRITICAL: g_critical ("%s", output); break;
		}
	}

	return SCM_UNSPECIFIED;

#undef FUNC_NAME
}


SCM_DEFINE_PUBLIC (log_info, "mu:log:info", 1, 0, 1,  (SCM FRM, SCM ARGS),
	    "log some message using a list of ARGS applied to FRM "
	    "(in 'simple-format' notation).\n")
#define FUNC_NAME s_info
{
	return write_log (LOG_INFO, FRM, ARGS);
}
#undef FUNC_NAME

SCM_DEFINE_PUBLIC (log_warning, "mu:log:warning", 1, 0, 1,  (SCM FRM, SCM ARGS),
	    "log some warning using a list of ARGS applied to FRM (in 'simple-format' "
	    "notation).\n")
#define FUNC_NAME s_warning
{
	return write_log (LOG_WARNING, FRM, ARGS);
}
#undef FUNC_NAME

SCM_DEFINE_PUBLIC (log_critical, "mu:log:critical", 1, 0, 1,  (SCM FRM, SCM ARGS),
	    "log some critical message using a list of ARGS applied to FRM "
	    "(in 'simple-format' notation).\n")
#define FUNC_NAME s_critical
{
	return write_log (LOG_CRITICAL, FRM, ARGS);
}
#undef FUNC_NAME


void*
mu_guile_log_init (void *data)
{
#include "mu-guile-log.x"

	return NULL;
}
