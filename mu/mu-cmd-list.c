/* -*-mode: c; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-*/

/*
** Copyright (C) 2008-2012 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
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
#include <signal.h>

#include "mu-msg.h"
#include "mu-str.h"
#include "mu-date.h"
#include "mu-maildir.h"
#include "mu-runtime.h"

#include "mu-util.h"
#include "mu-cmd.h"
#include "mu-msg-fields.h"

/* Reverse-mapping of Xapian prefix character (as returned from "mu_msg_field_xapian_prefix()"
   back to "enum enum _MuMsgFieldId" .
   The index to this table is the prefix character (thus, 256 entries). */
typedef struct
{
	gboolean valid;	     /* if TRUE, such a prefix does exist in mu */
	gboolean print;	     /* if TRUE, print terms of this type */
	const char* name;    /* name of the field, as returned by mu_msg_field_name() */
	char shortcut;       /* shortcut, as returned by mu_msg_field_shortcut() */
	MuMsgFieldType type; /* as returned by  mu_msg_field_type() */
} FieldType;
FieldType field_types[256];

static void fill_prefix_ids()
{
	memset(&field_types, 0, sizeof(field_types));
	for (MuMsgFieldId i = 0 ;i < MU_MSG_FIELD_ID_NUM ; ++i) {
		char prefix = mu_msg_field_xapian_prefix(i);
		unsigned int j = (unsigned int)prefix;
		field_types[j].valid = TRUE;
		field_types[j].print = TRUE; /* TODO: let the user choose which types to print */
		field_types[j].name = mu_msg_field_name(i);
		field_types[j].shortcut = mu_msg_field_shortcut(i);
		field_types[j].type = mu_msg_field_type(i);
	}
}


static MuError print_term(const char* term,
		          const unsigned int term_freq,
		          void *store)
{
	unsigned int prefix;
	const char* term_value;

	g_return_val_if_fail (term!=NULL, MU_ERROR_INTERNAL);
	g_return_val_if_fail (*term!=0, MU_ERROR_INTERNAL);

	prefix = (unsigned int)term[0]; /* first letter of each term is the mu xapian prefix */
	term_value = &term[1];

	g_return_val_if_fail (field_types[prefix].valid, MU_ERROR_INTERNAL);
	g_return_val_if_fail (*term_value!=0, MU_ERROR_INTERNAL);

	if (field_types[prefix].print) {
		g_print("%s\t%s\t%u\n",field_types[prefix].name,term_value,term_freq);
	}

	return MU_OK;
}

static gboolean
execute_list (MuStore *store, MuConfig *opts, GError **err)
{
	MuError foreach_err;
	foreach_err = mu_store_foreach_term(store, print_term, store, err);
	return (foreach_err==MU_OK);
}


static gboolean
format_params_valid (MuConfig *opts, GError **err)
{
	switch (opts->format) {
	case MU_CONFIG_FORMAT_EXEC:
		break;
	case MU_CONFIG_FORMAT_PLAIN:
		break;
	default:  mu_util_g_set_error (err, MU_ERROR_IN_PARAMETERS,
				       "invalid output format %s",
			 opts->formatstr ? opts->formatstr : "<none>");
		return FALSE;
	}
	return TRUE;
}

static gboolean
query_params_valid (MuConfig *opts, GError **err)
{
	const gchar *xpath;

	xpath = mu_runtime_path (MU_RUNTIME_PATH_XAPIANDB);

	if (mu_util_check_dir (xpath, TRUE, FALSE))
		return TRUE;

	mu_util_g_set_error (err, MU_ERROR_FILE_CANNOT_READ,
			     "'%s' is not a readable Xapian directory",
			     xpath);
	return FALSE;
}


static void
show_usage (void)
{
	g_print ("%s", "usage: mu list <search expression>\n");
}


MuError
mu_cmd_list (MuStore *store, MuConfig *opts, GError **err)
{
	g_return_val_if_fail (opts, MU_ERROR_INTERNAL);
	g_return_val_if_fail (opts->cmd == MU_CONFIG_CMD_LIST,
			      MU_ERROR_INTERNAL);

	if (opts->exec)
		opts->format = MU_CONFIG_FORMAT_EXEC; /* pseudo format */

	if (!query_params_valid (opts, err) || !format_params_valid(opts, err)) {

		if (MU_G_ERROR_CODE(err) == MU_ERROR_IN_PARAMETERS)
			show_usage ();

		return MU_G_ERROR_CODE (err);
	}

	fill_prefix_ids();

	if (!execute_list (store, opts, err))
		return MU_G_ERROR_CODE(err);
	else
		return MU_OK;
}
