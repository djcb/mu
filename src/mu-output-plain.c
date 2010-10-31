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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "mu-msg.h"
#include "mu-maildir.h"
#include "mu-index.h"
#include "mu-msg-iter.h"
#include "mu-msg-str.h"

/* #include "mu-util.h" */
/* #include "mu-util-db.h" */
/* #include "mu-cmd.h" */
#include "mu-output-plain.h"


static const gchar*
display_field (MuMsgIter *iter, const MuMsgField* field)
{
	gint64 val;

	switch (mu_msg_field_type(field)) {
	case MU_MSG_FIELD_TYPE_STRING:
		return mu_msg_iter_get_field (iter, field);

	case MU_MSG_FIELD_TYPE_INT:
	
		if (mu_msg_field_id(field) == MU_MSG_FIELD_ID_PRIO) {
			val = mu_msg_iter_get_field_numeric (iter, field);
			return mu_msg_str_prio ((MuMsgPrio)val);
		}
		
		if (mu_msg_field_id(field) == MU_MSG_FIELD_ID_FLAGS) {
			val = mu_msg_iter_get_field_numeric (iter, field);
			return mu_msg_str_flags_s ((MuMsgPrio)val);
		}

		return mu_msg_iter_get_field (iter, field); /* as string */

	case MU_MSG_FIELD_TYPE_TIME_T: 
		val = mu_msg_iter_get_field_numeric (iter, field);
		return mu_msg_str_date_s ((time_t)val);

	case MU_MSG_FIELD_TYPE_BYTESIZE: 
		val = mu_msg_iter_get_field_numeric (iter, field);
		return mu_msg_str_size_s ((time_t)val);
	default:
		g_return_val_if_reached (NULL);
	}
}


static void
print_summary (MuMsgIter *iter, size_t summary_len)
{
	const char *summ;
	MuMsg *msg;

	msg = mu_msg_iter_get_msg (iter);
	if (!msg) {
		g_warning ("%s: failed to create msg object", __FUNCTION__);
		return;
	}

	summ = mu_msg_get_summary (msg, summary_len);
	g_print ("Summary: %s\n", summ ? summ : "<none>");
	
	mu_msg_destroy (msg);
}



gboolean
mu_output_plain_row (MuMsgIter *iter, const char *fields, size_t summary_len)
{
	const char* myfields;
	int len = 0;
	
	g_return_val_if_fail (iter, FALSE);
	g_return_val_if_fail (fields, FALSE);
	g_return_val_if_fail (!mu_msg_iter_is_done(iter), FALSE);

	myfields = fields;
	while (*myfields) {
		const MuMsgField* field;
		field =	mu_msg_field_from_shortcut (*myfields);
		if (!field || ( !mu_msg_field_xapian_value (field) &&
				!mu_msg_field_xapian_contact (field)))
			len += printf ("%c", *myfields);
		else
			len += printf ("%s",
				       display_field(iter, field));
		++myfields;
	}
	
	if (len > 0)
		g_print ("\n");
	
	if (summary_len > 0)
		print_summary (iter, summary_len);
	
	return TRUE;
}

